/-
  ask - A simple CLI for talking to models on OpenRouter
-/

import Parlance
import Parlance.Repl
import Oracle
import Wisp

open Parlance
open Oracle

def defaultModel : String := "anthropic/claude-sonnet-4"

def commonModels : List String := [
  "anthropic/claude-sonnet-4",
  "anthropic/claude-haiku",
  "openai/gpt-4o",
  "openai/gpt-4o-mini",
  "google/gemini-2.0-flash-001",
  "meta-llama/llama-3.3-70b-instruct"
]

def cmd : Command := command "ask" do
  Cmd.version "0.1.0"
  Cmd.description "Talk to AI models on OpenRouter"
  Cmd.flag "model" (short := some 'm')
    (argType := .string)
    (description := s!"Model to use (default: {defaultModel})")
  Cmd.boolFlag "interactive" (short := some 'i')
    (description := "Start interactive REPL for multi-turn conversation")
  Cmd.flag "system" (short := some 's')
    (argType := .string)
    (description := "System prompt to use")
  Cmd.boolFlag "list-models" (short := some 'l')
    (description := "List common model names")
  Cmd.arg "prompt" (argType := .string) (required := false)
    (description := "The prompt to send")

/-- REPL state for multi-turn conversations -/
structure ReplState where
  client : Client
  history : Array Message
  model : String

/-- REPL help text -/
def replHelpText : String :=
  "Commands:\n" ++
  "  /quit, /exit, /q  - Exit the REPL\n" ++
  "  /clear            - Clear conversation history\n" ++
  "  /model <name>     - Switch to a different model\n" ++
  "  /history          - Show conversation history\n" ++
  "  /help, /?         - Show this help\n" ++
  "\n" ++
  "Editing shortcuts:\n" ++
  "  Ctrl+A/E          - Start/end of line\n" ++
  "  Ctrl+K            - Delete to end\n" ++
  "  Ctrl+U            - Delete to start\n" ++
  "  Ctrl+W            - Delete word\n" ++
  "  Ctrl+D            - Exit (on empty line)"

/-- Handle a slash command. Returns (new state, should exit) -/
def handleSlashCommand (state : ReplState) (cmd : String) : IO (ReplState × Bool) := do
  let parts := cmd.splitOn " "
  match parts.head? with
  | some "/quit" | some "/exit" | some "/q" =>
    IO.println "Goodbye!"
    pure (state, true)
  | some "/clear" =>
    -- Keep system message if present
    let newHistory := state.history.filter (·.role == .system)
    printSuccess "Conversation cleared."
    pure ({ state with history := newHistory }, false)
  | some "/model" =>
    match parts[1]? with
    | some newModel =>
      let newConfig := { state.client.config with model := newModel }
      let newClient := { state.client with config := newConfig }
      printSuccess s!"Model changed to: {newModel}"
      pure ({ state with client := newClient, model := newModel }, false)
    | none =>
      printInfo s!"Current model: {state.model}"
      pure (state, false)
  | some "/history" =>
    if state.history.isEmpty then
      printInfo "No conversation history."
    else
      for msg in state.history do
        let role := match msg.role with
          | .system => "system"
          | .user => "user"
          | .assistant => "assistant"
          | .tool => "tool"
          | .developer => "developer"
        let content := if msg.content.length > 100 then
          msg.content.take 100 ++ "..."
        else msg.content
        IO.println s!"[{role}] {content}"
    pure (state, false)
  | some "/help" | some "/?" =>
    IO.println replHelpText
    pure (state, false)
  | _ =>
    printWarning s!"Unknown command: {cmd}. Type /help for commands."
    pure (state, false)

/-- Run the interactive REPL loop -/
partial def runRepl (client : Client) (systemPrompt : Option String) (model : String) : IO Unit := do
  -- Initialize history with system prompt if provided
  let initialHistory := match systemPrompt with
    | some sys => #[Message.system sys]
    | none => #[]

  let stateRef ← IO.mkRef (ReplState.mk client initialHistory model)

  printInfo s!"Interactive mode (model: {model})"
  IO.println "Type /help for commands, Ctrl+D to exit.\n"

  Repl.simple "ask> " fun input => do
    let state ← stateRef.get

    -- Handle slash commands
    if input.startsWith "/" then
      let (newState, shouldExit) ← handleSlashCommand state input.trim
      stateRef.set newState
      pure shouldExit
    else
      -- Add user message to history
      let newHistory := state.history.push (Message.user input)

      -- Send to API
      match ← state.client.completeStream newHistory with
      | .ok stream =>
        -- Stream and collect response
        let response ← stream.printContent
        IO.println ""  -- Blank line for readability

        -- Add assistant response to history
        let finalHistory := newHistory.push (Message.assistant response)
        stateRef.set { state with history := finalHistory }
        pure false

      | .error e =>
        printError s!"API error: {e}"
        pure false

def main (args : List String) : IO UInt32 := do
  -- Handle shell completion
  if let some action := Completion.handleCompletionRequest cmd "ask" args then
    action
    return 0

  match parse cmd args with
  | .error .helpRequested =>
    IO.println cmd.helpText
    return 0
  | .error e =>
    printError (ParseError.toString e)
    return 1
  | .ok result =>
    -- List models mode
    if result.hasFlag "list-models" then
      printInfo "Common OpenRouter models:"
      for m in commonModels do
        IO.println s!"  {m}"
      return 0

    -- Get API key
    let some apiKey ← IO.getEnv "OPENROUTER_API_KEY"
      | do printError "OPENROUTER_API_KEY environment variable not set"
           return 1

    -- Get model and system prompt
    let model := result.getString! "model" defaultModel
    let systemPrompt := result.getString "system"

    -- Create client
    let client := Client.withModel apiKey model

    -- Interactive mode
    if result.hasFlag "interactive" then
      runRepl client systemPrompt model
      Wisp.HTTP.Client.shutdown
      return 0

    -- Single-turn mode: Get prompt (from arg or stdin)
    let prompt ← match result.getString "prompt" with
      | some p => pure p
      | none => do
        -- Read from stdin
        let stdin ← IO.getStdin
        let content ← stdin.readToEnd
        if content.isEmpty then
          printError "No prompt provided. Use: ask \"your prompt\", pipe to stdin, or use -i for interactive mode"
          return 1
        pure content.trim

    -- Send single request
    let exitCode ← match ← client.promptStream prompt systemPrompt with
      | .ok _ =>
        IO.println ""  -- Newline after streamed content
        pure (0 : UInt32)
      | .error e =>
        printError s!"API error: {e}"
        pure (1 : UInt32)
    -- Shutdown the HTTP client manager to allow clean exit
    Wisp.HTTP.Client.shutdown
    return exitCode
