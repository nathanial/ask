/-
  ask - A simple CLI for talking to models on OpenRouter
-/

import Parlance
import Parlance.Repl
import Oracle
import Wisp
import Chronicle

open Parlance
open Oracle

/-- Print streaming content with markdown rendering and optional word wrapping.
    Returns (raw content, chunk count) for history and debugging. -/
partial def printStreamMarkdown (stream : ChatStream) (wrapWidth : Option Nat := none)
    : IO (String × Nat) := do
  let mdStateRef ← IO.mkRef Markdown.State.new
  let wrapStateRef ← IO.mkRef (wrapWidth.map fun w => Wrap.State.new w)
  let contentRef ← IO.mkRef ""
  let countRef ← IO.mkRef 0
  let stdout ← IO.getStdout
  stream.forEach fun chunk => do
    countRef.modify (· + 1)
    if let some content := chunk.content then
      -- Accumulate raw content for history
      contentRef.modify (· ++ content)
      -- Feed through markdown parser
      let mdState ← mdStateRef.get
      let (newMdState, styledOutput) := Markdown.feed mdState content
      mdStateRef.set newMdState
      -- Optionally wrap the styled output
      let wrapState? ← wrapStateRef.get
      let output ← match wrapState? with
        | some wrapState =>
          let (newWrapState, wrapped) := Wrap.feed wrapState styledOutput
          wrapStateRef.set (some newWrapState)
          pure wrapped
        | none => pure styledOutput
      IO.print output
      stdout.flush
  -- Flush remaining markdown buffer
  let finalMdState ← mdStateRef.get
  let styledRemainder := Markdown.finish finalMdState
  -- Flush remaining wrap buffer
  let wrapState? ← wrapStateRef.get
  let finalOutput := match wrapState? with
    | some wrapState =>
      let (finalWrapState, wrapped) := Wrap.feed wrapState styledRemainder
      wrapped ++ Wrap.finish finalWrapState
    | none => styledRemainder
  IO.print finalOutput
  IO.println ""
  let content ← contentRef.get
  let count ← countRef.get
  pure (content, count)

def defaultModel : String := "google/gemini-3-flash-preview:online"

def commonModels : List String := [
  "google/gemini-3-flash-preview:online",
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
  Cmd.flag "log"
    (argType := .string)
    (description := "Enable logging to file (e.g., ~/.ask/ask.log)")
  Cmd.flag "log-level"
    (argType := .string)
    (description := "Log level: trace, debug, info, warn, error (default: info)")
  Cmd.boolFlag "list-models" (short := some 'l')
    (description := "List common model names")
  Cmd.boolFlag "raw" (short := some 'r')
    (description := "Disable markdown rendering (output raw text)")
  Cmd.flag "width" (short := some 'w')
    (argType := .int)
    (description := "Wrap lines at specified width (default: 80, 0 to disable)")
  Cmd.arg "prompt" (argType := .string) (required := false)
    (description := "The prompt to send")

/-- Parse log level string to Chronicle.Level -/
def parseLogLevel (s : Option String) : Chronicle.Level :=
  match s with
  | some "trace" => .trace
  | some "debug" => .debug
  | some "info" => .info
  | some "warn" => .warn
  | some "error" => .error
  | _ => .info  -- default

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
partial def runRepl (client : Client) (systemPrompt : Option String) (model : String)
    (rawMode : Bool := false) (wrapWidth : Option Nat := none)
    (logger : Option Chronicle.Logger := none) : IO Unit := do
  -- Log REPL start
  if let some l := logger then
    l.info s!"Interactive mode started (model: {model})"

  -- Initialize history with system prompt if provided
  let initialHistory := match systemPrompt with
    | some sys => #[Message.system sys]
    | none => #[]

  let stateRef ← IO.mkRef (ReplState.mk client initialHistory model)

  printInfo s!"Interactive mode (model: {model})"
  IO.println "Type /help for commands, Ctrl+D to exit.\n"

  Repl.simple "ask> " logger fun input => do
    let state ← stateRef.get

    -- Handle slash commands
    if input.startsWith "/" then
      let (newState, shouldExit) ← handleSlashCommand state input.trim
      stateRef.set newState
      pure shouldExit
    else
      -- Add user message to history
      let newHistory := state.history.push (Message.user input)

      -- Log the request we're about to send
      if let some l := logger then
        l.debug s!"Sending request with {newHistory.size} messages"
        let mut i := 0
        for msg in newHistory do
          let role := match msg.role with
            | .system => "system"
            | .user => "user"
            | .assistant => "assistant"
            | .tool => "tool"
            | .developer => "developer"
          let preview := if msg.content.length > 80 then
            msg.content.take 80 ++ "..."
          else
            msg.content
          let preview := preview.replace "\n" " "
          l.trace s!"  [{i}] {role}: {preview}"
          i := i + 1

      -- Send to API
      match ← state.client.completeStream newHistory with
      | .ok stream =>
        -- Stream and collect response (with chunk count for debugging)
        let (response, chunkCount) ← if rawMode then
          stream.printContentWithCount
        else
          printStreamMarkdown stream wrapWidth

        -- Log the result
        if let some l := logger then
          l.debug s!"Stream completed: {chunkCount} chunks, {response.length} chars"

        -- Only add non-empty responses to history
        if response.isEmpty then
          printWarning s!"Received empty response from API (chunks: {chunkCount})"
          -- Don't add empty response to history, keep history as-is
          stateRef.set { state with history := newHistory }
        else
          IO.println ""  -- Blank line for readability
          -- Add assistant response to history
          let finalHistory := newHistory.push (Message.assistant response)
          stateRef.set { state with history := finalHistory }
        pure false

      | .error e =>
        if let some l := logger then
          l.error s!"API error: {e}"
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

    -- Get logging options
    let logPath := result.getString "log"
    let logLevel := parseLogLevel (result.getString "log-level")

    -- Helper to run the actual logic with optional logger
    let runWithLogger (logger : Option Chronicle.Logger) : IO UInt32 := do
      -- Log startup
      if let some l := logger then
        l.info s!"ask started (model: {model})"

      -- Create client with optional logger
      let baseClient := Client.withModel apiKey model
      let client := match logger with
        | some l => { baseClient with config := baseClient.config.setLogger l }
        | none => baseClient

      -- Check raw mode and wrap width flags
      let rawMode := result.hasFlag "raw"
      let wrapWidth : Option Nat := match result.getInt "width" with
        | some w => if w.toNat == 0 then none else some w.toNat
        | none => some 80  -- Default to 80 columns

      -- Interactive mode
      if result.hasFlag "interactive" then
        runRepl client systemPrompt model rawMode wrapWidth logger
        if let some l := logger then
          l.info "ask exiting"
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

      -- Build messages
      let messages := match systemPrompt with
        | some sys => #[Message.system sys, Message.user prompt]
        | none => #[Message.user prompt]

      -- Send single request
      let exitCode ← match ← client.completeStream messages with
        | .ok stream =>
          if rawMode then
            let _ ← stream.printContent
          else
            let _ ← printStreamMarkdown stream wrapWidth
          pure (0 : UInt32)
        | .error e =>
          printError s!"API error: {e}"
          pure (1 : UInt32)

      if let some l := logger then
        l.info "ask exiting"

      -- Shutdown the HTTP client manager to allow clean exit
      Wisp.HTTP.Client.shutdown
      return exitCode

    -- Run with or without logging
    match logPath with
    | some path =>
      let config := Chronicle.Config.default path |>.withLevel logLevel
      Chronicle.Logger.withLogger config fun logger => do
        runWithLogger (some logger)
    | none =>
      runWithLogger none
