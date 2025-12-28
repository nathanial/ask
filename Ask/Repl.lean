/-
  Ask.Repl - Interactive REPL for multi-turn conversations
-/

import Parlance
import Parlance.Repl
import Oracle
import Chronicle

namespace Ask.Repl

open Parlance
open Oracle

/-- REPL state for multi-turn conversations -/
structure State where
  client : Client
  history : Array Message
  model : String

/-- REPL help text -/
def helpText : String :=
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
def handleSlashCommand (state : State) (cmd : String) : IO (State × Bool) := do
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
    IO.println helpText
    pure (state, false)
  | _ =>
    printWarning s!"Unknown command: {cmd}. Type /help for commands."
    pure (state, false)

/-- Configuration for running the REPL -/
structure Config where
  client : Client
  systemPrompt : Option String
  model : String
  rawMode : Bool := false
  wrapWidth : Option Nat := none
  chatOpts : ChatOptions := {}
  logger : Option Chronicle.Logger := none
  /-- Function to print streaming content with optional markdown/wrapping.
      Returns (raw content, chunk count). -/
  printStream : ChatStream → Option Nat → IO (String × Nat)

/-- Run the interactive REPL loop -/
partial def run (cfg : Config) : IO Unit := do
  -- Log REPL start
  if let some l := cfg.logger then
    l.info s!"Interactive mode started (model: {cfg.model})"

  -- Initialize history with system prompt if provided
  let initialHistory := match cfg.systemPrompt with
    | some sys => #[Message.system sys]
    | none => #[]

  let stateRef ← IO.mkRef (State.mk cfg.client initialHistory cfg.model)

  printInfo s!"Interactive mode (model: {cfg.model})"
  IO.println "Type /help for commands, Ctrl+D to exit.\n"

  Repl.simple "ask> " cfg.logger fun input => do
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
      if let some l := cfg.logger then
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
      match ← state.client.completeStream newHistory cfg.chatOpts with
      | .ok stream =>
        -- Stream and collect response (with chunk count for debugging)
        let (response, chunkCount) ← if cfg.rawMode then
          stream.printContentWithCount
        else
          cfg.printStream stream cfg.wrapWidth

        -- Log the result
        if let some l := cfg.logger then
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
        if let some l := cfg.logger then
          l.error s!"API error: {e}"
        printError s!"API error: {e}"
        pure false

end Ask.Repl
