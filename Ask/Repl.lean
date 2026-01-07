/-
  Ask.Repl - Interactive REPL for multi-turn conversations
-/

import Parlance
import Parlance.Repl
import Oracle
import Chronicle
import Ask.History
import Ask.Error

namespace Ask.Repl

open Parlance
open Oracle

/-- REPL state for multi-turn conversations -/
structure State where
  client : Client
  history : Array Message
  model : String
  sessionPath : Option System.FilePath := none  -- Current save file path
  sessionCreatedAt : Option Nat := none         -- When session started (for metadata)

/-- REPL help text -/
def helpText : String :=
  "Commands:\n" ++
  "  /quit, /exit, /q  - Exit the REPL\n" ++
  "  /clear            - Clear conversation history\n" ++
  "  /model <name>     - Switch to a different model\n" ++
  "  /history          - Show conversation history\n" ++
  "  /save [name]      - Save conversation to file\n" ++
  "  /load [name]      - Load conversation from file\n" ++
  "  /list             - List saved conversations\n" ++
  "  /help, /?         - Show this help\n" ++
  "\n" ++
  "Editing shortcuts:\n" ++
  "  Ctrl+A/E          - Start/end of line\n" ++
  "  Ctrl+K            - Delete to end\n" ++
  "  Ctrl+U            - Delete to start\n" ++
  "  Ctrl+W            - Delete word\n" ++
  "  Ctrl+D            - Exit (on empty line)"

/-- Handle a slash command. Returns (new state, should exit) -/
def handleSlashCommand (state : State) (cmd : String)
    (logger : Option Chronicle.Logger := none) (autoSave : Bool := true) : IO (State × Bool) := do
  let parts := cmd.splitOn " "
  match parts.head? with
  | some "/quit" | some "/exit" | some "/q" =>
    -- Auto-save before exit if enabled and there's meaningful history
    if autoSave && state.history.size > 1 then  -- More than just system prompt
      History.ensureHistoryDir
      let conv ← History.buildConversation state.model state.history state.sessionCreatedAt
      let filename ← History.generateFilename state.model
      let path ← History.resolveHistoryPath filename
      match ← History.saveConversation conv path with
      | .ok () => printInfo s!"Auto-saved to: {path}"
      | .error e => Ask.Error.reportWarning logger s!"Auto-save failed: {e}"
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
        let contentStr := msg.content.asString
        let content := if contentStr.length > 100 then
          contentStr.take 100 ++ "..."
        else contentStr
        IO.println s!"[{role}] {content}"
    pure (state, false)

  | some "/save" =>
    if state.history.size <= 1 then
      printWarning "Nothing to save (no conversation history)."
      pure (state, false)
    else
      History.ensureHistoryDir
      -- Use provided filename or generate one
      let filename ← match parts[1]? with
        | some name => pure name
        | none => History.generateFilename state.model
      let path ← History.resolveHistoryPath filename
      let conv ← History.buildConversation state.model state.history state.sessionCreatedAt
      match ← History.saveConversation conv path with
      | .ok () =>
        printSuccess s!"Saved to: {path}"
        pure ({ state with sessionPath := some path }, false)
      | .error e =>
        Ask.Error.reportError logger s!"Save failed: {e}"
        pure (state, false)

  | some "/load" =>
    match parts[1]? with
    | some filename =>
      let path ← History.resolveHistoryPath filename
      match ← History.loadConversation path with
      | .ok conv =>
        printSuccess s!"Loaded {conv.messages.size} messages from {filename}"
        pure ({
          state with
          history := conv.messages
          model := conv.metadata.model
          sessionPath := some path
          sessionCreatedAt := some conv.metadata.createdAt
        }, false)
      | .error e =>
        Ask.Error.reportError logger s!"Load failed: {e}"
        pure (state, false)
    | none =>
      -- Show available files
      let files ← History.listHistoryFiles
      if files.isEmpty then
        printInfo "No saved conversations. Use /save to save the current conversation."
      else
        printInfo "Available conversations (use /load <filename>):"
        for info in files do
          IO.println s!"  {info.filename}"
          IO.println s!"    Model: {info.model}, Messages: {info.messageCount}"
      pure (state, false)

  | some "/list" =>
    let files ← History.listHistoryFiles
    if files.isEmpty then
      printInfo "No saved conversations in ~/.ask/history/"
    else
      printInfo "Saved conversations:"
      for info in files do
        IO.println s!"  {info.filename}"
        IO.println s!"    Model: {info.model}"
        IO.println s!"    Messages: {info.messageCount}"
    pure (state, false)

  | some "/help" | some "/?" =>
    IO.println helpText
    pure (state, false)
  | _ =>
    Ask.Error.reportWarning logger s!"Unknown command: {cmd}. Type /help for commands."
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
  autoSave : Bool := true  -- Auto-save conversation on exit
  initialHistory : Option (Array Message) := none  -- Load from saved conversation
  sessionCreatedAt : Option Nat := none  -- When session started (for loaded conversations)
  /-- Function to print streaming content with optional markdown/wrapping.
      Returns (raw content, chunk count). -/
  printStream : ChatStream → Option Nat → IO (String × Nat)

/-- Run the interactive REPL loop -/
partial def run (cfg : Config) : IO Unit := do
  -- Log REPL start
  if let some l := cfg.logger then
    l.info s!"Interactive mode started (model: {cfg.model})"

  -- Use provided initial history or create from system prompt
  let initialHistory := match cfg.initialHistory with
    | some hist => hist
    | none => match cfg.systemPrompt with
      | some sys => #[Message.system sys]
      | none => #[]

  -- Use provided session time or get current time
  let sessionCreatedAt ← match cfg.sessionCreatedAt with
    | some t => pure t
    | none => History.nowSeconds

  let stateRef ← IO.mkRef {
    client := cfg.client
    history := initialHistory
    model := cfg.model
    sessionPath := none
    sessionCreatedAt := some sessionCreatedAt
  }

  printInfo s!"Interactive mode (model: {cfg.model})"
  IO.println "Type /help for commands, Ctrl+D to exit.\n"

  Repl.simple "ask> " cfg.logger fun input => do
    let state ← stateRef.get

    -- Handle slash commands
    if input.startsWith "/" then
      let (newState, shouldExit) ← handleSlashCommand state input.trim cfg.logger cfg.autoSave
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
          let msgStr := msg.content.asString
          let preview := if msgStr.length > 80 then
            msgStr.take 80 ++ "..."
          else
            msgStr
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
          Ask.Error.reportWarning cfg.logger s!"Received empty response from API (chunks: {chunkCount})"
          -- Don't add empty response to history, keep history as-is
          stateRef.set { state with history := newHistory }
        else
          IO.println ""  -- Blank line for readability
          -- Add assistant response to history
          let finalHistory := newHistory.push (Message.assistant response)
          stateRef.set { state with history := finalHistory }
        pure false

      | .error e =>
        Ask.Error.reportError cfg.logger s!"API error: {e}"
        pure false

end Ask.Repl
