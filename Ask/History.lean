/-
  Ask.History - Conversation history persistence
  Save and load conversations to/from JSON files.
-/

import Oracle
import Lean.Data.Json

namespace Ask.History

open Oracle
open Lean Json

/-- Metadata about a saved conversation -/
structure ConversationMetadata where
  model : String
  createdAt : Nat        -- Unix timestamp (seconds)
  updatedAt : Nat        -- Unix timestamp (seconds)
  messageCount : Nat
  systemPrompt : Option String := none
  deriving Repr, Inhabited

/-- A saved conversation with metadata and messages -/
structure SavedConversation where
  version : Nat := 1     -- Format version for future compatibility
  metadata : ConversationMetadata
  messages : Array Message
  deriving Repr, Inhabited

-- JSON instances for ConversationMetadata
def ConversationMetadata.toJson (m : ConversationMetadata) : Json :=
  Json.mkObj [
    ("model", Json.str m.model),
    ("createdAt", Lean.toJson m.createdAt),
    ("updatedAt", Lean.toJson m.updatedAt),
    ("messageCount", Lean.toJson m.messageCount),
    ("systemPrompt", match m.systemPrompt with
      | some s => Json.str s
      | none => Json.null)
  ]

instance : ToJson ConversationMetadata := ⟨ConversationMetadata.toJson⟩

def ConversationMetadata.fromJson? (json : Json) : Except String ConversationMetadata := do
  let model ← json.getObjValAs? String "model"
  let createdAt ← json.getObjValAs? Nat "createdAt"
  let updatedAt ← json.getObjValAs? Nat "updatedAt"
  let messageCount ← json.getObjValAs? Nat "messageCount"
  let systemPrompt := json.getObjValAs? String "systemPrompt" |>.toOption
  pure { model, createdAt, updatedAt, messageCount, systemPrompt }

instance : FromJson ConversationMetadata := ⟨ConversationMetadata.fromJson?⟩

-- JSON instances for SavedConversation
def SavedConversation.toJson (conv : SavedConversation) : Json :=
  Json.mkObj [
    ("version", Lean.toJson conv.version),
    ("metadata", Lean.toJson conv.metadata),
    ("messages", Lean.toJson conv.messages)
  ]

instance : ToJson SavedConversation := ⟨SavedConversation.toJson⟩

def SavedConversation.fromJson? (json : Json) : Except String SavedConversation := do
  let version ← json.getObjValAs? Nat "version"
  let metadata ← json.getObjValAs? ConversationMetadata "metadata"
  let messages ← json.getObjValAs? (Array Message) "messages"
  pure { version, metadata, messages }

instance : FromJson SavedConversation := ⟨SavedConversation.fromJson?⟩

/-- Get current Unix timestamp in seconds -/
def nowSeconds : IO Nat := do
  let ms ← IO.monoMsNow
  -- Note: monoMsNow is monotonic time, not wall clock time
  -- For wall clock, we use a workaround via system time
  let stdout ← IO.Process.output { cmd := "date", args := #["+%s"] }
  match stdout.stdout.trim.toNat? with
  | some n => pure n
  | none => pure (ms / 1000)  -- Fallback to monotonic

/-- Get the history directory path (~/.ask/history/) -/
def getHistoryDir : IO System.FilePath := do
  if let some home ← IO.getEnv "HOME" then
    pure (home ++ "/.ask/history")
  else
    -- Fallback to current directory
    pure ".ask/history"

/-- Ensure the history directory exists -/
def ensureHistoryDir : IO Unit := do
  let dir ← getHistoryDir
  IO.FS.createDirAll dir

/-- Filter characters in a string by a predicate -/
private def filterChars (s : String) (p : Char → Bool) : String :=
  String.mk (s.toList.filter p)

/-- Convert model name to filesystem-safe slug -/
def modelToSlug (model : String) : String :=
  -- "google/gemini-3-flash-preview" -> "gemini-flash"
  -- "anthropic/claude-sonnet-4" -> "claude-sonnet"
  let parts := model.splitOn "/"
  let name := parts.getLast!
  -- Take first two parts separated by dash, remove version suffixes
  let nameParts := name.splitOn "-"
  let slug := if nameParts.length >= 2 then
    s!"{nameParts[0]!}-{nameParts[1]!}"
  else
    nameParts[0]!
  -- Remove any remaining special characters
  filterChars slug fun c => c.isAlphanum || c == '-'

/-- Generate a filename for auto-save -/
def generateFilename (model : String) : IO String := do
  let timestamp ← nowSeconds
  let slug := modelToSlug model
  pure s!"{timestamp}-{slug}.json"

/-- Save a conversation to a file (atomic write via temp+rename) -/
def saveConversation (conv : SavedConversation) (path : System.FilePath) : IO (Except String Unit) := do
  try
    -- Create parent directories if needed
    if let some parent := path.parent then
      IO.FS.createDirAll parent

    -- Serialize to JSON
    let json := toJson conv
    let content := json.pretty

    -- Write to temp file first for atomic operation
    let tmpPath := path.toString ++ ".tmp." ++ toString (← IO.monoNanosNow)
    IO.FS.writeFile tmpPath content

    -- Atomic rename
    IO.FS.rename tmpPath path
    pure (.ok ())
  catch e =>
    pure (.error (toString e))

/-- Load a conversation from a file -/
def loadConversation (path : System.FilePath) : IO (Except String SavedConversation) := do
  try
    let content ← IO.FS.readFile path
    match Json.parse content with
    | .error e => pure (.error s!"JSON parse error: {e}")
    | .ok json =>
      match fromJson? json with
      | .error e => pure (.error s!"Invalid conversation format: {e}")
      | .ok conv => pure (.ok conv)
  catch e =>
    pure (.error (toString e))

/-- Information about a saved history file -/
structure HistoryFileInfo where
  path : System.FilePath
  filename : String
  model : String
  messageCount : Nat
  updatedAt : Nat
  deriving Repr

/-- List available history files, sorted by modification time (newest first) -/
def listHistoryFiles : IO (Array HistoryFileInfo) := do
  let dir ← getHistoryDir
  let mut files : Array HistoryFileInfo := #[]

  -- Check if directory exists
  if !(← dir.pathExists) then
    return files

  -- Read directory entries
  let entries ← dir.readDir

  for entry in entries do
    let path := entry.path
    let filename := entry.fileName

    -- Only process .json files
    if filename.endsWith ".json" then
      -- Try to load and extract metadata
      match ← loadConversation path with
      | .ok conv =>
        files := files.push {
          path := path
          filename := filename
          model := conv.metadata.model
          messageCount := conv.metadata.messageCount
          updatedAt := conv.metadata.updatedAt
        }
      | .error _ => pure ()  -- Skip files that fail to parse

  -- Sort by updatedAt, newest first
  let sorted := files.qsort fun a b => a.updatedAt > b.updatedAt
  pure sorted

/-- Build metadata from current state -/
def buildMetadata (model : String) (messages : Array Message) (createdAt : Option Nat := none)
    : IO ConversationMetadata := do
  let now ← nowSeconds
  let systemPrompt := messages.find? (·.role == .system) |>.map (·.content.asString)
  pure {
    model := model
    createdAt := createdAt.getD now
    updatedAt := now
    messageCount := messages.size
    systemPrompt := systemPrompt
  }

/-- Build a SavedConversation from current state -/
def buildConversation (model : String) (messages : Array Message) (createdAt : Option Nat := none)
    : IO SavedConversation := do
  let metadata ← buildMetadata model messages createdAt
  pure { version := 1, metadata := metadata, messages := messages }

/-- Resolve a filename to a full path in the history directory -/
def resolveHistoryPath (filename : String) : IO System.FilePath := do
  let dir ← getHistoryDir
  -- If already a full path, use it; otherwise prepend history dir
  if filename.startsWith "/" || filename.startsWith "~" then
    pure filename
  else
    -- Add .json extension if not present
    let filename := if filename.endsWith ".json" then filename else filename ++ ".json"
    pure (dir / filename)

end Ask.History
