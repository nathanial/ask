/-
  ask - A simple CLI for talking to models on OpenRouter
-/

import Parlance
import Oracle

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
  Cmd.boolFlag "list-models" (short := some 'l')
    (description := "List common model names")
  Cmd.arg "prompt" (argType := .string) (required := false)
    (description := "The prompt to send")

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

    -- Get model
    let model := result.getString! "model" defaultModel

    -- Get prompt (from arg or stdin)
    let prompt ← match result.getString "prompt" with
      | some p => pure p
      | none => do
        -- Read from stdin
        let stdin ← IO.getStdin
        let content ← stdin.readToEnd
        if content.isEmpty then
          printError "No prompt provided. Use: ask \"your prompt\" or pipe to stdin"
          return 1
        pure content.trim

    -- Create client and send request
    let client := Client.withModel apiKey model
    match ← client.promptStream prompt with
    | .ok _ =>
      IO.println ""  -- Newline after streamed content
      return 0
    | .error e =>
      printError s!"API error: {e}"
      return 1
