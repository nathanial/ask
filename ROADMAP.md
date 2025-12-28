# Roadmap

This document outlines potential improvements, new features, and code cleanup opportunities for the `ask` CLI tool.

## Feature Proposals

### [Priority: High] Conversation History Persistence
**Description:** Save and load conversation history to/from files, enabling users to resume conversations across sessions.
**Rationale:** Users often want to continue multi-turn conversations later or share conversation context. This is a common expectation for chat CLI tools.
**Affected Files:** `Main.lean` (new slash commands), new file for persistence logic
**Estimated Effort:** Medium
**Dependencies:** None
**Implementation Notes:**
- Add `/save <filename>` command to save current history
- Add `/load <filename>` command to load previous history
- Consider auto-save to a default location (e.g., `~/.ask/history/`)
- Use JSON format for portability

### [Priority: High] Temperature and Max Tokens Configuration
**Description:** Add CLI flags for controlling generation parameters like temperature and max tokens.
**Rationale:** The Oracle library supports these parameters (see `ChatRequest.withTemperature`, `ChatRequest.withMaxTokens`), but ask does not expose them. Power users need control over creativity vs. determinism and response length.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small
**Dependencies:** None
**Implementation Notes:**
- Add `--temperature` / `-t` flag (Float, 0.0-2.0)
- Add `--max-tokens` flag (Nat)
- Update both single-turn and REPL modes

### [Priority: High] Code Block Rendering
**Description:** Extend markdown renderer to properly handle fenced code blocks with optional syntax highlighting.
**Rationale:** LLM responses frequently include code blocks. The current markdown parser (`Parlance.Markdown`) handles inline code but not fenced blocks (triple backticks). Code blocks are essential for a CLI tool targeting developers.
**Affected Files:** Would require enhancement to `parlance/Parlance/Markdown.lean`, then updates to `Main.lean` to use it
**Estimated Effort:** Medium
**Dependencies:** Requires parlance library enhancement
**Implementation Notes:**
- Parse ``` and ~~~ fenced blocks
- Optional: Add basic syntax highlighting using ANSI colors
- Handle language specifier (```python, ```lean, etc.)

### [Priority: Medium] Configuration File Support
**Description:** Support a configuration file (e.g., `~/.askrc` or `~/.config/ask/config.json`) for default settings.
**Rationale:** Users frequently use the same model, system prompt, or settings. A config file eliminates repetitive command-line arguments.
**Affected Files:** `Main.lean`, new configuration module
**Estimated Effort:** Medium
**Dependencies:** None
**Implementation Notes:**
- JSON or TOML format
- Settings: default model, system prompt, temperature, max tokens, wrap width, log path
- Command-line flags should override config file settings

### [Priority: Medium] Model Completion and Search
**Description:** Provide interactive model selection with completion/search from OpenRouter's available models.
**Rationale:** OpenRouter supports hundreds of models. The current `--list-models` shows only 7 hardcoded common models. Users need discovery.
**Affected Files:** `Main.lean`
**Estimated Effort:** Medium
**Dependencies:** May need Oracle library enhancement to query model list API
**Implementation Notes:**
- Fetch available models from OpenRouter API
- Add `/models` REPL command to list/search
- Consider caching model list locally

### [Priority: Medium] Streaming Token Usage Display
**Description:** Display token usage statistics after each response (input tokens, output tokens, cost estimate).
**Rationale:** The Oracle library's `ChatResponse` includes `Usage` data (prompt_tokens, completion_tokens, total_tokens). Cost awareness is important for users.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small
**Dependencies:** None
**Implementation Notes:**
- Add `--usage` flag to display token counts
- Parse usage from stream end metadata
- Optional: Calculate and display estimated cost based on model pricing

### [Priority: Medium] Multi-line Input in REPL
**Description:** Support multi-line input in interactive mode for longer prompts or code snippets.
**Rationale:** Many prompts span multiple lines (code, structured content). Current REPL is single-line only.
**Affected Files:** `Main.lean`, possibly `parlance/Parlance/Repl`
**Estimated Effort:** Medium
**Dependencies:** May require parlance enhancement
**Implementation Notes:**
- Use a delimiter (e.g., `<<<` to start, `>>>` to end)
- Or use a continuation character (trailing `\`)
- Alternative: Bracket-aware input (don't submit until brackets balanced)

### [Priority: Medium] File Context Injection
**Description:** Add flag to include file contents as context in the prompt.
**Rationale:** Common use case: "explain this code" or "refactor this file". Currently requires shell piping.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small
**Dependencies:** None
**Implementation Notes:**
- Add `--file` / `-f` flag accepting one or more file paths
- Format: inject file content with filename header before user prompt
- Support glob patterns for multiple files

### [Priority: Low] Export Conversation to Markdown
**Description:** Export full conversation to a formatted markdown file.
**Rationale:** Users may want to save conversations for documentation or sharing outside the CLI.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small
**Dependencies:** History persistence feature
**Implementation Notes:**
- Add `/export <filename>` REPL command
- Format with proper user/assistant markers
- Include model name and timestamp

### [Priority: Low] Response Timing Display
**Description:** Show time-to-first-token and total response time.
**Rationale:** Useful for benchmarking models and debugging latency issues.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small
**Dependencies:** Would benefit from Chronos library integration
**Implementation Notes:**
- Track time before stream starts
- Track time when first chunk arrives
- Track time when stream completes
- Display with `--timing` flag or in verbose mode

### [Priority: Low] Prompt Templates
**Description:** Support reusable prompt templates with variable substitution.
**Rationale:** Power users often reuse similar prompt structures (e.g., "translate to {lang}:", "explain like I'm {audience}:").
**Affected Files:** `Main.lean`, new template module
**Estimated Effort:** Medium
**Dependencies:** Configuration file support
**Implementation Notes:**
- Store templates in config file or `~/.ask/templates/`
- Simple {{variable}} substitution
- Add `--template` flag

### [Priority: Low] Retry and Backoff for API Errors
**Description:** Automatic retry with exponential backoff for transient errors (rate limits, timeouts).
**Rationale:** The Oracle library detects rate limit errors (429) with Retry-After header, but ask doesn't handle retries.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small
**Dependencies:** None
**Implementation Notes:**
- Respect `Retry-After` header from `rateLimitError`
- Configurable max retries
- Exponential backoff for other transient errors

## Code Improvements

### [Priority: High] Extract REPL Logic into Separate Module
**Current State:** All REPL logic (state, slash commands, loop) is in `Main.lean`, making it 358 lines.
**Proposed Change:** Extract `ReplState`, `handleSlashCommand`, `runRepl`, and related code into a dedicated `Ask/Repl.lean` module.
**Benefits:** Better separation of concerns, easier testing, more maintainable code.
**Affected Files:** `Main.lean` (split into `Main.lean` and `Ask/Repl.lean`)
**Estimated Effort:** Small

### [Priority: High] Consistent Error Handling Pattern
**Current State:** Error handling mixes printError calls with return codes in different patterns.
**Proposed Change:** Create a consistent error handling monad or pattern that handles logging, printing, and exit codes uniformly.
**Benefits:** Cleaner code, consistent user experience, easier debugging.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small

### [Priority: Medium] Simplify printStreamMarkdown Function
**Current State:** The `printStreamMarkdown` function at lines 16-56 uses multiple `IO.mkRef` for state management, making it somewhat complex.
**Proposed Change:** Consider using a single state structure or StateT monad transformer for cleaner state threading.
**Benefits:** More idiomatic Lean code, easier to understand and modify.
**Affected Files:** `Main.lean` (lines 16-56)
**Estimated Effort:** Small

### [Priority: Medium] Model List as External Resource
**Current State:** `commonModels` is a hardcoded list (lines 60-68) that will become outdated.
**Proposed Change:** Either fetch from API or load from an updatable configuration file.
**Benefits:** Users get access to latest models without code updates.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small

### [Priority: Low] Type-Safe Command Dispatch
**Current State:** `handleSlashCommand` uses string pattern matching on command names (lines 130-172).
**Proposed Change:** Define an inductive type for REPL commands and parse into that type before dispatch.
**Benefits:** Compiler-checked exhaustiveness, easier to add new commands, better documentation.
**Affected Files:** `Main.lean`
**Estimated Effort:** Small

## Code Cleanup

### [Priority: High] Update Documentation in CLAUDE.md
**Issue:** CLAUDE.md doesn't document all current features (markdown rendering, word wrapping, REPL mode, logging).
**Location:** `/Users/Shared/Projects/lean-workspace/ask/CLAUDE.md`
**Action Required:** Update usage examples to include all flags and features:
- Interactive mode (`-i`)
- Raw mode (`-r`)
- Width control (`-w`)
- Logging (`--log`, `--log-level`)
- System prompt (`-s`)
**Estimated Effort:** Small

### [Priority: Medium] Add Tests
**Issue:** The ask project has no test suite despite depending on crucible.
**Location:** Missing `Tests/` directory
**Action Required:**
- Add `lake test` target to lakefile.lean
- Test slash command parsing
- Test markdown rendering integration
- Test argument parsing edge cases
**Estimated Effort:** Medium

### [Priority: Medium] Partial Function Annotation
**Issue:** `printStreamMarkdown` (line 16) and `runRepl` (line 175) are marked `partial` due to their use of streaming/looping, which is appropriate.
**Location:** `Main.lean` lines 16, 175
**Action Required:** Document why these functions are partial (infinite streams, REPL loop) for future maintainers. Consider if termination proofs are possible.
**Estimated Effort:** Small

### [Priority: Low] Remove Redundant Model Definition
**Issue:** `defaultModel` is defined separately from `commonModels` (lines 58, 60), but default should be first in common list.
**Location:** `Main.lean` lines 58-68
**Action Required:** Either derive defaultModel from commonModels[0] or document why they're separate.
**Estimated Effort:** Small

### [Priority: Low] Explicit Imports
**Issue:** `Main.lean` uses `open Parlance` and `open Oracle` which imports many symbols. This could lead to future conflicts.
**Location:** `Main.lean` lines 11-12
**Action Required:** Consider more selective imports or qualified names for clarity, especially as the file grows.
**Estimated Effort:** Small

## API Enhancements

### [Priority: Medium] Structured Output Support
**Description:** Support Oracle's `ResponseFormat.jsonSchema` for structured responses.
**Rationale:** Some use cases benefit from structured JSON output that can be piped to other tools.
**Affected Files:** `Main.lean`
**Estimated Effort:** Medium
**Implementation Notes:**
- Add `--json` flag to request JSON output
- Add `--schema <file>` flag to provide JSON schema
- Disable markdown rendering when JSON output is requested

### [Priority: Low] Tool/Function Calling Support
**Description:** Enable tool/function calling capabilities from the CLI.
**Rationale:** Oracle supports tool calling (`Tool`, `ToolChoice`), which enables agentic workflows. Could integrate with shell commands.
**Affected Files:** `Main.lean`, new tool definition module
**Estimated Effort:** Large
**Implementation Notes:**
- Define built-in tools (shell execution, file read, etc.)
- Add `--tools` flag or configuration
- Handle tool call responses and execute tools
- Security considerations required

## Related Library Improvements

These improvements would benefit ask but require changes to dependency libraries:

### Parlance: Code Block Support in Markdown
- Add fenced code block parsing to `Parlance.Markdown`
- Would directly benefit ask's markdown rendering

### Parlance: REPL History
- Add command history with up/down arrow navigation
- Currently missing from `Parlance.Repl`

### Oracle: Model List API
- Add API to fetch available models from OpenRouter
- Would enable dynamic model discovery in ask

### Parlance: Progress Spinner for Non-Streaming
- Animated spinner while waiting for response
- `Parlance.Output.Spinner` exists but requires threading for animation
