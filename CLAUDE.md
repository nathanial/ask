# CLAUDE.md

## Overview

**ask** is a minimal CLI tool for talking to AI models on OpenRouter. Built with parlance (CLI library) and oracle (OpenRouter client).

## Build

```bash
lake update && lake build
```

## Usage

```bash
# Simple prompt
ask "What is the capital of France?"

# Specify model
ask -m openai/gpt-4o "Write a haiku"

# Pipe from stdin
cat document.txt | ask "Summarize this:"

# Interactive multi-turn conversation
ask -i
ask -i -m anthropic/claude-sonnet-4

# With system prompt
ask -s "You are a helpful coding assistant" "Explain recursion"

# List common models
ask --list-models

# Help
ask --help
```

## Options

| Flag | Description |
|------|-------------|
| `-m, --model <MODEL>` | Model to use (default: google/gemini-3-flash-preview:online) |
| `-i, --interactive` | Start interactive REPL for multi-turn conversation |
| `-s, --system <PROMPT>` | System prompt to use |
| `-t, --temperature <FLOAT>` | Sampling temperature (0.0-2.0) |
| `--max-tokens <INT>` | Maximum tokens in response |
| `-r, --raw` | Disable markdown rendering (output raw text) |
| `-w, --width <INT>` | Wrap lines at width (default: 80, 0 to disable) |
| `-l, --list-models` | List common model names |
| `--log <PATH>` | Enable logging to file |
| `--log-level <LEVEL>` | Log level: trace, debug, info, warn, error |

## Interactive Mode

In interactive mode (`-i`), use slash commands:

| Command | Description |
|---------|-------------|
| `/quit`, `/exit`, `/q` | Exit the REPL |
| `/clear` | Clear conversation history |
| `/model <name>` | Switch to a different model |
| `/history` | Show conversation history |
| `/help`, `/?` | Show help |

Keyboard shortcuts:
- `Ctrl+A/E` - Start/end of line
- `Ctrl+K` - Delete to end
- `Ctrl+U` - Delete to start
- `Ctrl+W` - Delete word
- `Ctrl+D` - Exit (on empty line)

## Features

- **Streaming responses** - See output as it's generated
- **Markdown rendering** - Bold, italic, code, headers, and clickable links
- **Word wrapping** - Automatically wraps at 80 columns (configurable)
- **Multi-turn conversations** - Interactive mode with history
- **Temperature control** - Adjust creativity vs determinism
- **Logging** - Debug with file-based logging

## Environment

- `OPENROUTER_API_KEY` - Required. Your OpenRouter API key.

## Dependencies

- **parlance** - CLI argument parsing, styled output, shell completion, markdown rendering
- **oracle** - OpenRouter API client with streaming support
- **chronicle** - File-based logging
