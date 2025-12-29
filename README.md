# ask

A minimal CLI for talking to AI models on OpenRouter.

## Installation

```bash
lake build
```

## Usage

```bash
# Set your API key
export OPENROUTER_API_KEY="your-key-here"

# Simple prompt
ask "What is the capital of France?"

# Specify a model
ask -m openai/gpt-4o "Write a haiku about programming"
ask -m anthropic/claude-sonnet-4 "Explain recursion briefly"

# With system prompt
ask -s "You are a helpful coding assistant" "Explain recursion"

# Pipe from stdin
cat document.txt | ask "Summarize this document"
echo "Fix this code: def foo(" | ask

# Interactive multi-turn conversation
ask -i
ask -i -m anthropic/claude-sonnet-4

# Control output
ask -r "Hello"                    # Raw output (no markdown)
ask -w 120 "Tell me a story"      # Wrap at 120 columns
ask -w 0 "Tell me a story"        # Disable word wrapping

# Adjust model parameters
ask -t 0.7 "Write a creative story"       # Higher temperature
ask --max-tokens 500 "Summarize briefly"  # Limit response length

# List common models
ask --list-models

# Help
ask --help
```

## Options

| Flag | Short | Description |
|------|-------|-------------|
| `--model` | `-m` | Model to use (default: google/gemini-3-flash-preview) |
| `--interactive` | `-i` | Start interactive REPL for multi-turn conversation |
| `--system` | `-s` | System prompt to use |
| `--temperature` | `-t` | Sampling temperature (0.0-2.0) |
| `--max-tokens` | | Maximum tokens in response |
| `--raw` | `-r` | Disable markdown rendering (output raw text) |
| `--width` | `-w` | Wrap lines at specified width (default: 80, 0 to disable) |
| `--list-models` | `-l` | List common model names |
| `--log` | | Enable logging to file (e.g., ~/.ask/ask.log) |
| `--log-level` | | Log level: trace, debug, info, warn, error (default: info) |
| `--help` | `-h` | Print help |

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
- `Ctrl+K` - Delete to end of line
- `Ctrl+U` - Delete to start of line
- `Ctrl+W` - Delete word
- `Ctrl+D` - Exit (on empty line)

## Features

- **Streaming responses** - See output as it's generated
- **Markdown rendering** - Bold, italic, code blocks, headers, and clickable links
- **Word wrapping** - Automatically wraps at 80 columns (configurable)
- **Multi-turn conversations** - Interactive mode with conversation history
- **Temperature control** - Adjust creativity vs determinism
- **Logging** - Debug with file-based logging via Chronicle

## Environment Variables

- `OPENROUTER_API_KEY` - Required. Your OpenRouter API key.

## Shell Completion

```bash
# Bash
ask --generate-completion bash > ~/.bash_completion.d/ask

# Zsh
ask --generate-completion zsh > ~/.zsh/completions/_ask

# Fish
ask --generate-completion fish > ~/.config/fish/completions/ask.fish
```

## Dependencies

- [parlance](https://github.com/nathanial/parlance) - CLI argument parsing, styled output, markdown rendering
- [oracle](https://github.com/nathanial/oracle) - OpenRouter API client with streaming support
- [chronicle](https://github.com/nathanial/chronicle) - File-based logging

## License

MIT
