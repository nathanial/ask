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
ask -m anthropic/claude-haiku "Explain recursion briefly"

# Pipe from stdin
cat document.txt | ask "Summarize this document"
echo "Fix this code: def foo(" | ask

# List common models
ask --list-models

# Help
ask --help
```

## Options

| Flag | Short | Description |
|------|-------|-------------|
| `--model` | `-m` | Model to use (default: anthropic/claude-sonnet-4) |
| `--list-models` | `-l` | List common model names |
| `--help` | `-h` | Print help |
| `--version` | `-V` | Print version |

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

- [parlance](../parlance) - CLI argument parsing and styled output
- [oracle](../oracle) - OpenRouter API client

## License

MIT
