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

# List common models
ask --list-models

# Help
ask --help
```

## Environment

- `OPENROUTER_API_KEY` - Required. Your OpenRouter API key.

## Dependencies

- **parlance** - CLI argument parsing, styled output, shell completion
- **oracle** - OpenRouter API client with streaming support
