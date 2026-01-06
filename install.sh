#!/bin/bash
set -e

# Ask install script
# Builds and installs the ask binary

INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"
SCRIPT_DIR="$(cd "$(dirname "$0")" && pwd)"

echo "Building ask..."
cd "$SCRIPT_DIR"
lake build ask

echo "Installing to $INSTALL_DIR..."
mkdir -p "$INSTALL_DIR"
cp .lake/build/bin/ask "$INSTALL_DIR/ask"
chmod +x "$INSTALL_DIR/ask"

# Create history directory
HISTORY_DIR="$HOME/.ask/history"
if [ ! -d "$HISTORY_DIR" ]; then
    echo "Creating history directory at $HISTORY_DIR..."
    mkdir -p "$HISTORY_DIR"
fi

echo ""
echo "Installed ask to $INSTALL_DIR/ask"

# Check if install dir is in PATH
if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
    echo ""
    echo "NOTE: $INSTALL_DIR is not in your PATH."
    echo "Add it by adding this line to your shell config (~/.bashrc, ~/.zshrc, etc.):"
    echo ""
    echo "  export PATH=\"\$HOME/.local/bin:\$PATH\""
    echo ""
fi

# Check for API key
if [ -z "$OPENROUTER_API_KEY" ]; then
    echo ""
    echo "NOTE: OPENROUTER_API_KEY environment variable is not set."
    echo "Get your API key from https://openrouter.ai/keys and add to your shell config:"
    echo ""
    echo "  export OPENROUTER_API_KEY=\"your-api-key-here\""
    echo ""
fi

echo "Run 'ask --help' to get started."
