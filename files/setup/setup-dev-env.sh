#!/bin/sh
# Set up a development environment for mac

_info_msg() {
  YELLOW='\033[0;33m'
  NC='\033[0m' # No Color
  echo "${YELLOW}ℹ: $*${NC}"
}

# Install Homebrew
if [ ! -x /usr/local/bin/brew ]; then
  _info_msg "Installing Homebrew…"
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
fi

# Install NVM
if [ ! -d ~/.nvm ]; then
  _info_msg "Installing NVM…"
  curl -o- https://raw.githubusercontent.com/creationix/nvm/v0.33.2/install.sh | bash
fi
