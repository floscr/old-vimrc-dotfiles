#!/bin/sh
# Install Homebrew (http://mxcl.github.com/homebrew) and other dependencies
if [ ! -x /usr/local/bin/brew ]; then
  echo "Installing Homebrew..."
  ruby -e "$(curl -fsSL https://raw.github.com/Homebrew/homebrew/go/install)"
fi

echo "Installing development dependencies from Brewfile..."
brew tap Homebrew/bundle # Bundler to install from brewfile
brew bundle -v
