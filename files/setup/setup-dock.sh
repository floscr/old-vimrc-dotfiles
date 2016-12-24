#!/usr/bin/bash

# Install utility for handling the dock from the command line
brew install dockutil

# Enable hover highlight in grid view
defaults write com.apple.dock mouse-over-hilite-stack -boolean yes;killall Dock

# Remove all items to start adding custom stuff
dockutil --remove all

# Add downloads and grid folder as grid views
dockutil --add ~/Downloads --view grid
dockutil --add /Applications/ --view grid
