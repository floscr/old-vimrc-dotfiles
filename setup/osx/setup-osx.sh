#!/usr/bin/bash

# Reduce Transparency
defaults write com.apple.universalaccess reduceTransparency -bool true

# Turn off big cursor on mouse shake
defaults write ~/Library/Preferences/.GlobalPreferences CGDisableCursorLocationMagnification -bool YES

#==============================================================================#
# Finder                                                                       #
# =============================================================================#

# Show hidden files
defaults write com.apple.finder AppleShowAllFiles true

# Show File Extensions
defaults write NSGlobalDomain AppleShowAllExtensions -bool true

# Unhide User Library Folder
chflags nohidden ~/Library

# Show 'quit finder' menu in Finder
defaults write com.apple.finder QuitMenuItem -bool true

# Expand Save Panel by Default
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode -bool true
defaults write NSGlobalDomain NSNavPanelExpandedStateForSaveMode2 -bool true

# Hide Desktop Icons
defaults write com.apple.finder CreateDesktop -bool false

# Show status bar
defaults write com.apple.finder ShowStatusBar -bool true

# Sets default save target to be a local disk, not iCloud.
defaults write NSGlobalDomain NSDocumentSaveNewDocumentsToCloud -bool false

# Set Current Folder as Default Search Scope
defaults write com.apple.finder FXDefaultSearchScope -string "SCcf"

# Set Default Finder Location to Home Folder
defaults write com.apple.finder NewWindowTarget -string "PfLo" && \
defaults write com.apple.finder NewWindowTargetPath -string "file://${HOME}"

# Disable Creation of Metadata Files on Network Volumes
# Avoids creation of .DS_Store and AppleDouble files.
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true

# Avoids creation of .DS_Store and AppleDouble files.
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true

# Disable definition shortcut
defaults write com.apple.symbolichotkeys AppleSymbolicHotKeys -dict-add 70 '<dict><key>enabled</key><false/></dict>'

#==============================================================================#
# Etc                                                                          #
# =============================================================================#

# Set all F-keys to standard keys
# Brightness keys have to be rebound with something like keybord-maestro
defaults write -g com.apple.keyboard.fnState -bool true

# Disable autocorrect
defaults write -g NSAutomaticSpellingCorrectionEnabled -bool false

# Expand Print Panel by Default
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint -bool true
defaults write NSGlobalDomain PMPrintingExpandedStateForPrint2 -bool true

# Quit Printer App After Print Jobs Complete
defaults write com.apple.print.PrintingPrefs "Quit When Finished" -bool true

# Save Screenshots in custom directory
SCREENSHOTS_DIR=~/Pictures/Screenshots
if [[ ! -d $SCREENSHOTS_DIR ]]; then
  mkdir -p $SCREENSHOTS_DIR
fi
defaults write com.apple.screencapture location $SCREENSHOTS_DIR



