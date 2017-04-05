#!/bin/bash

function _vim_template () {
  # List all template files without extension
  templates=`ls -1 ~/.config/nvim/templates | sed -e 's/\.vim$//'`
  compadd `echo $templates | sed "s/ //g"`
}

# Copy template lvimrc from templates directory
function vim_template() {
  if [[ -f .lvimrc ]]; then
    echo ".lvimrc exists in current path!"
  fi
  cp ~/.config/nvim/templates/$1.vim .lvimrc
}
compdef _vim_template vim_template

# Clone repo in my general repository directory
function gccd() {
  cd ~/Code/Repositories
  ccd "$@"
}

# Silent simple irb shortut
function sirb() {
  irb --simple-prompt --noecho
}

# List npm scripts from package json using jq tool
function npm_scripts() {
  # If jq doesnt exist install via brew
  if ! _command_exists jq; then
    echo "Installing jq via brew"
    brew install jq
  fi

  # Exit if there is no package.json in the current directory
  if [[ ! -f package.json ]]; then
    echo "No package.json in the current directory"
    exit
  fi

  cat package.json | jq '.scripts'
}

function vscode() {
  VSCODE_CWD="$PWD" open -n -b "com.microsoft.VSCode" --args "$*";
}

# [slt]:
# Quicky open sublime project in the current repository
# Workaround for the non working sublime cli
# alias subl="slt"
function slt() {
  # Check if the sublime cli is installed
  sublime_cli="subl"
  command -v $sublime_cli >/dev/null 2>&1 || {
  echo >&2 "The $sublime_cli cli is not installed. Aborting"
  return 1
}

# If there are arguments given, just open those in sublime and exit the script
if [[ ! -z "$@" ]]; then
  open -a "Sublime Text" "$@"
  return 1
fi

# Check if sublime is in the process list
# http://stackoverflow.com/questions/1821886/check-if-mac-process-is-running-using-bash-by-process-name
sublime_is_running=$(ps aux | grep "Sublime Text" | wc -l)

# Open sublime in the background if its not running
if [[ ! $sublime_is_running -gt 1 ]]; then
  subl --background
  # Wait 0.1 second to circumvent sublime only opening the project file
  sleep 0.1
fi

# Open sublime project file if there is one in the current directory
project_in_current_dir=$(find . -maxdepth 1 -name '*.sublime-project' -print -quit)
if [[ ! -z "$project_in_current_dir" ]]; then
  open -a "Sublime Text" "$project_in_current_dir"
  return 1
fi

# When in a git repository, check if there is a sublime project file at the root
git_root=$(git rev-parse --show-toplevel 2> /dev/null)
if [[ ! -z "$git_root" ]]; then
  project_in_repo_root=$(find $git_root -maxdepth 1 -name '*.sublime-project' -print -quit)
  if [[ ! -z "$project_in_repo_root" ]]; then
    open "$project_in_repo_root"
    return 1
  fi
fi

open -a "Sublime Text" .
}

# Quickly generate md5 hash
function md5() {
  if [[ -z $@ ]]; then
    echo "md5 Your String or Password"
    echo "Use two spaces before the command to hide from history"
    return
  fi
  echo -n "$@" | openssl md5
}

function ie11() {
  iectrl open --start "IE11 - Win7" $@
}

function ie10() {
  iectrl open --start "IE10 - Win7" $@
}

function ie9() {
  iectrl open --start "IE9 - Win7" $@
}

function ie8() {
  iectrl open --start "IE8 - WinXP" $@
}

function ie7() {
  iectrl open --start "IE7 - WinXP" $@
}

function ie6() {
  iectrl open --start "IE6 - WinXP" $@
}

# Open a chromium instance that allows cross site post from localhost
function chromium() {
  /Applications/Chromium.app/Contents/MacOS/Chromium --args --disable-web-security --user-data-dir -–allow-file-access-from-files
}

# Download kirby cms, switch to latest branch and load all submodules
function getkirby() {
  if [[ ! -z "$1" ]]; then
    dir="$1"
  else
    dir="kirbycms"
  fi

  git clone --recursive https://github.com/getkirby/starterkit.git $dir
  cd $dir
}

# Opens a man page as a pdf in preview
function pman() {
  man -t $1 | open -f -a /Applications/Preview.app
 }

# List global installed packages
# npm ls -g can be super slow so this is a workaround
# Requires node to be installed via nvm
#
# The Official way, which is way to slow:
# npm ls --depth=0 -g "$@" 2>/dev/null
function npmls() {
  # Workaround for a fast global package listing
  ls -1 ~/.nvm/versions/node/$(node -v)/lib/node_modules
}

# [numhash]:
# Create a numeric hash with x digits
function numhash() {
  if [[ -z $1 ]]; then
    # If there is no parameter passed create a 32 character hash
    local digit=32
  else
    local digit="$1"
  fi

  # Create a numeric x character long hash
  cat /dev/urandom | env LC_CTYPE=C tr -dc '0-9' | fold -w $digit | head -n 1
}
