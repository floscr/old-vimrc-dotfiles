#!/usr/bin/env bash

# Copy template lvimrc from templates directory
function vim_template() {
  if [[ -f .lvimrc ]]; then
    echo ".lvimrc exists in current path!"
  fi
  cp ~/.config/nvim/templates/$1.vim .lvimrc
}

function _vim_template() {
  # List all files in templates directory without .vim extension
  templates=`ls -1 ~/.config/nvim/templates | sed -e 's/\.vim$//'`
  compadd `echo $templates | sed "s/ //g"`
}

compdef _vim_template vim_template

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
function chromium_unsafe() {
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
