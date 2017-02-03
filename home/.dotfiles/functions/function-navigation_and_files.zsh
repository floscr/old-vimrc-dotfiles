#!/bin/bash

# Rename files in current directory to lowercase filenames
# http://stackoverflow.com/a/13051934
function rename_all_files_to_lowercase {
  for i in $(find . -type f -name "*[A-Z]*"); do
    mv "$i" "$(echo $i | tr A-Z a-z)"
  done
}

# Open current location or given file in finder/default app
alias o="_open_location"
function _open_location() {
  if [[ -z "$@" ]]; then
    open .
  else
    open "$@"
  fi
}

# Return octal file rights
# http://askubuntu.com/questions/152001/how-can-i-get-octal-file-permissions-from-command-line
function permissions() {
  if [[ ! -z "$@" ]]; then
    FILES="$@"
    stat -f '%A %a %N' $FILES
  else
    stat -f '%A %a %N' *
  fi
}

# Make a directory on directly jump into it
function mkcd() {
  mkdir "$1"
  cd "$1"
}
