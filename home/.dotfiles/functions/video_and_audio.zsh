#!/usr/bin/env bash

# Download preset for binding of isaac youtube channel videos
function isac() {
  youtube-dl -f 18+140 "$@"
}

# Download youtube clip as mp3
function youtube-mp3() {
  if [[ -z "$@" ]]; then
    echo "Pass an youtube url!"
    exit 0
  fi

  youtube-dl --extract-audio --audio-format mp3 "$1"
  file="$(find . -type f -name "*.m4a" -print0 | xargs -0 stat -f "%m %N" | sort -rn | head -1 | cut -f2- -d " ")"
  echo $file
}
