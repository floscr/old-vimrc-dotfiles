#!/usr/bin/env bash

# Download youtbe video and notify
alias y='__youtube_dl_notify'
function __youtube_dl_notify() {
  youtube-dl $@
  title="$(youtube-dl --get-title $@)"
  terminal-notifier \
    -title "Video Downloaded" \
    -message "$title" \
    -sound Submarine
}

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
