#!/usr/bin/bash

# Config
MAX_SIZE=10000 # 10MB
MIN_AUDIO_BIT_RATE='128' # Kbps

# Check if the needed brew package is installed first
if ! brew ls |  grep -q media-info; then
  echo "Install mediainfo brew package first!"
  echo "$: brew install mediainfo"
fi

# Print a colorized text
RESTORE='\033[0m'
GREEN='\033[00;32m'
RED='\033[0;31m'
function error_message() {
  echo -en "${RED}"
  echo -e "$*"
  echo -en "${RESTORE}"
}

function success() {
  echo -en "${GREEN}"
  echo "$*"
  echo -en "${RESTORE}"
}


HAS_ERRORS=0
for file in *; do
  # Ignore script files
  if [[ $file != *".sh" ]]; then

    ERRORS=()

    # Check if the aspect ration is 16:9
    MEDIA_INFO=`mediainfo $file`
    if [[ $MEDIA_INFO != *"16:9"* ]]; then
      ERRORS+=("Aspect ratio not matching 16:9!")
    fi

    # Check if the audio bitrate is lower than the minimum
    AUDIO_BIT_RATE_INFO=`MediaInfo --Language=raw --Full --Inform="Audio;%BitRate/String%" $file`
    AUDIO_BIT_RATE=`echo $AUDIO_BIT_RATE_INFO | sed -e 's/[^0-9]*//g'`
    if [[ ! $AUDIO_BIT_RATE -ge $MIN_AUDIO_BIT_RATE ]]; then
      ERRORS+=("Bitrate is smaller than $MIN_AUDIO_BIT_RATE kbps!")
    fi

    # Check if the file is bigger than the max file size
    FILE_SIZE=$(du -k "$file" | cut -f 1)
    if [[ $FILE_SIZE -ge $MAX_SIZE ]]; then
      ERRORS+=("File is bigger than $((FILE_SIZE/1024))MB! \n  Filesize: $((FILE_SIZE/1024))MB")
    fi

    # Error display loop
    if [ ! ${#ERRORS[@]} -eq 0 ]; then
      error_message "Errors in File:"
      error_message "$file"
      echo
      for i in "${ERRORS[@]}"; do
        error_message "- $i"
      done
      echo
      if [[ $HAS_ERRORS == 1 ]]; then
        error_message "--------------------------------------------------------------------------------"
        echo
      fi
      HAS_ERRORS=1
    fi

  fi
done

if [[ $HAS_ERRORS == 0 ]]; then
  success "All files match the terms 🍺"
fi
