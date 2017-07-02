#!/usr/bin/env bash

# Check if a command exists
# Example
# if _command_exists nvim; then echo 'Nvim exists!'; fi
# http://stackoverflow.com/a/3931779
_command_exists() {
  type "$1" &> /dev/null ;
}

# Echo red error message
# _error_msg "My Message"
_error_msg() {
  RED='\033[0;31m'
  NC='\033[0m' # No Color
  echo "${RED}$@${NC}"
}

# Echo yellow info message
# _info_msg "My Message"
_info_msg() {
  YELLOW='\033[0;33m'
  NC='\033[0m' # No Color
  echo "${YELLOW}ℹ: $@${NC}"
}

# Echo green success message
# _success_msg "My Message"
_success_msg() {
  YELLOW='\033[0;32m'
  NC='\033[0m' # No Color
  echo "${YELLOW}✔  $@${NC}"
}
