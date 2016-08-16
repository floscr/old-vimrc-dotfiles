#!/bin/bash
# Description of script
 
# Required program(s)
req_progs=(prog1 prog2)
for p in ${req_progs[@]}; do
  hash "$p" 2>&- || \
  { echo >&2 " Required program \"$p\" not installed."; exit 1; }
done
 
# Display usage if no parameters given
if [[ -z "$@" ]]; then
  
fi
 
# Text color variables
txtred='\e[0;31m'       # red
txtgrn='\e[0;32m'       # green
txtylw='\e[0;33m'       # yellow
txtblu='\e[0;34m'       # blue
txtpur='\e[0;35m'       # purple
txtcyn='\e[0;36m'       # cyan
txtwht='\e[0;37m'       # white
bldred='\e[1;31m'       # red    - Bold
bldgrn='\e[1;32m'       # green
bldylw='\e[1;33m'       # yellow
bldblu='\e[1;34m'       # blue
bldpur='\e[1;35m'       # purple
bldcyn='\e[1;36m'       # cyan
bldwht='\e[1;37m'       # white
txtund=$(tput sgr 0 1)  # Underline
txtbld=$(tput bold)     # Bold
txtrst='\e[0m'          # Text reset
 
# Feedback indicators
info=${bldwht}*${txtrst}
pass=${bldblu}*${txtrst}
warn=${bldred}!${txtrst}
 
# Indicator usage
echo -e "${info} "
echo -e "${pass} "
echo -e "${warn} "
 
# Check if root
if [ $(whoami) != root ]; then
  echo " Must be root to use."
  exit
fi
 
# Check if selection exists
if [ ! -e "$@" ]; then
  echo " Selection \""$@"\" does not exist."
  exit
fi
