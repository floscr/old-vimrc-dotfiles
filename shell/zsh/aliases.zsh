load shell/aliases.sh

#
autoload -U zmv

zman() { PAGER="less -g -s '+/^       "$1"'" man zshall; }
alias ag='noglob ag'

# Canonical hex dump; some systems have this symlinked
is-callable hd || alias hd="hexdump -C"
