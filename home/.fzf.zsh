# Setup fzf
# ---------
if [[ ! "$PATH" == */Users/florianschrodl/.fzf/bin* ]]; then
  export PATH="$PATH:/Users/florianschrodl/.fzf/bin"
fi

# Auto-completion
# ---------------
# [[ $- == *i* ]] && source "/Users/florianschrodl/.fzf/shell/completion.zsh" 2> /dev/null

# Key bindings
# ------------
source "/Users/florianschrodl/.fzf/shell/key-bindings.zsh"

