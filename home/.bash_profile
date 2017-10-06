if [[ -f .profile ]]; then
  . .profile
fi

export SHELL="$(which zsh)"
exec "$(which zsh)" -l
