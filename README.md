Dotfiles
========

My configuration files for vim, tmux and zsh.

## Installation

### Homesick

[Homesick] is a ruby gem to manage your dotfiles.
```bash
# Install homesick, clone the repo and symlink all files in ./home to ~
gem install homesick
homesick clone https://github.com/floscr/Dotfiles
homesick symlink
```


## Performance Optimization / Hacks

My ZSH was getting slooooow, here are my workarounds to get a speedy Command Line again:

I've turned my ZSH startup time from a whopping **2.22s** to a good **0.18s**

To debug my startup time I used:
```bash
/usr/bin/time zsh -i -c exit
```

1. **Lazyload NVM**
  [NVM] was the main reason for the slowdown.
  At first I replaced the brew version of NVM, which is not oficially supported, with the [official installer command](https://github.com/creationix/nvm#install-script).
  It's getting better, but NVM still causes major slowdowns.
  So I lazyload NVM whenever a node command is called.
  `_group_lazy_load $HOME/.nvm/nvm.sh \ nvm \ node \ npm \ npmls \ yarn \ iectrl \ gulp \ vue \ browser-sync`
  The Lazyload helpers can be found in `.zsh-functions`








[NVM]: https://github.com/creationix/nvm
[Homesick]: https://github.com/technicalpickles/homesick
