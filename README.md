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

My Zsh config was getting slooooow, here are my workarounds to get a speed CLI again:

I've turned my zsh startup time from a whopping **2.22s** to an acceptable **0.18s**

To debug my startup time I used:
```bash
/usr/bin/time zsh -i -c exit
```

1. Lazyload NVM
  [NVM] was the main reason for the slowdown
  At first I uninstalled the brew version of [NVM]

[NVM]: https://github.com/creationix/nvm
[Homesick]: https://github.com/technicalpickles/homesick
