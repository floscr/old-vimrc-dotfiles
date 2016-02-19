Florians Dotfiles
=================

Dotfiles for my setup using zsh, tmux and neovim

## Setup

### Cloning

I clone my dotfiles using using [homesick]

```bash
gem install homesick
homesick clone https://github.com/floscr/Dotfiles
homesick symlink
```

### Tmux with true color support

To get NeoVim & tmux working with true color support you have to do the following things:

1. **Download iTerm nightly:**    
   [iTerm nightly]

2. **Intall tmux with truecolor patch**    

   ```bash
   brew edit tmux
   ```

   Then, before `def install`, add this.

   ```ruby
     option "with-truecolor", "Build with truecolor patch enabled"
     patch do
       url "https://gist.githubusercontent.com/zchee/9f6f2ca17acf49e04088/raw/0c9bf0d84e69cb49b5e59950dd6dde6ca265f9a1/tmux-truecolor.diff"
       sha1 "8e91ab1c076899feba1340854e96594aafee55de"
     end if build.with? "truecolor"
   ```

   Now you can run `brew install tmux --with-truecolor` and get all the colors for iterm nightly.

[iTerm nightly]: https://iterm2.com/downloads/nightly/
[homesick]: https://github.com/technicalpickles/homesick
