Florians Dotfiles
=================

Dotfiles for my setup using zsh, tmux and neovim

## Setup

### Installation

I clone my dotfiles using using [homesick]

```bash
gem install homesick
homesick clone https://github.com/floscr/Dotfiles
homesick symlink
```

## Shell Aliases & Functions

From time to time i will describe my custom functions and aliases here:    

### Functions

  + `t`
    Display nice tree structure in current directory.
    Uses [tree](http://mama.indstate.edu/users/ice/tree/)

  + `tgulp`
    Run gulp in a new split without leaving the current split.    
    This also closes all other splits!

* * *

### Custom German Programming keyboard

Since the US keyboard is much better when programming but I just can't shake some keys that are hardwired to my brain, I've created my own Frankenstein of a keyboard input for German and English typing.    

It's pretty much the US keyboard, except for a few changes:    

  1. <kbd>Y</kbd> & <kbd>Z</kbd> are switched    
  *Just can't shake that*
  2. <kbd>^</kbd> replaces <kbd>§</kbd>    
  *Who needs that symbol ever.*
  3. Umlaut are in the same position as in the German keyboard, but are triggered by pressing ⌥.    
  example: <kbd>ALT</kbd> + <kbd>'</kbd> = <kbd>ä</kbd>    
  example: <kbd>ALT</kbd> + <kbd>SHIFT</kbd> + <kbd>'</kbd> = <kbd>Ä</kbd>

Here a few screenshots:

![](./images/keyboard-preview.png)
![](./images/keyboard-preview-alt.png)
![](./images/keyboard-preview-alt-shift.png)

I've modified the keyboard with [ukulele.app](http://scripts.sil.org/cms/scripts/page.php?site_id=nrsi&id=ukelele)

### Tmux with true color support

To get NeoVim & tmux working with true color support you have to do the following things:

0. **Download [iTerm2](https://iterm2.com)
0. brew update && brewn install tmux
0. put this in your ~/.tmux.conf

```txt
set -g default-terminal "xterm-256color"
set-option -ga terminal-overrides ",xterm-256color:Tc"
```

0. add this to your ~/.vimrc

```viml
if has('nvim')
  let $NVIM_TUI_ENABLE_TRUE_COLOR=1
endif
```

0. now you have tmux+nvim+iterm2 with full truecolor support.

* more info here: https://github.com/ninrod/tricks/blob/master/shell/tmux.md#the-standard-way-since-tmux-22

[homesick]: https://github.com/technicalpickles/homesick
