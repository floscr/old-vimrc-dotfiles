" Load the vimrc to nvimrc
source ~/.dotfiles/vim/defaults.vim
source ~/.dotfiles/vim/plugins.vim
source ~/.dotfiles/vim/theme.vim
source ~/.dotfiles/vim/pluginsettings.vim
source ~/.dotfiles/vim/shortcuts.vim

"Allow usage of mouse in iTerm
set ttymouse=xterm2

" Macvim specific settings
if has("gui_macvim")
  macmenu &File.Print key=<nop>
  "Disable the Print key for MacVim.

  set guifont=Inconsolata:h18
  set lines=40
  set columns=120
  set colorcolumn=80
  set gcr=n:blinkon0 " Turn off blinking cursor in normal mode
  set guioptions-=e  " We don't want Gui tabs.
  set linespace=14   " Macvim-specific line-height.

  set guioptions-=l  " Disable Gui scrollbars.
  set guioptions-=L
  set guioptions-=r
  set guioptions-=R
endif


