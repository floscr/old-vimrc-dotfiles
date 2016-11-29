source ~/.config/nvim/init.vim

if has("gui_macvim")
  " Disable red color highlight
  au ColorScheme * hi Error NONE
  au ColorScheme * hi ErrorMsg NONE
  au GuiEnter * hi Error NONE
  au GuiEnter * hi ErrorMsg NONE

  set guifont=Inconsolata:h18
  " set lines=40
  " set columns=120
  set colorcolumn=80
  set gcr=n:blinkon0 " Turn off blinking cursor in normal mode
  set guioptions-=e  " We don't want Gui tabs.
  set linespace=14   " Macvim-specific line-height.

  set guioptions-=l  " Disable Gui scrollbars.
  set guioptions-=L
  set guioptions-=r
  set guioptions-=R

  set showcmd      " display incomplete command
endif
