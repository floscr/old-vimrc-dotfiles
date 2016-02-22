" Load the vimrc to nvimrc
source ~/.dotfiles/vim/defaults.vim
source ~/.dotfiles/vim/plugins.neo.vim
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
	set gcr=n:blinkon0 " Turn off blinking cursor in normal mode
	set guioptions-=e  " We don't want Gui tabs.
	set linespace=15   " Macvim-specific line-height.

	set guioptions-=l  " Disable Gui scrollbars.
	set guioptions-=L
	set guioptions-=r
	set guioptions-=R

	" This is the default. %s is replaced with fzf command

	" Use urxvt instead
	let g:fzf_launcher = 'urxvt -geometry 120x30 -e sh -c %s'
	" let g:fzf_launcher = 'macvim_fzf %s'
endif


