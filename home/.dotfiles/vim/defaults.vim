" Use Vim settings, rather then Vi settings. This setting must be as early as
" possible, as it has side effects.
set nocompatible

match ErrorMsg '\s\+$'

" Set the leader to spacebar
let mapleader = "\<Space>"
set vb t_vb= " Disable System Bell

set cursorline					" highlight current line

set hidden					" Enable switching buffers without saving
set backspace=2			" Backspace deletes like most programs in insert mode
set history=50			" 50 History Entries
set ruler						" show the cursor position all the time
set laststatus=2		" Always display the status line
set autoread				" Reload files changed outside vim
set autochdir				" Set working dir to the current file
set cpoptions+=$		" When changing words append a $ sign
set mouse=a					" Mouse Control
set shortmess+=I    " Turn off the intro

" SEARCHING
set incsearch  " do incremental searching
set ignorecase " case insensitive searching (unless specified)
set smartcase

" LINE WRAPPING
set colorcolumn=+1
" Make it obvious where 80 characters is
" set textwidth=80
" set nowrap					" Don't wrap lines

" PERFORMANCE
set lazyredraw	   " Wait to redraw
" set showcmd			 " display incomplete command
" set nocursorline " Don't paint cursor line
" set ttyfast			 " Faster scrolling
" set noshowmatch  " Don't match parentheses/brackets

" UNDO
" Save undo history to .VIM_UNDO_FILES
if has('persistent_undo')
	set undolevels=5000
	set undodir=$HOME/.VIM_UNDO_FILES
	set undofile
endif

filetype plugin indent on

" Share the clipboard between multiple vim instances
" set clipboard=unnamedplus

set nobackup
set noswapfile

" INDENTATION
set tabstop=2
set shiftwidth=2
set shiftround
set noexpandtab
set smarttab
set autoindent " Automatic indentation
set copyindent " Copy previous indetation on autoindenting

" Numbers
set number
set numberwidth=5

" SPLITS
" Open new split panes to right and bottom
set splitbelow
set splitright

" Whitespace Display Options
set list
" set listchars=eol:⌐,tab:⋅⋅,trail:~,extends:>,precedes:<
set listchars=tab:⋅⋅,trail:●,extends:#,nbsp:.

" SCROLLING
set scrolloff=10				 "Start scrolling when we're 10 lines away from margins
set sidescrolloff=15
set sidescroll=1

" FOLDING
set foldmethod=syntax
set foldlevelstart=20
let javaScript_fold=1					" JavaScript
let perl_fold=1								" Perl
let r_syntax_folding=1				" R
let php_folding=1							" PHP
let ruby_fold=1								" Ruby
let sh_fold_enabled=1					" sh
let vimsyn_folding='af'				" Vim script
let xml_syntax_folding=1			" XML

set foldmethod=indent
set foldnestmax=2 " RELATIVE NUMBERING

" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
	syntax on
endif

autocmd BufNewFile,BufReadPost *.md set filetype=markdown
source ~/.dotfiles/vim/shortcuts.vim
au Filetype javascript source ~/.dotfiles/vim/syntax/javascript.vim
au Filetype markdown source ~/.dotfiles/vim/syntax/markdown.vim
au Filetype html source ~/.dotfiles/vim/syntax/html.vim
au Filetype css,vue source ~/.dotfiles/vim/syntax/css.vim
au Filetype vue source ~/.dotfiles/vim/syntax/vue.vim
