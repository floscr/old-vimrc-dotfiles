" Use Vim settings, rather then Vi settings. This setting must be as early as
" possible, as it has side effects.
set nocompatible

match ErrorMsg '\s\+$'

" Set the leader to spacebar
let mapleader = "\<Space>"
set vb t_vb= " Disable System Bell

set cursorline   " highlight current line
set hidden       " Enable switching buffers without saving
set backspace=2  " Backspace deletes like most programs in insert mode
set history=50   " 50 History Entries
set ruler        " show the cursor position all the time
set laststatus=2 " Always display the status line
set autoread     " Reload files changed outside vim
set autochdir    " Set working dir to the current file
set cpoptions+=$ " When changing words append a $ sign
set mouse=a      " Mouse Control
set shortmess+=I " Turn off the intro
set wildmenu     " Visual autocomplete for cmd menu
set showmatch    " Show matching tags

" SEARCHING
set incsearch  " do incremental searching
set ignorecase " case insensitive searching (unless specified)
set hlsearch
set smartcase

" LINE WRAPPING
set colorcolumn=+1
" Make it obvious where 80 characters is
" set textwidth=80
set nowrap " Don't wrap lines

" PERFORMANCE
set lazyredraw   " Wait to redraw
" set showcmd      " display incomplete command
" set nocursorline " Don't paint cursor line
" set ttyfast      " Faster scrolling
" set noshowmatch  " Don't match parentheses/brackets

" UNDO
" Save undo history to .VIM_UNDO_FILES
if has('persistent_undo')
  set undolevels=5000
  set undodir=$HOME/.VIM_UNDO_FILES
  set undofile
endif

" Syntax Highlighting
filetype plugin indent on
" Switch syntax highlighting on, when the terminal has colors
" Also switch on highlighting the last used search pattern.
if (&t_Co > 2 || has("gui_running")) && !exists("syntax_on")
  syntax on
endif

set nobackup
set noswapfile

" INDENTATION
set expandtab
set softtabstop=2
set shiftwidth=2
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
set listchars=tab:⋅⋅,trail:●,extends:#,nbsp:.

" SCROLLING
set scrolloff=10 "Start scrolling when we're 10 lines away from margins
set sidescrolloff=15
set sidescroll=1

" FOLDING
set foldmethod=syntax
set foldlevelstart=20
let javaScript_fold=1
let perl_fold=1
let r_syntax_folding=1
let php_folding=1
let ruby_fold=1
let sh_fold_enabled=1
let vimsyn_folding='af'
let xml_syntax_folding=1

" Load .vimrc in the base directory of a git repo, if it exists
let $git_vimrc= expand(system("echo -n $(git rev-parse --show-toplevel)/.lvimrc"))
if filereadable($git_vimrc)
  source $git_vimrc
endif

autocmd BufNewFile,BufReadPost *.md set filetype=markdown
autocmd BufNewFile,BufReadPost *.twig set syntax=jinja

source ~/.dotfiles/vim/shortcuts.vim
