" Use Vim settings, rather then Vi settings. This setting must be as early as
" possible, as it has side effects.
set nocompatible

match ErrorMsg '\s\+$'

" Set the leader to spacebar
let mapleader = "\<Space>"

set vb t_vb=      " Disable System Bell
set hidden        " Enable switching buffers without saving
set backspace=2   " Backspace deletes like most programs in insert mode
set history=50    " 50 History Entries
set ruler         " show the cursor position all the time
set laststatus=2  " Always display the status line
set autoread      " Reload files changed outside vim
set autochdir     " Set working dir to the current file
set mouse=a       " Mouse Control
set shortmess+=I  " Turn off the intro
set complete=.,b,u,]
set wildmenu      " Visual autocomplete for cmd menu
set noshowmatch   " Show matching tags
                  " having this turned on will make the cursor jump around
                  " weirdly
set synmaxcol=800 " Turn off syntax highlighting for lines longer than 800 characters

" set cpoptions+=$ " When changing words append a $ sign

" Do not show cursorline on inactive panes
augroup CursorLine
  au!
  au VimEnter,WinEnter,BufWinEnter * setlocal cursorline
  au WinLeave * setlocal nocursorline
augroup END

" Workaround to get autochdir working again
" https://github.com/vim/vim/issues/704
" autocmd VimEnter * set autochdir

" SEARCHING
set incsearch  " do incremental searching
set ignorecase " case insensitive searching (unless specified)
set hlsearch
set smartcase

" LINE WRAPPING
" set colorcolumn=+1
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

" Turn the filetype specific plugin loadin on in .vim/ftplugin
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
set noshiftround
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
set foldmethod=indent
set foldnestmax=10      "deepest fold is 10 levels
set nofoldenable        "dont fold by default
set foldlevel=1         "this is just what i use

" Automatic fold saving
autocmd BufWrite * mkview
autocmd BufRead * silent loadview

" NETRW
let g:netrw_list_hide='^\.git/$'

" " Load .vimrc in the base directory of a git repo, if it exists
" let $git_vimrc= expand(system("echo -n $(git rev-parse --show-toplevel)/.lvimrc"))
" if filereadable($git_vimrc)
"   source $git_vimrc
" endif

" -----------
" Textobjects
" -----------

" Textobjects for []
onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" buffer text-object
xnoremap i% GoggV
omap i% :<C-u>normal vi%<CR>

" ----------------
" Custom Functions
" ----------------

" Scrolling
" Save the scroll position when switching buffers
function! AutoSaveWinView()
    if !exists("w:SavedBufView")
        let w:SavedBufView = {}
    endif
    let w:SavedBufView[bufnr("%")] = winsaveview()
endfunction

" Restore current view settings.
function! AutoRestoreWinView()
    let buf = bufnr("%")
    if exists("w:SavedBufView") && has_key(w:SavedBufView, buf)
        let v = winsaveview()
        let atStartOfFile = v.lnum == 1 && v.col == 0
        if atStartOfFile && !&diff
            call winrestview(w:SavedBufView[buf])
        endif
        unlet w:SavedBufView[buf]
    endif
endfunction

function! OpenWithMarkedApp()
  silent! execute '!open "' . bufname("%") . '" -a /Applications/Marked\ 2.app'
endfunction
command! Marked call OpenWithMarkedApp()

" When switching buffers, preserve window view.
if v:version >= 700
    autocmd BufLeave * call AutoSaveWinView()
    autocmd BufEnter * call AutoRestoreWinView()
endif

command! -nargs=* Wrap set wrap linebreak nolist

" ------------
" Autocommands
" ------------

" Automatic path switching only for files
" http://vim.wikia.com/wiki/Set_working_directory_to_the_current_file
autocmd BufEnter * silent! lcd %:p:h

au BufRead,BufNewFile *.vue set filetype=html

" AUTOCOMMANDS Filetypes
autocmd BufNewFile,BufReadPost *.md,*.txt set filetype=markdown
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja

" "Autosource the vimrc and vim files
" autocmd! Bufwritepost .vimrc,*.vim source $MYVIMRC

" autocmd BufRead,BufNewFile *.md setlocal spell spelllang=en_us
" autocmd FileType gitcommit setlocal spell spelllang=en_us
" autocmd BufRead,BufNewFile *.txt setlocal spell spelllang=en_us

" ------
" Colors
" ------

syntax enable
set background=dark
let g:hybrid_reduced_contrast = 1 " Remove this line if using the default palette.
colorscheme hybrid
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
highlight SpecialKey ctermfg=66 guifg=#223843

" Nicer guttercolors for hybrid theme
if g:colors_name == "hybrid"
  highlight GitGutterAdd guifg=#99C794
  highlight GitGutterDelete guifg=#CC6665
  highlight GitGutterChangeDelete guifg=#B294BB
  highlight vertsplit guifg=#2E3C47
endif

source ~/.dotfiles/vim/shortcuts.vim