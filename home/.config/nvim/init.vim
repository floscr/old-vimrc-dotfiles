" -----------------------------------------------------------------------------
" -----------------------------------------------------------------------------
" /\\\________/\\\_____________________________________________________________
" \/\\\_______\/\\\____________________________________________________________
" _\//\\\______/\\\___/\\\_____________________________________________________
" ___\//\\\____/\\\___\///_____/\\\\\__/\\\\\____/\\/\\\\\\\______/\\\\\\\\____
" _____\//\\\__/\\\_____/\\\__/\\\///\\\\\///\\\_\/\\\/////\\\___/\\\//////____
" _______\//\\\/\\\_____\/\\\_\/\\\_\//\\\__\/\\\_\/\\\___\///___/\\\__________
" _________\//\\\\\______\/\\\_\/\\\__\/\\\__\/\\\_\/\\\_________\//\\\________
" ___________\//\\\_______\/\\\_\/\\\__\/\\\__\/\\\_\/\\\__________\///\\\\\\\\
" _____________\///________\///__\///___\///___\///__\///_____________\////////
" -----------------------------------------------------------------------------
" -----------------------------------------------------------------------------

source ~/.config/nvim/plugins.vim

" =============================================================================
" Default Settings
" Neovim defaults: https://neovim.io/doc/user/vim_diff.html#nvim-option-defaults
" =============================================================================

set autochdir        " Set working dir to path of the current file
set hidden           " Enables to switch between unsaved buffers and keep undo history
set lazyredraw       " Don't redraw while executing macros (better performance)
set nojoinspaces     " No extra space when joining a line which ends with . ? !
set noshowmatch      " Show matching tags
set nostartofline    " Prevent cursor from moving to beginning of line when switching buffers
set noswapfile       " Dont create swapfiles
set number           " Show Line numbers
set relativenumber   " Show Relative Numbers
set shell=$SHELL     " Setting shell to zsh
set shortmess+=I     " Turn off the intro message
set showmode         " Always show mode
set splitbelow       " Split new window below current window
set splitright       " Split new window right of current window
set suffixesadd+=.js " Automatically add suffic when pressing gf to go to a file
set synmaxcol=1500   " Turn off syntax highlighting after X lines

" Disable Netrw
" Netrw is the default filebrowser plugin for vim which I replace with FileBeagle
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

" -----------------------------------------------------------------------------
" Sessions
" -----------------------------------------------------------------------------

" Autosaving Buffer Options like folds
set sessionoptions-=options " Disable options for session saving
set viewoptions-=options    " http://stackoverflow.com/questions/26917336/vim-specific-mkview-and-loadview-in-order-to-avoid-issues
augroup autosave_buffer
 autocmd!
 autocmd BufWinLeave *.* mkview
 autocmd BufWinEnter *.* silent! loadview
augroup END

" -----------------------------------------------------------------------------
" Color Settings
" -----------------------------------------------------------------------------

if has('termguicolors')
  set termguicolors
endif

set background=dark
let g:hybrid_reduced_contrast = 1
colorscheme hybrid

" -----------------------------------------------------------------------------
" Wrap Settings
" -----------------------------------------------------------------------------

set colorcolumn=80           " Add a colorized column tho show the maximal text length
set textwidth=80             " Set the recommended text length to 80 characters
set nowrap                   " Don't wrap lines
set textwidth=0 wrapmargin=0 " this turns off physical line wrapping (ie: automatic insertion of newlines)

" -----------------------------------------------------------------------------
" Timeout settings
" Time out on key codes but not mappings. Basically this makes terminal Vim work sanely.
" -----------------------------------------------------------------------------

set notimeout
set ttimeout
set ttimeoutlen=10

" -----------------------------------------------------------------------------
" Search settings
" -----------------------------------------------------------------------------

set incsearch  " Incremental search
set ignorecase " Ignore case by default
set smartcase  " Make search case sensitive only if it contains uppercase letters
set wrapscan   " Search again from top when reached the bottom

" -----------------------------------------------------------------------------
" Persistent undo settings
" -----------------------------------------------------------------------------

if has('persistent_undo')
  set undofile
  set undodir=~/.config/nvim/tmp/undo//
endif

" -----------------------------------------------------------------------------
" Hidden characters settings
" -----------------------------------------------------------------------------

set list
set listchars=tab:⋅⋅,trail:●,extends:#,nbsp:.
set showbreak=↪

" -----------------------------------------------------------------------------
" Indentation
" -----------------------------------------------------------------------------

set expandtab
set softtabstop=2
set shiftwidth=2
set noshiftround
set autoindent " Automatic indentation
set copyindent " Copy previous indetation on autoindenting

" -----------------------------------------------------------------------------
" Folding settings
" -----------------------------------------------------------------------------

set foldmethod=indent
set foldlevelstart=99
set foldopen-=block   " Disable fold opening when jumping paragraphs

" set foldmethod=manual " Fold by indentation
" set foldnestmax=2     " Maximum fold level
" set nofoldenable      " dont fold by default
" set foldlevel=1       " Fold by Levels
" " set foldlevel=99
" set foldlevelstart=99 " Open folds on beginning of file
" set foldcolumn=0      " Disable fold column

" let g:vim_markdown_folding_disabled = 1

function! MyFoldText()
  let line = getline(v:foldstart)

  let nucolwidth = &fdc + &number * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 3
  let foldedlinecount = v:foldend - v:foldstart

  " expand tabs into spaces
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')

  let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
  let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
  return line . '...' . repeat(" ",fillcharcount) . foldedlinecount . '...' . ' '
endfunction
set foldtext=MyFoldText()

" " Restore the fold method
autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=indent | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

" -----------------------------------------------------------------------------
"  Omni completion
" -----------------------------------------------------------------------------

set completeopt-=preview " Don't show preview scratch buffers
set wildignore=*.o,*.obj,*~
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*.gem
set wildignore+=tmp/**

" -----------------------------------------------------------------------------
" Scrolloff
" -----------------------------------------------------------------------------

" Start scrolling:
" Bottom and Top: 10 Lines
" Side: 15 lines
set scrolloff=10
set sidescrolloff=15
" Scroll one column on the side
set sidescroll=1

" -----------------------------------------------------------------------------
" Neovim specific settings
" -----------------------------------------------------------------------------

let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1
let g:loaded_python_provider=1 " Disable python 2 interface
let g:python_host_skip_check=1 " Skip python 2 host check
let g:python3_host_prog = '/usr/local/bin/python3'
let g:python_host_prog = '/usr/bin/python'

" =============================================================================
" Keyboard Mappings / Shortcuts
" =============================================================================

" Set leader to Space
let g:mapleader="\<space>"

" Reload .vimrc
" When sourcing files, the last seach gets highlighted
" This mapping auto disables the highlight
nnoremap <silent> <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<CR><esc>:echo "Vimrc reloaded!"<CR>

" Clear highlighting on escape in normal mode
nnoremap <silent><esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" Quit current buffer
nnoremap <C-c> :q<return>

" Source current file
nmap <silent> <leader>sf :source %<CR><ESC>:echo "Current file sourced!"<CR>

" When jump to next match also center screen
" Note: Use :norm! to make it count as one command. (i.e. for i_CTRL-o)
nnoremap <silent> n :norm! nzz<CR>
nnoremap <silent> N :norm! Nzz<CR>
vnoremap <silent> n :norm! nzz<CR>
vnoremap <silent> N :norm! Nzz<CR>

" Quickfix next/previous
nnoremap ]q :cn<CR>
nnoremap [q :cp<CR>

" Replay last Macro
nnoremap Q @q

" Toggle spellcheck
nmap <silent> <leader>ss :set spell!<cr>

" Yank text to the OS X clipboard
noremap <leader>y "*y
noremap <leader>yy "*Y

" Neovim Terminal
" Press escape to exit insert mode
if has('nvim')
  tnoremap <ESC> <C-\><C-n>
  tnoremap ,<ESC> <ESC>
endif

" Make . work with visually selected lines
xnoremap . :norm.<CR>

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Reverse join (Turn single line comments to inline comments)
nnoremap K jddkPmzJ`z

" Keep selection when tabbing
xnoremap <  <gv
xnoremap >  >gv

" Indentation using tab
imap <S-Tab> <C-o><<
map <S-Tab> <<
map <Tab> >>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

" Create file under cursor
nnoremap gF :e <cfile><cr>

" Enter command by pressing enter
nnoremap <Cr> :

" Show current file in finder
nnoremap <leader><cr> :silent !open .<cr>

" Make * star work in visual mode
vnoremap <silent> * y:let @/=@"<cr>:set hlsearch<cr>n

" Use the last used search to use in replace command
nmap <expr> M ':%s/' . @/ . '//g<LEFT><LEFT>'

" Toggle the error list
nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>

" Workaround for ctrl-h to work
" https://github.com/neovim/neovim/issues/2048
if has('nvim')
  nmap <BS> <C-W>h
endif

" -----------------------------------------------------------------------------
" Custom Text Objects
" -----------------------------------------------------------------------------

" Textobjects for []
onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" -----------------------------------------------------------------------------
" Buffer & Window management
" -----------------------------------------------------------------------------

" Buffer switching and terminalion
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :Bdelete<cr>
map gdo :Bonly<cr>

" Buffer list
map gl :ls<return>

" -----------------------------------------------------------------------------
" Ultisnips Shortcut Settings
" Must be loaded before plugin for the shortcut to work,
" so it wont go in after/plugin/...
" -----------------------------------------------------------------------------

let g:UltiSnipsExpandTrigger="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsJumpBackwardTrigger="<s-c-j>"
let g:UltiSnipsSnippetDirectories=["UltiSnips"]

" -----------------------------------------------------------------------------
" Autocommands
" -----------------------------------------------------------------------------

" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja
" Set html5 syntax for vue files to fix broken indentation
au BufRead,BufNewFile *.vue set filetype=html
au BufRead,BufNewFile *.zsh* set filetype=zsh

" Remove trailing whitespaces automatically before save
autocmd BufWritePre * call utils#stripTrailingWhitespaces()

" Restore enter for the quickfix window
autocmd FileType qf nnoremap <buffer> <CR> <CR>

" Exit FZF by pressing escape
autocmd! FileType fzf tnoremap <buffer> <esc> <C-c>

" Preview quickfix result
autocmd FileType qf nnoremap <buffer> <Tab> <Enter><C-W>j

" -----------------------------------------------------
" Run linters after save
" -----------------------------------------------------

