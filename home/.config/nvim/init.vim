" ======================================================================================================================
" 1.0 Plugin manager (Plug) settings
" ======================================================================================================================
"{{{

" Autoinstall {{{
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif
" }}}

call plug#begin('~/.config/nvim/plugged')
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Language agnostic plugins {{{
" ---------------------------------------------------------------------------------------------------------------------
" Asynchronous maker and linter (needs linters to work)
Plug 'benekastah/neomake', { 'on': ['Neomake'] }
" Snippet suport
Plug 'SirVer/ultisnips'              
" Toggle comment
Plug 'tomtom/tcomment_vim'           
" Create directories on save
Plug 'duggiefresh/vim-easydir'       
" Delete all buffers except the current one
Plug 'vim-scripts/BufOnly.vim'       
" More . repeat functionality
Plug 'tpope/vim-repeat'              
" Repeat the last F keyword with f
Plug 'rhysd/clever-f.vim'            
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Text insertion/manipulation {{{
" ---------------------------------------------------------------------------------------------------------------------
" Automatically closing pair stuff
Plug 'cohama/lexima.vim'
" Split oneliners 
Plug 'AndrewRadev/splitjoin.vim'     
" Sort alphabetically gsa
Plug 'christoomey/vim-sort-motion'   
" Switch argument order
Plug 'AndrewRadev/sideways.vim'      
" Toggle booleans with '+'
Plug 'Toggle'                        
" Change surrounding characters
Plug 'tpope/vim-surround'                         
" Easy alignment
Plug 'godlygeek/tabular', { 'on':  'Tabularize' } 
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Text Objects
" ---------------------------------------------------------------------------------------------------------------------
Plug 'kana/vim-textobj-user'                  " Custom Text Objects
Plug 'kana/vim-textobj-function'              " Function Text Object
Plug 'kana/vim-textobj-line'                  " Line text object
Plug 'thinca/vim-textobj-function-javascript' " Function Text Object for JS
Plug 'michaeljsmith/vim-indent-object'        " Indentation Text Object
Plug 'vim-scripts/ReplaceWithRegister'        " Replace motion with gr{motion}
Plug 'whatyouhide/vim-textobj-xmlattr'        " XML Attribute Textobject X

" Improved targets line cin) next parens
Plug 'wellle/targets.vim'
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Javascript
" ---------------------------------------------------------------------------------------------------------------------
" Open files with 'gf' without extensions
Plug 'moll/vim-node' 
" Modern JS support (indent, syntax, etc)
Plug 'pangloss/vim-javascript'
" JSON syntax
Plug 'sheerun/vim-json'
" Vue support
Plug 'posva/vim-vue', { 'for': ['vue'] }
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" PHP
" ---------------------------------------------------------------------------------------------------------------------
" Better indentation support for PHP files with HTML
Plug 'captbaritone/better-indent-support-for-php-with-html', { 'for': ['php'] }
" Twig support for vim
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja'] }
"}}}


" ---------------------------------------------------------------------------------------------------------------------
" HTML / CSS
" ---------------------------------------------------------------------------------------------------------------------
" HTML5 syntax
Plug 'othree/html5.vim'
" Color highlighter
Plug 'lilydjwg/colorizer', { 'for': ['css', 'sass', 'scss', 'less', 'html', 'xdefaults', 'javascript', 'javascript.jsx'] }

Plug 'mattn/emmet-vim'
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Interface improving {{{
" ---------------------------------------------------------------------------------------------------------------------
" Lightline (simple status line)
Plug 'itchyny/lightline.vim'
" Buffers tabline
Plug 'ap/vim-buftabline'
" Native colorpicker
Plug 'KabbAmine/vCoolor.vim'
" Nerdtree file browser
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeFind', 'NERDTreeToggle'] }
" incrementally highlights ALL pattern matches unlike default 'incsearch'.
Plug 'haya14busa/incsearch.vim'
" Multiple Cursors
Plug 'terryma/vim-multiple-cursors'

" Search plugins
if has("gui_macvim")
  Plug 'ctrlpvim/ctrlp.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
endif
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Focus Mode
" ---------------------------------------------------------------------------------------------------------------------
" Syntax highlighting just on the current line
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
" Focus Mode
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }   
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" External tools integration plugins {{{
" ---------------------------------------------------------------------------------------------------------------------
" Fugitive
Plug 'tpope/vim-fugitive'
" Gitgutter
Plug 'airblade/vim-gitgutter'
" Color picker
Plug 'KabbAmine/vCoolor.vim', { 'on': ['VCoolor', 'VCase'] }
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Colorschemes {{{
" ---------------------------------------------------------------------------------------------------------------------
" Hybrid
Plug 'w0ng/vim-hybrid'
" Badwolf
Plug 'sjl/badwolf'
" Molokai
Plug 'tomasr/molokai'
" Iceberg
Plug 'cocopon/iceberg.vim'
" Tender
Plug 'jacoborus/tender.vim'
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" Other {{{
" ---------------------------------------------------------------------------------------------------------------------
" Easily expand selected region
Plug 'terryma/vim-expand-region'
" Matchit enhances jump motions
Plug 'edsono/vim-matchit'
" Delete all but current buffer
Plug 'vim-scripts/BufOnly.vim', { 'on': 'Bonly' }
"}}}

" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" 1.2 End of plugin declaration
" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
call plug#end()
"}}}


" ======================================================================================================================
" 2.0 Basic settings (Neovim defaults: https://neovim.io/doc/user/vim_diff.html#nvim-option-defaults) {{{
" ======================================================================================================================
"{{{
set shell=/bin/zsh                          " Setting shell to zsh
set number                                  " Line numbers on
set showmode                                " Always show mode
set hidden                                  " Enables to switch between unsaved buffers and keep undo history
set noswapfile                              " New buffers will be loaded without creating a swapfile
set lazyredraw                              " Don't redraw while executing macros (better performance)
set showmatch                               " Show matching brackets when text indicator is over them
set nostartofline                           " Prevent cursor from moving to beginning of line when switching buffers
set nojoinspaces                            " No extra space when joining a line which ends with . ? !
set suffixesadd+=.js,.rb                    " Add js and ruby files to suffixes
set synmaxcol=160                           " Don't try to syntax highlight minified files

" ---------------------------------------------------------------------------------------------------------------------
" 2.1 Split settings (more natural) {{{
" ---------------------------------------------------------------------------------------------------------------------
set splitbelow                              " Splitting a window will put the new window below the current
set splitright                              " Splitting a window will put the new window right of the current
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" 2.2 Timeout settings {{{
" ---------------------------------------------------------------------------------------------------------------------
" Time out on key codes but not mappings. Basically this makes terminal Vim work sanely. (by Steve Losh)
set notimeout
set ttimeout
set ttimeoutlen=10
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" 2.4 Search settings {{{
" ---------------------------------------------------------------------------------------------------------------------
set incsearch                               " Incremental search
set ignorecase                              " Ignore case by default
set smartcase                               " Make search case sensitive only if it contains uppercase letters
set wrapscan                                " Search again from top when reached the bottom
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" 2.5 Persistent undo settings {{{
" ---------------------------------------------------------------------------------------------------------------------
if has('persistent_undo')
  set undofile
  set undodir=~/.config/nvim/tmp/undo//
endif
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" 2.6 White characters settings {{{
" ---------------------------------------------------------------------------------------------------------------------
set list
set listchars=tab:⋅⋅,trail:●,extends:#,nbsp:.
set showbreak=↪
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" 2.7 Filetype settings {{{
" ---------------------------------------------------------------------------------------------------------------------
filetype plugin on
filetype indent on
"}}}

" ---------------------------------------------------------------------------------------------------------------------
" 2.8 Folding settings {{{
" ---------------------------------------------------------------------------------------------------------------------
"}}}
set foldmethod=indent " Fold by indentation
set foldnestmax=2     " deepest fold is 10 levels
set nofoldenable      " dont fold by default
set foldlevel=1       " this is just what i use

" ---------------------------------------------------------------------------------------------------------------------
" 2.9 Omni completion settings {{{
" ---------------------------------------------------------------------------------------------------------------------
set completeopt-=preview                    " Don't show preview scratch buffers
set wildignore=*.o,*.obj,*~
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*.gem
set wildignore+=tmp/**
"}}}


" ---------------------------------------------------------------------------------------------------------------------
" 2.10 Neovim specific settings {{{
" ---------------------------------------------------------------------------------------------------------------------
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1         " Set an environment variable to use the t_SI/t_EI hack
" let g:loaded_python_provider=1              " Disable python 2 interface
" let g:python_host_skip_check=1              " Skip python 2 host check
" let g:python3_host_prog='/usr/bin/python3'  " Set python 3 host program
"}}}

" -----------------------------------------------------
" 2.12 True colors settings {{{
" -----------------------------------------------------
if has('termguicolors')
  set termguicolors " Turn on true colors support
endif
" Tmux still doesn't support this
"}}}


" ======================================================================================================================
" 3.0 Mapping settings
" ======================================================================================================================

" -----------------------------------------------------
" 3.1 Setting leader {{{
" -----------------------------------------------------
let g:mapleader="\<space>"
"}}}

" Reload .vimrc
" This would cause the last search to be highlighted,
" Workaround to disable this.
nnoremap <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<return><esc>

" -----------------------------------------------------
" 3.3 Vim defaults overriding {{{
" -----------------------------------------------------

" When cycling windows ignore NERDTree 
nmap <silent> <C-w><C-w> :call utils#intelligentCycling()<CR>
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

" When jump to next match also center screen
" Note: Use :norm! to make it count as one command. (i.e. for i_CTRL-o)
nnoremap <silent> n :norm! nzz<CR>
nnoremap <silent> N :norm! Nzz<CR>
vnoremap <silent> n :norm! nzz<CR>
vnoremap <silent> N :norm! Nzz<CR>

" Quick replay 'q' macro
nnoremap Q @q

" Don't yank to default register when changing something
nnoremap c "xc
xnoremap c "xc

" Don't cancel visual select when shifting
xnoremap <  <gv
xnoremap >  >gv

" Terminal mode mappings
if has('nvim')
  tnoremap <ESC> <C-\><C-n>
  tnoremap ,<ESC> <ESC>
endif

" Make . work with visually selected lines
xnoremap . :norm.<CR>

" Keep the cursor in place while joining lines
nnoremap J mzJ`z
"
" " -----------------------------------------------------
" " 3.4 Common tasks {{{
" " -----------------------------------------------------
"
" " Error mnemonic (Neomake uses location list)
" nnoremap ]e :lnext<CR>
" nnoremap [e :lprevious<CR>
"
" " [S]plit line (sister to [J]oin lines) S is covered by cc.
" nnoremap S mzi<CR><ESC>`z
"
" " Reselect last-pasted text
" nnoremap gp `[v`]
"
" " -----------------------------------------------------
" " 3.5 Window / Buffer Management
" " -----------------------------------------------------
"
" " Go to next buffer
" map gn :bn<cr>
" " Go to previous buffer
" map gp :bp<cr>
" " Switch to last buffer
" map gb :b#<cr>
" " Close all buffers
" map gdd :Bclose<cr>
" " Close all buffers except the current one
" map gdo :Bonly<cr>
"
" " -----------------------------------------------------
" " 3.5 Etc
" " -----------------------------------------------------
"
" " Clear search highlights when tapping escape
" nnoremap <silent><esc> :noh<return><esc>
" nnoremap <esc>^[ <esc>^[
"
" " -----------------------------------------------------
" " 3.6 F-key actions {{{
" " -----------------------------------------------------
"
" " Toggle spelling
" nnoremap <silent> <F4> :set spell!<CR> :set spell?<CR>
" " Source (reload configuration)
" nnoremap <silent> <F5> :source $MYVIMRC<CR>
" " Toggle search highlight
" nnoremap <silent> <F6> :set nohlsearch!<CR> :set nohlsearch?<CR>
" " Toggle white characters visibility
" nnoremap <silent> <F7> :set list!<CR> :set list?<CR>


" ======================================================================================================================
" 4.0 Plugins settings
" ======================================================================================================================
"{{{

" -----------------------------------------------------
" FZF
" -----------------------------------------------------

let g:fzf_action = {
	\ 'ctrl-t': 'tab split',
	\ 'ctrl-x': 'split',
	\ 'ctrl-v': 'vsplit' }

" Search in current git index
nnoremap <silent> <C-p> :GitFiles<CR>
" Search Recent Files
" nnoremap <silent> <C-e>:History<CR>
" Search open buffers
" Search available commands
nnoremap <silent> <leader>c :Commands<CR>
" Search lines in all open buffers
nnoremap <silent> <leader>; :BLines<CR>
" Search lines in current file
nnoremap <silent> <leader>. :Lines<CR>
" Search commits
nnoremap <silent> <leader>gl :Commits<CR>
" Search commits for current file
nnoremap <silent> <leader>ga :BCommits<CR>

" -----------------------------------------------------
" Tabularize
" -----------------------------------------------------

" Tabularize by characters with easy shortcuts
" Match // Tabularize Comments after commands not at beginning of line
map <Leader>/ :Tabularize /\s\zs\/\/<cr>
map <Leader>= :Tabularize /=<cr>
map <Leader>: :Tabularize /:\s\zs<cr>
map <Leader>, :Tabularize /,<cr>
map <Leader>" :Tabularize /\s\zs"<cr>

" -----------------------------------------------------
" Lightline settings {{{
" -----------------------------------------------------
let g:lightline = {
      \ 'colorscheme': 'powerline',
      \ 'tab': {
      \   'active': [ 'filename' ],
      \   'inactive': [ 'filename' ]
      \ },
      \ 'active': {
      \   'left': [ [ 'mode', 'paste' ], [ 'readonly', 'filename' ] ],
      \   'right': [ [ 'lineinfo' ], [ 'percent' ], [ 'filetype', 'fileencoding', 'fileformat' ] ]
      \ },
      \ 'component': {
      \   'readonly': '%{&filetype=="help"?"HELP":&readonly?"RO":""}'
      \ },
      \ 'component_function': {
      \   'mode': 'utils#lightLineMode',
      \   'filename': 'utils#lightLineFilename',
      \   'filetype': 'utils#lightLineFiletype',
      \   'fileformat': 'utils#lightLineFileformat',
      \   'fileencoding': 'utils#lightLineFileencoding'
      \ },
      \ 'component_visible_condition': {
      \   'readonly': '(&readonly)'
      \ },
      \ 'separator': { 'left': '', 'right': '' },
      \ 'subseparator': { 'left': '', 'right': '' }
      \ }
"}}}


