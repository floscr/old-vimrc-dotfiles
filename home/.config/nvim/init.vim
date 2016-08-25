" =============================================================================
" 1.0 Plugin manager (Plug) settings
" =============================================================================

" Autoinstall
if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')

" -----------------------------------------------------------------------------
" Language agnostic plugins
" -----------------------------------------------------------------------------

" Asynchronous maker and linter (needs linters to work)
Plug 'benekastah/neomake', { 'on': ['Neomake'] }
" Autocomplete
Plug 'Shougo/deoplete.nvim'
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
" Automatically closing pair stuff
Plug 'cohama/lexima.vim'
" Edit Macros
Plug 'dohsimpson/vim-macroeditor', { 'on': ['MacroEdit'] }

" -----------------------------------------------------------------------------
" Text insertion/manipulation
" -----------------------------------------------------------------------------

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
" CIQ to match any pairs
Plug 'kurkale6ka/vim-pairs'
" Easy alignment
Plug 'godlygeek/tabular', { 'on':  'Tabularize' }

" -----------------------------------------------------------------------------
" Text Objects
" -----------------------------------------------------------------------------

Plug 'kana/vim-textobj-user'                  " Custom Text Objects
Plug 'kana/vim-textobj-function'              " Function Text Object
Plug 'kana/vim-textobj-line'                  " Line text object
Plug 'thinca/vim-textobj-function-javascript' " Function Text Object for JS
Plug 'michaeljsmith/vim-indent-object'        " Indentation Text Object
Plug 'vim-scripts/ReplaceWithRegister'        " Replace motion with gr{motion}
Plug 'whatyouhide/vim-textobj-xmlattr'        " XML Attribute Textobject X

" Improved targets line cin) next parens
Plug 'wellle/targets.vim'

" -----------------------------------------------------------------------------
" Javascript
" -----------------------------------------------------------------------------

" Open files with 'gf' without extensions
Plug 'moll/vim-node'
" Modern JS support (indent, syntax, etc)
Plug 'pangloss/vim-javascript'
Plug 'jelera/vim-javascript-syntax'
" JSON syntax
Plug 'sheerun/vim-json'
" Vue support
Plug 'posva/vim-vue', { 'for': ['vue'] }

" -----------------------------------------------------------------------------
" PHP
" -----------------------------------------------------------------------------

" Better indentation support for PHP files with HTML
Plug 'captbaritone/better-indent-support-for-php-with-html', { 'for': ['php'] }
" Twig support for vim
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja'] }
" Blade Syntax
Plug 'jwalton512/vim-blade'

" -----------------------------------------------------------------------------
" HTML / CSS
" -----------------------------------------------------------------------------

" HTML5 syntax
Plug 'othree/html5.vim'
" Color highlighter
Plug 'lilydjwg/colorizer', { 'for': ['css', 'sass', 'scss', 'less', 'html', 'xdefaults', 'javascript', 'javascript.jsx'] }

" Pug / Jade Syntax
Plug 'digitaltoad/vim-pug'

Plug 'mattn/emmet-vim'

" -----------------------------------------------------------------------------
" Interface improving
" -----------------------------------------------------------------------------

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
" Undo Tree visualization
Plug 'mbbill/undotree', { 'on': ['UndotreeToggle', 'UndotreeFocus', 'UndotreeHide', 'UndotreeShow'] }
" Interactive Coding with vim
Plug 'metakirby5/codi.vim', { 'on': ['Codi'] }

" Search plugins
if has("gui_macvim")
  Plug 'ctrlpvim/ctrlp.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
endif

" -----------------------------------------------------------------------------
" Focus Mode
" -----------------------------------------------------------------------------

" Syntax highlighting just on the current line
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
" Focus Mode
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }

" -----------------------------------------------------------------------------
" External tools integration plugins
" -----------------------------------------------------------------------------

" Fugitive
Plug 'tpope/vim-fugitive'
" Git log viewer (Gitv! for file mode)
Plug 'gregsexton/gitv', { 'on': 'Gitv' }
" Gitgutter
Plug 'airblade/vim-gitgutter'
" Color picker
Plug 'KabbAmine/vCoolor.vim', { 'on': ['VCoolor', 'VCase'] }

" -----------------------------------------------------------------------------
" Colorschemes
" -----------------------------------------------------------------------------

" Hybrid
Plug 'w0ng/vim-hybrid'
" Hybrid lightline theme
Plug 'cocopon/lightline-hybrid.vim'
" Papercolor Theme
Plug 'NLKNguyen/papercolor-theme'

" -----------------------------------------------------------------------------
" Other
" -----------------------------------------------------------------------------

" Easily expand selected region
Plug 'terryma/vim-expand-region'
" Matchit enhances jump motions
Plug 'edsono/vim-matchit'
" Delete all but current buffer
Plug 'vim-scripts/BufOnly.vim', { 'on': 'Bonly' }
" Delete current buffer
Plug 'rbgrouleff/bclose.vim', { 'on': 'Bclose' }

" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" 1.2 End of plugin declaration
" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
call plug#end()

" =============================================================================
" 2.0 Basic settings (Neovim defaults: https://neovim.io/doc/user/vim_diff.html#nvim-option-defaults)
" =============================================================================

set shell=/bin/zsh   " Setting shell to zsh
set number           " Line numbers on
set showmode         " Always show mode
set hidden           " Enables to switch between unsaved buffers and keep undo history
set noswapfile       " New buffers will be loaded without creating a swapfile
set lazyredraw       " Don't redraw while executing macros (better performance)
set nostartofline    " Prevent cursor from moving to beginning of line when switching buffers
set nojoinspaces     " No extra space when joining a line which ends with . ? !
set suffixesadd+=.js " Add js and ruby files to suffixes
set autochdir        " Set working dir to the current file
set shortmess+=I     " Turn off the intro
set synmaxcol=800    " Turn off syntax highlighting for lines longer than 800 characters
set noshowmatch      " Show matching tags
                     " having this turned on will make the cursor jump around
                     " weirdly
" set showmatch        " Show matching brackets when text indicator is over them

" -----------------------------------------------------------------------------
" 2.1 Color Settings
" -----------------------------------------------------------------------------
 " For Neovim 0.1.3 and 0.1.4
 let $NVIM_TUI_ENABLE_TRUE_COLOR=1

 " Or if you have Neovim >= 0.1.5
 if (has("termguicolors"))
   set termguicolors
 endif

set background=dark
let g:hybrid_reduced_contrast = 1
colorscheme hybrid
let $NVIM_TUI_ENABLE_TRUE_COLOR=1

" -----------------------------------------------------------------------------
" 2.1 Split settings (more natural)
" -----------------------------------------------------------------------------
set splitbelow " Splitting a window will put the new window below the current
set splitright " Splitting a window will put the new window right of the current

" -----------------------------------------------------------------------------
" 2.1 Wrap Settings
" -----------------------------------------------------------------------------
set colorcolumn=+1           " Add a colorized column tho show the maximal text length
set textwidth=80             " Set the recommended text length to 80 characters
set nowrap                   " Don't wrap lines
set textwidth=0 wrapmargin=0 " this turns off physical line wrapping (ie: automatic insertion of newlines)

" -----------------------------------------------------------------------------
" 2.2 Timeout settings
" -----------------------------------------------------------------------------
" Time out on key codes but not mappings. Basically this makes terminal Vim work sanely. (by Steve Losh)
set notimeout
set ttimeout
set ttimeoutlen=10

" -----------------------------------------------------------------------------
" 2.4 Search settings
" -----------------------------------------------------------------------------
set incsearch                               " Incremental search
set ignorecase                              " Ignore case by default
set smartcase                               " Make search case sensitive only if it contains uppercase letters
set wrapscan                                " Search again from top when reached the bottom

" -----------------------------------------------------------------------------
" 2.5 Persistent undo settings
" -----------------------------------------------------------------------------
if has('persistent_undo')
  set undofile
  set undodir=~/.config/nvim/tmp/undo//
endif

" -----------------------------------------------------------------------------
" 2.6 White characters settings
" -----------------------------------------------------------------------------
set list
set listchars=tab:⋅⋅,trail:●,extends:#,nbsp:.
set showbreak=↪

" -----------------------------------------------------------------------------
" 2.6 Indentation
" -----------------------------------------------------------------------------
set expandtab
set softtabstop=2
set shiftwidth=2
set noshiftround
set autoindent " Automatic indentation
set copyindent " Copy previous indetation on autoindenting

" -----------------------------------------------------------------------------
" 2.7 Filetype settings
" -----------------------------------------------------------------------------
filetype plugin on
filetype indent on

" -----------------------------------------------------------------------------
" 2.8 Folding settings
" -----------------------------------------------------------------------------
set foldmethod=indent " Fold by indentation
set foldnestmax=2     " deepest fold is 10 levels
set nofoldenable      " dont fold by default
set foldlevel=1       " this is just what i use

" -----------------------------------------------------------------------------
" 2.9 Omni completion settings
" -----------------------------------------------------------------------------
set completeopt-=preview                    " Don't show preview scratch buffers
set wildignore=*.o,*.obj,*~
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*.gem
set wildignore+=tmp/**

" -----------------------------------------------------------------------------
" 3.0 Scrolloff
" -----------------------------------------------------------------------------
" Start scrolling:
" Bottom and Top: 10 Lines
" Side: 15 lines
set scrolloff=10
set sidescrolloff=15
" Scroll one column on the side
set sidescroll=1

" -----------------------------------------------------------------------------
" 2.10 Neovim specific settings
" -----------------------------------------------------------------------------
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1         " Set an environment variable to use the t_SI/t_EI hack
let g:loaded_python_provider = 1
let g:python3_host_prog = '/usr/local/bin/python3'
let g:python_host_prog = '/usr/bin/python'

" -----------------------------------------------------
" 2.12 True colors settings
" -----------------------------------------------------
if has('termguicolors')
  set termguicolors " Turn on true colors support
endif
" Tmux still doesn't support this

" =============================================================================
" 3.0 Mapping settings
" =============================================================================

" -----------------------------------------------------
" 3.1 Setting leader
" -----------------------------------------------------
let g:mapleader="\<space>"

" Reload .vimrc
" This would cause the last search to be highlighted,
" Workaround to disable this.
nnoremap <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<return><esc>

" -----------------------------------------------------
" 3.3 Vim defaults overriding
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

" Yank text to the OS X clipboard
noremap <leader>y "*y
noremap <leader>yy "*Y

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

" Quick Close
nnoremap <C-c> :q<return>

" Indentation using tab
" Normal tab is bound to Deoplete completion
imap <S-Tab> <C-o><<
map <S-Tab> <<
map <Tab> >>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

" Create file under cursor
nnoremap gF :e <cfile><cr>

" Enter command by pressing enter
nnoremap <Cr> :

" Open current file in finder
nnoremap <leader><cr> :silent !open .<cr>

" -----------------------------------------------------
" 3.5 Buffer & Window management
" -----------------------------------------------------

" Buffers
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :Bclose<cr>
map gdo :Bonly<cr>
map gl :ls<return>

" Clear highlighting on escape in normal mode
nnoremap <silent><esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" -----------------------------------------------------
" 3.5 Text Objects
" -----------------------------------------------------

" [] Brackets text object
onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" Buffer text object
xnoremap i% GoggV
omap i% :<C-u>normal vi%<CR>

" =============================================================================
" 4.0 Plugins settings
" =============================================================================

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
nnoremap <silent> <leader>h :History<CR>
" Search open buffers
nnoremap <silent> <leader>l :Buffers<CR>
" Search available commands
nnoremap <silent> <leader>c :Commands<CR>
" Search lines in all open buffers
nnoremap <silent> <leader>; :BLines<CR>
" Search lines in current file
nnoremap <silent> <leader>. :Lines<CR>
" Search commits
nnoremap <silent> <leader>gl :Commits<CR>
" Search commits for current file
nnoremap <silent> <leader>gL :BCommits<CR>

" -----------------------------------------------------
" NERDTree
" -----------------------------------------------------

" Search commits for current file
nnoremap <silent> <F3> :call utils#nerdWrapper()<CR>

" -----------------------------------------------------
" Neomake
" -----------------------------------------------------

" Error mnemonic (Neomake uses location list)
nnoremap ]e :lnext<CR>
nnoremap [e :lprevious<CR>

" -----------------------------------------------------
" UltiSnips
" -----------------------------------------------------

let g:UltiSnipsExpandTrigger="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsJumpBackwardTrigger="<s-c-j>"
let g:UltiSnipsSnippetDirectories=["UltiSnips"]

" Open the current filetype snippet file
function! EditFileTypeSnippet()
  execute 'edit ~/.homesick/repos/dotfiles/home/.config/nvim/UltiSnips/' . &filetype . '.snippets'
  " if(&filetype == 'scss')
  " endif
endfunction
nnoremap <leader>es :call EditFileTypeSnippet()<CR>

" -----------------------------------------------------
" Deoplete
" -----------------------------------------------------

let g:deoplete#enable_at_startup=1
let g:deoplete#enable_refresh_always=0
let g:deoplete#file#enable_buffer_path=1
" Trigger deoplete only when pressing tab
let g:deoplete#disable_auto_complete=1

let g:deoplete#sources={}
let g:deoplete#sources._    = ['buffer', 'file', 'ultisnips']
let g:deoplete#sources.vim  = ['buffer', 'member', 'file', 'ultisnips']
let g:deoplete#sources['javascript.jsx'] = ['buffer', 'file', 'ultisnips']
let g:deoplete#sources.css  = ['buffer', 'member', 'file', 'omni', 'ultisnips']
let g:deoplete#sources.scss = ['buffer', 'member', 'file', 'omni', 'ultisnips']
let g:deoplete#sources.html = ['buffer', 'member', 'file', 'omni', 'ultisnips']

" Insert <TAB> or select next match
inoremap <silent> <expr> <Tab> utils#tabComplete()

" Manually trigger tag autocomplete
inoremap <silent> <expr> <C-]> utils#manualTagComplete()

" -----------------------------------------------------
" Fugitive
" -----------------------------------------------------

" Open quickfix window automatically after grepping
autocmd QuickFixCmdPost *grep* cwindow

" Always use vertical diffs
set diffopt+=vertical

" -------
" Commits
" -------

" Add the current file to index
nnoremap <silent> <leader>gf :Git add %:p<CR><CR>
" Add all changes to index
nnoremap <silent> <leader>ga :Git add .<CR><CR>
" Git Status
nnoremap <leader>gs :Gstatus<CR>
" Commit added index
nnoremap <leader>gc :Gcommit -v -q<CR>
" Add and commit current file
nnoremap <space>gt :Gcommit -v -q %:p<CR>

" ---
" Etc
" ---

" Diff current file
nnoremap <leader>gd :Gdiff<CR>
" Show the previus version of a file
nnoremap <space>ge :Gedit<CR>
" Hard reset all changes
nnoremap <silent> <space>grh :silent! Git reset --hard<CR>
" Reset current file
nnoremap <silent> <space>grf :silent! Git checkout HEAD -- %<CR>
" Git grep
nnoremap <space>gp :Ggrep<Space>

" -----------------------------------------------------
" GitGutter
" -----------------------------------------------------

" Only load gitgutter when the file is loaded/saved
" This increases the speed in tab switching
let g:gitgutter_max_signs=9999
let g:gitgutter_eager = 1
let g:gitgutter_realtime = 0

" Next/Prev Git Hunk and center
nmap ghn <Plug>GitGutterNextHunk
nmap ghp <Plug>GitGutterPrevHunk

" Add/Revert Hunks
nmap gha <Plug>GitGutterStageHunk
nmap ghu <Plug>GitGutterUndoHunk

" Gutter Color Overrides
if g:colors_name == "hybrid"
  highlight GitGutterAdd guifg=#99C794
  highlight GitGutterDelete guifg=#CC6665
  highlight GitGutterChangeDelete guifg=#B294BB
  highlight vertsplit guifg=#2E3C47
endif

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
" Emmet
" -----------------------------------------------------

" Remap emmet leader key
let g:user_emmet_leader_key='<C-e>'

" -----------------------------------------------------
" Lightline
" -----------------------------------------------------

let g:lightline = {}
let g:lightline.colorscheme = 'hybrid'

" -----------------------------------------------------
" BufTabLine
" -----------------------------------------------------

nmap <leader>1 <Plug>BufTabLine.Go(1)
nmap <leader>2 <Plug>BufTabLine.Go(2)
nmap <leader>3 <Plug>BufTabLine.Go(3)
nmap <leader>4 <Plug>BufTabLine.Go(4)
nmap <leader>5 <Plug>BufTabLine.Go(5)
nmap <leader>6 <Plug>BufTabLine.Go(6)
nmap <leader>7 <Plug>BufTabLine.Go(7)
nmap <leader>8 <Plug>BufTabLine.Go(8)
nmap <leader>9 <Plug>BufTabLine.Go(9)
nmap <leader>0 <Plug>BufTabLine.Go(10)

let g:buftabline_numbers = 2

" BufBar Theme Overrides
if g:colors_name == "hybrid"
  highlight BufTabLineFill guibg=#2E3C47
  highlight BufTabLineHidden guibg=#2E3C47
endif

" -----------------------------------------------------
" Incsearch
" -----------------------------------------------------

" Set the default search mappings to incsearch
map /  <Plug>(incsearch-forward)
map ?  <Plug>(incsearch-backward)
map g/ <Plug>(incsearch-stay)

" -----------------------------------------------------
" Undotree
" -----------------------------------------------------

nnoremap <silent> U :UndotreeToggle<Cr>

" =============================================================================
" 7.0 Autocommands
" =============================================================================

" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja

" Remove trailing whitespaces automatically before save
autocmd BufWritePre * call utils#stripTrailingWhitespaces()

" Restore enter for the quickfix window
autocmd FileType qf nnoremap <buffer> <CR> <CR>

" Exit FZF by pressing escape
autocmd! FileType fzf tnoremap <buffer> <esc> <C-c>

" -----------------------------------------------------
" 7.1 Run linters after save
" -----------------------------------------------------

" npm install -g eslint
autocmd BufWritePost *.js Neomake eslint

