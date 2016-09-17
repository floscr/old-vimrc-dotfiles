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

" Editor config for vim
Plug 'editorconfig/editorconfig-vim'
" Async Linting, Use project .eslintrc first
Plug 'benekastah/neomake', { 'on': ['Neomake'] } | Plug 'jaawerth/neomake-local-eslint-first'
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
" Close buffer
Plug 'rbgrouleff/bclose.vim'
" UNIX like commands for VIM
Plug 'tpope/vim-eunuch'
" Ctags
Plug 'craigemery/vim-autotag'
" Edit Macros
Plug 'dohsimpson/vim-macroeditor', { 'on': ['MacroEdit'] }
" Editorconfig loading
Plug 'editorconfig/editorconfig-vim'

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
Plug 'kana/vim-textobj-entire'                " Entire Buffer ae
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
Plug 'moll/vim-node', { 'for': ['js', 'vue'] }
" Modern JS support (indent, syntax, etc)
Plug 'pangloss/vim-javascript', { 'branch': 'develop' }
Plug 'jelera/vim-javascript-syntax'
" JSON syntax
Plug 'sheerun/vim-json', { 'for': ['json'] }
" Vue support
Plug 'posva/vim-vue', { 'for': ['vue'] }

Plug 'carlitux/deoplete-ternjs',  { 'do': 'npm install --cache-min Infinity --loglevel http -g tern' }
Plug 'ternjs/tern_for_vim',       { 'do': 'npm install --cache-min Infinity --loglevel http' }


" -----------------------------------------------------------------------------
" PHP
" -----------------------------------------------------------------------------

" Better PHP Syntax
Plug 'StanAngeloff/php.vim', { 'for': 'php' }
" Better indentation support for PHP files with HTML
Plug 'captbaritone/better-indent-support-for-php-with-html', { 'for': ['php'] }
" Twig support for vim
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja'] }
" Blade Syntax
Plug 'jwalton512/vim-blade', { 'for': ['php'] }
" DocBlocks for PHP
Plug 'tobyS/vmustache' | Plug 'tobyS/pdv', { 'for': ['php'] }

" -----------------------------------------------------------------------------
" HTML / CSS
" -----------------------------------------------------------------------------

" CSS3 Syntax
Plug 'hail2u/vim-css3-syntax', { 'for': ['css', 'sass', 'scss', 'html', 'vue'] }
" HTML5 syntax
Plug 'othree/html5.vim'
" Color highlighter
Plug 'lilydjwg/colorizer', { 'for': ['css', 'sass', 'scss', 'less', 'html', 'xdefaults', 'javascript', 'javascript.jsx'] }
" Show matching tag
Plug 'gregsexton/MatchTag', { 'for': ['html', 'javascript', 'blade', 'php', 'twig'] }
" Pug / Jade Syntax
Plug 'digitaltoad/vim-pug', { 'for': ['jade', 'pug'] }
" Emmet
Plug 'mattn/emmet-vim'

" -----------------------------------------------------------------------------
" Interface improving
" -----------------------------------------------------------------------------

Plug 'kshenoy/vim-signature'
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
" Close Buffer
Plug 'moll/vim-bbye', { 'on': 'Bdelete' }

" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" 1.2 End of plugin declaration
" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

call plug#end()

" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
" 1.3 Custom plugins
" ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

" Trim trailing whitespace when saving a buffer
source ~/.config/nvim/custom-plugins/autowrite.vim

" =============================================================================
" 2.0 Default Settings (Neovim defaults: https://neovim.io/doc/user/vim_diff.html#nvim-option-defaults)
" =============================================================================

set shell=/bin/zsh             " Setting shell to zsh
set number                     " Line numbers on
set showmode                   " Always show mode
set hidden                     " Enables to switch between unsaved buffers and keep undo history
set noswapfile                 " New buffers will be loaded without creating a swapfile
set lazyredraw                 " Don't redraw while executing macros (better performance)
set nostartofline              " Prevent cursor from moving to beginning of line when switching buffers
set nojoinspaces               " No extra space when joining a line which ends with . ? !
set suffixesadd+=.js           " Add js and ruby files to suffixes
set autochdir                  " Set working dir to the current file
set shortmess+=I               " Turn off the intro
set synmaxcol=800              " Turn off syntax highlighting for lines longer than 800 characters
set noshowmatch                " Show matching tags
                               " having this turned on will make the cursor jump around
                               " weirdly
set backspace=indent,eol,start " Better backspace

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
set colorcolumn=80           " Add a colorized column tho show the maximal text length
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
let g:loaded_python_provider=1              " Disable python 2 interface
let g:python_host_skip_check=1              " Skip python 2 host check
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
" 3.3 Keyboard shortcuts / bindings
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

" Reverse join (Turn single line comments to inline comments)
nnoremap ,J jddkPmzJ`z

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

" Make * star work in visual mode
vnoremap <silent> * y:let @/=@"<cr>:set hlsearch<cr>n

" Use the last used search to use in replace command
nmap <expr> M ':%s/' . @/ . '//g<LEFT><LEFT>'

" Textobjects for []
onoremap ir i[
onoremap ar a[
vnoremap ir i[
vnoremap ar a[

" Toggle the error list
nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>

" Workaround for ctrl-h to work
" workaround for https://github.com/neovim/neovim/issues/2048
 if has('nvim')
   nmap <BS> <C-W>h
 endif

nnoremap ,ocf :OpenChangedFiles<CR>

" -----------------------------------------------------
" 3.5 Buffer & Window management
" -----------------------------------------------------

" Buffers
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :Bdelete<cr>
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

" Reverse to find if not in git root
function! s:find_git_root()
  return system('git rev-parse --show-toplevel 2> /dev/null')[:-2]
endfunction
command! ProjectFiles execute 'Files' s:find_git_root()

" Search in current git index
nnoremap <silent> <C-p> :ProjectFiles<CR>
" Search Recent Files
nnoremap <silent> <leader>h :History<CR>
" Search open buffers
nnoremap <silent> <leader>b :Buffers<CR>
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

map <leader>n :NERDTreeToggle<CR>
map <leader>f :NERDTreeFind<CR>

" -----------------------------------------------------
" Neomake
" -----------------------------------------------------

" Error mnemonic (Neomake uses location list)
nmap <silent> [e :LocationPrevious<CR>
nmap <silent> ]e :LocationNext<CR>

" nnoremap ]e :lnext<CR>
" nnoremap [e :lprevious<CR>

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

let g:tern_request_timeout = 1
let g:tern_show_signature_in_pum = '0'  " This do disable full signature type on autocomplete

let g:deoplete#enable_at_startup=1
let g:deoplete#enable_refresh_always=0
let g:deoplete#file#enable_buffer_path=1
" Trigger deoplete only when pressing tab
" let g:deoplete#disable_auto_complete=1

" omnifuncs
augroup omnifuncs
  autocmd!
  autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType scsss setlocal omnifunc=csscomplete#CompleteCSS
  autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
  autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
  autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
  autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags
augroup end
" tern
if exists('g:plugs["tern_for_vim"]')
  let g:tern_show_argument_hints = 'on_hold'
  let g:tern_show_signature_in_pum = 1
  autocmd FileType javascript setlocal omnifunc=tern#Complete
endif

let g:tern#command = ["tern"]
let g:tern#arguments = ["--persistent"]
" let g:deoplete#sources={}
" let g:deoplete#sources._    = ['buffer', 'file', 'ultisnips']
" let g:deoplete#sources.vim  = ['buffer', 'member', 'file', 'ultisnips']
" let g:deoplete#sources['javascript.jsx'] = ['tern', 'buffer', 'file', 'ultisnips']
" let g:deoplete#sources.css  = ['buffer', 'member', 'file', 'omni', 'ultisnips']
" let g:deoplete#sources.scss = ['tern', 'buffer', 'member', 'file', 'omni', 'ultisnips']
" let g:deoplete#sources.html = ['buffer', 'member', 'file', 'omni', 'ultisnips']

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

" Disable default Keyboard mappings
let g:gitgutter_map_keys = 0

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

let g:lightline = {
  \ 'component': {
  \   'filename': '%n: %t'
  \ }
  \ }
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

" -----------------------------------------------------
" NeoMake
" -----------------------------------------------------

let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_verbose=0
let g:neomake_warning_sign = {
      \ 'text': '❯',
      \ 'texthl': 'WarningMsg',
      \ }
let g:neomake_error_sign = {
      \ 'text': '❯',
      \ 'texthl': 'ErrorMsg',
      \ }
"}}}

" -----------------------------------------------------
" PDV - PHP Documentor for VIM - 2
" -----------------------------------------------------

let g:pdv_template_dir = $HOME . '/.config/nvim/plugged/pdv/templates_snip'
nnoremap <silent> ,p :call pdv#DocumentWithSnip()<CR>

" -----------------------------------------------------
" PHP Syntax Plugin
" -----------------------------------------------------

function! PhpSyntaxOverride()
  hi! def link phpDocTags  phpDefine
  hi! def link phpDocParam phpType
endfunction

" Load the better syntax highlighting support
augroup phpSyntaxOverride
  autocmd!
  autocmd FileType php call PhpSyntaxOverride()
augroup END

" =============================================================================
" 7.0 Autocommands
" =============================================================================

" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja
" Set html5 syntax for vue files to fix broken indentation
au BufRead,BufNewFile *.vue set filetype=html

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

source ~/.config/nvim/autoload/utils.vim
