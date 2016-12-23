" -----------------------------------------------------------------------------
" Plug AutoInstall
" -----------------------------------------------------------------------------

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
" UNIX like commands for VIM
Plug 'tpope/vim-eunuch'
" Edit Macros
Plug 'dohsimpson/vim-macroeditor', { 'on': ['MacroEdit'] }
" Editorconfig loading
Plug 'editorconfig/editorconfig-vim'
" Search and Replace Tool
Plug 'wincent/ferret'
" Session Management
Plug 'tpope/vim-obsession'
" Better replacement
Plug 'tpope/vim-abolish'
" Ctags
" Plug 'ludovicchabant/vim-gutentags'
" Open URL
Plug 'dhruvasagar/vim-open-url'
" Make Header
Plug 'floscr/MakeHeader.vim'

" Local .vimrc files
" Plug 'krisajenkins/vim-projectlocal'

Plug 'janko-m/vim-test'
Plug 'arnaud-lb/vim-php-namespace', { 'for': ['php'] }

" -----------------------------------------------------------------------------
" Text insertion/manipulation
" -----------------------------------------------------------------------------

Plug 'AndrewRadev/splitjoin.vim' " Split oneliners
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
Plug 'jasonlong/vim-textobj-css'              " CSS Class caf

" Improved targets line cin) next parens
Plug 'wellle/targets.vim'

" -----------------------------------------------------------------------------
" Javascript
" -----------------------------------------------------------------------------

Plug 'moll/vim-node', { 'for': ['js', 'vue'] }          " Open files with 'gf' without extensions
Plug 'pangloss/vim-javascript', { 'branch': 'develop' } " Modern JS support (indent, syntax, etc)
Plug 'jelera/vim-javascript-syntax'                     " More Syntax highlighting?
Plug 'othree/jspc.vim'
" Plug 'sheerun/vim-json', { 'for': ['json'] }            " JSON syntax
Plug 'posva/vim-vue', { 'for': ['vue'] }                " Vue support
Plug 'heavenshell/vim-jsdoc'                            " Js Doc Blcok
Plug 'maksimr/vim-jsbeautify'                           " Beautify files

" Plug 'carlitux/deoplete-ternjs',  { 'do': 'npm install --cache-min Infinity --loglevel http -g tern' }
" Plug 'ternjs/tern_for_vim',       { 'do': 'npm install --cache-min Infinity --loglevel http' }

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
Plug 'hail2u/vim-css3-syntax', { 'for': ['css', 'html', 'vue'] }
" SCSS Syntax
Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss'] }
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
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all --no-update-rc' }
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
" Project Root File Creation
Plug 'dbakker/vim-projectroot'

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

" Lightweight File Explorer
" Like dirvish, but supports autochdir
Plug 'jeetsukumaran/vim-filebeagle'

Plug 'christoomey/vim-tmux-navigator'
Plug 'wellle/tmux-complete.vim'

Plug 'floscr/FZF-cdnj' | Plug 'mattn/webapi-vim'

Plug 'floscr/regex-bookmarks'

" -----------------------------------------------------------------------------
" Plug End
" -----------------------------------------------------------------------------

call plug#end()
