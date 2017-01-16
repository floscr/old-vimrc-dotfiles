" Plug setup {{{1
" --------------------------------------------------------------------------

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')

" }}}

" Defaults {{{1
" --------------------------------------------------------------------------

if !has('nvim')
  Plug 'noahfrederick/vim-neovim-defaults'
endif

" Vim/TMUX seamless split navigation
Plug 'christoomey/vim-tmux-navigator'

" More . repeat functionality
Plug 'tpope/vim-repeat'

" Ability to load json files for vim
" Used in:
"   + Emmet - To load custom snippets.json
Plug 'mattn/webapi-vim'
" }}}
" Commands {{{
" --------------------------------------------------------------------------

" UNIX like commands for VIM
" :Remove / :Rename / :Move etc...
Plug 'tpope/vim-eunuch'

" Edit Macros
Plug 'dohsimpson/vim-macroeditor', { 'on': ['MacroEdit'] }

" Create non-existing directories on save
Plug 'duggiefresh/vim-easydir'

" Open URLs in Browser
" <gB>: Open url under cursor in the default web browser
" <gG>: Google search word under cursor in the default web browser
" <gW>: Wiki search word under cursor in the default web browser
Plug 'dhruvasagar/vim-open-url'

"}}}
" User Interface {{{
" --------------------------------------------------------------------------

" Marker named fold nesting
Plug 'dbmrq/vim-chalk'

" Lightweight File Explorer
" Like dirvish, but supports autochdir
Plug 'jeetsukumaran/vim-filebeagle'

" Undo Tree visualization
Plug 'mbbill/undotree', {
      \ 'on': [
      \   'UndotreeToggle', 'UndotreeFocus', 'UndotreeHide', 'UndotreeShow'
      \ ]}

" Light Status bar
Plug 'itchyny/lightline.vim'
" Hybrid lightline theme
Plug 'cocopon/lightline-hybrid.vim'
" Buffers tabline
Plug 'ap/vim-buftabline'

" Delete all but current buffer
Plug 'vim-scripts/BufOnly.vim', { 'on': 'Bonly' }
" Close Buffer
Plug 'moll/vim-bbye', { 'on': 'Bdelete' }

" Session Management
Plug 'tpope/vim-obsession'

" Native colorpicker
Plug 'KabbAmine/vCoolor.vim'

" Nerdtree file browser
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeFind', 'NERDTreeToggle'] }


" }}}
" Motions {{{
" --------------------------------------------------------------------------

" Punctuation text objects
" Advanced change inside two delimiters
" <ciq> -> 'Text to be changed' || \"Text to be changed\"
Plug 'kurkale6ka/vim-pairs'

" Replace instance with Register with gr{motion}
" <griq> -> Replace content in quotes with register
Plug 'vim-scripts/ReplaceWithRegister'

" Change surrounding characters under cursor
" <cs'"> -> Change surrounding ' to "
Plug 'tpope/vim-surround'

" clever-f.vim extends f, F, t and T mappings for more convenience
" instead of <;,> - <f> is available to repeat
Plug 'rhysd/clever-f.vim'

" Split/Join single/multiple line statments
" <gJ> - Join multiple line statments
" <gS> - Split multiple line statments
Plug 'AndrewRadev/splitjoin.vim' " Split oneliners

" Sort alphabetically motion
" <gsip> - Sort alphabetically in paragraph
Plug 'christoomey/vim-sort-motion'

" Switch function argument order
Plug 'AndrewRadev/sideways.vim', {
      \ 'on': ['SidewaysLeft', 'SidewaysRight']
      \ }

" }}}
" Text Editing {{{
" --------------------------------------------------------------------------

" Emmet
Plug 'mattn/emmet-vim'

" Multiple Cursors
Plug 'terryma/vim-multiple-cursors'

" Toggle comments
" <gcc> To togglecomment
" Plug 'tomtom/tcomment_vim'
Plug 'tpope/vim-commentary'

" Visualy Align text blocks by a character
Plug 'godlygeek/tabular', { 'on':  'Tabularize' }

" Make comment block header
Plug 'floscr/MakeHeader.vim'

" Automatically closing pairs
Plug 'cohama/lexima.vim'

" Format text files
Plug 'sbdchd/neoformat'

" }}}
" Text Objects {{{
" --------------------------------------------------------------------------

Plug 'kana/vim-textobj-user'                  " Custom Text Objects
Plug 'kana/vim-textobj-function'              " Function Text Object
" Plug 'kana/vim-textobj-line'                " Line text object
Plug 'kana/vim-textobj-entire'                " Entire Buffer ae
Plug 'thinca/vim-textobj-function-javascript' " Function Text Object for JS
Plug 'michaeljsmith/vim-indent-object'        " Indentation Text Object
Plug 'whatyouhide/vim-textobj-xmlattr'        " XML Attribute Textobject X
" Plug 'jasonlong/vim-textobj-css'            " CSS Class caf
Plug 'b4winckler/vim-angry'                   " Function Argument Text objects

" }}}
" Autocomplete {{{
" --------------------------------------------------------------------------

" Autocomplete
Plug 'Shougo/deoplete.nvim'

" Snippet suport
Plug 'SirVer/ultisnips'

" }}}
" Linting {{{
" --------------------------------------------------------------------------

" Async Linting, Use project .eslintrc first
Plug 'benekastah/neomake', {
      \ 'on': ['Neomake'],
      \ }

" Use project .eslintrc before the ~/.eslintrc
Plug 'jaawerth/neomake-local-eslint-first'

" }}}
" Focus Mode {{{
" --------------------------------------------------------------------------

" Syntax highlighting just on the current line
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
" Focus Mode
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }

" }}}
" Search Utilities {{{
" --------------------------------------------------------------------------

" incrementally highlights ALL pattern matches unlike default 'incsearch'.
Plug 'haya14busa/incsearch.vim'

" Interactive search and replace
Plug 'osyo-manga/vim-over', { 'on': 'OverCommandLine' }

" Fuzzy File Finders
" For MacVim install Ctrlp
" For Terminal install FZF
if has("gui_macvim")
  Plug 'ctrlpvim/ctrlp.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all --no-update-rc' }
  Plug 'junegunn/fzf.vim'
endif

" }}}
" Colorschemes {{{
" --------------------------------------------------------------------------

" Hybrid
Plug 'w0ng/vim-hybrid'
Plug 'NLKNguyen/papercolor-theme'

" }}}
" Git {{{
" --------------------------------------------------------------------------

" Git commands
Plug 'tpope/vim-fugitive'
" Show git diff signs on column bar
Plug 'airblade/vim-gitgutter'
" Create file from the project root
Plug 'dbakker/vim-projectroot'
" }}}

" Syntax {{{1
" --------------------------------------------------------------------------

" Editor config for vim
Plug 'editorconfig/editorconfig-vim'

" PHP {{{2
" --------------------------------------------------------------------------

" Better PHP Syntax
Plug 'StanAngeloff/php.vim', { 'for': 'php' }
" Better indentation support for PHP files with HTML
Plug 'captbaritone/better-indent-support-for-php-with-html', { 'for': ['php'] }

" Twig support for vim
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja'] }

" Blade Syntax
Plug 'jwalton512/vim-blade', { 'for': ['php', 'blade'] }

" DocBlocks for PHP
Plug 'tobyS/vmustache' | Plug 'tobyS/pdv', { 'for': ['php'] }

" Testing commands
Plug 'janko-m/vim-test', { 'for': ['php', 'blade'] }

Plug 'noahfrederick/vim-laravel', { 'for': ['php', 'blade'] }

" Automatic namespace creation on <leader>u
Plug 'arnaud-lb/vim-php-namespace', { 'for': ['php'] }

" }}}2
" Javascript {{{2
" --------------------------------------------------------------------------

" Open files with 'gf' without extensions
Plug 'moll/vim-node', { 'for': ['js', 'vue'] }

 " Modern JS support (indent, syntax, etc)
Plug 'pangloss/vim-javascript'
" More javascript syntax highlighting
Plug 'jelera/vim-javascript-syntax'

" Expand JavaScript Parameter Omni Complete
Plug 'othree/jspc.vim'

" Vue syntax highlighting
Plug 'posva/vim-vue', { 'for': ['vue'] }

" generates JSDoc block comments based on a function signature
Plug 'heavenshell/vim-jsdoc'

" Beautify JS Files
" Update submodules after plugin install
Plug 'maksimr/vim-jsbeautify', {
      \ 'do': 'git submodule update --init --recursive',
      \ }

" Advanced javascript motions
" Use [[ to navigate between methods
Plug 'okcompute/vim-javascript-motions', { 'for': ['javascript'] }

" }}}2
" HTML / CSS {{{2
" --------------------------------------------------------------------------

" HTML5 syntax
Plug 'othree/html5.vim'

" CSS3 Syntax
Plug 'hail2u/vim-css3-syntax', { 'for': ['css', 'scss', 'sass', 'html', 'vue'] }
" SCSS Syntax
Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss'] }


" Add Colorized background to CSS color values
Plug 'lilydjwg/colorizer', { 'for': ['css', 'sass', 'scss', 'less', 'html', 'xdefaults', 'javascript', 'javascript.jsx'] }

" Show matching html tag
Plug 'gregsexton/MatchTag', {
      \ 'for': ['html', 'javascript', 'blade', 'php', 'twig'],
      \ }

" Find CDN asset links with FZF
Plug 'floscr/FZF-cdnj'

" Stylus support
Plug 'wavded/vim-stylus', { 'for': ['stylus'] }

" }}}2
" Markdown {{{2
" --------------------------------------------------------------------------

" Extended Markdown
Plug 'plasticboy/vim-markdown', { 'for': ['markdown'] }

" }}}2
" }}}1

" Plug end {{{1
" --------------------------------------------------------------------------
call plug#end()
" }}}
