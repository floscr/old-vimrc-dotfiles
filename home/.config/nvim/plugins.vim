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

" }}}
" VIM Extending Plugins {{{1

" Load local .lvimrc
Plug 'embear/vim-localvimrc'

" Vim/TMUX seamless split navigation
Plug 'christoomey/vim-tmux-navigator'

" Interact with tmux
Plug 'benmills/vimux'

" More . repeat functionality
Plug 'tpope/vim-repeat'

" Lots of word manipulations plugins
Plug 'tpope/vim-abolish'

" Ability to load json files for vim
" Used in:
"   + Emmet - To load custom snippets.json
Plug 'mattn/webapi-vim'

" Asynchronous Command Execution
Plug 'skywind3000/asyncrun.vim'

Plug 'tpope/vim-unimpaired'

" }}}1
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

Plug 'majutsushi/tagbar', { 'on': 'TagbarToggle' }

" Light Status bar
Plug 'itchyny/lightline.vim'
" Hybrid lightline theme
Plug 'floscr/lightline-hybrid.vim'
" Buffers tabline
Plug 'ap/vim-buftabline'

" Delete all but current buffer
Plug 'vim-scripts/BufOnly.vim', { 'on': 'Bonly' }
" Close Buffer
Plug 'moll/vim-bbye', { 'on': 'Bdelete' }

" Session Management
" Plug 'tpope/vim-obsession'

" Native colorpicker
Plug 'KabbAmine/vCoolor.vim'
let g:vcoolor_disable_mappings = 1

" Nerdtree file browser
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeFind', 'NERDTreeToggle'] }

" Search and Replace
Plug 'wincent/ferret'

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
" tpope/vim-commentary wont recognise comment type in files with multiple
" filetypes...
Plug 'tomtom/tcomment_vim'
" Plug 'tpope/vim-commentary'

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
Plug 'trungdq88/vim-textobj-xmlattr'          " XML Attribute Textobject X
                                              " -> fork that supports jsx
" Plug 'jasonlong/vim-textobj-css'            " CSS Class caf
Plug 'b4winckler/vim-angry'                   " Function Argument Text objects

" }}}
" Autocomplete {{{
" --------------------------------------------------------------------------

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

" Plug 'prabirshrestha/asyncomplete.vim'
" Plug 'prabirshrestha/asyncomplete-buffer.vim'
" Plug 'prabirshrestha/asyncomplete-tscompletejob.vim'

" Plug 'floscr/tscompletejob' | Plug 'prabirshrestha/asyncomplete.vim'
" let g:tscompletejob_node_cmd = expand('~/.nvm/versions/node/v6.10.3/bin/node')
" let g:tscompletejob_autoload_filetypes = ['ts', 'tsx', 'javascript', 'js']

" Snippet suport
Plug 'SirVer/ultisnips'

" }}}
" Linting {{{
" --------------------------------------------------------------------------

Plug 'w0rp/ale'

" }}}
" Refactoring {{{
" --------------------------------------------------------------------------

Plug 'floscr/vim-xtract', { 'branch': 'feature/vue-extract' }

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
  " Skip FZF installation via plugin
  " FZF should be managed via brew
  " \ 'do': './install --all --no-update-rc'
  Plug 'junegunn/fzf', {
        \ 'dir': '~/.fzf',
        \ }
  Plug 'junegunn/fzf.vim'
endif

" }}}
" Colorschemes {{{
" --------------------------------------------------------------------------

" Hybrid
Plug 'floscr/vim-hybrid'
Plug 'rakr/vim-one'
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

" Show diff preview when doing an interactive rebase
Plug 'hotwatermorning/auto-git-diff', { 'for': [ 'gitrebase' ] }

" }}}
" Notes {{{
" --------------------------------------------------------------------------

Plug 'vimwiki/vimwiki'
" Use .markdown extension so vimwiki wont mess with other markdown files

let wiki = {}
let wiki.path = '~/Dropbox/VimWiki'

" Syntax
let wiki.syntax = 'markdown'
" Workaround - vimwiki messes up the markdown syntax...
" So i chose the least commonly used extension for my vimwiki files
let wiki.ext = '.markdown'

" HTML exporting
let wiki.template_path = '~/Dropbox/VimWiki/template/'
let wiki.template_default = 'default'
let wiki.template_ext = '.tpl'
let wiki.custom_wiki2html = 'vimwiki_markdown'

" Add all syntax filetypes you want highlighted in your vimwiki files here
let wiki.nested_syntaxes = {
      \ 'sh': 'sh',
      \ 'bash': 'sh',
      \ 'vim': 'vim',
      \ 'javascript': 'javascript',
      \ 'js': 'javascript',
      \ }

" Assign dictionary to settings
let g:vimwiki_list = [wiki]

" Enable folding
let g:vimwiki_folding='expr'

" }}}
" Debugging {{{
" --------------------------------------------------------------------------
" Debug startuptime with :StartupTime
Plug 'tweekmonster/startuptime.vim', { 'on': ['StartupTime'] }
" }}}

" Syntax {{{1
" --------------------------------------------------------------------------

" Editor config for vim
Plug 'editorconfig/editorconfig-vim'


" GO {{{2
" --------------------------------------------------------------------------

Plug 'cespare/vim-toml', { 'for': [ 'toml' ] }

" }}}2
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
Plug 'moll/vim-node'

 " Modern JS support (indent, syntax, etc)
Plug 'pangloss/vim-javascript'

" Expand JavaScript Parameter Omni Complete
Plug 'othree/jspc.vim', { 'for': ['javascript'] }

" Flow typing
" Plug 'flowtype/vim-flow'

" Vue syntax highlighting
Plug 'posva/vim-vue', { 'for': ['vue'] }

" generates JSDoc block comments based on a function signature
Plug 'heavenshell/vim-jsdoc', { 'for': ['javascript', 'vue.html.javascript.css'] }

" Beautify JS Files
" Update submodules after plugin install
Plug 'maksimr/vim-jsbeautify', {
      \ 'do': 'git submodule update --init --recursive',
      \ }

" Advanced javascript motions
" Use [[ to navigate between methods
Plug 'okcompute/vim-javascript-motions', { 'for': ['javascript', 'vue.html.javascript.css'] }

Plug 'mvolkmann/vim-js-arrow-function', { 'for': ['javascript', 'javascript.jsx'] }

" Plug 'mitermayer/vim-prettier', {
"       \ 'do': 'yarn install',
"       \ 'for': ['javascript', 'javascript.jsx', 'typescript', 'css', 'less', 'scss'] }
"
" let g:prettier#config#trailing_comma = 'none'
" let g:prettier#config#print_width = 100
" let g:prettier#config#tab_width = 4
" let g:prettier#config#use_tabs = 'false'
" let g:prettier#config#bracket_spacing = 'true'

" }}}2
" HTML / CSS {{{2
" --------------------------------------------------------------------------

" HTML5 syntax
Plug 'othree/html5.vim'

" CSS3 Syntax
Plug 'hail2u/vim-css3-syntax', { 'for': ['css', 'scss', 'sass', 'html'] }
" SCSS Syntax
Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss', 'vue.html.javascript.css'] }


" Add Colorized background to CSS color values
Plug 'lilydjwg/colorizer', { 'for': ['css', 'sass', 'scss', 'less', 'html', 'xdefaults', 'javascript', 'javascript.jsx', 'vim', 'vue.html.javascript.css'] }

" React
Plug 'mxw/vim-jsx', { 'for': ['jsx', 'javascript.jsx', 'javascript'] }
let g:jsx_ext_required = 0
" Plug 'fleischie/vim-styled-components', { 'for': ['javascript.jsx'] }

" Show matching html tag
Plug 'gregsexton/MatchTag', {
      \ 'for': ['html', 'javascript', 'blade', 'php', 'twig'],
      \ }

" Find CDN asset links with FZF
Plug 'floscr/FZF-cdnj'

" Stylus support
Plug 'wavded/vim-stylus', { 'for': ['stylus'] }
Plug 'digitaltoad/vim-pug', { 'for': ['jade', 'pug'] }

" }}}2
" Markdown {{{2
" --------------------------------------------------------------------------

" Extended Markdown
Plug 'plasticboy/vim-markdown'

" Markdown folding
Plug 'nelstrom/vim-markdown-folding'

" Checklist toggle for markdown
" with <leader>tt
" Plug 'floscr/vim-checkbox', { 'for': ['markdown'] }

" Does not work because vim-markdonw overrides syntax
" Plug 'gonzaloserrano/vim-markdown-todo', { 'for': ['markdown'] }

" }}}2
" }}}1

" Plug end {{{1
" --------------------------------------------------------------------------
call plug#end()
" }}}
