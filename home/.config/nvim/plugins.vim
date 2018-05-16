" Plug setup {{{1
" --------------------------------------------------------------------------

if empty(glob('~/.config/nvim/autoload/plug.vim'))
  silent !curl -fLo ~/.config/nvim/autoload/plug.vim --create-dirs
        \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.config/nvim/plugged')

" Defaults {{{1
" --------------------------------------------------------------------------

if !has('nvim')
  Plug 'noahfrederick/vim-neovim-defaults'
endif

" Dependencies {{{1
" --------------------------------------------------------------------------

" Ability to load json files for vim
" Used in:
"   + Emmet - To load custom snippets.json
Plug 'mattn/webapi-vim'

" VIM Extending Plugins {{{1
" --------------------------------------------------------------------------

" Vim/TMUX seamless split navigation
Plug 'christoomey/vim-tmux-navigator'

" Interact with tmux
Plug 'benmills/vimux'

" Lots of word manipulations plugins
" Case conversion: {{{2
" crs        -  snake_case
" crm        -  MixedCase
" crc        -  camelCase
" cru        -  UPPER_CASE
" cr-        -  dash-case
" cr.        -  dot.case
" cr<space>  -  space case
" crT        -  Title Case
" }}}2
Plug 'tpope/vim-abolish'

" Asynchronous Command Execution
Plug 'skywind3000/asyncrun.vim'

" Pairs of handy bracket mappings {{{2
" [ / ] + <Space> - Adds Newlines
" [ / ] + x       - Encoding HTML Characters
" [ / ] + u       - Encoding HTML Urls
" [ / ] + f       - Next/Previous File in Dir
" [ / ] + b       - Buffer switching
" [ / ] + os      - Toggle Spelling
" [ / ] + ox      - Toggle cursorLine
" }}}2
Plug 'tpope/vim-unimpaired'

" More . repeat functionality
Plug 'tpope/vim-repeat'

" Complete from Buffer in the commandline {{{2
" <c-x> Complete from buffer forward
" <c-y> Complete from buffer backward
" 2}}}
Plug 'floscr/CmdlineComplete'

" Project Management {{{1
" --------------------------------------------------------------------------

" Load local .lvimrc
Plug 'embear/vim-localvimrc'

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
Plug 'KabbAmine/vCoolor.vim', { 'on': 'VCoolor' }
let g:vcoolor_disable_mappings = 1

" Nerdtree file browser
Plug 'scrooloose/nerdtree', { 'on': ['NERDTreeFind', 'NERDTreeToggle'] }

" Search and Replace
Plug 'wincent/ferret'

" Zoom Split <C-W>m
Plug 'dhruvasagar/vim-zoom'

" Motions {{{
" --------------------------------------------------------------------------

" Punctuation text objects
" Advanced change inside two delimiters
" <ciq> -> 'Text to be changed' || \"Text to be changed\"
Plug 'kurkale6ka/vim-pairs'

" Move by indent
Plug 'jeetsukumaran/vim-indentwise'

" Replace instance with Register with gr{motion}
" <griq> -> Replace content in quotes with register
Plug 'vim-scripts/ReplaceWithRegister'

" Change surrounding characters under cursor
" <cs'"> -> Change surrounding ' to "
" <ysst> -> Surround Line with tag
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
nnoremap ,> :silent SidewaysRight<CR>
nnoremap ,< :silent SidewaysLeft<CR>

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
" Plug 'floscr/MakeHeader.vim'

" Automatically closing pairs
Plug 'cohama/lexima.vim'

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

" Autocomplete {{{
" --------------------------------------------------------------------------

" Autocomplete
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
let g:deoplete#enable_at_startup = 1

" Snippet suport
Plug 'SirVer/ultisnips'

" Linting {{{
" --------------------------------------------------------------------------

Plug 'w0rp/ale'

" Focus Mode {{{
" --------------------------------------------------------------------------

" Syntax highlighting just on the current line
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' }
" Focus Mode
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }

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

" Colorschemes {{{
" --------------------------------------------------------------------------

" Hybrid
Plug 'floscr/vim-hybrid'
Plug 'rakr/vim-one'
Plug 'NLKNguyen/papercolor-theme'

" Git {{{
" --------------------------------------------------------------------------

" Git commands
Plug 'tpope/vim-fugitive'

" Make :Gbrowse work for github repos
Plug 'tpope/vim-rhubarb'

" Show git diff signs on column bar
Plug 'airblade/vim-gitgutter'

" Create file from the project root
Plug 'dbakker/vim-projectroot'

" Show diff preview when doing an interactive rebase
Plug 'hotwatermorning/auto-git-diff', { 'for': [ 'gitrebase' ] }

" Interactive Git
Plug 'jreybert/vimagit', { 'on': ['Magit'] }

" Notes {{{
" --------------------------------------------------------------------------

Plug 'vimwiki/vimwiki'
" Use .markdown extension so vimwiki wont mess with other markdown files

let blogwiki = {}
let blogwiki.path = '~/Dropbox/Blog'
let blogwiki.ext = '.markdown'

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
      \ 'bash': 'sh',
      \ 'ex': 'elixir',
      \ 'hs': 'haskell',
      \ 'javascript': 'javascript',
      \ 'js': 'javascript',
      \ 'py': 'python',
      \ 'rb': 'ruby',
      \ 'sh': 'sh',
      \ 'vim': 'vim',
      \ }

" Assign dictionary to settings
let g:vimwiki_list = [wiki, blogwiki]

" Enable folding
let g:vimwiki_folding='expr'

" Debugging {{{
" --------------------------------------------------------------------------

" Debug startuptime with :StartupTime
Plug 'tweekmonster/startuptime.vim', { 'on': ['StartupTime'] }

" Documentation {{{
" --------------------------------------------------------------------------

" Show man page inside vim help
Plug 'vim-utils/vim-man', { 'on': ['Man', 'Vman'] }

" For the LOLs {{{
" --------------------------------------------------------------------------

" Vim Breakout Game with your code lines
" Plug 'johngrib/vim-game-code-break', { 'on': ['VimGameCodeBreak'] }

" Work {{{
" --------------------------------------------------------------------------

Plug 'git@bitbucket.org:artish/vim-meistersnippet.git'
Plug 'git@bitbucket.org:artish/meisterlabs.vim.git'

" Syntax {{{1
" --------------------------------------------------------------------------

" Editor config for vim
Plug 'editorconfig/editorconfig-vim'

" Reason {{{2
" --------------------------------------------------------------------------

Plug 'reasonml-editor/vim-reason-plus'
Plug 'autozimu/LanguageClient-neovim', {
    \ 'branch': 'next',
    \ 'do': 'bash install.sh',
    \ }
let g:LanguageClient_serverCommands = {
    \ 'reason': ['ocaml-language-server', '--stdio'],
    \ 'ocaml': ['ocaml-language-server', '--stdio'],
    \ }

" Swift {{{2
" --------------------------------------------------------------------------

Plug 'keith/swift.vim', { 'for': [ 'swift' ] }

" GO {{{2
" --------------------------------------------------------------------------

Plug 'cespare/vim-toml', { 'for': [ 'toml' ] }

" Haskell {{{2
" --------------------------------------------------------------------------

Plug 'neovimhaskell/haskell-vim', { 'for': [ 'haskell' ] }
" Completion Plugin
Plug 'eagletmt/neco-ghc', { 'for': [ 'haskell' ] }
" Plug 'jaspervdj/stylish-haskell', { 'for': [ 'haskell' ] }
" Plug 'commercialhaskell/hindent', { 'for': [ 'haskell' ] }

" Purescript {{{2
" --------------------------------------------------------------------------

Plug 'purescript-contrib/purescript-vim', { 'for': [ 'purescript' ] }

" Elixir {{{2
" --------------------------------------------------------------------------

Plug 'elixir-editors/vim-elixir'

" Javascript {{{2
" --------------------------------------------------------------------------

" Open files with 'gf' without extensions
Plug 'moll/vim-node'

 " Modern JS support (indent, syntax, etc)
Plug 'pangloss/vim-javascript'

" Expand JavaScript Parameter Omni Complete
Plug 'othree/jspc.vim', { 'for': ['javascript'] }

" Vue syntax highlighting
Plug 'posva/vim-vue', { 'for': ['vue'] }

" Advanced javascript motions
" Use [[ to navigate between methods
Plug 'okcompute/vim-javascript-motions', { 'for': ['javascript', 'vue.html.javascript.css'] }

" Split/Join Arrow Functions
Plug 'mvolkmann/vim-js-arrow-function', { 'for': ['javascript', 'javascript.jsx'] }

" Required
" npm i -g import-js
Plug 'galooshi/vim-import-js', {
      \ 'for': ['javascript', 'javascript.jsx']
      \ }

" Flow typing
" Plug 'flowtype/vim-flow'

" {{{3 Prettier
" Plug 'mitermayer/vim-prettier', {
"       \ 'do': 'yarn install',
"       \ 'for': ['javascript', 'javascript.jsx', 'typescript', 'css', 'less', 'scss'] }
"
" let g:prettier#config#trailing_comma = 'none'
" let g:prettier#config#print_width = 100
" let g:prettier#config#tab_width = 4
" let g:prettier#config#use_tabs = 'false'
" let g:prettier#config#bracket_spacing = 'true'
" }}}3
" HTML / CSS {{{2
" --------------------------------------------------------------------------

" HTML5 syntax
Plug 'othree/html5.vim'

" CSS3 Syntax
Plug 'hail2u/vim-css3-syntax', { 'for': ['css', 'scss', 'sass', 'html'] }
" SCSS Syntax
Plug 'cakebaker/scss-syntax.vim', { 'for': ['sass', 'scss', 'vue.html.javascript.css'] }


" Add Colorized background to CSS color values
" Plug 'lilydjwg/colorizer', { 'for': ['css', 'sass', 'scss', 'less', 'html', 'xdefaults', 'javascript', 'javascript.jsx', 'vim', 'vue.html.javascript.css'] }

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

" Markdown {{{2
" --------------------------------------------------------------------------

" Extended Markdown
Plug 'plasticboy/vim-markdown'

" Editor config for vim
Plug 'https://bitbucket.org/artish/orgwiki'

" Markdown folding
Plug 'nelstrom/vim-markdown-folding'

" Checklist toggle for markdown
" with <leader>tt
" Plug 'floscr/vim-checkbox', { 'for': ['markdown'] }

" Does not work because vim-markdonw overrides syntax
" Plug 'gonzaloserrano/vim-markdown-todo', { 'for': ['markdown'] }

" Plug end {{{1
" --------------------------------------------------------------------------

call plug#end()
