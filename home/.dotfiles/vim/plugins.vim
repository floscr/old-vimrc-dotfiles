call plug#begin('~/.vim/plugged')

" -----------------------------------------------------------------------------
" Language agnostic plugins
" -----------------------------------------------------------------------------

Plug 'sheerun/vim-polyglot'          " Most common languages file types (syntax, indenting, etc.)
Plug 'ervandew/supertab'             " Tab Expanding
Plug 'tpope/vim-commentary'          " Commenting support (gc)
Plug 'duggiefresh/vim-easydir'       " Create directories on save
Plug 'vim-scripts/BufOnly.vim'       " Delete all buffers except the current one
Plug 'tpope/vim-repeat'              " Repeat certain cmds like surround
Plug 'cohama/lexima.vim'             " Automatically closing tags
Plug 'kurkale6ka/vim-pairs'          " CIQ to match any pairs
Plug 'editorconfig/editorconfig-vim' " Editor config for vim
Plug 'tpope/vim-unimpaired'
" Plug 'mhinz/vim-startify'            " Pretty start screen
" Plug 'xolox/vim-misc'                " Needed for vim-startify
" Plug 'xolox/vim-session'             " Sessions Restore, have to define later

" -----------------------------------------------------------------------------
" Snippets
" -----------------------------------------------------------------------------

Plug 'SirVer/ultisnips'

" -----------------------------------------------------------------------------
" Syntax Specific
" -----------------------------------------------------------------------------

" LINTING
Plug 'scrooloose/syntastic'

" JAVASCRIPT
Plug 'moll/vim-node'                " Open require statements with <gf>
Plug 'pangloss/vim-javascript'
Plug 'jelera/vim-javascript-syntax' " JS Syntax Highlighting
Plug 'posva/vim-vue'                " Vue.JS Syntax Hightlighting
" Plug 'heavenshell/vim-jsdoc'        " DocBlockr

" PHP
Plug 'evidens/vim-twig'
Plug 'tokutake/twig-indent' " Fix indentation for twig

" MARKDOWN
Plug 'sampsyo/autolink.vim' " Lucky link expansion for markdown

" CSS
Plug 'mattn/emmet-vim'              " Emmet CSS Expansions

" ------------------------------------------------------------------------------
" Interface
" ------------------------------------------------------------------------------

Plug 'tpope/vim-vinegar'
" Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' }

Plug 'vim-signature'             " Save marks in the gutter
Plug 'easymotion/vim-easymotion' " Letter Navigation
Plug 'junegunn/vim-peekaboo'     " Shwo register sidebar when pasting
Plug 'junegunn/limelight.vim'    " Syntax highlighting only on current paragraph
Plug 'junegunn/goyo.vim'         " Focus Mode

Plug 'bling/vim-bufferline'      " Buffer bar in the lightline
Plug 'vim-airline/vim-airline'

if has("gui_macvim")
  Plug 'ctrlpvim/ctrlp.vim'
else
  Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
  Plug 'junegunn/fzf.vim'
endif

" ------------------------------------------------------------------------------
" Git
" ------------------------------------------------------------------------------

Plug 'tpope/vim-fugitive'           " Git Implementation
Plug 'airblade/vim-gitgutter'       " Sublime GitGutter Adaption

" ------------------------------------------------------------------------------
" Text insertion/manipulation
" ------------------------------------------------------------------------------

Plug 'terryma/vim-multiple-cursors'               " Multiple cursors
Plug 'tpope/vim-surround'                         " Change surrounding characters
Plug 'godlygeek/tabular', { 'on':  'Tabularize' } " Easy alignment
                                                  " VSelect lines :Tabularize /= <- Regex
Plug 'wellle/targets.vim'                         " Additional text targets

" Custom textobjects
Plug 'kana/vim-textobj-user'
Plug 'kana/vim-textobj-function'              " Function Text Object
Plug 'thinca/vim-textobj-function-javascript' " Function Text Object for JS

" -----------------------------------------------------------------------------
" Color Scheme
" -----------------------------------------------------------------------------

Plug 'floscr/oceanic-next'

call plug#end()

source ~/.dotfiles/vim/plugins/autowrite.vim
