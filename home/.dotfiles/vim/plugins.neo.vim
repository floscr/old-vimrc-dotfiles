call plug#begin('~/.vim/plugged')

" -----------------------------------------------------------------------------
" Language agnostic plugins
" -----------------------------------------------------------------------------

Plug 'sheerun/vim-polyglot'       " Most common languages file types (syntax, indenting, etc.)
Plug 'cohama/lexima.vim'          " Automatically closing tags
Plug 'ervandew/supertab'          " Tab Expanding
Plug 'tpope/vim-commentary'       " Commenting support (gc)
Plug 'duggiefresh/vim-easydir'    " Create directories on save
Plug 'vim-scripts/BufOnly.vim'    " Delete all buffers except the current one
Plug 'vim-signature'              " Save marks in the gutter
Plug 'tpope/vim-repeat'           " Repeat certain cmds lik surround
Plug 'junegunn/limelight.vim'     " Syntax highlighting only on current paragraph
                                  " Has a nice sideeffect of speeding up slow
                                  " syntax plugins
Plug 'junegunn/vim-peekaboo'      " Shwo register sidebar when pasting
Plug 'junegunn/goyo.vim'          " Focus Mode
Plug 'kurkale6ka/vim-pairs'       " CIQ to match any pairs

" -----------------------------------------------------------------------------
" Snippets
" -----------------------------------------------------------------------------

Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

" -----------------------------------------------------------------------------
" Syntax Specific
" -----------------------------------------------------------------------------

" Linting
Plug 'scrooloose/syntastic'

" JAVASCRIPT
Plug 'moll/vim-node'                " Open require statements with <gf>
Plug 'pangloss/vim-javascript'
Plug 'jelera/vim-javascript-syntax' " JS Syntax Highlighting
Plug 'heavenshell/vim-jsdoc'        " DocBlockr
Plug 'posva/vim-vue'                " Vue.JS Syntax Hightlighting

" CSS
Plug 'mattn/emmet-vim'              " Emmet CSS Expansions

" ------------------------------------------------------------------------------
" Interface
" ------------------------------------------------------------------------------

Plug 'scrooloose/nerdtree', { 'on':  'NERDTreeToggle' } " Sidebar file-browser
Plug 'easymotion/vim-easymotion'                        " Letter Navigation
Plug 'bling/vim-bufferline'                             " Buffer bar in the lightline

" Plug 'itchyny/lightline.vim'                            " Lightline (simple status line)
Plug 'vim-airline/vim-airline'

" Plug 'ctrlpvim/ctrlp.vim'                               " Fuzzy File Search
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" ------------------------------------------------------------------------------
" External tools integration plugins
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

" -----------------------------------------------------------------------------
" Color Scheme
" -----------------------------------------------------------------------------

Plug 'mhartington/oceanic-next'
Plug 'chriskempson/base16-vim'

" -----------------------------------------------------------------------------
" ETC
" -----------------------------------------------------------------------------

Plug 'tpope/vim-repeat'          " Repeat certain cmds like surround

call plug#end()

source ~/.dotfiles/vim/plugins/autowrite.vim
