call plug#begin('~/.vim/plugged')

" -----------------------------------------------------------------------------
" Language agnostic plugins
" -----------------------------------------------------------------------------

Plug 'sheerun/vim-polyglot'          " Most common languages file types (syntax, indenting, etc.)
Plug 'ervandew/supertab'             " Tab Expanding
" Plug 'tpope/vim-commentary'          " Commenting support (gc)
Plug 'tomtom/tcomment_vim'
Plug 'duggiefresh/vim-easydir'       " Create directories on save
Plug 'vim-scripts/BufOnly.vim'       " Delete all buffers except the current one
Plug 'tpope/vim-repeat'              " Repeat certain cmds like surround
Plug 'jiangmiao/auto-pairs'
" Plug 'cohama/lexima.vim'             " Automatically closing tags
Plug 'kurkale6ka/vim-pairs'          " CIQ to match any pairs
Plug 'editorconfig/editorconfig-vim' " Editor config for vim
Plug 'tpope/vim-unimpaired'          " ]q for :cnext [q for :cprevious
Plug 'rhysd/clever-f.vim'            " Repeat last F keyword with f

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

" JSON
Plug 'elzr/vim-json'

" PHP
Plug 'evidens/vim-twig'
Plug 'stephpy/vim-php-cs-fixer'
Plug 'captbaritone/better-indent-support-for-php-with-html'
Plug 'tokutake/twig-indent' " Fix indentation for twig

" MARKDOWN
Plug 'sampsyo/autolink.vim' " Lucky link expansion for markdown
                            " <leader>am -> Link at the bottom
                            " <leader>ac -> Fetch link URL

" CSS
Plug 'mattn/emmet-vim'              " Emmet CSS Expansions

" ------------------------------------------------------------------------------
" Interface
" ------------------------------------------------------------------------------

Plug 'tpope/vim-vinegar'

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

" -----------------------------------------------------------------------------
" Custom Plugins
" -----------------------------------------------------------------------------

" Trim whitespace on file write
source ~/.dotfiles/vim/plugins/autowrite.vim

" Format file
source ~/.dotfiles/vim/plugins/formatfiletype.vim

" Rename current buffer
source ~/.dotfiles/vim/plugins/Rename.vim

" Make comment header depending on filetype
source ~/.dotfiles/vim/plugins/makeheader.vim
