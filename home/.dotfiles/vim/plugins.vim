call plug#begin('~/.vim/plugged')

" -----------------------------------------------------------------------------
" Language agnostic plugins
" -----------------------------------------------------------------------------

Plug 'sheerun/vim-polyglot'          " Most common languages file types (syntax, indenting, etc.)
Plug 'ervandew/supertab'             " Tab Expanding
Plug 'tomtom/tcomment_vim'           " Commenting
Plug 'duggiefresh/vim-easydir'       " Create directories on save
Plug 'vim-scripts/BufOnly.vim'       " Delete all buffers except the current one
Plug 'tpope/vim-repeat'              " Repeat certain cmds like surround
Plug 'jiangmiao/auto-pairs'          " Auto closing pairs
Plug 'kurkale6ka/vim-pairs'          " CIQ to match any pairs
Plug 'editorconfig/editorconfig-vim' " Editor config for vim
Plug 'tpope/vim-unimpaired'          " ]q for :cnext [q for :cprevious
Plug 'rhysd/clever-f.vim'            " Repeat last F keyword with f
Plug 'tpope/vim-dispatch'            " Async Execution
Plug 'AndrewRadev/splitjoin.vim'     " Split and join lines with gS/gJ
Plug 'christoomey/vim-sort-motion'   " Sort with motion sip
Plug 'AndrewRadev/sideways.vim'      " Switch arg order
Plug 'Toggle'                        " Toggle booleans with +
Plug 'dyng/ctrlsf.vim'

Plug 'scrooloose/nerdtree'
" Plug 'justinmk/vim-dirvish'
" Plug 'maxbrunsfeld/vim-yankstack' " Yanked text history

Plug 'vim-scripts/vim-auto-save', { 'on': 'AutoSaveToggle' }
Plug 'haya14busa/incsearch.vim'

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
Plug 'moll/vim-node'                      " Open require statements with <gf>
Plug 'pangloss/vim-javascript'
Plug 'jelera/vim-javascript-syntax'       " JS Syntax Highlighting
Plug 'posva/vim-vue', { 'for': ['vue'] }

" JSON
Plug 'elzr/vim-json', { 'for': [ 'json' ] }

" PHP
Plug 'stephpy/vim-php-cs-fixer', { 'for': ['php'] }
Plug 'captbaritone/better-indent-support-for-php-with-html', { 'for': ['php'] }
Plug 'mitsuhiko/vim-jinja', { 'for': ['htmljinja'] }

" MARKDOWN
" Lucky link expansion for markdown
" <leader>am -> Link at the bottom
" <leader>ac -> Fetch link URL
" Plug 'sampsyo/autolink.vim', { 'for': [ 'markdown' ] }
Plug 'avdgaag/vim-lucky-markdown', { 'for': [ 'markdown' ] }

" CSS
Plug 'ap/vim-css-color'
Plug 'KabbAmine/vCoolor.vim'
Plug 'mattn/emmet-vim'

" ------------------------------------------------------------------------------
" Interface
" ------------------------------------------------------------------------------

Plug 'easymotion/vim-easymotion'                     " Letter Navigation
Plug 'junegunn/limelight.vim', { 'on': 'Limelight' } " Syntax highlighting only on current paragraph
Plug 'junegunn/goyo.vim', { 'on': 'Goyo' }           " Focus Mode

Plug 'bling/vim-bufferline'                          " Buffer bar in the lightline
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

Plug 'kana/vim-textobj-user'                  " Custom Text Objects
Plug 'kana/vim-textobj-function'              " Function Text Object
Plug 'kana/vim-textobj-line'                  " Line text object
Plug 'thinca/vim-textobj-function-javascript' " Function Text Object for JS
Plug 'michaeljsmith/vim-indent-object'        " Indentation Text Object
Plug 'vim-scripts/ReplaceWithRegister'        " Replace motion with gr{motion}
Plug 'whatyouhide/vim-textobj-xmlattr'        " XML Attribute Textobject X

" -----------------------------------------------------------------------------
" Color Scheme
" -----------------------------------------------------------------------------

Plug 'floscr/oceanic-next'
Plug 'w0ng/vim-hybrid'
Plug 'reedes/vim-colors-pencil'

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
