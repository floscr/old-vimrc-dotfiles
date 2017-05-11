" /\\\________/\\\_____________________________________________________________
" \/\\\_______\/\\\____________________________________________________________
" _\//\\\______/\\\___/\\\_____________________________________________________
" ___\//\\\____/\\\___\///_____/\\\\\__/\\\\\____/\\/\\\\\\\______/\\\\\\\\____
" _____\//\\\__/\\\_____/\\\__/\\\///\\\\\///\\\_\/\\\/////\\\___/\\\//////____
" _______\//\\\/\\\_____\/\\\_\/\\\_\//\\\__\/\\\_\/\\\___\///___/\\\__________
" _________\//\\\\\______\/\\\_\/\\\__\/\\\__\/\\\_\/\\\_________\//\\\________
" ___________\//\\\_______\/\\\_\/\\\__\/\\\__\/\\\_\/\\\__________\///\\\\\\\\
" _____________\///________\///__\///___\///___\///__\///_____________\////////

source ~/.config/nvim/plugins.vim

" =============================================================================
" Default Settings {{{1
" Neovim defaults: https://neovim.io/doc/user/vim_diff.html#nvim-option-defaults
" =============================================================================

" Set leader to Space
let g:mapleader="\<space>"

set autochdir        " Set working dir to path of the current file
set hidden           " Enables to switch between unsaved buffers and keep undo history
set lazyredraw       " Don't redraw while executing macros (better performance)
set nojoinspaces     " No extra space when joining a line which ends with . ? !
set noshowmatch      " Show matching tags
set nostartofline    " Prevent cursor from moving to beginning of line when switching buffers
set noswapfile       " Dont create swapfiles
set number           " Show Line numbers
set relativenumber   " Show Relative Numbers
set shell=$SHELL     " Setting shell to zsh
set shortmess+=I     " Turn off the intro message
set showmode         " Always show mode
set splitbelow       " Split new window below current window
set splitright       " Split new window right of current window
set suffixesadd+=.js " Automatically add suffic when pressing gf to go to a file
set synmaxcol=1500   " Turn off syntax highlighting after X lines
set updatetime=750   " make vim update more rapidly

" Disable Netrw
" Netrw is the default filebrowser plugin for vim which I replace with FileBeagle
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

" Clipboard {{{2
" -----------------------------------------------------------------------------
set clipboard=unnamed
" }}}2
" Sessions {{{2
" -----------------------------------------------------------------------------

" Autosaving Buffer Options like folds
set sessionoptions-=options " Disable options for session saving
set viewoptions-=options    " http://stackoverflow.com/questions/26917336/vim-specific-mkview-and-loadview-in-order-to-avoid-issues
autocmd BufWinLeave *.* mkview
autocmd BufWinEnter *.* silent! loadview
" }}}2
" Color Settings {{{2
" -----------------------------------------------------------------------------

" Enable Multicolor support
if has('termguicolors')
  set termguicolors
endif

" Dark hybrid theme low contrast mode
" https://github.com/w0ng/vim-hybrid
set background=dark
let g:hybrid_reduced_contrast = 1
colorscheme hybrid

" Fix the vertical split bar background color
hi VertSplit guibg=#232C31

"}}}2
" Wrap Settings {{{2
" -----------------------------------------------------------------------------

set colorcolumn=80           " Add a colorized column tho show the maximal text length
set textwidth=80             " Set the recommended text length to 80 characters
set nowrap                   " Don't wrap lines
set textwidth=0 wrapmargin=0 " this turns off physical line wrapping (ie: automatic insertion of newlines)"}}}
"}}}2
" Search settings {{{2
" -----------------------------------------------------------------------------

set incsearch  " Incremental search
set ignorecase " Ignore case by default
set smartcase  " Make search case sensitive only if it contains uppercase letters
set wrapscan   " Search again from top when reached the bottom
"}}}2
" Persistent undo {{{2
" -----------------------------------------------------------------------------

if has('persistent_undo')
  set undofile
  set undodir=~/.config/nvim/tmp/undo//
endif
" }}}
" Hidden characters settings {{{2
" -----------------------------------------------------------------------------

set list
set listchars=tab:⋅⋅,trail:●,extends:#,nbsp:.
set showbreak=↪

"}}}
" Indentation"{{{2
" -----------------------------------------------------------------------------

set expandtab
set softtabstop=2 " Indent using 2 spaces
set tabstop=2     " Display tabs as 2 spaces
set shiftwidth=2
set noshiftround
set autoindent    " Automatic indentation
set copyindent    " Copy previous indetation on autoindenting

"}}}
" Folding settings {{{2
" -----------------------------------------------------------------------------

set foldmethod=indent " Fold by indentation
set foldopen-=block   " Disable fold opening when jumping paragraphs
set foldlevelstart=99 " start unfolded
set foldnestmax=2     " Maximum fold nesting level

function! MyFoldText() "{{{3
let line = getline(v:foldstart)
  let nucolwidth = &fdc + &number * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 3
  let foldedlinecount = v:foldend - v:foldstart

  " expand tabs into spaces
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')

  let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
  let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
  return line . '...' . repeat(" ",fillcharcount) . foldedlinecount . '...' . ' '
endfunction
set foldtext=MyFoldText() "}}}3

" }}}2
"  Omni completion {{{2
" -----------------------------------------------------------------------------

set completeopt-=preview " Don't show preview scratch buffers
set wildignore=*.o,*.obj,*~
set wildignore+=*vim/backups*
set wildignore+=*sass-cache*
set wildignore+=*DS_Store*
set wildignore+=*.gem
set wildignore+=tmp/**

" Remove tags from complete
set complete=.,w,b,u
" }}}2
" Scrolloff {{{2
" -----------------------------------------------------------------------------

" Start scrolling:
" Bottom and Top: 10 Lines
" Side: 15 lines
set scrolloff=10
set sidescrolloff=15
" Scroll one column on the side
set sidescroll=1
"}}}2
" Python"{{{2
" -----------------------------------------------------------------------------

let g:loaded_python_provider=1 " Disable python 2 interface
let g:python_host_skip_check=1 " Skip python 2 host check
let g:python3_host_skip_check = 1
let g:python3_host_prog = '/usr/local/bin/python3'
let g:python_host_prog = '/usr/bin/python'"}}}
" }}}2
" Local VimRC Whitelist "{{{2
" -----------------------------------------------------------------------------

" To allow all local configurations for certain projects put them in
" a whitelist.vim file in the current dir in this format:
" let g:localvimrc_whitelist=['/home/user/project1/', '/opt/project2/', '/usr/local/projects/vim-[^/]*/']
if filereadable(expand('~/.config/nvim/whitelist.vim'))
  source ~/.config/nvim/whitelist.vim
endif

" }}}2

" }}}1
" =============================================================================

" Plugin settings {{{1
" ==========================================================================

" Editorconfig {{{2
" -----------------------------------------------------------------------------

" Use external Editorconfig
let g:EditorConfig_core_mode = 'external_command'

" Dont use editorconfig with fugitive or ssh
let g:EditorConfig_exclude_patterns = ['fugitive://.*', 'scp://.*']

" }}}2
" Emmet {{{2
" -----------------------------------------------------------------------------

let g:user_emmet_leader_key='<C-e>'

" }}}2
" ALE Linter {{{2
" -----------------------------------------------------------------------------

let g:ale_lint_on_save = 1
let g:ale_lint_on_text_changed = 0
let g:ale_lint_on_enter = 0
" let g:ale_lint_delay = 500
let g:ale_linters = {
      \ 'javascript': ['eslint'],
      \ 'html': [],
      \}

" }}}2
" GitGutter {{{2
" -----------------------------------------------------------------------------

" Only load gitgutter when the file is loaded/saved
" This increases the speed in tab switching
let g:gitgutter_max_signs=9999
let g:gitgutter_eager = 1
let g:gitgutter_realtime = 0

" Always show sign column to prevent editor jumps
let g:gitgutter_sign_column_always = 1

" Disable default Keyboard mappings
let g:gitgutter_map_keys = 0

" }}}2
" Vim-Javascript {{{2
" -----------------------------------------------------------------------------

" Enable syntax highlighting for jsdoc
let g:javascript_plugin_jsdoc = 1

" }}}2
" Over.vim {{{2
" Interactive Search and replace
" -----------------------------------------------------------------------------

let g:over_enable_auto_nohlsearch = 1
let g:over_enable_cmd_window = 1

" }}}2
" NV {{{2
" -----------------------------------------------------------------------------

let g:nv_directories = ['~/Dropbox/Notes']

" }}}2
" Vim-Markdown {{{2
" -----------------------------------------------------------------------------

let g:vim_markdown_folding_disabled = 1
let g:vim_markdown_no_default_key_mappings = 1
" }}}2
" vim-markdown-folding {{{2
" -----------------------------------------------------------------------------

" Fold at the Header instead just folding the content
let g:markdown_fold_style = 'nested'
" Dont override my fold style with the plugin fold style
let g:markdown_fold_override_foldtext = 0

" }}}2
" AsyncRun {{{2

" Enable Async Fugitive GPush/Gpull
command! -bang -nargs=* -complete=file Make AsyncRun -program=make @ <args>

" }}}2

inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"
imap <c-space> <Plug>(asyncomplete_force_refresh)

call asyncomplete#register_source(asyncomplete#sources#buffer#get_source_options({
    \ 'name': 'buffer',
    \ 'whitelist': ['*'],
    \ 'blacklist': ['go'],
    \ 'completor': function('asyncomplete#sources#buffer#completor'),
    \ }))

call asyncomplete#register_source(asyncomplete#sources#tscompletejob#get_source_options({
    \ 'name': 'tscompletejob',
    \ 'whitelist': ['typescript', 'javascript'],
    \ 'completor': function('asyncomplete#sources#tscompletejob#completor'),
    \ }))

" }}}1

" Utilities"{{{
" -----------------------------------------------------------------------------

" Source the utils.vim because we need the functions in global scope
source ~/.config/nvim/autoload/utils.vim
"}}}
