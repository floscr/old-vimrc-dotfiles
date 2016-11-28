" -----------------------------------------------------------------------------
" -----------------------------------------------------------------------------
" /\\\________/\\\_____________________________________________________________
" \/\\\_______\/\\\____________________________________________________________
" _\//\\\______/\\\___/\\\_____________________________________________________
" ___\//\\\____/\\\___\///_____/\\\\\__/\\\\\____/\\/\\\\\\\______/\\\\\\\\____
" _____\//\\\__/\\\_____/\\\__/\\\///\\\\\///\\\_\/\\\/////\\\___/\\\//////____
" _______\//\\\/\\\_____\/\\\_\/\\\_\//\\\__\/\\\_\/\\\___\///___/\\\__________
" _________\//\\\\\______\/\\\_\/\\\__\/\\\__\/\\\_\/\\\_________\//\\\________
" ___________\//\\\_______\/\\\_\/\\\__\/\\\__\/\\\_\/\\\__________\///\\\\\\\\
" _____________\///________\///__\///___\///___\///__\///_____________\////////
" -----------------------------------------------------------------------------
" -----------------------------------------------------------------------------

source ~/.config/nvim/plugins.vim

" =============================================================================
" Default Settings
" Neovim defaults: https://neovim.io/doc/user/vim_diff.html#nvim-option-defaults
" =============================================================================

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
set suffixesadd+=.js " Automatically add suffic when pressing gf to go to a file
set synmaxcol=1500   " Turn off syntax highlighting after X lines

" Disable Netrw
let g:loaded_netrw       = 1
let g:loaded_netrwPlugin = 1

let g:vim_markdown_folding_disabled = 1

function! MyFoldText() " {{{
  let line = getline(v:foldstart)

  let nucolwidth = &fdc + &number * &numberwidth
  let windowwidth = winwidth(0) - nucolwidth - 3
  let foldedlinecount = v:foldend - v:foldstart

  " expand tabs into spaces
  let onetab = strpart('          ', 0, &tabstop)
  let line = substitute(line, '\t', onetab, 'g')

  let line = strpart(line, 0, windowwidth - 2 -len(foldedlinecount))
  let fillcharcount = windowwidth - len(line) - len(foldedlinecount)
  return line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endfunction " }}}

function! JavaScriptFold() "{{{
  " syntax region foldBraces start=/{/ end=/}/ transparent fold keepend extend
  setlocal foldmethod=syntax
  setlocal foldlevel=99
  echo "hello"
  syn region foldBraces start=/{/ skip=/\(\/\/.*\)\|\(\/.*\/\)/ end=/}/ transparent fold keepend extend
endfunction "}}}

autocmd InsertEnter * if !exists('w:last_fdm') | let w:last_fdm=&foldmethod | setlocal foldmethod=manual | endif
autocmd InsertLeave,WinLeave * if exists('w:last_fdm') | let &l:foldmethod=w:last_fdm | unlet w:last_fdm | endif

set foldlevel=99
" Space to toggle folds.
autocmd FileType vim setlocal foldmethod=marker
autocmd FileType vim setlocal foldlevel=0

" au FileType html call HTMLFold()
" autocmd FileType html setlocal foldmethod=syntax
autocmd FileType html setlocal fdl=99

" autocmd FileType javascript call JavaScriptFold()
autocmd FileType javascript,html,css,scss,typescript setlocal foldlevel=99
autocmd FileType javascript,typescript,css,scss,json setlocal foldmethod=marker
autocmd FileType javascript,typescript,css,scss,json setlocal foldmarker={,}
autocmd FileType coffee setl foldmethod=indent
" au FileType html nnoremap <buffer> <leader>F zfat
" }}}

" autocmd BufWinLeave .* mkview
" autocmd BufWinEnter .* silent loadview
" autocmd BufWinLeave *.* mkview!
" autocmd BufWinEnter *.* silent loadview

set sessionoptions-=options
" set viewoptions=folds,cursor,unix,slash

augroup autosave_buffer
 autocmd!
 autocmd BufWinLeave *.* mkview
 autocmd BufWinEnter *.* silent! loadview
augroup END

" http://stackoverflow.com/questions/26917336/vim-specific-mkview-and-loadview-in-order-to-avoid-issues
set viewoptions-=options


" au BufWinLeave * mkview
" au BufWinEnter * silent loadview

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
set incsearch  " Incremental search
set ignorecase " Ignore case by default
set smartcase  " Make search case sensitive only if it contains uppercase letters
set wrapscan   " Search again from top when reached the bottom

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

set foldopen-=block   " Disable fold opening when jumping paragraphs
set foldmethod=indent " Fold by indentation
set foldnestmax=2     " deepest fold is 10 levels
set nofoldenable      " dont fold by default
set foldlevel=1       " this is just what i use
set foldlevelstart=99 " Open folds on beginning of file
set foldcolumn=0      " Disable fold column

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

" Source current file
nmap <silent> <leader>sf :source %<cr>

" -----------------------------------------------------
" 3.3 Keyboard shortcuts / bindings
" -----------------------------------------------------

" When cycling windows ignore NERDTree
" nmap <silent> <C-w><C-w> :call utils#intelligentCycling()<CR>
" nnoremap <C-h> <C-w>h
" nnoremap <C-j> <C-w>j
" nnoremap <C-k> <C-w>k
" nnoremap <C-l> <C-w>l

" When jump to next match also center screen
" Note: Use :norm! to make it count as one command. (i.e. for i_CTRL-o)
nnoremap <silent> n :norm! nzz<CR>
nnoremap <silent> N :norm! Nzz<CR>
vnoremap <silent> n :norm! nzz<CR>
vnoremap <silent> N :norm! Nzz<CR>

" Quickfix list
nnoremap ]q :cn<CR>
nnoremap [q :cp<CR>

" Quick replay 'q' macro
nnoremap Q @q

" Don't yank to default register when changing something
nnoremap c "xc
xnoremap c "xc

" Toggle spellcheck
nmap <silent> <leader>ss :set spell!<cr>

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
nnoremap K jddkPmzJ`z

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
  if system('git rev-parse --show-toplevel 2> /dev/null') != ''
    return 'GitFiles'
  endif
  return 'Files'
endfunction
command! ProjectFiles execute s:find_git_root()

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

" Filename completion with fzf
imap <c-x><c-f> <plug>(fzf-complete-path)

" -----------------------------------------------------
" NERDTree
" -----------------------------------------------------

let NERDTreeIgnore=['\.pyc', '\~$', '\.swo$', '\.swp$', '\.git$', '\.hg', '\.svn', '\.bzr', 'node_modules', '.DS_Store']
let NERDTreeShowHidden=1
let g:NERDTreeMouseMode=3

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

let g:deoplete#enable_at_startup=1
let g:deoplete#enable_refresh_always=0
let g:deoplete#file#enable_buffer_path=1
" Trigger deoplete only when pressing tab
let g:deoplete#disable_auto_complete=1

let g:deoplete#sources={}
let g:deoplete#sources._    = ['buffer', 'file', 'ultisnips']
let g:deoplete#sources.vim  = ['buffer', 'member', 'file', 'ultisnips']
let g:deoplete#sources['javascript.jsx'] = ['buffer', 'file', 'ultisnips']
let g:deoplete#sources.css  = ['buffer', 'member', 'file', 'omni', 'ultisnips']
let g:deoplete#sources.scss = ['buffer', 'member', 'file', 'omni', 'ultisnips']
let g:deoplete#sources.html = ['buffer', 'member', 'file', 'omni', 'ultisnips']

" Insert <TAB> or select next match
inoremap <silent> <expr> <Tab> utils#tabComplete()
imap <silent> <expr> <Tab> utils#tabComplete()

" use tab to backward cycle
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"

" Manually trigger tag autocomplete
inoremap <silent> <expr> <C-]> utils#manualTagComplete()

" -----------------------------------------------------
" Fugitive
" -----------------------------------------------------

" Open quickfix window automatically after grepping
autocmd QuickFixCmdPost *grep* cwindow

" Always use vertical diffs
set diffopt+=vertical

" Add the current file to index
nnoremap <silent> <leader>gf :Git add %:p<CR><CR>
" Add all changes to index
nnoremap <silent> <leader>ga :Git add .<CR><CR><CR>
" Git Status
nnoremap <leader>gs :Gstatus<CR>
" Commit added index
nnoremap <leader>gc :Gcommit -v -q<CR>
" Add and commit current file
nnoremap <space>gt :Gcommit -v -q %:p<CR>

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
nnoremap <silent> [c <Esc>:GitGutterPrevHunk<CR>zMzvzz
nnoremap <silent> ]c <Esc>:GitGutterNextHunk<CR>zMzvzz
" nmap ghn <Plug>GitGutterNextHunk
" nmap ghp <Plug>GitGutterPrevHunk

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
let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.config/nvim/emmet/snippets.json')), "\n"))

" -----------------------------------------------------
" Lightline
" -----------------------------------------------------

let g:lightline = {
      \ 'colorscheme': 'hybrid',
      \ 'active': {
      \	'left': [
      \		[ 'mode' ],
      \		[ 'fugitive', 'filename' ],
      \	]
      \ },
      \ 'component_function': {
      \	'fugitive': 'LightlineFugitive',
      \	'filename': 'LightlineFilename',
      \	'fileformat': 'LightlineFileformat',
      \	'filetype': 'LightlineFiletype',
      \	'fileencoding': 'LightlineFileencoding',
      \	'mode': 'LightlineMode',
      \ },
      \ 'component': {
      \	'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \ 'component_visible_condition': {
      \	'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ }
      \ }

function! LightlineModified()
  return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help' && &readonly ? 'RO' : ''
endfunction

function! LightlineFilename()
  let fname = expand('%:t')
  return fname == 'ControlP' ? g:lightline.ctrlp_item :
        \ fname =~ 'NERD_tree' ? '' :
        \ ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ ('' != fname ? fname : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
  try
    if expand('%:t') !~? 'NERD' && exists('*fugitive#head')
      let mark = '!'
      let _ = fugitive#head()
      return strlen(_) ? _.mark : ''
    endif
  catch
  endtry
  return ''
endfunction

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
  let fname = expand('%:t')
  return fname == 'ControlP' ? 'CtrlP' :
        \ fname =~ 'NERD_tree' ? 'NERDTree' :
        \ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

let g:ctrlp_status_func = {
      \	'main': 'LightlineCtrlPStatusFunc_1',
      \	'prog': 'LightlineCtrlPStatusFunc_2',
      \ }

function! LightlineCtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  return lightline#statusline(0)
endfunction

function! LightlineCtrlPStatusFunc_2(str)
  return lightline#statusline(0)
endfunction

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

let g:undotree_WindowLayout = 4
let g:undotree_SetFocusWhenToggle = 1
let g:undotree_SplitWidth = 60

function! g:Undotree_CustomMap()
  nmap <buffer> k <plug>UndotreeGoNextState
  nmap <buffer> j <plug>UndotreeGoPreviousState
  nmap <buffer> <Esc> <plug>UndotreeClose
endfunction

" -----------------------------------------------------
" NeoMake
" -----------------------------------------------------

let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_verbose=1
let g:neomake_warning_sign = {
      \ 'text': '⚠',
      \ 'texthl': 'WarningMsg',
      \ }
let g:neomake_error_sign = {
      \ 'text': '✘',
      \ 'texthl': 'ErrorMsg',
      \ }
"}}}

" -----------------------------------------------------
" PDV - PHP Documentor for VIM - 2
" -----------------------------------------------------

let g:pdv_template_dir = $HOME . '/.config/nvim/plugged/pdv/templates_snip'
nnoremap <silent> ,p :call pdv#DocumentWithSnip()<CR>

" -----------------------------------------------------
" MatchTag
" -----------------------------------------------------

" Custom match tag colors
highlight MatchParen guibg=#2E3C47 guifg=#BCC8C6

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

" -----------------------------------------------------
" JsBeautify
" -----------------------------------------------------

autocmd FileType javascript,vue noremap <buffer>  <c-f> :call JsBeautify()<cr>
autocmd FileType json noremap <buffer> <c-f> :call JsonBeautify()<cr>
autocmd FileType jsx noremap <buffer> <c-f> :call JsxBeautify()<cr>
autocmd FileType html,twig noremap <buffer> <c-f> :call HtmlBeautify()<cr>
autocmd FileType css,scss,sass noremap <buffer> <c-f> :call CSSBeautify()<cr>

" Visual Range Beautify
autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
autocmd FileType json vnoremap <buffer> <c-f> :call RangeJsonBeautify()<cr>
autocmd FileType jsx vnoremap <buffer> <c-f> :call RangeJsxBeautify()<cr>
autocmd FileType html vnoremap <buffer> <c-f> :call RangeHtmlBeautify()<cr>
autocmd FileType css vnoremap <buffer> <c-f> :call RangeCSSBeautify()<cr>

" Hardcode the Beautifier config here
" ~/.editorconfig doesnt seem to be working
let g:config_Beautifier = {}
let g:config_Beautifier['js'] = {}
let g:config_Beautifier['js'].indent_size = '2'
let g:config_Beautifier['css'] = {}
let g:config_Beautifier['css'].indent_size = '2'

" -----------------------------------------------------
" Sideways
" -----------------------------------------------------

" Move function argument to the left/right
nmap <leader>] <Plug>SidewaysRight
nmap <leader>[ <Plug>SidewaysLeft

" -----------------------------------------------------
" JsDoc
" -----------------------------------------------------

let g:jsdoc_allow_input_prompt=1
let g:jsdoc_input_description=1
let g:jsdoc_enable_es6=1

" Filebeagle
let g:filebeagle_suppress_keymaps = 1
map <silent> - <Plug>FileBeagleOpenCurrentBufferDir

" =============================================================================
" 7.0 Autocommands
" =============================================================================

" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja
" Set html5 syntax for vue files to fix broken indentation
au BufRead,BufNewFile *.vue set filetype=html
au BufRead,BufNewFile *.zsh* set filetype=zsh

" Remove trailing whitespaces automatically before save
autocmd BufWritePre * call utils#stripTrailingWhitespaces()

" Restore enter for the quickfix window
autocmd FileType qf nnoremap <buffer> <CR> <CR>

" Exit FZF by pressing escape
autocmd! FileType fzf tnoremap <buffer> <esc> <C-c>

" Preview quickfix result
autocmd FileType qf nnoremap <buffer> <Tab> <Enter><C-W>j

" How should we execute the search?
" --heading and --stats are required!
let g:side_search_prg = 'ag --word-regexp'
  \. " --ignore='*.js.map'"
  \. " --heading --stats -B 1 -A 4"

" Can use `vnew` or `new`
let g:side_search_splitter = 'vnew'

" I like 40% splits, change it if you don't
let g:side_search_split_pct = 0.4

" -----------------------------------------------------
" 7.1 Run linters after save
" -----------------------------------------------------

" npm install -g eslint
autocmd BufWritePost *.js Neomake eslint

source ~/.config/nvim/autoload/utils.vim
