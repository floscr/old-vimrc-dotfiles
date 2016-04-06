" Workaround for alt keys to work with mac
" º = A-j
" ∆ = A-k
if has("mac")
  nnoremap º :m .+1<CR>==
  nnoremap ∆ :m .-2<CR>==
  vnoremap º :m '>+1<CR>gv=gv
  vnoremap ∆ :m '<-2<CR>gv=gv

  " Number up/down
  " ⌥ + a/x
  nnoremap å <C-a>
  nnoremap Å <C-a><C-a><C-a><C-a><C-a><C-a><C-a><C-a><C-a><C-a>
  nnoremap ≈ <C-x>
  nnoremap Ù <C-x><C-x><C-x><C-x><C-x><C-x><C-x><C-x><C-x><C-x>
endif


" Create file under cursor
nnoremap gF :e <cfile><cr>

" Enter command by pressing enter
nnoremap <Cr> :

" Split lines / opposite of J
nnoremap S ht lr<cr>k$

" Zoom / Restore window.
function! s:ZoomToggle() abort
  if exists('t:zoomed') && t:zoomed
    execute t:zoom_winrestcmd
    let t:zoomed = 0
  else
    let t:zoom_winrestcmd = winrestcmd()
    resize
    vertical resize
    let t:zoomed = 1
  endif
endfunction
command! ZoomToggle call s:ZoomToggle()
nnoremap <silent> <C-w>z :ZoomToggle<CR>

" Use the last used search to use in replace command
nmap <expr> M ':%s/' . @/ . '//g<LEFT><LEFT>'

" Yank from cursor to end of line
map Y ^y$

" Resize window with the arrow keys =)
noremap <up> <C-W>+
noremap <down> <C-W>-
noremap <left> 3<C-W><
noremap <right> 3<C-W>>

" TABBING BEHAVIOUR
" UnIndet with <shift-tab>
imap <S-Tab> <C-o><<
map <S-Tab> <<
map <Tab> >>
" Visual Indentation
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv

" Clear highlighting on escape in normal mode
nnoremap <silent><esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" Quick Close
nnoremap <C-c> :q<return>

" Quickly open/reload vim
nnoremap <leader>ev :e ~/.homesick/repos/dotfiles/home/.vimrc<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

""" SYSTEM CLIPBOARD COPY & PASTE SUPPORT
" map <silent><Leader>p :set paste<CR><esc>"*p:set nopaste<cr>"
" map <silent><Leader><S-p> :set paste<CR>O<esc>"*]p:set nopaste<cr>"
" map <silent><C-v> :set paste<CR>o<esc>"*]p:set nopaste<cr>"

" Yank text to the OS X clipboard
noremap <leader>y "*y
noremap <leader>yy "*Y

" Select the last thing that has been pasted
nnoremap gV `[V`]`]`

" Zoom when in Tmux(>v1.8)
if exists('$TMUX')
  nnoremap <silent> <leader>z :call system("tmux resize-pane -Z")<CR>
endif

" Run the q macro
noremap Q @q

" PASTE Mode
" nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" OpenChangedFiles (<Leader>O)---------------------- {{{
" https://github.com/ignu/dotfiles2.0/blob/master/vimrc#L539
function! OpenChangedFiles()
  only " Close all windows, unless they're modified
  let status = system('git status -s | grep "^ \?\(M\|A\)" | cut -d " " -f 3')
  let filenames = split(status, "\n")

  if len(filenames) < 1
    let status = system('git show --pretty="format:" --name-only')
    let filenames = split(status, "\n")
  endif

  exec "edit " . filenames[0]

  for filename in filenames[1:]
    if len(filenames) > 4
      exec "tabedit " . filename
    else
      exec "sp " . filename
    endif
  endfor
endfunction
command! OpenChangedFiles :call OpenChangedFiles()
noremap<Leader>O :OpenChangedFiles <CR>
" }}}

" Buffers
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :bd<cr>
map gdo :Bonly<cr>
map gl :ls<return>

" Open current file in finder
nnoremap <leader><cr> :silent !open .<cr>

