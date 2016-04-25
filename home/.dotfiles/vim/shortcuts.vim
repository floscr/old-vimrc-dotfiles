" Create file under cursor
nnoremap gF :e <cfile><cr>

" Enter command by pressing enter
nnoremap <Cr> :

" Make * star work in visual mode
vnoremap <silent> * y:let @/=@"<cr>:set hlsearch<cr>n

" Break line on cursor
function! s:BreakHere()
  s/^\(\s*\)\(.\{-}\)\(\s*\)\(\%#\)\(\s*\)\(.*\)/\1\2\r\1\4\6
  call histdel("/", -1)
endfunction
nnoremap K :call BreakHere()<CR>

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

" Reload .vimrc
" This would cause the last search to be highlighted,
" Workaround to disable this.
nnoremap <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<return><esc>

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


" Delete buffer while keeping window layout (don't close buffer's windows).
" Version 2008-11-18 from http://vim.wikia.com/wiki/VimTip165
if v:version < 700 || exists('loaded_bclose') || &cp
  finish
endif

let loaded_bclose = 1
if !exists('bclose_multiple')
  let bclose_multiple = 1
endif

" Display an error message.
function! s:Warn(msg)
  echohl ErrorMsg
  echomsg a:msg
  echohl NONE
endfunction

" Command ':Bclose' executes ':bd' to delete buffer in current window.
" The window will show the alternate buffer (Ctrl-^) if it exists,
" or the previous buffer (:bp), or a blank buffer if no previous.
" Command ':Bclose!' is the same, but executes ':bd!' (discard changes).
" An optional argument can specify which buffer to close (name or number).
function! s:Bclose(bang, buffer)
  if empty(a:buffer)
    let btarget = bufnr('%')
  elseif a:buffer =~ '^\d\+$'
    let btarget = bufnr(str2nr(a:buffer))
  else
    let btarget = bufnr(a:buffer)
  endif
  if btarget < 0
    call s:Warn('No matching buffer for '.a:buffer)
    return
  endif
  if empty(a:bang) && getbufvar(btarget, '&modified')
    call s:Warn('No write since last change for buffer '.btarget.' (use :Bclose!)')
    return
  endif
  " Numbers of windows that view target buffer which we will delete.
  let wnums = filter(range(1, winnr('$')), 'winbufnr(v:val) == btarget')
  if !g:bclose_multiple && len(wnums) > 1
    call s:Warn('Buffer is in multiple windows (use ":let bclose_multiple=1")')
    return
  endif
  let wcurrent = winnr()
  for w in wnums
    execute w.'wincmd w'
    let prevbuf = bufnr('#')
    if prevbuf > 0 && buflisted(prevbuf) && prevbuf != w
      buffer #
    else
      bprevious
    endif
    if btarget == bufnr('%')
      " Numbers of listed buffers which are not the target to be deleted.
      let blisted = filter(range(1, bufnr('$')), 'buflisted(v:val) && v:val != btarget')
      " Listed, not target, and not displayed.
      let bhidden = filter(copy(blisted), 'bufwinnr(v:val) < 0')
      " Take the first buffer, if any (could be more intelligent).
      let bjump = (bhidden + blisted + [-1])[0]
      if bjump > 0
        execute 'buffer '.bjump
      else
        execute 'enew'.a:bang
      endif
    endif
  endfor
  execute 'bdelete'.a:bang.' '.btarget
  execute wcurrent.'wincmd w'
endfunction
command! -bang -complete=buffer -nargs=? Bclose call s:Bclose('<bang>', '<args>')
" nnoremap <silent> <Leader>r :Bclose<CR>


" Buffers
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :Bclose<cr>
map gdo :Bonly<cr>
map gl :ls<return>

" Open current file in finder
nnoremap <leader><cr> :silent !open .<cr>

