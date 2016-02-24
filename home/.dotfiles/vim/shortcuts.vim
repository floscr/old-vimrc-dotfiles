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

" Enter Visualblock mode
command! Vb normal! <C-v>

" Create the file under the cursor
" Create file under cursor
" map <leader>gf :e <cfile><cr>
nnoremap gF :e <cfile><cr>

" Enter command by pressing enter
nnoremap <Cr> :

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

" Create file under cursor
map <leader>gf :e <cfile><cr>

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
nnoremap <leader>ev :split $MYVIMRC<CR>
nnoremap <leader>sv :source $MYVIMRC<CR>

" Quicker window movement
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-h> <C-w>h
nnoremap <C-l> <C-w>l

""" SYSTEM CLIPBOARD COPY & PASTE SUPPORT
set pastetoggle=<F4> "F2 before pasting to preserve indentation
"Copy paste to/from clipboard
vnoremap <C-c> "*y

map <silent><Leader>p :set paste<CR><esc>"*p:set nopaste<cr>"
" map <silent><Leader>p :set paste<CR>o<esc>"*]p:set nopaste<cr>"
map <silent><Leader><S-p> :set paste<CR>O<esc>"*]p:set nopaste<cr>"
map <silent><C-v> :set paste<CR>o<esc>"*]p:set nopaste<cr>"

" Yank text to the OS X clipboard
noremap <leader>y "*y
noremap <leader>yy "*Y

" Run the q macro
noremap Q @q

" PASTE Mode
nnoremap <silent> <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar>:nohl<CR>

" Remove all trailing whitespace
" nnoremap <Leader>rtw :%s/\s\+$//e<CR>

" Buffers
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :bd<cr>
map gdo :Bonly<cr>
map gl :ls<return>

" Open current file in finder
nnoremap <leader><cr> :silent !open .<cr>

function! NumberToggle()
	if(&relativenumber == 1)
		set number
		set norelativenumber
	else
		set relativenumber
	endif
endfunc

nnoremap <Leader>0 :call NumberToggle()<cr>
