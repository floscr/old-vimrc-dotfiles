" Replay last Macro
nnoremap Q @q

" Buffer switching and terminalion
map gn :bn<cr>
map gp :bp<cr>
map gb :b#<cr>
map gdd :Bdelete<cr>
map gdo :Bonly<cr>

" Reverse join (Turn single line comments to inline comments)
nnoremap gJ jddkPmzJ`z

" Keep the cursor in place while joining lines
nnoremap J mzJ`z

" Make . work with visually selected lines
xnoremap . :norm.<CR>

" Clear highlighting on escape in normal mode
nnoremap <silent><esc> :noh<return><esc>
nnoremap <esc>^[ <esc>^[

" Quit current buffer
nnoremap <C-c> :q<return>

" " When jump to next match also center screen
" " Note: Use :norm! to make it count as one command. (i.e. for i_CTRL-o)
" nnoremap <silent> n :norm! nzz<CR>
" nnoremap <silent> N :norm! Nzz<CR>
" vnoremap <silent> n :norm! nzz<CR>
" vnoremap <silent> N :norm! Nzz<CR>

" Make * star work in visual mode
vnoremap <silent> * y:let @/=@"<cr>:set hlsearch<cr>n

" Use the last used search to use in replace command
nmap <expr> M ':%s/' . @/ . '//g<LEFT><LEFT>'

" Trigger over.vim
map <C-_> :OverCommandLine<CR>%s/
" Indentation using tab
imap <S-Tab> <C-o><<
map <S-Tab> <<
map <Tab> >>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv


" Quickfix next/previous
nnoremap ]q :cn<CR>
nnoremap [q :cp<CR>

if has('nvim')
  " Neovim Terminal
  " Press escape to exit insert mode
  tnoremap <ESC> <C-\><C-n>
  tnoremap ,<ESC> <ESC>

  " Workaround for ctrl-h to work
  " https://github.com/neovim/neovim/issues/2048
  nmap <BS> <C-W>h
endif

" Enter command by pressing enter
nnoremap <Cr> :

" Create file under cursor
nnoremap gF :e <cfile><cr>

" Manual fold around the current bracket pair
" Example: (Javascript, | represents the cursor)
" function | () {
"   console.log('Hello world')
" }
" <zfaf>
" fucntion () {...}
nnoremap <silent> zfaf V$%$zf
