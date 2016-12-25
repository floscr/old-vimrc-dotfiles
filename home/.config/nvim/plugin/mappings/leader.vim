" Reload .vimrc
" When sourcing files, the last seach gets highlighted
" This mapping auto disables the highlight
nnoremap <silent> <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<CR><esc>:echo "Vimrc reloaded!"<CR>

" Source current file
nmap <silent> <leader>sf :source %<CR><ESC>:echo "Current file sourced!"<CR>

" Toggle spellcheck
nmap <silent> <leader>ss :set spell!<cr>

" Yank text to the OS X clipboard
noremap <leader>y "*y
noremap <leader>yy "*Y

" Show current file in finder
nnoremap <leader><cr> :silent !open .<cr>

" Toggle the error list
nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>
