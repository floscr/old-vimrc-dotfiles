" Reload .vimrc
" When sourcing files, the last seach gets highlighted
" This mapping auto disables the highlight
nnoremap <silent> <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<CR><esc>:echo "Vimrc reloaded!"<CR>

" Source current file
nmap <silent> <leader>sf :source %<CR><ESC>:echo "Current file sourced!"<CR>
" Source current file and Install plugins
nmap <silent> <leader>sp :source %<CR><ESC>:PlugInstall<CR>

" Color Picker
nnoremap <leader>v :VCoolor<CR>

" Toggle spellcheck
nmap <silent> <leader>ss :set spell!<cr>

" Show current file in finder
nnoremap <leader><cr> :silent !open .<cr>

" Toggle the error list
nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>

nmap <silent> <leader>gg :Gcd<CR>:GGrep<CR>
