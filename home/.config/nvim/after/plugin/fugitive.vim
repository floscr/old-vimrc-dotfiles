" Open quickfix window automatically after grepping
autocmd QuickFixCmdPost *grep* cwindow

" Always use vertical diffs
set diffopt+=vertical

" Add the current file to index
nnoremap <silent> <leader>gf :Git add %:p<CR><CR>
" Add all changes to index
" :Git add . would return a message with [Process Exited 0]
nnoremap <silent> <leader>ga :silent !git add -A<CR>:echo "Added all files to index."<CR>
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

