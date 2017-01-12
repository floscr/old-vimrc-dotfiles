" ---
" PHP
" ---

" Lint php files
autocmd BufWritePost,BufEnter *.php Neomake
autocmd BufWritePost,BufEnter *.php setlocal commentstring=//%s

nnoremap <buffer><silent><leader>pf :call PhpCsFixerFixFile()<CR>

" Tabularize fix for fat arrow array
map <Leader>= :Tabularize /=[> ]<cr>
