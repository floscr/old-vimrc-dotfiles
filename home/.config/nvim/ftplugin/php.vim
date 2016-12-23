" ---
" PHP
" ---

" Lint php files
autocmd BufWritePost,BufEnter *.php Neomake

nnoremap <buffer><silent><leader>pf :call PhpCsFixerFixFile()<CR>

" Tabularize fix for fat arrow array
map <Leader>= :Tabularize /=[> ]<cr>
