" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja
autocmd BufRead,BufNewFile,BufReadPost *.njk set ft=htmljinja

" Set html5 syntax for vue files to fix broken indentation
au BufRead,BufNewFile *.vue set filetype=html

" Fix ZSH filetype
au BufRead,BufNewFile *.zsh* set filetype=zsh

" Remove trailing whitespaces automatically before save
autocmd BufWritePre * call utils#stripTrailingWhitespaces()

" Restore enter for the quickfix window
autocmd FileType qf nnoremap <buffer> <CR> <CR>

" Preview quickfix result
autocmd FileType qf nnoremap <buffer> <Tab> <Enter><C-W>j
