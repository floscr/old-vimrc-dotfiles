" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja
autocmd BufRead,BufNewFile,BufReadPost *.njk set ft=htmljinja

" Set html5 syntax for vue files to fix broken indentation
au BufRead,BufNewFile *.vue set filetype=html

" Fix ZSH filetype
au BufRead,BufNewFile *.zsh* set filetype=zsh

" Config files
au BufRead,BufNewFile *.conf set filetype=conf

" Remove trailing whitespaces automatically before save
autocmd BufWritePre * call utils#stripTrailingWhitespaces()

" Restore enter for the quickfix window
autocmd FileType qf nnoremap <buffer> <CR> <CR>

" Preview quickfix result
autocmd FileType qf nnoremap <buffer> <Tab> <Enter><C-W>j

" don't move the cursor back when exiting from insert mode
function! DesiredCol()
    let col = getpos('.')[2]
    if col == 1
        return col
    endif
    return col + 1
endfunction
autocmd! InsertLeave * call cursor(getpos('.')[1], DesiredCol())
