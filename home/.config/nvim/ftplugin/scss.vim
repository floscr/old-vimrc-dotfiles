" Set - as part of the word in css
autocmd FileType css,scss setl iskeyword+=-
autocmd FileType scss set omnifunc=csscomplete#CompleteCSS

" Create scss files under cursor
nmap <buffer> <silent> gF :e %:p:h/<cfile>:h/_<cfile>:t.scss<CR>

" Autoformat using stylefmt
noremap <buffer> <silent> <F3> :!stylefmt %<CR>
