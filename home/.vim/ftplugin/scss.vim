" Set - as part of the word in css
autocmd FileType css,scss setl iskeyword+=-
autocmd FileType scss set omnifunc=csscomplete#CompleteCSS

autocmd FileType scss set iskeyword+=-

" Quickly navigate classes in css
nmap <silent> <buffer> <leader>{ ?{<CR>
nmap <silent> <buffer> <leader>} ?}<CR>

" Create scss files under cursor
nmap <buffer> <silent> gF :e %:p:h/<cfile>:h/_<cfile>:t.scss<CR>

" imap <buffer> <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

" Autoformat using stylefmt
noremap <buffer> <silent> <F3> :!stylefmt %<CR>
