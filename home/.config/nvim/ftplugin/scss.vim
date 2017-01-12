" Set - as part of the word in css
autocmd FileType css,scss setl iskeyword+=-
autocmd FileType scss set omnifunc=csscomplete#CompleteCSS

" Wrap lighten around a word
autocmd FileType scss let b:surround_{char2nr('e')} = "lighten(\r, 10%)"

" Create scss files under cursor
nmap <buffer> <silent> gF :e %:p:h/<cfile>:h/_<cfile>:t.scss<CR>

" imap <buffer> <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

" Autoformat using stylefmt
noremap <buffer> <silent> <F3> :!stylefmt %<CR>
