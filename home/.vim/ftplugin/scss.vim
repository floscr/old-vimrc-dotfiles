" Set - as part of the word in css
au! FileType css,scss,sass setl iskeyword+=-
autocmd FileType css,scss,sass set omnifunc=csscomplete#CompleteCSS

" Quickly navigate classes in css
nmap <silent> <buffer> <leader>{ ?{<CR>
nmap <silent> <buffer> <leader>} ?}<CR>
