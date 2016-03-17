" Set - as part of the word in css
autocmd FileType scss set omnifunc=csscomplete#CompleteCSS

" Quickly navigate classes in css
nmap <silent> <buffer> <leader>{ ?{<CR>
nmap <silent> <buffer> <leader>} ?}<CR>

" Create scss files under cursor
nmap <silent> <buffer> gF :e <cfile>:h/_<cfile>:t.scss<CR>
