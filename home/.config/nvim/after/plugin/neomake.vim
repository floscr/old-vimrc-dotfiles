" Scroll through error list
nmap <silent> [e :LocationPrevious<CR>
nmap <silent> ]e :LocationNext<CR>

let g:neomake_javascript_enabled_makers = ['eslint']
let g:neomake_verbose=1
let g:neomake_warning_sign = {
      \ 'text': '⚠',
      \ 'texthl': 'WarningMsg',
      \ }
let g:neomake_error_sign = {
      \ 'text': '✘',
      \ 'texthl': 'ErrorMsg',
      \ }


" Highlight warning
highlight WarningMsg ctermfg=167 guifg=#E9C864

" npm install -g eslint
autocmd BufWritePost *.js Neomake eslint
