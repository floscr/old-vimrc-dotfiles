" Curly jumping in json

" Incsearch behaves weird with upwards search (?)
" This is a workaround, but it will error out when
" the top of the file has been reached
nnoremap <buffer> <silent> { /[{}\[\]]<CR>NN<esc> :let @/ = ""<CR>

nnoremap <buffer> <silent> } /[{}\[\]]<CR><esc> :let @/ = ""<CR>
