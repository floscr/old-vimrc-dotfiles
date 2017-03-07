" These visual enhancements make vim unbearably slow.
setlocal nocursorline
setlocal norelativenumber

" Fix syntax highlighting from stopping
au BufEnter *.vue syn sync fromstart
