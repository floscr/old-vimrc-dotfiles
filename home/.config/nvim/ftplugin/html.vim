" Set - as part of the word in css
autocmd FileType html setl iskeyword+=-

"HTML Editing
setlocal matchpairs+=<:>

" Treat <li> and <p> tags like the block tags they are
let g:html_indent_tags = 'li\|p'
