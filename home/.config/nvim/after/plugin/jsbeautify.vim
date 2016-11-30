autocmd FileType javascript,vue noremap <buffer>  <c-f> :call JsBeautify()<cr>
autocmd FileType json noremap <buffer> <c-f> :call JsonBeautify()<cr>
autocmd FileType jsx noremap <buffer> <c-f> :call JsxBeautify()<cr>
autocmd FileType html,twig noremap <buffer> <c-f> :call HtmlBeautify()<cr>
autocmd FileType css,scss,sass noremap <buffer> <c-f> :call CSSBeautify()<cr>

" Visual Range Beautify
autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
autocmd FileType json vnoremap <buffer> <c-f> :call RangeJsonBeautify()<cr>
autocmd FileType jsx vnoremap <buffer> <c-f> :call RangeJsxBeautify()<cr>
autocmd FileType html vnoremap <buffer> <c-f> :call RangeHtmlBeautify()<cr>
autocmd FileType css vnoremap <buffer> <c-f> :call RangeCSSBeautify()<cr>

" Hardcode the Beautifier config here
" ~/.editorconfig doesnt seem to be working
let g:config_Beautifier = {}
let g:config_Beautifier['js'] = {}
let g:config_Beautifier['js'].indent_size = '2'
let g:config_Beautifier['html'] = {}
let g:config_Beautifier['html'].indent_size = '2'
let g:config_Beautifier['json'] = {}
let g:config_Beautifier['json'].indent_size = '2'
let g:config_Beautifier['css'] = {}
let g:config_Beautifier['css'].indent_size = '2'

