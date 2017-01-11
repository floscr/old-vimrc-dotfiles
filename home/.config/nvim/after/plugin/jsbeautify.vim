" Fix js files using eslint
function! JsEslintFix ()
  let localEslintExecutable = system("echo $(git rev-parse --show-toplevel)/node_modules/eslint/bin/eslint.js")

  " Remove new lines from command
  let localEslintExecutable = substitute(localEslintExecutable, '\n', '', '')

  " When a local eslint executable is installed, use that one
  " Otherwise use global eslint
  let eslint = filereadable(localEslintExecutable) ? localEslintExecutable : 'eslint'

  execute('!' . eslint . ' --fix %')

  echo 'Javascript cleaned up!'
endfunc
command! JsEslintFix call JsEslintFix()

" Convert file to ES6
function! JsPrettier()
  execute('!prettier --write % --single-quote --trailing-comma')
  execute('edit')
  echo 'Prettyfied JS!'
endfunction
command! JsPrettier call JsPrettier()

" Convert file to ES6
function! JsLebab()
  execute('!lebab --replace % --transform template')
  execute('!lebab --replace % --transform let')
  execute('!lebab --replace % --transform arrow')
  execute('edit')
  echo 'Javascript transformed to ES6!'
endfunction
command! JsLebab call JsLebab()

autocmd FileType javascript,vue noremap <buffer>  <c-f> :call JsEslintFix()<cr>
autocmd FileType json noremap <buffer> <c-f> :call JsonBeautify()<cr>
autocmd FileType html,twig,blade noremap <buffer> <c-f> :call HtmlBeautify()<cr>
autocmd FileType css,scss,sass noremap <buffer> <c-f> :call CSSBeautify()<cr>

" Visual Range Beautify
autocmd FileType javascript vnoremap <buffer>  <c-f> :call RangeJsBeautify()<cr>
autocmd FileType json vnoremap <buffer> <c-f> :call RangeJsonBeautify()<cr>
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
