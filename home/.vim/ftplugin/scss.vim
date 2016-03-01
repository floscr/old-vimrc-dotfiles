" Set - as part of the word in css
au! FileType css,scss setl iskeyword+=-

function! JumpToLastClassNames()
  silent! normal! /}^f.w
endfunction

function! JumpToNextClassNames()
  silent! normal! /{^f.w
endfunction

nmap <silent> <buffer> } :call JumpToLastClassNames()<CR>
nmap <silent> <buffer> { :call JumpToNextClassNames()<CR>
