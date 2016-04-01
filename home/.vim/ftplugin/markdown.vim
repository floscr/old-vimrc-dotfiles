function! ToggleTodoListBrackets()
  if getline('.')=~#'.*\[\s\]'
    .s/.*\[\zs\s\ze\]/x/
  else
    .s/.*\[\zsx\ze\]/ /
  endif
endfunction
command! ToggleTodoListBrackets call ToggleTodoListBrackets()
