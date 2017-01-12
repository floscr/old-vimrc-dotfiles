call functions#plaintext()

" function! ToggleTodoListBrackets()
"   if getline('.')=~#'.*\[\s\]'
"     .s/.*\[\zs\s\ze\]/x/
"   else
"     .s/.*\[\zsx\ze\]/ /
"   endif
" endfunction
" command! ToggleTodoListBrackets call ToggleTodoListBrackets()
" nmap <buffer> <silent> <leader>x :call ToggleTodoListBrackets()<cr>

" nmap <buffer> <silent> <leader>L :LuckyLink<CR>

map <C-b> ysaW*.
map ]] <Plug>Markdown_MoveToNextHeader
map [[ <Plug>Markdown_MoveToPreviousHeader

let g:writersRoomIsEnabled = 0
function! WritersRoom()
  if g:writersRoomIsEnabled
    set background=dark
    colorscheme hybrid
    let g:writersRoomIsEnabled = 0
  else
    set background=light
    colorscheme PaperColor
    let g:writersRoomIsEnabled = 1
  endif
endfunction
command! WritersRoom call WritersRoom()
