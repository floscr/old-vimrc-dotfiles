call functions#plaintext()

" Set word bold
map <buffer> <C-b> ysaW*.

" Markdown Header Movements
map <buffer> ]] <Plug>Markdown_MoveToNextHeader
map <buffer> [[ <Plug>Markdown_MoveToPreviousHeader

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
