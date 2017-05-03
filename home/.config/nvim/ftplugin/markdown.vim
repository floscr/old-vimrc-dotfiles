call functions#plaintext()

" Disable double space matching
let g:lexima_enable_space_rules=0

" Set word bold
map <buffer> <C-b> ysaW*.

" Markdown Header Movements
map <buffer> ]] <Plug>Markdown_MoveToNextHeader
map <buffer> [[ <Plug>Markdown_MoveToPreviousHeader

nnoremap <buffer> <leader>p :MarkdownChromeTabLink<CR>
nnoremap <buffer> <leader>m :Marked<CR>

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
