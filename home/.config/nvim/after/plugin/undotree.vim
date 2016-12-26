nnoremap <silent> U :UndotreeToggle<Cr>

" Set the Undotree window layout
" Style 4
" +---+----------+
" |   | undotree |
" +---+----------+
" |       diff   |
" +--------------+
let g:undotree_WindowLayout = 4
let g:undotree_SetFocusWhenToggle = 1
let g:undotree_SplitWidth = 60

function! g:Undotree_CustomMap()
  nmap <buffer> k <Plug>UndotreeGoNextState
  nmap <buffer> j <Plug>UndotreeGoPreviousState
  nmap <buffer> <Esc> <Plug>UndotreeClose
endfunction
