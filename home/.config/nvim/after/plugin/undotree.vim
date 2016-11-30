nnoremap <silent> U :UndotreeToggle<Cr>

let g:undotree_WindowLayout = 4
let g:undotree_SetFocusWhenToggle = 1
let g:undotree_SplitWidth = 60

function! g:Undotree_CustomMap()
  nmap <buffer> k <plug>UndotreeGoNextState
  nmap <buffer> j <plug>UndotreeGoPreviousState
  nmap <buffer> <Esc> <plug>UndotreeClose
endfunction

