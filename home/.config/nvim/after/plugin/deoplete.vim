inoremap <expr> <Tab> pumvisible() ? "\<C-n>" : "\<Tab>"
inoremap <expr> <S-Tab> pumvisible() ? "\<C-p>" : "\<S-Tab>"
inoremap <expr> <cr> pumvisible() ? "\<C-y>" : "\<cr>"

function! Multiple_cursors_before()
  let b:deoplete_disable_auto_complete=2
endfunction
function! Multiple_cursors_after()
  let b:deoplete_disable_auto_complete=0
endfunction

let g:deoplete#file#enable_buffer_path=1

call deoplete#custom#buffer_option({
      \ 'auto_complete_delay' : 10,
      \ 'enable_buffer_path': 1,
      \ 'smart_case'          : 1,
      \ })

" call deoplete#custom#source('_', 'matchers', ['matcher_full_fuzzy'])
" call deoplete#custom#source('ultisnips', 'rank', 630)
" call deoplete#custom#source('buffer', 'mark', 'B')
" call deoplete#custom#source('around',       'mark', 'A')

" Fix flickering
" https://github.com/Shougo/deoplete.nvim/issues/726
" call deoplete#custom#option('num_processes', 5)
" call deoplete#custom#option('refresh_always', 1)
