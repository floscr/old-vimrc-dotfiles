let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

" Reverse to find if not in git root
function! s:find_git_root()
  if system('git rev-parse --show-toplevel 2> /dev/null') != ''
    return 'GitFiles'
  endif
  return 'Files'
endfunction
command! ProjectFiles execute s:find_git_root()

" Search in current git index
nnoremap <silent> <C-p> :ProjectFiles<CR>
" Search Recent Files
nnoremap <silent> <leader>h :History<CR>
" Search open buffers
nnoremap <silent> <leader>b :Buffers<CR>
" Search available commands
nnoremap <silent> <leader>c :Commands<CR>
" Search lines in all open buffers
nnoremap <silent> <leader>; :BLines<CR>
" Search lines in current file
nnoremap <silent> <leader>. :Lines<CR>
" Search commits
nnoremap <silent> <leader>gl :Commits<CR>
" Search commits for current file
nnoremap <silent> <leader>gL :BCommits<CR>

" Filename completion with fzf
imap <c-x><c-f> <plug>(fzf-complete-path)
