let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

" Check if the current file is inside git root
function! s:find_git_root()
  if system('git rev-parse --show-toplevel 2> /dev/null') != ''
    " Show all git indexed files plus new none staged files
    " Exclude image formats from the search
    return 'GFiles -o --exclude-standard -c --exclude=\*.{jpg,png,gif,psd}'
  endif
  return 'Files'
endfunction
command! ProjectFiles execute s:find_git_root()

" Search either current Files by Git index or just file system
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

" Exit FZF by pressing escape
autocmd! FileType fzf tnoremap <buffer> <esc> <C-c>

" AG in git root
function! s:with_git_root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  return v:shell_error ? {} : {'dir': root}
endfunction

command! -nargs=* Rag
  \ call fzf#vim#ag(<q-args>, extend(s:with_git_root(), g:fzf#vim#default_layout))