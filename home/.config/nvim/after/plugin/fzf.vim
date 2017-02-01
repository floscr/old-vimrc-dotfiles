let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit' }

" Check if the current file is inside git root
function! s:find_git_root()
  if system('git rev-parse --show-toplevel 2> /dev/null') != ''
    " Show all git indexed files plus new none staged files
    " Exclude image formats from the search
    " --exclude or -x dont seem to work
    " Direct file name would not work either very well (png would exclude
    " json...)
    " so we're down to grep regex...
    return 'GFiles -o --exclude-standard -c | grep -ov "\.jpg\|\.png\|\.gif$"'
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

" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep('git grep --line-number '.shellescape(<q-args>), 0, <bang>0)

" Similarly, we can apply it to fzf#vim#grep. To use ripgrep instead of ag:
command! -bang -nargs=* Rg
  \ call fzf#vim#grep(
  \   ' rg -uu --column --line-number --no-heading --color=always '.shellescape(<q-args>), 1,
  \   <bang>0 ? fzf#vim#with_preview('up:60%')
  \           : fzf#vim#with_preview('right:50%:hidden', '?'),
  \   <bang>0)

" Augmenting Ag command using fzf#vim#with_preview function
"   * fzf#vim#with_preview([[options], preview window, [toggle keys...]])
"   * Preview script requires Ruby
"   * Install Highlight or CodeRay to enable syntax highlighting
"
"   :Ag  - Start fzf with hidden preview window that can be enabled with "?" key
"   :Ag! - Start fzf in fullscreen and display the preview window above
autocmd VimEnter * command! -bang -nargs=* Ag
  \ call fzf#vim#ag(<q-args>,
  \                 <bang>0 ? fzf#vim#with_preview('up:60%')
  \                         : fzf#vim#with_preview('right:50%:hidden', '?'),
  \                 <bang>0)
