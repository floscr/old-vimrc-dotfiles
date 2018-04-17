" Global {{{
" -----------------------------------------------------------------------------

let g:fzf_history_dir = '~/.local/share/fzf-history'
let g:fzf_action = {
      \ 'ctrl-t': 'tab split',
      \ 'ctrl-x': 'split',
      \ 'ctrl-v': 'vsplit',
      \ }

" }}}
" Theme {{{
" -----------------------------------------------------------------------------

" Fix the color mismatch for FZF buffers
hi FZFHighlight guibg=#171D20

let g:fzf_colors = {
      \ 'pointer': ['bg', 'Search'],
      \ 'bg':      ['bg', 'IncSearch', 'NonText'],
      \ 'fg+':     ['fg', 'IncSearch'],
      \ 'bg+':     ['bg', 'FZFHighlight'],
      \ 'header':  ['bg', 'Search'],
      \ 'marker':  ['fg', 'String'],
      \ }

" Add preview window via coderay
let g:fzf_files_options = '--preview "(coderay {} || cat {}) 2> /dev/null | head -'.&lines.'"'

" Fix colors for fzf
" autocmd FileType fzf hi! IncSearch term=bold ctermfg=Cyan guifg=#80a0ff gui=bold guibg=#ffffff

" }}}
" Ctrl-P {{{
" -----------------------------------------------------------------------------
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
nnoremap <silent> <C-p> :ProjectFiles<CR>

" }}}
" Shortcuts {{{
" -----------------------------------------------------------------------------

" Recent Files
nnoremap <silent> <leader>h :History<CR>
" open buffers
nnoremap <silent> <leader>b :Buffers<CR>
nnoremap <silent> <C-b> :Buffers<CR>
" available commands
nnoremap <silent> <leader>c :Commands<CR>
" lines in all open buffers
nnoremap <silent> <leader>; :BLines<CR>
" lines in current file
nnoremap <silent> <leader>. :Lines<CR>
" commits
nnoremap <silent> <leader>gl :Commits<CR>
" commits for current file
nnoremap <silent> <leader>gL :BCommits<CR>

" Exit FZF by pressing escape
autocmd! FileType fzf tnoremap <buffer> <esc> <C-c>

" }}}
" Commands {{{
" -----------------------------------------------------------------------------

" AG in git root
function! s:with_git_root()
  let root = systemlist('git rev-parse --show-toplevel')[0]
  return v:shell_error ? {} : {'dir': root}
endfunction

" Command for git grep
" - fzf#vim#grep(command, with_column, [options], [fullscreen])
command! -bang -nargs=* GGrep
  \ call fzf#vim#grep(
  \ 'git grep --line-number ' . shellescape(<q-args>), 1,
  \ fzf#vim#with_preview({'options': '--delimiter : --nth 3..'}, 'right:40%'),
  \ <bang>0)

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

autocmd VimEnter * command! -bang -nargs=* BCommits
  \ call fzf#vim#buffer_commits({
  \ 'down': '~80%',
  \ })

" }}}
" FZFLines: Search in all open buffer lines {{{
" -----------------------------------------------------------------------------

function! s:line_handler(l)
  let keys = split(a:l, ':\t')
  exec 'buf' keys[0]
  exec keys[1]
  normal! ^zz
endfunction

function! s:buffer_lines()
  let res = []
  for b in filter(range(1, bufnr('$')), 'buflisted(v:val)')
    call extend(res, map(getbufline(b,0,"$"), 'b . ":\t" . (v:key + 1) . ":\t" . v:val '))
  endfor
  return res
endfunction

command! FZFLines call fzf#run({
      \ 'source':  <sid>buffer_lines(),
      \ 'sink':    function('<sid>line_handler'),
      \ 'options': '--extended --nth=3..',
      \ 'down':    '60%',
      \ })

" }}}
