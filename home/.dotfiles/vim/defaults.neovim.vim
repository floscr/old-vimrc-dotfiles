" --------------------
" NEOVIM TERMINAL MODE
" --------------------

" Exit terminal insert mode
tnoremap <Leader>e <C-\><C-n>

" ---
" FZF
" ---
" Open files in a split
let g:fzf_action = {
			\ 'ctrl-t': 'tab split',
			\ 'ctrl-x': 'split',
			\ 'ctrl-v': 'vsplit' }

" nnoremap <silent> <leader><space><space> :GitFiles<CR>
nnoremap <silent> <C-p> :GitFiles<CR>
nnoremap <silent> <leader>c :Commands<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>. :Lines<CR>
nnoremap <silent> <leader>h :History<CR>
nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>

imap <C-x><C-f> <plug>(fzf-complete-file-ag)
imap <C-x><C-l> <plug>(fzf-complete-line)

