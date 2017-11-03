let s:agOptions = {
      \ 'dir': '~/Dropbox/Notes',
      \ 'down': '~40%',
      \ }

command! -bang -nargs=* VimWikiAg call fzf#vim#ag(<q-args>, '', s:agOptions)
nnoremap <buffer><silent><c-p> :VimWikiAg<CR>
