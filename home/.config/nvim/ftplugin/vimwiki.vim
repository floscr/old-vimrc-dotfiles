let s:agOptions = {
      \ 'dir': '~/Dropbox/VimWiki',
      \ 'down': '~40%',
      \ }

command! -bang -nargs=* VimWikiAg call fzf#vim#ag(<q-args>, '', s:agOptions)
nnoremap <buffer><silent><c-p> :VimWikiAg<CR>
