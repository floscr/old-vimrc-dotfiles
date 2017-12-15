let s:agOptions = {
      \ 'dir': '~/Dropbox/VimWiki',
      \ 'down': '~40%',
      \ }

command! -bang -nargs=* VimWikiAg call fzf#vim#ag(<q-args>, '', s:agOptions)
nnoremap <buffer><silent><c-p> :VimWikiAg<CR>

" Add the file name to the current header
nnoremap <silent> <Leader>gmh gg<ESC>:call append(line('.') - 1, '# ' . expand('%:r'))<CR>k^

" Remove unneeded vimwiki bindings {{{
nmap <Plug>NoVimwikiRemoveHeaderLevel <Plug>VimwikiRemoveHeaderLevel
" }}}
