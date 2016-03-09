macmenu File.Print key=<nop>

" -----------------------------------------------------------
" SUBLIME LIKE COMMANDS
" Because they're sometimes way less work
" I'm always saving to register t when yanking temporary text
" -----------------------------------------------------------

" Duplicate line below
noremap <silent> <D-d> "tyy"tpk
vnoremap <silent> <D-d> "ty"tpgv
inoremap <silent> <D-d> <Esc>"tyy"tpkA

" Duplicate line above
noremap <silent> <D-D> "tyy"tPj
vnoremap <silent> <D-D> "ty"tPgv
inoremap <silent> <D-D> <Esc>"tyy"tPjA

" Insert Newline above
map <D-Enter> :call append(line('.'), '')<CR>
imap <D-Enter> <Esc>:call append(line('.'), '')<CR>a
vmap <D-Enter> <Esc>O<Esc>gv

" Insert Newline above
map <D-S-Enter> :call append(line('.')-1, '')<CR>
imap <D-S-Enter> <Esc>:call append(line('.')-1, '')<CR>a
vmap <D-S-Enter> <Esc>O<Esc>gv

