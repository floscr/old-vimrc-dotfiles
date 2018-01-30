let g:haskellmode_completion_ghc = 0
let g:ghcmod_ghc_options = ['-Wall', '-Wincomplete-uni-patterns',
                            \'-Wincomplete-record-updates',
                            \'-Wmissing-import-lists']
au FileType haskell setlocal omnifunc=necoghc#omnifunc

map <Leader>: :Tabularize /::<CR>
