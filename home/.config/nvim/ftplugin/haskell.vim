au FileType haskell setlocal omnifunc=necoghc#omnifunc
let g:ghcmod_ghc_options = ['-Wall', '-Wincomplete-uni-patterns',
                            \'-Wincomplete-record-updates',
                            \'-Wmissing-import-lists']
