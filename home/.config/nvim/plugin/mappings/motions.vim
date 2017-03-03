" Wrap lighten around a word
autocmd FileType scss,vue,html let b:surround_{char2nr('e')} = "lighten(\r, 10%)"

" Expand console log with variable to message
autocmd FileType javascript,vue,html let b:surround_{char2nr('Q')} = "`${\r}`"
