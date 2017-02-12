" Wrap lighten around a word
autocmd FileType scss,vue,html let b:surround_{char2nr('e')} = "lighten(\r, 10%)"
