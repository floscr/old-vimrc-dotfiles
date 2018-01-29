" Tabularize by characters with easy shortcuts
" Match // Tabularize Comments after commands not at beginning of line
map <Leader>/ :Tabularize /\s\zs\/\/<CR>
map <Leader>= :Tabularize /=<CR>
map <Leader>: :Tabularize /:\s\zs<CR>
map <Leader>, :Tabularize /,<CR>
map <Leader>" :Tabularize /\s\zs"<CR>
map <Leader># :Tabularize /\s\zs#<CR>
