" Reload .vimrc
" When sourcing files, the last seach gets highlighted
" This mapping auto disables the highlight
nnoremap <silent> <leader>sv :source $MYVIMRC<CR><esc> :let @/ = ""<CR><esc>:echo "Vimrc reloaded!"<CR>

" Source current file
nmap <silent> <leader>sf :source %<CR><ESC>:echo "Current file sourced!"<CR>
" Source current file and Install plugins
nmap <silent> <leader>sp :source %<CR><ESC>:PlugInstall<CR>

" Color Picker.
nnoremap <leader>v :VCoolor<CR>

" Toggle spellcheck
nmap <silent> <leader>ss :set spell!<CR>

" Show current file in finder
nnoremap <leader><CR> :silent execute('!open ' . expand('%:p:h'))<CR>

nnoremap <silent> <leader>lt :ALEToggle<CR>

" Toggle the error list
" nmap <silent> <leader>l :call ToggleList("Location List", 'l')<CR>

" Fugitive
nmap <silent> <leader>gg :Gcd<CR>:GGrep<CR>
nmap <silent> <leader>gb :Gblame<CR>

" Search all lines in all open buffers
nmap <silent> <leader>L :FZFLines<CR>

" Splits
nnoremap <silent> <leader>\| :vsplit<CR>
nnoremap <silent> <leader>- :split<CR>

" Prompt for a command to run
map <Leader>xc :VimuxInterruptRunner<CR>:VimuxPromptCommand<CR>

" Rerun last Vimux command
map <Leader>xx :VimuxInterruptRunner<CR>:VimuxRunLastCommand<CR>

" {{{ Vimwiki

nmap <silent> <leader>we :e ~/Dropbox/VimWiki/Einkaufsliste.markdown<CR>
nmap <silent> <leader>wm :e ~/Dropbox/VimWiki/todo/Work.markdown<CR>
nmap <silent> <leader>wh :e ~/Dropbox/VimWiki/learn/Haskell\ Programming\ Ebook.markdown<CR>
nnoremap <silent> <Leader>wk :e ~/Dropbox/VimWiki/collections/cooking/index.markdown<CR>

" Vimwiki }}}
