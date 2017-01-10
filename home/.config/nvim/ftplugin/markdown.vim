" function! ToggleTodoListBrackets()
"   if getline('.')=~#'.*\[\s\]'
"     .s/.*\[\zs\s\ze\]/x/
"   else
"     .s/.*\[\zsx\ze\]/ /
"   endif
" endfunction
" command! ToggleTodoListBrackets call ToggleTodoListBrackets()
" nmap <buffer> <silent> <leader>x :call ToggleTodoListBrackets()<cr>

" nmap <buffer> <silent> <leader>L :LuckyLink<CR>

" au BufRead,BufNewFile *.md,*.txt syntax match StrikeoutMatch /^.*\[x\].*$/
" hi def  StrikeoutColor guifg=#2E3C47
" hi link StrikeoutMatch StrikeoutColor
"   set background=light
"   colorscheme pencil
" endfunction
" command! WritersRoom call WritersRoom()

" let g:writersRoomIsEnabled = 0
" function! WritersRoom()
"   if g:writersRoomIsEnabled
"     source $MYVIMRC
"     execute Goyo
"     let g:writersRoomIsEnabled = 0
"   else
"     set background=light
"     call goyo#execute()
"     highlight Cursor guibg=#4DBCFB
"     let g:writersRoomIsEnabled = 1
"   endif
" endfunction
" command! WritersRoom call WritersRoom()

