" Strings are limegreen in hybrid theme so we match it to that Group
hi link MarkdownStrikeoutMatch String
" Highlighting for tasks
" hi def  MarkdownStrikeoutColor guifg=LightGreen
au BufRead,BufNewFile *.md,*.txt syntax match MarkdownStrikeoutMatch /^.*\[x\].*$/
