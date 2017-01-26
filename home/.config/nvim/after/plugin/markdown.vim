" Highlighting for tasks
hi def  MarkdownStrikeoutColor guifg=#2C3D48
hi link MarkdownStrikeoutMatch MarkdownStrikeoutColor
au BufRead,BufNewFile *.md,*.txt syntax match MarkdownStrikeoutMatch /^.*\[x\].*$/
