" Next/Prev Git Hunk and center
nnoremap <silent> [c <Esc>:GitGutterPrevHunk<CR>
nnoremap <silent> ]c <Esc>:GitGutterNextHunk<CR>
" nmap ghn <Plug>GitGutterNextHunk
" nmap ghp <Plug>GitGutterPrevHunk

" Add/Revert Hunks
nmap gha <Plug>GitGutterStageHunk
nmap ghu <Plug>GitGutterUndoHunk

" Gutter Color Overrides
if g:colors_name == "hybrid"
  highlight GitGutterAdd guifg=#99C794
  highlight GitGutterDelete guifg=#CC6665
  highlight GitGutterChangeDelete guifg=#B294BB
  highlight vertsplit guifg=#2E3C47
endif

