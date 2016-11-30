" Only load gitgutter when the file is loaded/saved
" This increases the speed in tab switching
let g:gitgutter_max_signs=9999
let g:gitgutter_eager = 1
let g:gitgutter_realtime = 0

" Disable default Keyboard mappings
let g:gitgutter_map_keys = 0

" Next/Prev Git Hunk and center
nnoremap <silent> [c <Esc>:GitGutterPrevHunk<CR>zMzvzz
nnoremap <silent> ]c <Esc>:GitGutterNextHunk<CR>zMzvzz
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

