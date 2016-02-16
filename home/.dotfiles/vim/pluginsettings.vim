" --------
" NERDTREE
" --------

function! ToggleNERDTreeFind()
  execute ':NERDTreeFind'
endfunction

 " function! ToggleNERDTreeFind()
 "    if g:NERDTree.IsOpen()
 "        execute ':NERDTreeClose'
 "    else
 "        execute ':NERDTreeFind'
 "    endif
 "  execute ':NERDTreeFind'
 " endfunction

nnoremap <silent> <leader>kb :NERDTreeToggle<CR>
nnoremap <silent> <leader>kn :call ToggleNERDTreeFind()<CR>
let NERDTreeIgnore = ['\.git$']

" ---
" FZF
" ---

" Open files in a split
let g:fzf_action = {
  \ 'ctrl-t': 'tab split',
  \ 'ctrl-x': 'split',
  \ 'ctrl-v': 'vsplit' }

" nnoremap <silent> <leader><space><space> :GitFiles<CR>
nnoremap <silent> <C-p> :GitFiles<CR>
nnoremap <silent> <leader>c :Commands<CR>
nnoremap <silent> <leader>a :Buffers<CR>
nnoremap <silent> <leader>; :BLines<CR>
nnoremap <silent> <leader>. :Lines<CR>
nnoremap <silent> <leader>? :History<CR>
nnoremap <silent> <leader>gl :Commits<CR>
nnoremap <silent> <leader>ga :BCommits<CR>

imap <C-x><C-f> <plug>(fzf-complete-file-ag)
imap <C-x><C-l> <plug>(fzf-complete-line)
"
" --------
" Fugitive
" --------

nnoremap <silent> <space>gf :Git add %:p<CR><CR>
nnoremap <silent> <space>ga :Git add .<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gd :Gdiff<CR>

" Show the previus version of a file
nnoremap <space>ge :Gedit<CR>
nnoremap <space>gw :Gwrite<CR><CR>
nnoremap <space>gl :silent! Glog<CR>:bot copen<CR>

nnoremap <space>gp :Ggrep<Space>
nnoremap <space>gm :Gmove<Space>
nnoremap <space>gb :Git branch<Space>
nnoremap <space>go :Git checkout<Space>
nnoremap <space>gps :Dispatch! git push<CR>
nnoremap <space>gpl :Dispatch! git pull<CR>

" --------
" SYTASTIC
" --------

let g:syntastic_javascript_checkers = ['eslint']

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 0

" -----
" EMMET
" -----

" Remap emmet leader key
let g:user_emmet_leader_key='<C-e>'
" imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

" -------------------------
" ULTISNIPS & YOUCOMPLETEME
" -------------------------

let g:UltiSnipsSnippetDirectories=["~/.vim/UltiSnips"]

" make YCM compatible with UltiSnips (using supertab)
let g:ycm_key_list_select_completion = ['<C-n>', '<Down>']
let g:ycm_key_list_previous_completion = ['<C-p>', '<Up>']
let g:SuperTabDefaultCompletionType = '<C-n>'

" better key bindings for UltiSnipsExpandTrigger
let g:UltiSnipsExpandTrigger = "<tab>"
let g:UltiSnipsJumpForwardTrigger = "<tab>"
let g:UltiSnipsJumpBackwardTrigger = "<s-tab>"

" ------
" CTRL-P
" ------

" " Skip grep files
" let Grep_Skip_Files='*.bak *~ *.pyc *.o *.obj'
" let Grep_Skip_Dirs='.bzr .git .hg node_modules bower_components'
" let g:ctrlp_show_hidden = 1 " Show hidden files

" " Much faster ctr-p loading by using the git repository index
" let g:ctrlp_use_caching = 0
" if executable('ag')
"   set grepprg=ag\ --nogroup\ --nocolor

"   let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
" else
"   let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
"   let g:ctrlp_prompt_mappings = {
"         \ 'AcceptSelection("e")': ['<space>', '<cr>', '<2-LeftMouse>'],
"         \ }
" endif

" --------
" FUGITIVE
" --------
set diffopt+=vertical " Always use vertical diffs

" ---------
" GITGUTTER
" ---------

" Only load gitgutter when the file is loaded/saved
" This increases the speed in tab switching
let g:gitgutter_max_signs=9999
let g:gitgutter_eager = 0
let g:gitgutter_realtime = 0

" Next/Prev Git Hunk
nmap <leader>hn <Plug>GitGutterNextHunk
nmap <leader>hp <Plug>GitGutterPrevHunk
nmap <leader>hb <Plug>GitGutterPrevHunk

" Add/Revert Hunks
nmap <Leader>ha <PlugtGitGutterStageHunk
nmap <Leader>hu <Plug>GitGutterRevertHunk

" ------------
" VIM-SURROUND
" ------------

" Surround text currently selected while in visual mode
" (The surrounded text is kept selected after being surround)
" Changed to shortcuts that work with a german keyboard
vmap <leader>2 S"lvi"
vmap <leader># S'lvi'
vmap <leader>´ S`lvi`
vmap <leader>8 S)lvi(
vmap <leader>7 S}lvi{
vmap <leader>9 S]lvi[
vmap <leader>< S>lvi<

" ---------
" LIMELIGHT
" ---------

let g:limelight_conceal_guifg = '#5F7080'

" --------------
" VIM-Commentary
" --------------

" Fix the commentary for *.vue
autocmd FileType vue setlocal commentstring=//\ %s

" -------
" AIRLINE
" -------
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" vim-airline
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
  let g:airline#extensions#tabline#enabled = 1
  let g:airline#extensions#tabline#fnamemod = ':t'
  let g:airline#extensions#tabline#show_tab_nr = 1
  let g:airline_left_sep = ''
  let g:airline_right_sep = ''
  let g:airline_theme='oceanicnext'
  let g:airline#extensions#tabline#left_sep = ' '
  let g:airline#extensions#tabline#left_alt_sep = '|'

  nmap <leader>x :bp <BAR> bd #<CR>
" This replaes :tabnew which I used to bind to this mapping
  nmap <leader>n :enew<cr>
" Move to the next buffer
  nmap <leader>. :bnext<CR>
" Move to the previous buffer
  nmap <leader>, :bprevious<CR>
  nmap <leader>, :bprevious<CR>
  let g:airline#extensions#tabline#buffer_idx_mode = 1
  nmap <leader>1 <Plug>AirlineSelectTab1
  nmap <leader>2 <Plug>AirlineSelectTab2
  nmap <leader>3 <Plug>AirlineSelectTab3
  nmap <leader>4 <Plug>AirlineSelectTab4
  nmap <leader>5 <Plug>AirlineSelectTab5
  nmap <leader>6 <Plug>AirlineSelectTab6
  nmap <leader>7 <Plug>AirlineSelectTab7
  nmap <leader>8 <Plug>AirlineSelectTab8
  nmap <leader>9 <Plug>AirlineSelectTab9

let g:bufferline_echo = 0
