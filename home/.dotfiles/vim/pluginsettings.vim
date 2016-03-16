" -------------------------------------------
" CTRLP or FZF
" Depending if running macvim or terminal vim
" -------------------------------------------

if has("gui_running")
  " CTRLP
  let g:ctrlp_user_command = [
        \ '.git', 'cd %s && git ls-files . --others -co --exclude-standard',
        \ 'find %s -type f | egrep -v "(node_modules/|dist/|dst/|.git/|bower_components/)"'
        \ ]
  nmap <D-p> :CtrlP<cr>
  nmap <D-[> :CtrlPBuffer<cr>
  nmap <D-e> :CtrlPMRUFiles<cr>
  nnoremap <leader>. :CtrlPTag<cr>
  nmap <D-r> :CtrlPBufTag<cr>
else
  " FZF
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
  nnoremap <silent> <leader>h :History<CR>
  nnoremap <silent> <leader>gl :Commits<CR>
  nnoremap <silent> <leader>ga :BCommits<CR>

  imap <C-x><C-f> <plug>(fzf-complete-file-ag)
  imap <C-x><C-l> <plug>(fzf-complete-line)
endif

" --------
" Fugitive
" --------

set diffopt+=vertical " Always use vertical diffs

nnoremap <silent> <space>gf :Git add %:p<CR><CR>
nnoremap <silent> <space>ga :Git add .<CR><CR>
nnoremap <space>gs :Gstatus<CR>
nnoremap <space>gc :Gcommit -v -q<CR>
nnoremap <silent> <space>gC :Git add %:p<CR><CR>:Gcommit -v -q<CR>
nnoremap <space>gt :Gcommit -v -q %:p<CR>
nnoremap <space>gd :Gdiff<CR>

" Show the previus version of a file
nnoremap <space>ge :Gedit<CR>
nnoremap <space>gw :Gwrite<CR><CR>
nnoremap <space>gl :silent! Glog<CR>:bot copen<CR>

" Hard reset all changes
nnoremap <silent> <space>grh :silent! Git reset --hard<CR>

" Reset current file
nnoremap <silent> <space>grf :silent! Git checkout HEAD -- %<CR>

nnoremap <space>gp :Ggrep<Space>
nnoremap <space>gm :Gmove<Space>
nnoremap <space>gb :Git branch<Space>
nnoremap <space>go :Git checkout<Space>
nnoremap <space>gps :Dispatch! git push<CR>
nnoremap <space>gpl :Dispatch! git pull<CR>

" --------
" SYTASTIC
" --------

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_loc_list_height = 5
let g:syntastic_auto_loc_list = 0
let g:syntastic_check_on_open = 0
let g:syntastic_check_on_wq = 1

let g:syntastic_error_symbol = '!'
let g:syntastic_style_error_symbol = '⁉️'
let g:syntastic_warning_symbol = '~~'
let g:syntastic_style_warning_symbol = '*'

let g:syntastic_javascript_checkers = ['eslint']
" let b:syntastic_javascript_eslint_exec = StrTrim(system('npm-which eslint'))
" let g:syntastic_javascript_eslint_exec = 'eslint_d'

let g:syntastic_mode_map = { 'passive_filetypes': ['twig'] } " Ignore linting for twig

highlight link SyntasticErrorSign SignColumn
highlight link SyntasticWarningSign SignColumn
highlight link SyntasticStyleErrorSign SignColumn
highlight link SyntasticStyleWarningSign SignColumn

" -----
" EMMET
" -----

" Remap emmet leader key
let g:user_emmet_leader_key='<C-e>'
" let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.homesick/repos/dotfiles/home/.vim/emmet/emmet-snippets.json')), "\n"))
" imap <expr> <tab> emmet#expandAbbrIntelligent("\<tab>")

" -------------------------
" ULTISNIPS
" -------------------------

let g:UltiSnipsExpandTrigger="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsJumpBackwardTrigger="<s-c-j>"
let g:UltiSnipsSnippetDirectories=["UltiSnips"]

" Open the current filetype snippet file
function! EditFileTypeSnippet()
  execute 'edit ~/.homesick/repos/dotfiles/home/.vim/UltiSnips/' . &filetype . '.snippets'
  " if(&filetype == 'scss')
  " endif
endfunction
nnoremap <leader>es :call EditFileTypeSnippet()<CR>

" ---------
" GITGUTTER
" ---------

" Only load gitgutter when the file is loaded/saved
" This increases the speed in tab switching
let g:gitgutter_max_signs=9999
let g:gitgutter_eager = 0
let g:gitgutter_realtime = 0

" Next/Prev Git Hunk
nmap ghn <Plug>GitGutterNextHunk
nmap ghp <Plug>GitGutterPrevHunk

" Add/Revert Hunks
nmap <Leader>ha <Plug>GitGutterStageHunk
nmap <Leader>hu <Plug>GitGutterRevertHunk

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

let g:airline#extensions#tabline#enabled = 1
let g:airline#extensions#tabline#fnamemod = ':t'
let g:airline#extensions#tabline#show_tab_nr = 1
let g:airline_left_sep = ''
let g:airline_right_sep = ''
let g:airline_theme='oceanicnext'
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

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
