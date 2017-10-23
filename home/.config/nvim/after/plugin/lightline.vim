let g:lightline = {
      \ 'colorscheme': 'hybrid',
      \ 'active': {
      \ 'left': [
      \  [ 'mode' ],
      \  [ 'fugitive' ],
      \  [ 'filename' ],
      \  [ 'ale' ],
      \ ]
      \ },
      \ 'component_function': {
      \ 'fugitive': 'LightlineFugitive',
      \ 'filename': 'LightlineFilename',
      \ 'fileformat': 'LightlineFileformat',
      \ 'filetype': 'LightlineFiletype',
      \ 'fileencoding': 'LightlineFileencoding',
      \ 'mode': 'LightlineMode',
      \ },
      \ 'component': {
      \ 'fugitive': '%{exists("*fugitive#head")?fugitive#head():""}'
      \ },
      \   'component_expand': {
      \     'ale': 'ALEGetStatusLine'
      \   },
      \   'component_type': {
      \     'ale': 'error'
      \   },
      \ 'component_visible_condition': {
      \ 'fugitive': '(exists("*fugitive#head") && ""!=fugitive#head())'
      \ }
      \ }

let g:lightline.mode_map = {
      \ 'n' : 'N',
      \ 'i' : 'I',
      \ 'R' : 'R',
      \ 'v' : 'V',
      \ 'V' : 'V',
      \ "\<C-v>": 'VB',
      \ 'c' : 'C',
      \ 's' : 'S',
      \ 'S' : 'S',
      \ "\<C-s>": 'SB',
      \ 't': 'T',
      \ }

augroup LightLineOnALE
  autocmd!
  autocmd User ALELint call lightline#update()
augroup END

function! LightlineModified()
  return &ft =~ 'help' ? '' : &modified ? '+' : &modifiable ? '' : '-'
endfunction

function! LightlineReadonly()
  return &ft !~? 'help' && &readonly ? 'RO' : ''
endfunction

function! LightlineFilename()
  let fname = expand('%:t')
  return fname == 'ControlP' ? g:lightline.ctrlp_item :
        \ fname =~ 'NERD_tree' ? '' :
        \ ('' != LightlineReadonly() ? LightlineReadonly() . ' ' : '') .
        \ ('' != fname ? fname : '[No Name]') .
        \ ('' != LightlineModified() ? ' ' . LightlineModified() : '')
endfunction

function! LightlineFugitive()
  try
    if expand('%:t') !~? 'NERD' && exists('*fugitive#head')
      let mark = '!'
      let _ = fugitive#head()
      return strlen(_) ? _.mark : ''
    endif
  catch
  endtry
  return ''
endfunction

function! LightlineFileformat()
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

function! LightlineFiletype()
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

function! LightlineFileencoding()
  return winwidth(0) > 70 ? (strlen(&fenc) ? &fenc : &enc) : ''
endfunction

function! LightlineMode()
  let fname = expand('%:t')
  return fname == 'ControlP' ? 'CtrlP' :
        \ fname =~ 'NERD_tree' ? 'NERDTree' :
        \ winwidth(0) > 60 ? lightline#mode() : ''
endfunction

let g:ctrlp_status_func = {
      \ 'main': 'LightlineCtrlPStatusFunc_1',
      \ 'prog': 'LightlineCtrlPStatusFunc_2',
      \ }

function! LightlineCtrlPStatusFunc_1(focus, byfname, regex, prev, item, next, marked)
  let g:lightline.ctrlp_regex = a:regex
  let g:lightline.ctrlp_prev = a:prev
  let g:lightline.ctrlp_item = a:item
  let g:lightline.ctrlp_next = a:next
  return lightline#statusline(0)
endfunction

function! LightlineCtrlPStatusFunc_2(str)
  return lightline#statusline(0)
endfunction
