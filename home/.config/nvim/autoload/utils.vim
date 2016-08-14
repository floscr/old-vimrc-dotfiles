" When cycling ignore NERDTree and Tagbar
function! g:utils#intelligentCycling() abort
  " Cycle firstly
  wincmd w
  " Handle where you are now
  if &filetype ==# 'nerdtree'
    call g:utils#intelligentCycling()
  endif
  " If in terminal buffer start insert
  if &buftype ==# 'terminal'
    startinsert!
  endif
endfunction

" Howdoi integration
" Needs: `pip install howdoi`
function! g:utils#howDoI() abort
  let l:command_prefix = 'read '
  let l:howdoi = '!howdoi '

  call inputsave()
  let l:query = input('How do I: ')
  call inputrestore()

  if l:query !=# ''
    execute l:command_prefix . l:howdoi . l:query
  endif
endfunction

" Run NERDTreeFind or Toggle based on current buffer
function! g:utils#nerdWrapper() abort
  if &filetype ==# '' " Empty buffer
    :NERDTreeToggle
  elseif expand('%:t') =~? 'NERD_tree' " In NERD_tree buffer
    wincmd w
  else " Normal file buffer
    :NERDTreeFind
  endif
endfunction

" Format function
" Needs: `npm install js-beautify`, `gem install ruby-beautify`, `python`
function! g:utils#formatFile() abort
  let l:line = line('.')
  let l:col = col('.')
  let l:command_prefix = '%!'

  if &filetype ==? 'javascript.jsx'
    let l:command = 'js-beautify -X -f -'
  elseif &filetype ==? 'html'
    let l:command = 'html-beautify -f -'
  elseif &filetype ==? 'css'
    let l:command = 'css-beautify -f -'
  elseif &filetype ==? 'json'
    let l:command = 'python -m json.tool'
  elseif &filetype ==? 'ruby'
    let l:command = 'ruby-beautify -c 2 -s'
  else
    " Basic vim format fallback
    normal! gg=G
  endif

  if exists('l:command')
    execute l:command_prefix . l:command
  endif

  " Return back to where cursor was
  call cursor(l:line, l:col)
endfunction

" Reset tabs to 4 spaces
function! g:utils#retabToFourSpaces() abort
  setlocal tabstop=4 shiftwidth=4 expandtab
  retab
endfunction

" Reset tabs to 2 spaces
function! g:utils#retabToTwoSpaces() abort
  setlocal tabstop=2 shiftwidth=2 expandtab
  retab
endfunction
