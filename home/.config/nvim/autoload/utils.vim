" -----------------------------------------------------------------------------
" Logging
" -----------------------------------------------------------------------------

" Get the syntax highlighting group under the cursor
" :call SynStack()
" http://stackoverflow.com/a/9464929
function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

" Clear messages list
" http://stackoverflow.com/a/36777563/2298462
command! ClearMessages for n in range(200) | echom "" | endfor

" -----------------------------------------------------------------------------
" Keyboard Trigger Enhancements
" -----------------------------------------------------------------------------

" Stop jump by paragraph ({}) poluting the jumplist
function! s:KeepJumpsParagraphMotion(forward, count, visual)
    execute 'keepjumps normal! ' . (a:visual ? 'gv' : '') . a:count . (a:forward ? '}' : '{')
endfunction

" Fix Linting Error Locationlist jumping when there is only one error
" Has the nice side efect of being able to loop through errorlist
function! LocationPrevious()
  try
    lprev
  catch /^Vim\%((\a\+)\)\=:E553/
    llast
  endtry
endfunction
command! LocationPrevious call LocationPrevious()

function! LocationNext()
  try
    lnext
  catch /^Vim\%((\a\+)\)\=:E553/
    lfirst
  endtry
endfunctio
command! LocationNext call LocationNext()

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

" -----------------------------------------------------------------------------
" External Commands
" -----------------------------------------------------------------------------

" Redirect output of external command to a new empty buffer
" http://vim.wikia.com/wiki/Display_output_of_shell_commands_in_new_window
command! -complete=shellcmd -nargs=+ Shell call s:RunShellCommand(<q-args>)
function! s:RunShellCommand(cmdline)
  let isfirst = 1
  let words = []
  for word in split(a:cmdline)
    if isfirst
      let isfirst = 0  " don't change first word (shell command)
    else
      if word[0] =~ '\v[%#<]'
        let word = expand(word)
      endif
      let word = shellescape(word, 1)
    endif
    call add(words, word)
  endfor
  let expanded_cmdline = join(words)
  botright new
  setlocal buftype=nofile bufhidden=wipe nobuflisted noswapfile nowrap
  call setline(1, 'You entered:  ' . a:cmdline)
  call setline(2, 'Expanded to:  ' . expanded_cmdline)
  call append(line('$'), substitute(getline(2), '.', '=', 'g'))
  silent execute '$read !'. expanded_cmdline
  1
endfunction

" -----------------------------------------------------------------------------
" LuckyLink
" -----------------------------------------------------------------------------

function! LuckyLink()
  let wordUnderCursor = expand("<cword>")
  let link = system('echo $(googler ' . wordUnderCursor . ' --nocolor --np | grep http)')
  if &filetype == 'markdown'
    let save_cursor = getcurpos()
    execute 'norm j'
    let pattern = '\[' . wordUnderCursor . '\]\?[:]\s*$'
    let search_result = search(pattern)

    if empty(search_result)
      execute 'norm G'
      put='['. wordUnderCursor .']: ' . link
    else
      put=''.link
      execute 'norm kJ'
    endif

    call setpos('.', save_cursor)
  else
    put=''.link
  endif
endfunction
command! LuckyLink call LuckyLink()

" -----------------------------------------------------------------------------
" Git
" -----------------------------------------------------------------------------

" Async git push, Fugitive Gpush doesnt work with neovim without dispatch...
if (has('nvim'))
  " Async push / pull
  function! s:Push()
    function! s:JobHandler(job_id, data, event)
      echo "Push done!"
    endfunction
    let s:callbacks = {
    \ 'on_stdout': function('s:JobHandler'),
    \ 'on_stderr': function('s:JobHandler'),
    \ 'on_exit': function('s:JobHandler')
    \ }
    call jobstart(split(&shell) + split(&shellcmdflag) + ['{git push}'], s:callbacks)
  endfunction
  command! Push :call s:Push()
endif

" -----------------------------------------------------------------------------
" OSX Commands
" -----------------------------------------------------------------------------

" Open curent buffer with marked.app
function! OpenWithMarkedApp()
  silent! execute '!open "' . bufname("%") . '" -a "Marked 2"'
endfunction
command! Marked call OpenWithMarkedApp()

" -----------------------------------------------------------------------------
" Tab Completion & Emmet
" -----------------------------------------------------------------------------

" Allowed html tags to expand on tab
" let s:html_tags = ['!DOCTYPE','a','abbr','acronym','address','applet','area','article','aside','audio','b','base','basefont','bdi','bdo','big','blockquote','body','br','button','canvas','caption','center','cite','code','col','colgroup','datalist','dd','del','details','dfn','dialog','dir','div','dl','dt','em','embed','fieldset','figcaption','figure','font','footer','form','frame','frameset','h1','head','header','hr','html','i','iframe','img','input','ins','kbd','keygen','label','legend','li','link','main','map','mark','menu','menuitem','meta','meter','nav','noframes','noscript','object','ol','optgroup','option','output','p','param','pre','progress','q','rp','rt','ruby','s','samp','script','section','select','small','source','span','strike','strong','style','sub','summary','sup','table','tbody','td','textarea','tfoot','th','thead','time','title','tr','track','tt','u','ul','var','video','wbr']

let s:html_tags = ['a','div','b','br','i','abbr', 'h1', 'h2', 'h3', 'h4', 'h5', 'h6']

function! IsEmmetExpandableTag()
  let expr = matchstr(getline('.')[:col('.')], '\(\S\+\)$')
  return expr =~ '[.#>+*]' || index(s:html_tags, expr) >= 0
endfunction

function! IsEmmetExpandable()
  if emmet#getFileType() =~ 'css\|scss'
    if emmet#isExpandable() | return 1 | endif
  endif

  " if emmet#getFileType() == 'html'
  "   let expr = matchstr(getline('.')[:col('.')], '\(\S\+\)$')
  "   return expr =~ '[.#>+*]' || index(s:html_tags, expr) >= 0
  " endif

  return 0
endfunction

" Tab wrapper
function! g:utils#tabComplete()
  let l:col = col('.') - 1

  if pumvisible()
    return "\<C-n>"

  " In html or css expand emmet abbreviation
  elseif &filetype =~ 'html\|css' && IsEmmetExpandable()
    return "\<plug>(emmet-expand-abbr)"

  else
    if !l:col || getline('.')[l:col - 1] !~# '\k'
      return "\<TAB>"
    else
      return "\<C-n>"
    endif

  endif

endfunction

" -----------------------------------------------------------------------------
" Indentation
" -----------------------------------------------------------------------------

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

" Strip trailing spaces
function! g:utils#stripTrailingWhitespaces() abort
  " Preparation: save last search, and cursor position.
  let l:lastSearch = @/
  let l:line = line('.')
  let l:col = col('.')

  " Do the business:
  execute '%s/\s\+$//e'

  " Clean up: restore previous search history, and cursor position
  let @/ = l:lastSearch
  call cursor(l:line, l:col)
endfunction
