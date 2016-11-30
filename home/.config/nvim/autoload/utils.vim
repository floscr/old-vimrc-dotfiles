" Redirect command output to a buffer
" Useage: :TabMessage highlight
function! BuffMessage(cmd)
  redir => message
  silent execute a:cmd
  redir END
  if empty(message)
    echoerr "no output"
  else
    " use "new" instead of "tabnew" below if you prefer split windows instead of tabs
    enew
    setlocal buftype=nofile bufhidden=wipe noswapfile nobuflisted nomodified
    silent put=message
  endif
endfunction
command! -nargs=+ -complete=command BuffMessage call BuffMessage(<q-args>)

" Get the syntax highlighting group under the cursor
" :call SynStack()
" http://stackoverflow.com/a/9464929
function! SynStack()
  if !exists("*synstack")
    return
  endif
  echo map(synstack(line('.'), col('.')), 'synIDattr(v:val, "name")')
endfunc

function! LuckyLink()
  let wordUnderCursor = expand("<cword>")
  let link = system('echo $(googler ' . wordUnderCursor . ' -n 1 --nocolor --np | grep http)')
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

" Clear messages list
" http://stackoverflow.com/a/36777563/2298462
command! ClearMessages for n in range(200) | echom "" | endfor

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

" Open curent buffer with marked.app
function! OpenWithMarkedApp()
  silent! execute '!open "' . bufname("%") . '" -a "Marked 2"'
endfunction
command! Marked call OpenWithMarkedApp()

" Stop jump by paragraph ({}) poluting the jumplist
function! s:KeepJumpsParagraphMotion(forward, count, visual)
    execute 'keepjumps normal! ' . (a:visual ? 'gv' : '') . a:count . (a:forward ? '}' : '{')
endfunction

" Profile neovim and save results to profile.log
function! g:utils#profile() abort
  execute 'profile start profile.log'
  execute 'profile func *'
  execute 'profile file *'
  echom 'Profiling started (will last until you quit neovim).'
endfunction

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

" Be aware of whether you are right or left vertical split
" so you can use arrows more naturally.
" Inspired by https://github.com/ethagnawl.
function! g:utils#intelligentVerticalResize(direction) abort
  let l:window_resize_count = 5
  let l:current_window_is_last_window = (winnr() == winnr('$'))

  if (a:direction ==# 'left')
    let [l:modifier_1, l:modifier_2] = ['+', '-']
  else
    let [l:modifier_1, l:modifier_2] = ['-', '+']
  endif

  let l:modifier = l:current_window_is_last_window ? l:modifier_1 : l:modifier_2
  let l:command = 'vertical resize ' . l:modifier . l:window_resize_count . '<CR>'
  execute l:command
endfunction

" Run current file
function! g:utils#runCurrentFile() abort
  if &filetype ==? 'ruby'
    let l:command = 'ruby %'
  elseif &filetype ==? 'sh'
    let l:command = 'sh %'
  else
    echom "Can't run current file (unsupported filetype: " . &filetype . ')'
  endif

  if exists('command')
    execute ':terminal ' . l:command
  endif
endfunction

" Run NERDTreeFind or Toggle based on current buffer
function! g:utils#nerdWrapper() abort
  if &filetype ==# '' " Empty buffer
    :NERDTreeToggle
  elseif expand('%:t') =~? 'NERD_tree' " In NERD_tree buffer
    :q
  else " Normal file buffer
    :NERDTreeFind
  endif
endfunction

" Strip trailing spaces
function! g:utils#stripTrailingWhitespaces() abort
  " Preparation: save last search, and cursor position.

  " Exclude Markdown
  if &ft =~ 'markdown\|md'
    return
  endif

  let l:lastSearch = @/
  let l:line = line('.')
  let l:col = col('.')

  " Do the business:
  execute '%s/\s\+$//e'

  " Clean up: restore previous search history, and cursor position
  let @/ = l:lastSearch
  call cursor(l:line, l:col)
endfunction

let s:html_tags = ['!DOCTYPE','a','abbr','acronym','address','applet','area','article','aside','audio','b','base','basefont','bdi','bdo','big','blockquote','body','br','button','canvas','caption','center','cite','code','col','colgroup','datalist','dd','del','details','dfn','dialog','dir','div','dl','dt','em','embed','fieldset','figcaption','figure','font','footer','form','frame','frameset','h1','head','header','hr','html','i','iframe','img','input','ins','kbd','keygen','label','legend','li','link','main','map','mark','menu','menuitem','meta','meter','nav','noframes','noscript','object','ol','optgroup','option','output','p','param','pre','progress','q','rp','rt','ruby','s','samp','script','section','select','small','source','span','strike','strong','style','sub','summary','sup','table','tbody','td','textarea','tfoot','th','thead','time','title','tr','track','tt','u','ul','var','video','wbr']

function! s:realtag()
  return matchstr(getline('.')[:col('.')], '\(\S\+\)$')
endfunction

function! IsEmmetExpandable()
  if !emmet#isExpandable() | return 0 | endif
  if emmet#getFileType() == 'html' && index(s:html_tags, s:realtag()) == -1
    return 0
  endif
  return 1
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

" Manual Tag complete
function! g:utils#manualTagComplete() abort
  if pumvisible()
    return g:deoplete#mappings#close_popup()
  else
    return g:deoplete#mappings#manual_complete('tag')
  endif
endfunction

" Simple notes management
function! g:utils#openNotes() abort
  execute ':e ~/dev/notes/vim-notes.md'
endfunction

" Use omni complete source as default
function! g:utils#useOmniTabWrapper() abort
  inoremap <buffer> <expr> <TAB> utils#insertTabOmniWrapper()
endfunction

" Unite commands wrappers
function! g:utils#uniteSources() abort
  execute 'Unite -no-split -buffer-name=sources -start-insert source'
endfunction

function! g:utils#uniteMRUs() abort
  execute 'Unite -no-split -buffer-name=most-recently-used -start-insert neomru/file'
endfunction

function! g:utils#uniteFileBrowse() abort
  execute 'Unite -no-split -buffer-name=project-files -start-insert file'
endfunction

function! g:utils#uniteFileRec() abort
  execute 'Unite -no-split -buffer-name=file-recursive-search -start-insert file_rec/neovim'
endfunction

function! g:utils#uniteBuffers() abort
  execute 'Unite -no-split -buffer-name=buffers -start-insert buffer'
endfunction

function! g:utils#uniteOutline() abort
  execute 'Unite -no-split -buffer-name=symbols -start-insert outline'
endfunction

function! g:utils#uniteTags() abort
  execute 'Unite -no-split -buffer-name=tags -start-insert tag'
endfunction

function! g:utils#uniteHistory() abort
  execute 'Unite -no-split -buffer-name=edit-history change'
endfunction

function! g:utils#uniteLineSearch() abort
  execute 'Unite -no-split -buffer-name=line-search -start-insert line'
endfunction

function! g:utils#uniteYankHistory() abort
  execute 'Unite -no-split -buffer-name=yank-history history/yank'
endfunction

function! g:utils#uniteRegisters() abort
  execute 'Unite -no-split -buffer-name=registers register'
endfunction

function! g:utils#uniteWindows() abort
  execute 'Unite -no-split -buffer-name=splits window'
endfunction

function! g:utils#uniteSnippets() abort
  execute 'Unite -no-split -buffer-name=snippets -start-insert ultisnips'
endfunction

function! g:utils#uniteCustomMenu() abort
  execute 'Unite -no-split -buffer-name=menu -start-insert menu'
endfunction

function! g:utils#uniteJumps() abort
  execute 'Unite -no-split -buffer-name=jumps -start-insert jump'
endfunction

function! g:utils#uniteCommands() abort
  execute 'Unite -no-split -buffer-name=commands -start-insert command'
endfunction

function! g:utils#uniteMappings() abort
  execute 'Unite -no-split -buffer-name=mappings -start-insert mapping'
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

" Annotate file function (only ruby support for now)
" Needs: `gem install seeing_is_believing`
function! g:utils#annotateFile() abort
  let l:command_prefix = '%!'

  if &filetype ==? 'ruby'
    let l:command = 'seeing_is_believing -x'
  endif

  if exists('l:command')
    execute l:command_prefix . l:command
  endif
endfunction

" Mode function for Lightline statusline
function! g:utils#lightLineMode() abort
  let l:fname = expand('%:t')
  return l:fname =~? 'NERD_tree' ? 'NT' :
        \ &filetype ==? 'unite' ? 'Unite' :
        \ winwidth(0) > 70 ? g:lightline#mode() : ''
endfunction

" File format function for Lightline statusline
function! g:utils#lightLineFileformat() abort
  return winwidth(0) > 70 ? &fileformat : ''
endfunction

" Filetype function for Lightline statusline
function! g:utils#lightLineFiletype() abort
  return winwidth(0) > 70 ? (strlen(&filetype) ? &filetype : 'no ft') : ''
endfunction

" File encoding function for Lightline statusline
function! g:utils#lightLineFileencoding() abort
  return winwidth(0) > 70 ? (strlen(&fileencoding) ? &fileencoding : &encoding) : ''
endfunction

" File name function for Lightline statusline
function! g:utils#lightLineFilename() abort
  let l:fname = expand('%:t')
  return l:fname =~? 'NERD_tree' ? 'NERDTree' :
        \ &filetype ==? 'unite' ? g:unite#get_status_string() :
        \ ('' !=# l:fname ? l:fname : '[No Name]')
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

