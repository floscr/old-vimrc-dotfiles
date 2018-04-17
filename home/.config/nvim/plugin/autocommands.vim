" FocusGained {{{1
" -----------------------------------------------------------------------------

" Auto Load Buffer Except for Command Line Buffers
function! s:checkTimeOnEditableBuffers()
  if expand('%') !=# '[Command Line]'
    silent! checktime
  endif
endfunction

augroup checktime_update
  au!
  au FocusGained,BufEnter,CursorHold * call s:checkTimeOnEditableBuffers()
augroup END

" FileTypes {{{1
" -----------------------------------------------------------------------------

" Activate htmljinja for twig files
autocmd BufRead,BufNewFile,BufReadPost *.twig set ft=htmljinja
autocmd BufRead,BufNewFile,BufReadPost *.njk set ft=htmljinja

" Set html5 syntax for vue files to fix broken indentation
" au BufRead,BufNewFile *.vue set filetype=html

autocmd FileType help setlocal nospell

" Fix ZSH filetype
au BufRead,BufNewFile *.zsh* set filetype=zsh

" rc files
au BufRead,BufNewFile .babelrc,.eslintrc set filetype=json

au BufNewFile,BufRead *.txt set filetype=markdown

" Config files
au BufRead,BufNewFile *.conf set filetype=conf

" Remove trailing whitespaces automatically before save
autocmd BufWritePre * call utils#stripTrailingWhitespaces()

" QuickFix {{{1
" -----------------------------------------------------------------------------

augroup qf
  autocmd!
  " Preview quickfix result
  autocmd FileType qf nnoremap <buffer> <Tab> <Enter><C-W>j

  " Remove quickfix from the buffer list
  autocmd FileType qf set nobuflisted

  " Make quickfix prettier
  autocmd BufRead * if &buftype == 'quickfix'
        \| setlocal colorcolumn=
        \| setlocal nolist
        \| setlocal number!
        \| endif

  " Restore enter for the quickfix window
  autocmd FileType qf nnoremap <buffer> <CR> <CR>
augroup END

" Suffixes {{{1
" -----------------------------------------------------------------------------

augroup suffixes
  autocmd!

  let associations = [
        \["javascript", ".js,.javascript,.es,.esx,.json"],
        \["html", ".vue,.js"],
        \]

  for ft in associations
    execute "autocmd FileType " . ft[0] . " setlocal suffixesadd=" . ft[1]
  endfor
augroup END

" Magit {{{1
" -----------------------------------------------------------------------------

" Magit
autocmd FileType magit nmap <buffer> <Tab> <C-n>
autocmd FileType magit nmap <buffer> <S-Tab> <C-p>
