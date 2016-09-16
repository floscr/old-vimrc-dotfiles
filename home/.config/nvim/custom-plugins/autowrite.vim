" Removes trailing spaces
function! TrimWhiteSpace()
  " Don't strip whitespace in markdown documents
  if &ft =~ 'markdown\|md'
    return
  endif
  %s/\s\+$//e
endfunction

autocmd FileWritePre    * :call TrimWhiteSpace()
autocmd FileAppendPre   * :call TrimWhiteSpace()
autocmd FilterWritePre  * :call TrimWhiteSpace()
autocmd BufWritePre     * :call TrimWhiteSpace()
