function! MakeHeader()

  if &filetype == "vim"
    " Special short header for vim files
    " Yank current line into t register
    silent! normal "tyy
    " Replace pasted comment letters with -
    silent! normal "tpwv$r-
    " Paste above comment line
    silent! normal "tyykP
    " Go back to comment word
    silent! normal k^w
  else
    " Paste our default header everywhere else
    silent! normal o//-----------------------------------------------------------------------------
    silent! normal kO//-----------------------------------------------------------------------------
    silent! normal k^w
  endif

endfunction

command! MakeHeader call MakeHeader()

nnoremap <leader>mh :MakeHeader<cr>
