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
    silent! normal ^
    " Remove any comments on current line
    silent! .s/^\/\+\s//g
    " Paste our default header everywhere else
    silent! normal O/*--------------------------------------------------------*\
    silent! normal jo *--------------------------------------------------------*/
    silent! normal kI* l
  endif

endfunction

command! MakeHeader call MakeHeader()

nnoremap gmh :MakeHeader<cr>
