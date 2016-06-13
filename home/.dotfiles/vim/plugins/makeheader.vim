function! s:MakeHeader(type)

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

  elseif &filetype == "markdown"
    silent! normal yypVr=o

  elseif &filetype == "conf" || &filetype == "apache"
    silent! normal ^
    " Remove any comments on current line
    silent! .s/^\/\+\s//g
    " Paste our default header everywhere else
    silent! normal O#=============================================================================#
    silent! normal jo#=============================================================================#
    silent! normal kI# l

  else
    silent! normal ^
    " Remove any comments on current line
    silent! .s/^\/\+\s//g

    if a:type == "large"
      " Add the long lines
      silent! normal O/*--------------------------------------------------------*\
      silent! normal jo *--------------------------------------------------------*/
      silent! normal kI * l
    elseif a:type == "small"
      " Make comment
      silent! normal gcc
      " Upper case the comment
      silent! normal VU
      " Added the lines at wordlength
      silent! normal "tyy
      silent! normal "tPwv$r-
      silent! normal j"tpwv$r-
      silent! normal k
    else
      echo "Not correct size!"
    endif


  endif

endfunction

command! -nargs=1 MakeHeader call s:MakeHeader(<f-args>)

nnoremap gmh :MakeHeader small<cr>
nnoremap gmH :MakeHeader large<cr>
