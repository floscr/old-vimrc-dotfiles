function! ReformatFileByFileType()

  mark `
  " Fix Tab indentation
  silent! retab! %
  silent! normal gg=G

  if &filetype == "php"
    " Remove spaces padding inside braces
    silent! %s/\s)/)/
    silent! %s/(\s/(/

  endif

  " Cursor go back to initial position
  silent! normal ''
  mark `

endfunction

command! ReformatFileByFileType call ReformatFileByFileType()
