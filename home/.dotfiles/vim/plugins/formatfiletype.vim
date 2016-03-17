function! ReformatFileByFileType()

  mark `
  " Fix Tab indentation
  silent! retab! %
  silent! normal gg=G

  if &filetype == "php"
    " Remove spaces padding inside braces
    silent! %s/\s)/)/
    silent! %s/(\s/(/
    " Remove space before a comma
    silent! %s/\s\ze,//g
  elseif &filetype == "scss" || &filetype == "css" || &filetype == "sass"
    " Fix missing space before an {
    " .class{
    silent! %s/[A-Za-z0-9]\zs\ze{/ /g
    " Fix css arguments without whitespace after the :
    " margin-bottom:0;
    silent! %s/:\zs\ze\(hover\|focus\|active\|before\|after\|\s\)\@!/ /g
  endif

  " Cursor go back to initial position
  silent! normal ''
  mark `

endfunction

command! ReformatFileByFileType call ReformatFileByFileType()
