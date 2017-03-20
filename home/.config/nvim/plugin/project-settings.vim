" Enable htmljinja
augroup fs_2017
  autocmd!
  autocmd BufRead,BufNewFile ~/Code/Repositories/hugo-fs-2017/**/*.html setlocal ft=htmljinja
augroup END
