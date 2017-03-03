function! GitEdit()
  let filePath = expand('%:p')
  let gitRelativeFilePath = system('git ls-tree --full-name --name-only HEAD '. filePath)
  let gitFileContent = system('echo "`git show HEAD:'. gitRelativeFilePath .'`"')
  let lineNumber = line('.')

  vnew __test__
  normal! ggdG
  setlocal buftype=nowrite nomodified " bufhidden=delete nobuflisted
  " read !date


  call append(0, split(gitFileContent, '\v\n'))
  execute lineNumber
endfunction
command! GitEdit call GitEdit()

