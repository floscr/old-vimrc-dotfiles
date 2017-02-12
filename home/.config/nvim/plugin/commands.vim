function! MarkdownChromeTabLink()
  read !osascript ~/.homesick/repos/dotfiles/files/workflows/Get\ current\ Chrome\ Tab\ Markdown\ Link.scpt
  execute('norm kddkJ$')
endfunction
command! MarkdownChromeTabLink call MarkdownChromeTabLink()

function! MarkdownHeader()
  let filename = expand('%:r')
  execute('norm gg')
  put! =filename
  execute('norm gmh')
endfunction
command! MarkdownHeader call MarkdownHeader()
