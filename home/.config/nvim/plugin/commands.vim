function! MarkdownChromeTabLink()
  read !osascript ~/.homesick/repos/dotfiles/files/workflows/Get\ current\ Chrome\ Tab\ Markdown\ Link.scpt
  execute('norm kddkJ')
endfunction
command! MarkdownChromeTabLink call MarkdownChromeTabLink()
