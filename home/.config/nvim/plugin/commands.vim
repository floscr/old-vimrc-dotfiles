function! MarkdownChromeTabLink()
  read !osascript ~/.homesick/repos/dotfiles/files/workflows/chrome-tab-markdown-link.scpt
  execute('norm kJ')
endfunction
command! MarkdownChromeTabLink call MarkdownChromeTabLink()

function! MarkdownHeader()
  let filename = expand('%:r')
  execute('norm gg')
  put! =filename
  execute('norm gmh')
endfunction
command! MarkdownHeader call MarkdownHeader()

" Reverse selected lines
command! -bar -range=% Reverse <line1>,<line2>g/^/m<line1>-1|nohl
