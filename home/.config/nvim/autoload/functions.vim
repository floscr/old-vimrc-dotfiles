" Switch to plaintext mode with: call functions#plaintext()
function! functions#plaintext() abort
  setlocal nolist
  setlocal spell
  setlocal textwidth=0
  setlocal wrap
  setlocal wrapmargin=0
  set foldlevel=2

  nnoremap <buffer> j gj
  nnoremap <buffer> k gk

  " Enable english spell completion in markdown files
  setlocal spelllang=de_de
  setlocal spellfile=$HOME/.config/nvim/spell/de.utf-8.add
  setlocal complete+=kspell

  setlocal spell!
endfunction
