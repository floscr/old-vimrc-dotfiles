" Remap emmet leader key
let g:user_emmet_settings = webapi#json#decode(join(readfile(expand('~/.config/nvim/emmet/snippets.json')), "\n"))