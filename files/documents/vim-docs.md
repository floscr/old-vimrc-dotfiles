Vim Documentation
=================

## Project specific Settings with .lvimrc

I use [embear/vim-localvimrc](https://github.com/embear/vim-localvimrc) to load project specific settings.

To create project specific settings or vim modifiactions in a specific project,
create a `.lvimrc` file in your git root.

Example for modifying default FZF behaviour:

```vim
" Exclude app/ directory from search
function! s:gitFiles()
  return 'GFiles -o --exclude-standard -c | grep -ov "\.jpg\|\.png\|\.gif$" | grep -ov "^app"'
endfunction
command! ProjectFiles execute s:gitFiles()
```

This would ignore the `app` directory from your FZF Search.

To disable the ProjectFiles warning on the launch of the project,
add your project to the `home/.config/nvim/whitelist.vim` like this:

```vim
" Whitelisted projects
let g:localvimrc_whitelist=[
      \ '/Users/my-user/Code/my-project',
      \ ]
" Allow non sandbox modifications
let g:localvimrc_sandbox=0
```
