Vim Documentation
=================

## Project specific Settings with .lvimrc

I use [embear/vim-localvimrc](https://github.com/embear/vim-localvimrc) to load project specific settings.

To create project specific settings or vim modifiactions in a specific project,
create a `.lvimrc` file in your git root.

### Disable Warning

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

### Examples

#### Modifying default FZF behaviour

```vim
" Exclude app/ directory from search
function! s:gitFiles()
  return 'GFiles -o --exclude-standard -c | grep -ov "\.jpg\|\.png\|\.gif$" | grep -ov "^app"'
endfunction
command! ProjectFiles execute s:gitFiles()
```

This would ignore the `app` directory from your FZF Search.

#### Custom vimux actions

```vim
" Rerun last Vimux command
map <Leader>xy :call VimuxRunCommand('echo "hello world"')<CR>
```

#### Set jsx filetype for React projects

We dont want to load jsx on all filetypes, except in React projects.

```vim
au BufRead,BufNewFile *.js set filetype=javascript.jsx
```

**This needs sandbox mode to be disabled**