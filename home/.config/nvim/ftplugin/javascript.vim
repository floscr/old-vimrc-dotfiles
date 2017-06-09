" ----------
" JAVASCRIPT
" ----------

" Execute current file in node
noremap <buffer><silent><leader>bb :! node %<cr>

" Crate docblock header
noremap <buffer> ,p :JsDoc<CR>
