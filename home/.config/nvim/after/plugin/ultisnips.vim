let g:UltiSnipsExpandTrigger="<c-k>"
let g:UltiSnipsJumpForwardTrigger="<c-k>"
let g:UltiSnipsJumpBackwardTrigger="<s-c-j>"
let g:UltiSnipsSnippetDirectories=["UltiSnips"]

" Map keys from after
call UltiSnips#map_keys#MapKeys()

" Open the snippets file for the current filetype
nnoremap <leader>es :UltiSnipsEdit<CR>

" Remove the red color indentation highlight
highlight snipLeadingSpaces guifg=#2E3C47

