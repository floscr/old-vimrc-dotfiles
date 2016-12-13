let g:deoplete#enable_at_startup=1
let g:deoplete#enable_refresh_always=0
let g:deoplete#file#enable_buffer_path=1
" Trigger deoplete only when pressing tab
let g:deoplete#disable_auto_complete=1

" let g:deoplete#sources={}
" let g:deoplete#sources._    = ['buffer', 'file', 'ultisnips']
" let g:deoplete#sources.vim  = ['buffer', 'member', 'file', 'ultisnips']
" let g:deoplete#sources['javascript.jsx'] = ['buffer', 'file', 'ultisnips']
" let g:deoplete#sources.css  = ['buffer', 'member', 'file', 'omni', 'ultisnips']
" let g:deoplete#sources.scss = ['buffer', 'member', 'file', 'omni', 'ultisnips']
" let g:deoplete#sources.html = ['buffer', 'member', 'file', 'omni', 'ultisnips']
" let g:deoplete#sources.php = ['buffer']

" Insert <TAB> or select next match
inoremap <silent> <expr> <Tab> utils#tabComplete()
imap <silent> <expr> <Tab> utils#tabComplete()

" use tab to backward cycle
inoremap <silent><expr><s-tab> pumvisible() ? "\<c-p>" : "\<s-tab>"

" Manually trigger tag autocomplete
inoremap <silent> <expr> <C-]> utils#manualTagComplete()
