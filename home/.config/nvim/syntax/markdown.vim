" Continue line for todo lists
" - [ ] xyz
" ... <- Should match the line above
setlocal comments-=b:+,b:-
setlocal comments+=b:+\ [\ ],b:-\ [\ ],b:+,b:-
