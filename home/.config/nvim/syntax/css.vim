" Comments
syn match cssComment "//.*$"
hi def link cssBraceError cssBraces

" Function
syn region scssFunction contained matchgroup=scssFunctionName start="\<\(url(\)\@!\([[:alnum:]-_]\)\+\s*(" skip=+([^)]*)+ end=")" keepend extend
syn region cssMediaBlock transparent matchgroup=cssBraces start='{' end='}' contains=cssError,cssComment,cssDefinition,cssURL,cssUnicodeEscape,cssIdentifier

" Variable
syn match scssVariable "$[[:alnum:]_-]\+" containedin=scssFunction,cssInclude,cssMediaBlock,cssMediaType skipwhite

" Flexbox
syn keyword cssFontProp order
syn match cssFontProp contained "\<flex\(-\(basis\|direction\|flow\|grow\|shrink\|wrap\)\)\=\>"
syn keyword cssFontAttr contained flex row wrap
syn match cssFontAttr contained "\<inline-flex\>"
syn match cssFontAttr contained "\<\(row\|column\|wrap\)-reverse\>"
