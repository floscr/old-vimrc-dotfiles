syn match cssComment "//.*$"
hi def link cssBraceError cssBraces

syn region scssFunction contained matchgroup=scssFunctionName start="\<\(url(\)\@!\([[:alnum:]-_]\)\+\s*(" skip=+([^)]*)+ end=")" keepend extend
syn region cssMediaBlock transparent matchgroup=cssBraces start='{' end='}' contains=cssError,cssComment,cssDefinition,cssURL,cssUnicodeEscape,cssIdentifier

syn match scssVariable "$[[:alnum:]_-]\+" containedin=scssFunction,cssInclude,cssMediaBlock,cssMediaType skipwhite


