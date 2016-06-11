syntax keyword globalObjectsInJs window document

syntax keyword javascriptDocTags contained constant constructor constructs function ignore inner private public readonly static

command! -nargs=+ CustomHighLight hi def link <args>

CustomHighLight globalObjectsInJs Special

CustomHighLight javascriptDocTags SpecialComment




