" Description:	twig indenter
" Author: Hokuto Tokutake <tokutake.hokuto@gmail.com>
" Last Change:	Sat Apr 21 01:48:05 JST 2012

if exists('b:did_indnet')
  unlet b:did_indent
endif
source $VIMRUNTIME/indent/html.vim

" [-- local settings (must come before aborting the script) --]
setlocal indentexpr=TwigHtmlIndentGet(v:lnum)
setlocal indentkeys=o,O,*<Return>,<>>,{,}

if exists('g:twig_indent_tags')
  unlet g:twig_indent_tags
endif

if exists('g:twig_close_tags')
  unlet g:twig_close_tags
endif

" [-- helper function to assemble tag list --]
fun! <SID>TwigIndentPush(tag)
  if exists('g:twig_indent_tags')
    let g:twig_indent_tags = g:twig_indent_tags.'\|'.a:tag
  else
    let g:twig_indent_tags = a:tag
  endif
  call <SID>TwigClosePush(a:tag)
endfun

" [-- helper function to assemble tag list --]
fun! <SID>TwigClosePush(tag)
  if exists('g:twig_close_tags')
    let g:twig_close_tags = g:twig_close_tags.'\|'.'end'.a:tag
  else
    let g:twig_close_tags = 'end'.a:tag
  endif
endfun

call <SID>TwigIndentPush('block')
call <SID>TwigIndentPush('autoescape')
call <SID>TwigIndentPush('for')
call <SID>TwigIndentPush('if')
call <SID>TwigIndentPush('elseif')
call <SID>TwigIndentPush('else')
call <SID>TwigIndentPush('spaceless')

let s:cpo_save = &cpo
set cpo-=C

" [-- count tags of line a:lnum --]
fun! <SID>TwigIndent(lnum, pattern)
  let s = substitute('x'.getline(a:lnum),
        \ '.\{-}\(\({%\s*\)\('.a:pattern.'\)\>\)', "\1", 'g')
  let s = substitute(s, "[^\1].*$", '', '')
  return strlen(s)
endfun

fun! <SID>TwigIndentSum(lnum, style)
  if a:style == match(getline(a:lnum), '^\s*{%\s*end')
    if a:style == match(getline(a:lnum), '^\s*{%\s*\<\('.g:twig_close_tags.'\)\>')
      let open = <SID>TwigIndent(a:lnum, g:twig_indent_tags)
      let close = <SID>TwigIndent(a:lnum, g:twig_close_tags)
      if 0 != open || 0 != close
        return open - close
      endif
    endif
  endif
  return 0
endfun

fun! TwigHtmlIndentGetHelper(lnum)
  " Find a non-empty line above the current line.
  let lnum = prevnonblank(a:lnum - 1)

  " Hit the start of the file, use zero indent.
  if lnum == 0
    return 0
  endif

  let restore_ic = &ic
  setlocal ic " ignore case

  let ind = 0
  let ind = ind + <SID>TwigIndentSum(lnum, -1)
  let ind = ind + <SID>TwigIndentSum(a:lnum, 0)

  if restore_ic == 0
    setlocal noic
  endif

  return (&sw * ind) + HtmlIndentGet(a:lnum)
endfun


fun! TwigHtmlIndentGet(lnum)
  let ind = 0
  if 0 == match(getline(a:lnum), '^\s*{%\s*else')
    let ind = -&sw
  endif

  return TwigHtmlIndentGetHelper(a:lnum) + ind 
endfun

let &cpo = s:cpo_save
unlet s:cpo_save

" [-- EOF indent/html.twig.vim --]
" vim:set sw=2
