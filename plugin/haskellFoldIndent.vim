" =============================================================================
" Descriptions: Haskell folding and auto-indenting.
" Maintainer:   Merijn Verstraaten <merijn@inconsistent.nl>
" Version:      1.0
" License:      GNU General Public License v3.0
" =============================================================================
if exists("g:HaskellFoldIndent")
    finish
endif
let g:HaskellFoldIndent = 1

" Matches any whitespace followed by two or more - and then whitespace again
let s:commentStart = '%(%(|\s)--+\s)'
" Matches any number of non-comment characters
let s:nonComment = '%(' . s:commentStart . '@!.)*'

" Calculate the length of a prefix
fun! PrefixLen(line, pattern)
    return strlen(substitute(a:line, a:pattern, '\1', ''))
endfunction

" Used to check whether something is a function/value definition
fun! ValidTopLevel(line)
    return a:line =~ '\v^(data|newtype|type|foreign|import|module|class|infix)'
      \ || a:line =~ '\v^(instance||default|\$\(|\{-)'
endfunction

" Figure out the indent level of a line
fun! BaseIndent(line)
    if a:line =~ '^\s*where '
        return PrefixLen(a:line, '\v^(\s*where ).*$')
    elseif a:line =~ NotFollowedBy('do', '(do|let)')
        return PrefixLen(a:line, NotFollowedBy('do', '(do|let)'))
    elseif a:line =~ NotFollowedBy('let', '(do|let)')
        return PrefixLen(a:line, NotFollowedBy('let', '(do|let)'))
    else
        return PrefixLen(a:line, '\v^(\s*)\S.*$')
    endif
endfunction

" Figure out how far to indent type signatures
fun! SigIndent(line)
    if a:line =~ '\v^(( :: )@!.)* :: .*$'
        return PrefixLen(a:line, '\v^((( :: )@!.)* ):: .*$')
    elseif a:line =~ '^\s*\(=\|-\)>.*$'
        return PrefixLen(a:line, '^\(\s*\)\(=\|-\)>.*$')
    else
        return -1
    endif
endfunction

" Generate regex match which matches a keyword that is NOT followed by a set of
" other keywords
fun! NotFollowedBy(keyword, others)
    return '\v^(' . s:nonComment . ' ' . a:keyword . '(( ' . a:others
         \ . ' )@! ))(( ' . a:others . ' )@!.)*$'
endfunction

" Generate a regex match which matches a keyword that is NOT preceded by a set
" of other keywords OR a comment start
fun! NotPrecededBy(keyword, others)
    return '\v^(%(%(%(' . a:others . ' )|' . s:commentStart . ')@!.)* '
         \ . a:keyword . ' ).*$'
endfunction

fun! NextIndent(line)
    " Indent data constructors and guards equally far
    if a:line =~ "^\s*|"
        return PrefixLen(a:line, "\v^(\s*)|.*$")
    " Indent basic ADT declaration
    elseif a:line =~ '^\s*data .* = .*$'
        return PrefixLen(a:line, '\(\s*data .* \)= .*$')
    " Indent right after module start for export list
    elseif a:line =~ '^module \S* ($'
      \ || a:line =~ '^module \S*$'
        return &shiftwidth
    " Dedent after end of module export list
    elseif a:line =~ '\s*) where$'
        return 0
    elseif a:line =~ '^\s*where$'
        return BaseIndent(a:line) + (&shiftwidth + 1)/2
    " Arrow at the start of line means indented type signature
    elseif a:line =~ '^\s*->'
      \ || a:line =~ '^\s*=>'
        return SigIndent(a:line)
    " Check for the start of any common block
    elseif a:line =~ '\v^' . s:nonComment . ' do$'
      \ || a:line =~ '\v^' . s:nonComment . ' let$'
      \ || a:line =~ '\v^' . s:nonComment . ' \\case$'
      \ || a:line =~ '\v^' . s:nonComment . ' case .* of$'
        return BaseIndent(a:line) + &shiftwidth
    " Check for line-continuation
    elseif a:line =~ '\v^' . s:nonComment . ' \=$'
      \ || a:line =~ '\v^' . s:nonComment . ' ->$'
      \ || a:line =~ '\v^' . s:nonComment . ' <-$'
      \ || a:line =~ NotFollowedBy('then', '(if|do|else)')
        return BaseIndent(a:line) + &shiftwidth/2
    " Check for MultiWayIf
    elseif a:line =~ NotFollowedBy('if \|', '(if|do|let)')
        return PrefixLen(a:line, NotFollowedBy('if \|', '(if|do|let)')) - 2
    " Check for normal if
    elseif a:line =~ NotFollowedBy('if', '(if|do|else|let)')
        return PrefixLen(a:line, NotFollowedBy('if', '(if|do|else|let)'))
    " Check for declaration blocks
    elseif a:line =~ '\v^\s*(type|newtype|data) instance where$'
      \ || a:line =~ '\v^\s*(data|class|instance) .* where$'
      \ || a:line =~ '\v^\s*foreign (import|export) .* "[^"]*"$'
        return BaseIndent(a:line) + &shiftwidth
    " Indent hiding declarations
    elseif a:line =~ '\v\s*import( qualified)? \S*( as \S*)? hiding$'
        return BaseIndent(a:line) + 2*&shiftwidth
    " No equals sign or type annotation, so expect a guard.
    elseif a:line =~ '^\S' && !ValidTopLevel(a:line)
      \ && a:line !~ '\v^' . s:nonComment . ' = .*$'
      \ && a:line !~ '\v^' . s:nonComment . ' =$'
      \ && a:line !~ '\v^' . s:nonComment . ' :: .*$'
        return BaseIndent(a:line) + &shiftwidth
    endif
    return BaseIndent(a:line)
endfunction

fun! HaskellIndent(lnum)
    let line = getline(a:lnum)
    let prevl = getline(a:lnum-1)
    if line =~ '^\s*->' || line =~ '^\s*=>'
        return SigIndent(prevl)
    elseif line =~ '^\s*{'
        " Record syntax ADT declaration
        if prevl =~ '^\s*data'
            return BaseIndent(prevl) + 5
        " Record syntax GADT declaration
        elseif prevl =~ '\v^' . s:nonComment . ' ::$'
            return BaseIndent(prevl) + &shiftwidth
        " Line continuation starting with record update syntax
        else
            return BaseIndent(prevl) + &shiftwidth/2
        endif
    " Search for the first line that starts with , or { and indent equally far
    elseif line =~ '^\s*,'
        let lnum = a:lnum - 1
        while lnum > 0 && getline(lnum) !~ '\v^\s*(,|\{)'
            let lnum -= 1
        endwhile

        return lnum ? BaseIndent(getline(lnum)) : BaseIndent(prevl)
    " If the line starts with where, find the definition the where belongs to
    " and indent based on that
    elseif line =~ '^\s*where'
        let lnum = a:lnum - 1
        while lnum > 0
         \ && getline(lnum) !~ NotPrecededBy('\=', 'let')
         \ && getline(lnum) !~ '\v^' . s:nonComment . ' =$'
            let lnum -= 1
        endwhile

        if lnum == 0
            return BaseIndent(prevl)
        elseif line =~ '^\s*where$'
          \ || BaseIndent(getline(lnum)) + &shiftwidth == BaseIndent(prevl)
            return BaseIndent(getline(lnum)) + &shiftwidth/2
        else
            return BaseIndent(getline(lnum)) + &shiftwidth
        endif
    elseif line =~ '^\s*in '
        let lnum = a:lnum - 1
        while lnum > 0
         \ && getline(lnum) !~ NotFollowedBy('let', 'let')
         \ && getline(lnum) !~ '\v^' . s:nonComment . ' let$'
            let lnum -= 1
        endwhile

        if lnum > 0 && getline(lnum) !~ '^.* let$'
            return BaseIndent(getline(lnum)) - 4
        else
            return BaseIndent(prevl)
        endif
    else
        return NextIndent(prevl)
    endif
endfunction

fun! s:setHaskellFoldIndent()
    "setlocal foldexpr=
    "setlocal foldtext=
    "setlocal foldmethod=expr
    setlocal indentexpr=HaskellIndent(v:lnum)
    setlocal indentkeys=o,O,0=->,0==>,0{,0,,0=where\ ,0=where,0=in\ 
endfunction

augroup HaskellFoldIndent
    au!
    au FileType haskell call s:setHaskellFoldIndent()
augroup END
