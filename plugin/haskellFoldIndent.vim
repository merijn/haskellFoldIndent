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

if !exists("g:HaskellFoldIndent_AlignDataDecl")
    let g:HaskellFoldIndent_AlignDataDecl = 1
endif

" Matches any whitespace followed by two or more - and then whitespace again
let s:commentStart = '%(%(|\s)--+\s)'
" Matches any number of non-comment characters
let s:nonComment = '%(' . s:commentStart . '@!.)*'

" Calculate the length of a prefix
fun! s:PrefixLen(line, pattern)
    return strlen(substitute(a:line, a:pattern, '\1', ''))
endfunction

" Used to check whether something is a top level declaration
fun! s:ValidTopLevel(line)
    return a:line =~ '\v^(data|newtype|type|foreign|import|module|class|infix)'
      \ || a:line =~ '\v^(instance|default|\$\(|\{-|deriving)'
endfunction

" Used to check whether something is function/value definition
fun! s:IsDefinition(line)
    return a:line =~ '\v^' . s:nonComment . ' \= .*$'
      \ || a:line =~ '\v^' . s:nonComment . ' \=$'
      \ || a:line =~ '\v^' . s:nonComment . ' :: .*$'
endfunction

" Figure out the indent level of a line
fun! BaseIndent(line)
    if a:line =~ '^\s*where '
        return s:PrefixLen(a:line, '\v^(\s*where ).*$')
    elseif a:line =~ s:NotFollowedBy('do', '(do|let)')
        return s:PrefixLen(a:line, s:NotFollowedBy('do', '(do|let)'))
    elseif a:line =~ s:NotFollowedBy('let', '(do|let)')
        return s:PrefixLen(a:line, s:NotFollowedBy('let', '(do|let)'))
    else
        return s:PrefixLen(a:line, '\v^(\s*)\S.*$')
    endif
endfunction

" Figure out how far to indent type signatures
fun! s:SigIndent(line)
    if a:line =~ '\v^(( :: )@!.)* :: .*$'
        return s:PrefixLen(a:line, '\v^((( :: )@!.)* ):: .*$')
    elseif a:line =~ '^\s*\(=\|-\)>.*$'
        return s:PrefixLen(a:line, '^\(\s*\)\(=\|-\)>.*$')
    else
        return -1
    endif
endfunction

" Generate regex match which matches a keyword that is NOT followed by a set of
" other keywords
fun! s:NotFollowedBy(keyword, others)
    return '\v^(' . s:nonComment . ' ' . a:keyword . '(( ' . a:others
         \ . ' )@! ))(( ' . a:others . ' )@!.)*$'
endfunction

" Generate a regex match which matches a keyword that is NOT preceded by a set
" of other keywords OR a comment start
fun! s:NotPrecededBy(keyword, others)
    return '\v^(%(%(%(' . a:others . ' )|' . s:commentStart . ')@!.)* '
         \ . a:keyword . ' ).*$'
endfunction

fun! NextIndent(line)
    " Indent data constructors and guards equally far
    if a:line =~ "^\s*|"
        return s:PrefixLen(a:line, "\v^(\s*)|.*$")
    " Indent basic ADT declaration
    elseif a:line =~ '\v^\s*data .* \= (( deriving )@!.)*$'
        if g:HaskellFoldIndent_AlignDataDecl
            return s:PrefixLen(a:line, '\(\s*data .* \)= .*$')
        else
            return &shiftwidth
        endif
    " Indent right after module start for export list
    elseif a:line =~ '^module \S* ($'
      \ || a:line =~ '^module \S*$'
        return &shiftwidth
    elseif a:line =~ '^module .* where$'
      \ || a:line =~ '^\s*) where$'
        return 0
    elseif a:line =~ '^\s*where$'
        return BaseIndent(a:line) + (&shiftwidth + 1)/2
    elseif a:line =~ '^\s*where .*$'
        return BaseIndent(a:line)
    " Arrow at the start of line means indented type signature
    elseif a:line =~ '^\s*->'
      \ || a:line =~ '^\s*=>'
        return s:SigIndent(a:line)
    " Check for the start of any common block
    elseif a:line =~ '\v^' . s:nonComment . ' do$'
      \ || a:line =~ '\v^' . s:nonComment . ' let$'
      \ || a:line =~ '\v^' . s:nonComment . ' \\case$'
      \ || a:line =~ '\v^' . s:nonComment . ' case .* of$'
      \ || a:line =~ '\v^' . s:nonComment . ' where$'
        return BaseIndent(a:line) + &shiftwidth
    " Check for line-continuation
    elseif a:line =~ '\v^' . s:nonComment . ' \=$'
      \ || a:line =~ '\v^' . s:nonComment . ' ->$'
      \ || a:line =~ '\v^' . s:nonComment . ' <-$'
        return BaseIndent(a:line) + &shiftwidth/2
    " Check for MultiWayIf
    elseif a:line =~ s:NotFollowedBy('if \|', '(if|do|let)')
        return s:PrefixLen(a:line, s:NotFollowedBy('if \|', '(if|do|let)')) - 2
    " Check for normal if
    elseif a:line =~ s:NotFollowedBy('if', '(if|do|else|let)')
        return s:PrefixLen(a:line, s:NotFollowedBy('if', '(if|do|else|let)'))
    " Check for declaration blocks
    elseif a:line =~ '\v^\s*(type|newtype|data) instance where$'
      \ || a:line =~ '\v^\s*(data|class|instance) .* where$'
      \ || a:line =~ '\v^\s*foreign (import|export) .* "[^"]*"$'
        return BaseIndent(a:line) + &shiftwidth
    " Dedent after end of module export list
    elseif a:line =~ '\s*) where$'
        return 0
    " Indent hiding declarations
    elseif a:line =~ '\v\s*import( qualified)? \S*( as \S*)? hiding$'
        return BaseIndent(a:line) + 2*&shiftwidth
    " No equals sign or type annotation, so expect a guard.
    elseif a:line =~ '^\S' && !s:ValidTopLevel(a:line)
      \ && !s:IsDefinition(a:line)
      \ && a:line !~ '\v^\s*--+(\s|$)'
        return BaseIndent(a:line) + &shiftwidth
    endif
    return -1
endfunction

fun! s:BraceIndent(prevl)
    " Record syntax ADT declaration
    if a:prevl =~ '^\s*data'
        return BaseIndent(a:prevl) + 5
    " Record syntax GADT declaration
    elseif a:prevl =~ '\v^' . s:nonComment . ' ::$'
        return BaseIndent(a:prevl) + &shiftwidth
    " Line continuation starting with record update syntax
    else
        return BaseIndent(a:prevl) + &shiftwidth/2
    endif
endfunction

fun! s:WhereIndent(lnum)
    let lnum = a:lnum - 1
    while lnum > 0
        \ && getline(lnum) !~ s:NotPrecededBy('\=', 'let')
        \ && getline(lnum) !~ '\v^' . s:nonComment . ' \=$'
        let lnum -= 1
    endwhile

    if lnum == 0
        return BaseIndent(getline(a:lnum - 1))
    elseif getline(a:lnum) =~ '^\s*where$'
        \ || BaseIndent(getline(lnum)) + &shiftwidth == BaseIndent(a:lnum - 1)
        return BaseIndent(getline(lnum)) + &shiftwidth/2
    endif

    return BaseIndent(getline(lnum)) + &shiftwidth
endfunction

fun! s:LetInIndent(lnum)
    let lnum = a:lnum - 1
    while lnum > 0
        \ && getline(lnum) !~ s:NotFollowedBy('let', 'let')
        \ && getline(lnum) !~ '\v^' . s:nonComment . ' let$'
        let lnum -= 1
    endwhile

    if lnum && getline(lnum) !~ '^.* let$'
        return BaseIndent(getline(lnum)) - 4
    else
        return BaseIndent(getline(a:lnum - 1))
    endif
endfunction

fun! s:BarIndent(lnum)
    let lnum = a:lnum - 1
    while lnum > 0 && getline(lnum) =~ '\v^\s*--+\s'
        let lnum -= 1
    endwhile

    if getline(lnum) =~ '\v^\s*(data|\|) '
        return NextIndent(getline(lnum))
    else
        return BaseIndent(getline(lnum)) + &shiftwidth
    endif
endfunction

fun! HaskellIndent(lnum)
    let line = getline(a:lnum)
    let prevl = getline(a:lnum-1)
    if line =~ '^\s*->' || line =~ '^\s*=>'
        return s:SigIndent(prevl)
    elseif line =~ '^\s*{ '
        return s:BraceIndent(prevl)
    " Search for the first line that starts with , or { and indent equally far
    elseif line =~ '^\s*,'
        let lnum = a:lnum - 1
        while lnum > 0 && getline(lnum) !~ '^\s*\(,|{|(|[\)'
            let lnum -= 1
        endwhile

        return lnum ? BaseIndent(getline(lnum)) : BaseIndent(prevl)
    " If the line starts with where, find the definition the where belongs to
    " and indent based on that
    elseif line =~ '^\s*where'
        return s:WhereIndent(a:lnum)
    elseif line =~ '^\s*in '
        return s:LetInIndent(a:lnum)
    elseif line =~ '\v^\s*(\||deriving)' && prevl !~ '\v^\s*(data|\|) '
        return s:BarIndent(a:lnum)
    " If a comment is indented equally to a comment on the previous line, don't
    " change the indent
    elseif line =~ '\v^\s*--+' && BaseIndent(line) == s:PrefixLen(prevl, '\v^(' . s:nonComment . ') --.*$') + 1
        return BaseIndent(line)
    " Dedent the first line after a multiline type signature
    elseif line =~ '\v^\s*((-|\=)\>)@!\S' && prevl =~ '\v^\s*(-|\=)\>\s'
        let lnum = a:lnum - 1
        while lnum > 0
         \ && getline(lnum) !~ s:NotPrecededBy('::', '(::|-\>|\=\>)')
            let lnum -= 1
        endwhile

        return lnum ? BaseIndent(getline(lnum)) : BaseIndent(prevl)
    " Don't indent top level declarations
    elseif s:ValidTopLevel(line)
        let lnum = a:lnum - 1

        while lnum > 0 && !s:ValidTopLevel(getline(lnum))
            let lnum -= 1
        endwhile

        return lnum ? BaseIndent(getline(lnum)) : -1
    else
        return NextIndent(prevl)
    endif
endfunction

fun! s:setHaskellFoldIndent()
    "setlocal foldexpr=
    "setlocal foldtext=
    "setlocal foldmethod=expr
    setlocal indentexpr=HaskellIndent(v:lnum)
    setlocal indentkeys=o,O,0=->,0==>,0={\ ,0,,0=where\ ,0=where,0=in\ 
    setlocal autoindent
endfunction

augroup HaskellFoldIndent
    au!
    au FileType haskell call s:setHaskellFoldIndent()
augroup END
