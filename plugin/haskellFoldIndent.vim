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
    elseif a:line =~ '\v^.* do( do )@! \S*(( do )@!.)*$'
        return PrefixLen(a:line, '\v^(.* do( do )@! )\S*(( do )@!.)*$')
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

" Generate regex match for possibly nested layout symbols, e.g. do, if, let
fun! PossiblyNested(keywords, end)
    return '\v^(.* ' . a:end . '(( ' . a:keywords . ' )@! ))(( '
         \ . a:keywords . ' )@!.)*'
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
    " Arrow at the start of line means indented type signature
    elseif a:line =~ '^\s*->'
      \ || a:line =~ '^\s*=>'
        return SigIndent(a:line)
    " Check for the start of any common block
    elseif a:line =~ '^.* do$'
      \ || a:line =~ '^.* \\case$'
      \ || a:line =~ '^.* case .* of$'
        return BaseIndent(a:line) + &shiftwidth
    " Check for line-continuation
    elseif a:line =~ '.* =$'
      \ || a:line =~ '.* ->$'
      \ || a:line =~ '.* <-$'
        return BaseIndent(a:line) + &shiftwidth/2
    " Check for MultiWayIf
    elseif a:line =~ PossiblyNested('(if|do)', 'if \|')
        return PrefixLen(a:line, PossiblyNested('(if|do)', 'if \|')) - 2
    " Check for normal if
    elseif a:line =~ PossiblyNested('(if|do)', 'if')
        return PrefixLen(a:line, PossiblyNested('(if|do)', 'if'))
    " Check for do block
    elseif a:line =~ PossiblyNested('(if|do)', 'do')
        return PrefixLen(a:line, PossiblyNested('(if|do)', 'do'))
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
      \ && a:line !~ '^.* = .*$'
      \ && a:line !~ '^.* =$'
      \ && a:line !~ '^.* :: .*$'
        return BaseIndent(a:line) + &shiftwidth
    endif
    return BaseIndent(a:line)
endfunction

fun! HaskellIndent(lnum)
    let line = getline(a:lnum)
    let prevl = getline(a:lnum-1)
    if line =~ '^\s*->' || line =~ '^\s*=>'
        return SigIndent(prevl)
    elseif line =~ '^\s*{' && prevl =~ '^\s*data'
        return BaseIndent(prevl) + 5
    else
        return NextIndent(prevl)
    endif
endfunction

fun! s:setHaskellFoldIndent()
    "setlocal foldexpr=
    "setlocal foldtext=
    "setlocal foldmethod=expr
    setlocal indentexpr=HaskellIndent(v:lnum)
    setlocal indentkeys=o,O,=->,==>,0{
endfunction

augroup HaskellFoldIndent
    au!
    au FileType haskell call s:setHaskellFoldIndent()
augroup END
