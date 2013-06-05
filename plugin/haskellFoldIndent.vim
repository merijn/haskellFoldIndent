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

" Used to check whether something is a function/value definition
fun! ValidTopLevel(line)
    return a:line =~ '\v^(data|newtype|type|foreign|import|module|class|infix)'
      \ || a:line =~ '\v^(instance||default|\$\(|\{-)'
endfunction

" Figure out the indent level of a line
fun! BaseIndent(line)
    if a:line =~ '^\s*where '
        return strlen(substitute(a:line, '\v^(\s*where ).*$', '\1', ''))
    else
        return strlen(substitute(a:line, '\v^(\s*)\S.*$', '\1', ''))
    endif
endfunction

" Figure out how far to indent type signatures
fun! SigIndent(line)
    if a:line =~ '\v^(( :: )@!.)* :: .*$'
        return strlen(substitute(a:line, '\v^((( :: )@!.)* ):: .*$', '\1', ''))
    elseif a:line =~ '^\s*\(=\|-\)>.*$'
        return strlen(substitute(a:line, '^\(\s*\)\(=\|-\)>.*$', '\1', ''))
    else
        return -1
    endif
endfunction

" Layout block introducers
fun! BlockStarter(prefix, line)
    return a:line =~ a:prefix . ' do$'
      \ || a:line =~ a:prefix . ' \\case$'
      \ || a:line =~ a:prefix . ' case .* of$'
endfunction

fun! NextIndent(line)
    " Indent data constructors and guards equally far
    if a:line =~ "^\s*|"
        return strlen(substitute(a:line, "\v^(\s*)|.*$", '\1', ''))
    " Indent basic ADT declaration
    elseif a:line =~ '^\s*data .* = .*$'
        return strlen(substitute(a:line, '\(\s*data .* \)= .*$', '\1', ''))
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
    elseif BlockStarter('^.* =', a:line)
      \ || BlockStarter('^.* ->', a:line)
      \ || BlockStarter('^.* <-', a:line)
      \ || BlockStarter('^\s*then', a:line)
      \ || BlockStarter('^\s*else', a:line)
        return BaseIndent(a:line) + &shiftwidth
    " Check for line-continuation
    elseif a:line =~ '.* =$'
      \ || a:line =~ '.* ->$'
      \ || a:line =~ '.* <-$'
        return BaseIndent(a:line) + &shiftwidth/2
    " Check for MultiWayIf
    elseif a:line =~ '^.*\sif |.*$'
        return strlen(substitute(a:line, '^\(.*\sif \)|.*$', '\1', ''))
    " Check for normal if
    elseif a:line =~ '\v^.*\sif(( (then|else) )@!.)*$'
        return BaseIndent(a:line) + &shiftwidth
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
    else
        return NextIndent(prevl)
    endif
endfunction

fun! s:setHaskellFoldIndent()
    "setlocal foldexpr=
    "setlocal foldtext=
    "setlocal foldmethod=expr
    setlocal indentexpr=HaskellIndent(v:lnum)
    setlocal indentkeys=o,O,=->,==>
endfunction

augroup HaskellFoldIndent
    au!
    au FileType haskell call s:setHaskellFoldIndent()
augroup END
