=================
haskellFoldIndent
=================

A Vim plugin for better Haskell folding and auto-indenting.

Installation
============

If you use [pathogen.vim](https://github.com/tpope/vim-pathogen), simply run::

    cd ~/.vim/bundle
    hg clone https://bitbucket.org/merijnv/haskellfoldindent

or::

    cd ~/.vim/bundle
    git clone git://github.com/merijn/haskellFoldIndent.git

If you don't use pathogen, simply copy plugin/haskellFoldIndent.vim to
~/.vim/plugin/.

Using the plugin
================

As of right now, there is no configuration for the plugin. By default the
plugin will use the shiftwidth value from your vim configuration for indenting
new blocks. Continuations of previous lines will be indented by half your
shiftwidth.

Features & Examples
===================

Data declarations
-----------------

Multiline data declarations will automatically line up with the equals sign::

    data Foo a b = Foo a
                 | Bar b
                 | Baz a b

Typing a { on the first line after a declaration will align with the type
name::

    data Foo a b =
         { someA :: a
         , someB :: b
         }

GADT declarations will indent by shiftwidth spaces, so with shiftwidth=4::

    data Foo a b where
        Bar :: a -> b -> Foo a b
        Baz :: Foo a b

Type Signatures
---------------

Typing -> or => on the first line after a type signature will automatically
line up with the :: symbol::

    foo :: Monad m
        => (Double, Double)
        -> m Double

Automatic Block Indent
----------------------

Ending a line with do, case X of, or \case (when using LambdaCase) will
increase the indent level by shiftwidth::

    foo a b = do
        return (a + b)

    foo = \case
        Left _ -> 1
        Right _ -> 2

    foo x = case bar x of
        Nothing -> 1
        Just i -> i

    foo = do
        bar <- do
            blah blah

The automatic block indentation also handles...

Class and instance definitions::

    class Foo a where
        foo :: a -> a

    instance Foo Int where
        foo x = x

Type & data family blocks::

    type family Elem e (es :: '[*]) :: Bool

    type instance where
        Elem e (e ': es) = True
        Elem a (e ': es) = Elem a es
        Elem a '[]       = False

    data instance where
        Foo Int = {- ... -}
        Foo Char = {- ... -}

    newtype instance where
        Bar Int = {- ... -}
        Bar Char = {- ... -}

Foreign import/exports
----------------------

If a foreign import export line ends with a quoted text instead of a type
signature, the next line will be indented::

    foreign import ccall unsafe "string.h strerror"
        str_error :: CInt -> CString

Of course, automatically lining up type signatures works here too::

    foreign import ccall "unistd.h execve"
        execve :: CString
               -> Ptr (Ptr CChar)
               -> Ptr (Ptr CChar)
               -> IO Cint

Missing Features
================

Of course, there's a bunch of (important!) stuff still missing, because it
hasn't been implemented yet. Some stuff will never be implemented, because I
consider it bad.

Not yet implemented
-------------------

Bugs/unintended behaviour:
   * let/in expressions
   * where clauses
   * indent after inline if/then/else
   * handling multi-line type signatures inside record syntax declarations
   * handling record syntax in GADT syntax
   * resetting indentation after multi-line type signatures
   * haskell syntax in multiline comments
   * line continuations triggered by (, [ and {
   * indentation after blank lines
   * comments in between type signature lines
   * ??

Desirable features:
   * "smart" tabbing/tab stops
   * "smart" backspace
   * ??

Broken, won't fix
-----------------

split case-of
    case-of where the case and of are on separate lines are not accounted for.
    This is ugly anyway, so don't do it.

dangling else
    An if/then/else should either be entirely on one line *or* have a separate
    line for each if the three components.
