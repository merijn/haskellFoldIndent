=================
haskellFoldIndent
=================

A Vim plugin for better Haskell folding and auto-indenting.

Installation
============

If you use `pathogen.vim <https://github.com/tpope/vim-pathogen>`_, simply
run::

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
plugin will use the ``shiftwidth`` value from your vim configuration for
indenting new blocks. Continuations of previous lines will be indented by half
your ``shiftwidth``.

Features & Examples
===================

Data declarations
-----------------

Multiline data declarations will automatically line up with the equals sign::

    data Foo a b = Foo a
                 | Bar b
                 | Baz a b

Typing a ``{`` on the first line after a declaration will align with the type
name::

    data Foo a b = Foo
         { someA :: a
         , someB :: b
         }

GADT declarations will indent by ``shiftwidth`` spaces, so with
``shiftwidth=4``::

    data Foo a b where
        Bar :: a -> b -> Foo a b
        Baz :: Foo a b

Type Signatures
---------------

Typing ``->`` or ``=>`` on the first line after a type signature will
automatically line up with the ``::`` symbol::

    foo :: Monad m
        => (Double, Double)
        -> m Double

This works for multiline type signatures in record syntax too. Starting a line
with ``,`` after a multiline type signature will automatically dedent back to
the proper level for the record syntax definition::

    data Foo a b =
         { someA :: a
                 -> Int
                 -> Double
         , someB :: b
         }

And, of course, GADT record syntax works too::

    data Foo a b where
        FooBar ::
            { someA :: a
                    -> Int
                    -> Double
            , someB :: b
            }

Automatic Block Indent
----------------------

Ending a line with ``do``, ``case X of``, or ``\case`` (when using LambdaCase)
will increase the indent level by ``shiftwidth``::

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

Since there is no Vim support for precognition yet, there is no real way to
know which definition a ``where`` block belongs to, especially when you're
using ``where`` blocks for functions defined inside ``where`` blocks. To
simplify things it is assumed that a ``where`` block belongs to the first
non-``let`` definition above it.

A ``where`` block followed by a definition on the same line is indented
``shiftwidt`` spaces, unless that would line it up directly with the line above
it, e.g. if the line above is a ``do`` block, in which case it is indented by
half of ``shiftwidth``. If the ``where`` is on a line of its own, it is also
indented by half of ``shiftwidth``::

    foo a b = bar baz
        where bar = {- something -}
              baz = {- something -}

    foo a b = do
        bar
        baz
      where bar = {- something -}
            baz = {- something -}

    foo a b = do
        bar
        baz
      where
        bar = {- something -}
        baz = {- something -}

And it works with nested ``where``'s too (provided you don't want to dedent and
add more definitions to the where clause that defines ``baz``)::

    foo a b = a + b
        where baz = {- something -}
                  where xyzzy = {- something -}

Any ``let`` declarations in ``do`` blocks and ``let ... in`` expressions are
also handled. If the definitions following a ``let`` start on the same line,
then the ``in`` will be indented to match the indentation of ``let``, else
``in`` will have the same indentation as the line above it::

    foo a b = do
        let bar = 1
            baz = 2
        return a

    foo a b = let
        bar = 1
        baz = 2
        in bar + baz

    foo a b = let bar = 1
                  baz = 2
              in bar + baz

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

GADT record syntax::

    data Foo a where
        Foo ::
            { bar :: a
            , baz :: Int
            , quux :: Double
            } -> Foo Int

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
   * reindenting ruins indentation of nested where clauses and let/in
   * haskell syntax in multiline comments
   * multiline comments in general
   * line continuations triggered by (, [ and {
   * ??

Desirable features:
   * "smart" tabbing/tab stops
   * "smart" backspace
   * auto-dedent after a multiline type signature
   * better handling of mixing Haddock comments and multiline type signatures
     while writing
   * better indentation adjustment after blank lines
   * ??

Broken, won't fix
-----------------

split case-of
    case-of where the case and of are on separate lines are not accounted for.
    This is ugly anyway, so don't do it.

dangling else
    An if/then/else should either be entirely on one line *or* have a separate
    line for each if the three components.
