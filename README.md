# purescript-html-parser

An HTML parser for PureScript.

## Goals

The idea behind this library is that we may be able write HTML and have PureScript code generated from it.
This would make it easier to integrate HTML from designers without having to reimplement it all yourself in PureScript.
This can become even more powerful if/when [TemplatePureScript](https://github.com/purescript/purescript/issues/140)
becomes available.

## Usage

```purescript
$ psci
> import Text.HTML.Parser
> parseHTML """<html><body><h1 class="hello">Hello World!</h1></body></html>"""
Right (Cons (Element "html" (Nil)
        (Cons (Element "body" (Nil)
          (Cons (Element "h1" (Cons (Attribute "class" "hello") (Nil))
            (Cons (TextNode "Hello World!") (Nil))) (Nil))) (Nil))) (Nil))
```

See the [test suite](test/Main.purs) for more examples.

## Building

To install dependencies, build the project, and run tests, you can use a single command -

```
% make
```

Afterwards, you can build quickly without cleaning and re-installing deps with -

```
% make quick
```

See the [Makefile](Makefile) for more commands.
