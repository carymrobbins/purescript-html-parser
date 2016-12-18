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

You will need the following tools on your `PATH` -
  * psc 0.10.3 (see the [PureScript releases page](https://github.com/purescript/purescript/releases))
  * [pulp](https://github.com/bodil/pulp)
  * [bower](https://bower.io/)

To install dependencies, build the project, and run tests, you can use a single command -

```
% make deps build test
```

If you are only compiling the project and do not need to install dependencies or
run tests, you can simply run `make` without arguments -

```
% make
```

Note that using `make` validates that you have the appropriate tools available. If you need
to bypass this, you can simply delegate to using `bower` and `pulp` manually as desired.
