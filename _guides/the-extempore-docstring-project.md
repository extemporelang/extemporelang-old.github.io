---
layout: docs
title: The Extempore docstring project
categories: contributing
---

xtlang functions have always had docstrings (documentation strings), but
for a long time you’ve only been able to find them if you’re actually
looking through the source code. And let’s face it, not many folks do
that regularly. As a result, not many xtlang functions have docstrings,
and the ones that are there aren’t very detailed/informative.

# A new documentation website

[Pete](https://github.com/mrmagooey) has done some great work at putting
together an automatically-generated (i.e. pulled from the source code)
searchable documentation website for xtlang.

A [beta version of this doco website](http://TODO) is now online. It’s
searchable and filterable in a few different ways, and I encourage you
to check it out. The information (e.g. types, arguments) is pulled
directly from the Extempore source code (including the main libraries in
`libs/core` and `libs/external`), although the documentation generation

# Structured docstrings

As part of this docstring upgrade, xtlang now supports **better**
docstrings *in more places*. Better, because they now support a
lightweight javadoc/jsdoc-style markup for documenting individual
parameters, return values, providing links to other, related
functionality, and even examples.

``` {.extempore}
  (bind-func bens_great_function
    "a (one-line) description of the function: this is Ben's great function for adding two numbers together

  Here's some more detail. Sometimes, you just need to add two numbers.
  And the + operator just isn't up to the job.  Well, that's when you need
  bens_great_function (well, as long as the numbers are i64).

  @param - the first number to add
  @param - the second number to add
  @return - the sum of the two input arguments
  @example
  (bens_great_function 4 7) ;; returns 11
  @see bens_other_great_function - another great function to check out"
    (lambda (a:i64 b)
      (+ a b)))
```

The `@param` lines are just for providing docstrings for each argument:
the names and types of the arguments will be taken from the actual
closure body. All of the `@` tags are optional—you can still just write
a one-line string as the docstring as before, and it’ll still come up in
the docs, it just won’t have any specific examples, or “see also”
references, or docstrings for the invidual parameters or return value.

Also, like I said earlier, you can now add docstrings to all the
(xtlang) things.

``` {.extempore}
  (bind-val bens_global_variable i32 42
    "the answer to life, the universe and everything

  @see bens_other_global_variable - holds the question to which this is the answer")

  (bind-lib libmissile fire_ze_missiles [void,i64]*
    "fire ze missiles!

  Be careful - this function has side effects.

  @param - the number of missiles to fire.
  @return - check for success by looking out the window and seeing if everything is destroyed")

  (bind-type BenType <i32,float>
    "this is a great type.

  This type is handy for storing your favourite numbers.

  @param - Ben's favourite integer
  @param - Ben's favourite float
  @see BenType_print")

  (bind-poly print BenType_print
    "Overloaded print function for BenType

  This polymorphic overload allows println to \"do the
  right thing\" with variables of BenType*

  @example

  (let ((bt (BenType 3 5.2)))
    (println bt)) ;; prints <BenType: 3 5.200>

  @see BenType")

```

# The docstring project

As well as a general library cleanup, the job over the next little while
is to add/improve the docstrings in the various xtlang libraries. It’s
the sort of job which can be done bit-by-bit, and it’s also the sort of
job which the community can help with.

So, if you’re looking for a way to contribute, and there’s a particular
xtlang library which you use and feel familiar enough with to write some
docstrings for, then give it a go and send a pull request :)

This will hopefully be a virtuous cycle: as the docstrings improve then
Extempore/xtlang will become easier to grok, which will then mean that
more folks are in a position to write better docstrings and help out.
And having the [documentation website](http://TODO) means that these
docstrings will be searchable and visible even to those who aren’t used
to grepping through the source code. I’ve been maintaining documentation
for various libraries on my blog here for a couple of years now (and I’m
going to keep things up-to-date as best I can) but it’s easy to let the
source code and the docs get “out of sync”. By generating the docs
directly from the source code + docstrings, hopefully the job of keeping
things up to date will be a little bit easier.

So get amongst it :)
