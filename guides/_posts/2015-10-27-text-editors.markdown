---
title: Text Editors
---

## Introduction

For the majority of Extempore usage, text editors are the other half
of the coin when it comes to usage of the system. Your text editor is
responsible for maintaining a connection to the running extempore
process, sending segments of code to it, and displaying any responses.
Some text editors may even treat the extempore process as an inferior
process, rather than starting it separately.

## Emacs

The favoured text editor of the extempore devs due to its
extensibility and lisp heritage, Emacs offers the best integration
with extempore. It does however have a very steep learning curve.

Extempore integration comes in the form of extempore-mode, which can
be installed via the melpa repository.

## Sublime Text


## Vim


## Atom


## Other 

All Extempore requires from a text editor is the ability to connect to
a port via tcp, and send text to that port. If your text editor has
that capability then given some work it should be happy to be used as
an editor for Extempore.
