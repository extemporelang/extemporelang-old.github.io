---
title: Extempore mini-projects
tags: contributing
---

There are many aspects to developing a new language: tooling (editor
plugins, etc.), documentation, example code, the language itself, and
bugfixing (especially across multiple platforms).

Some of these are specialised, for example the arcane rites which must
be performed by the great high priest of xtlang generics (aka Andrew)
before entering the generics section of the xtlang compiler. Others
require other types of specialisation, such as the even more arcane
ritual cleansing (in honour of St IGNUcius) I must undergo before I do
battle with the Extempore Emacs mode.

There are still some other things to be done, and I just wanted to
highlight a few “mini-projects” which are hanging around and which would
be a useful and greatly appreciated contribution to the Extempore cause.

If you’re interested in taking on any of these projects, then drop us a
line on the [mailing list](mailto:extemporelang.googlegroups.com). We
can’t offer any money, although if you’re an ANU student we can maybe
figure out a way to get you some course credit. And of course, you get
the warm fuzzy feeling of contributing to open-source software.

## Atom editor

GitHub’s [Atom](https://atom.io/) text editor is gaining some traction
with the cool kids who aren’t rusted on Vim/Emacs users, and I think
that’s a good thing—it’s a pretty cool editor. It would be *awesome* to
have an Extempore mode for atom. As is often the case for developing an
Extempore plugin for a new editor, starting with the Scheme plugin and
modifying it to suit is probably a good way to get started.

**Useful skills**: Coffescript/Javascript

## Jupyter kernel

IPython is branching out to support more languages, and the core of this
new project is called Jupyter. It would be great if we could an
Extempore “plugin” (called a kernel in the Jupyter parlance).

**Useful skills**: Python

## Linguist plugin

It would be great to teach
[linguist](https://github.com/github/linguist), GitHub’s language
parser/syntax highliting engine about xtlang. That way, xtlang code
would get rendered nicely on GH issues/wikis, and we could also (I
think) use the linguist parser to do syntax-highlighting wherever else
we wanted it.

The best way to get **that** happening is probably to modify the Scheme
language support. I think linguist can understand tmbundles (TextMate
bundles), so looking at the Sublime Text extempore mode might be useful
as well.

**Useful skills**: *maybe* Ruby, or knowledge of TextMate bundles

## Pygments plugin

Similar to the previous project, this is about teaching
[Pygments](http://pygments.org/) about xtlang. This would be another way
to get syntax highlighting for xtlang code in places other than
Emacs/ST, which would be really handy.

**Useful skills**: Python

## ST2/3 plugin maintainer

There is [an Extempore plugin for Sublime Text
2/3](2012-10-23-extempore-st2-cheat-sheet.org), and it works decently,
although it’s not as full-featured as the Emacs one.

**Useful skills**: Python, knowledge of TextMate/Sublime Text bundles

## Docstrings

Extempore now supports much richer docstrings, which are then used to
generate the searchable
[docs website](https://extemporelang.github.io/docs). But there are
vast chunks of xtlang code which have no (or underwhelming)
docstrings. Let’s fix that, one docstring at a time! The way to do
that is to just edit the `.xtm` file directly, adding the docstring
in, and sending a pull request.

**Useful skills**: xtlang

## LLVM patch

If you’ve ever been frustrated by the fact that you have to patch LLVM
to build Extempore, you’ll understand why it would be awesome to get our
patch accepted to LLVM proper. To understand exactly why the patch is
necessary, I’ll quote from a recent email from Andrew to the Extempore
mailing list:

> There is an assumption in the LLVM parser that all definitions occur
> within the same compilation unit - i.e. the parser has local state
> about what has been parsed in this unit of work. Extempore obviously
> does lots of little units rather than one big unit and this causes
> problems for named types that were defined in another unit - which
> they always are. The patch simply checks the current module to see if
> the type has been previously defined, and intervenes appropriately if
> it has.
>
> I’m not totally sure why this problem doesn’t hit more people,
> although my suspicion is that (a) most people are using much larger
> compilation units and (b) most people are not using the IR parser
> directly like we are (i.e. they use the CPP interface primarily).
>
> I asked about this on the LLVM mailing list many years ago but did not
> get any responses. Happy for someone else to try to push it up again,
> or perhaps there is a better solution.

The route to getting our patch accepted upstream would be to create a
small C++ program which reproduced the conditions of the bug, then
submitting it as a bug report.

**Useful skills**: C++, knowledge of LLVM a bonus

## C++11-ification

The extempore executable is a cross-platform C++ application, and it’s
pretty battle-hardened, but it’s not necessarily the most clean & modern
C++ codebase around. A nice little project would be to go through and
clean up/modernise some of the things, especially now that C++11
features are pretty standard on most compilers. This would allow us to
get rid of some of the `#ifdef` guards for cross-platform support as
well.

Bonus points for upgrading the LLVM dependency to a more recent version
than the current version (3.4).

**Useful skills**: C++
