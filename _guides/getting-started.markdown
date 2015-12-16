---
title: Getting Started
---

## Introduction

The Extempore environment typically consists of two moving parts; the
extempore process itself, and a text editor which can send code to the
process in order to be evaluated. We support a number of different <a
href="/guides/getting-started/#text-editors">text editors</a>, and
installing extempore on most platforms is straight forward.

## Quick install

**Through homebrew (OSX)**

First,

```
brew tap benswift/extempore
```

then

```
brew install extempore
```

or, if you want the "extended" libs (e.g. graphics)

```
brew install extempore --with-extended
```

*Note:* If you've installed Extempore through homebrew previously
(i.e. if `brew info extempore` shows a version <= 0.59) then you'll
need to remove a couple of things first:

```
brew rm extempore kissfft
```

**Build from source (Linux/OSX)**

If you've got `git`, `cmake` and a C++ compiler toolchain
installed, then you can build Extempore with:

```
git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install && make aot
```

<!-- Get a binary on Windows - put link in here -->

Those are the "quick install" tips. For more detailed instructions,
see
[INSTALL.md](https://github.com/digego/extempore/blob/master/INSTALL.md)
in the Extempore source distribution.
