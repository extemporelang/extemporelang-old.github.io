---
layout: docs
title: Extempore & Emacs cheat
categories: tools emacs
---

Learning [Emacs](http://www.gnu.org/software/emacs/) can be pretty
daunting, and you don’t *have* to use Emacs to use Extempore (see the
[Sublime Text 2 plugin](https://github.com/benswift/extempore-sublime),
for instance). Still, if you’re willing to invest the time, Emacs is a
powerful and flexible editing environment, and hacking Extempore code in
Emacs can actually be pretty fun once you get on top of it :)

I won’t cover all the basics of Emacs here, because there are lots of
other places which do that pretty well. If you’ve never used Emacs
before, I suggest you go and read the [beginner’s guide to
Emacs](http://www.masteringemacs.org/articles/2010/10/04/beginners-guide-to-emacs/),
which explains some of the basic concepts and terminology. Emacs also
has a built-in tutorial, which you can bring up with `C-h t`.

Think of this post as more as a ‘cheat sheet’, for dipping back into to
refresh your memory when you just can’t remember how to do something off
the top of your head.

# If you’re already an Emacs user

**Update Nov 2015** `extempore-mode` is now in MELPA. So just
`M-x package-install RET extempore-mode RET` and you’re done.

If you don’t want to get it from MELPA, just [download the
file](https://github.com/extemporelang/extempore-emacs-mode/blob/master/extempore-mode.el)
and `(package-install-file "/path/to/extempore-mode.el")`

## Working with Extempore code

These are the steps youll need to take to start hacking on a piece of
Extempore code in Emacs:

1.  start Emacs (if it isn’t running already)
2.  open up an Emacs buffer with an Extempore file (`extempore-mode`
    should be loaded automatically when Emacs sees the `.xtm` file
    extension, assuming you added code above to your `.emacs`)
3.  call `M-x switch-to-extempore` (`C-c C-z`), and if Extempore isn’t
    already running you’ll can add (optional) command line args for
    Extempore here, such as which audio device to use (e.g.
    `./extempore --device 2`)
4.  connect to the running Extempore process: `C-c C-j` (this needs to
    be done for *every* `.xtm` buffer)

Then, to
[evaluate](2012-09-26-interacting-with-the-extempore-compiler.org)
Extempore code, use either

-   evaluate enclosing s-expression: `C-c C-c` or `C-M-x`
-   evaluate region: `C-c C-r`
-   evaluate whole buffer: `C-c C-b`

To restart the Extempore process, just `C-c C-c` in the `*extempore*`
buffer where `extempore` is running to kill it, then start it up again
with `M-x switch-to-extempore` (`C-c C-z`).

For more detail on how to ‘program’ in Extempore, have a look at this
post on [interacting with the Extempore
compiler](2012-09-26-interacting-with-the-extempore-compiler.org).

# If you’re new to Emacs

The first thing you’ll need to do is install Emacs if it isn’t already
installed on your system. You can check if Emacs is installed (and which
version) with

``` {.bash}
emacs --version
```

The Extempore Emacs mode (plugins are called ‘modes’ in Emacs) requires
Emacs version 24, which has been the current stable release since about
June 2012. If for some reason you’re stuck on Emacs 23, you can use the
Extempore minor mode (`extras/extempore-minor.el`).

**OS X**

Binaries are available from
[emacsformacosx.com](http://emacsformacosx.com). In general, the default
one (i.e. big ‘Download’ button on that page) is probably the best
option if you’re unsure.

If you’re a [Homebrew](http://mxcl.github.com/homebrew/) user, then you
can also get Emacs with

``` {.bash}
brew install --cocoa emacs
```

**Linux**

On Ubuntu, you can get Emacs version 24 with

``` {.bash}
sudo apt-get install emacs24
```

Other distros will probably have a similar package.

**Windows**

The [Official GNU Windows
binaries](http://ftp.gnu.org/gnu/emacs/windows/) are probably as good as
any. Scroll down to the bottom of that page for the most recent
versions.

Also, if you’re running Extempore in an Emacs `shell` buffer on Windows
you’ll need to pass the `--term ansi` option when starting Extempore.

## A .emacs to get you started

In Emacs, user configuration code goes into a file called `.emacs` in
your home directory. This includes settings about colour themes,
installed packages, keybindings, and whatever else you need to set Emacs
up just the way you like. Emacs will loads this file automatically on
start-up.

As a new Emacs user, you won’t have a `.emacs` yet, but if you want a
bit of help getting started you can use the one in the `extras`
directory. To use it to get started, there are two things to do:

1.  copy `extras/.emacs` into your home directory
2.  change the `extempore-path` variable to point to your Extempore
    source directory.

After step 1, when you start Emacs it will automatically open up the
`.emacs` file at the place where `extempore-path` is defined. You can
then change it to suit your setup (you only have to do this once) and
you’re good to go. Also, don’t worry if Emacs takes a while to load
after you do this. On the first run-through it’ll need to install some
packages, but after that subsequent start-ups shouldn’t take too long.

## Genral Emacs notes

Emacs is powered by a programming language called Emacs Lisp (elisp for
short). In Emacs, everything you do—every key you press, every shortcut
you invoke—is actually calling an elisp function. You can think of Emacs
as a (very specialised) elisp interpreter, on top of which sits millions
of lines (30+ years worth) of elisp code for editing files, interacting
with the OS, and doing all sorts of weird and wacky things.

In Emacs documentation, you’ll often see something like `M-x
load-theme` (pronounced *meta x, load theme*). To trigger this command,
press the **meta** key (which will probably be `alt` or `option` on a
modern keyboard) and the `x` key at the same time, then (releasing both
those keys) type in the function name `load-theme`. The elisp function
`load-theme` will be called, and you’ll see a prompt in your echo area
which says `Load custom theme`, and you can specify (by name) the name
of the colour theme you want to load.

Shortcut keys, too, are ultimately just triggering elisp functions. Even
pressing the letter `e` on the keyboard in typing actually calls a
function called `self-insert-command` to put the `e` into the buffer
you’re typing in. And `M-x` is just a way of calling these elisp
functions by name.

The main reason to bring this up is that in some ways Emacs is not
dissimilar to Extempore. You (as a programmer) are interacting with a
running interpreter, giving commands which are evaluated, and the state
of the world is updated in response to these commands. Of course, in
lots of was Emacs and Extempore are very different, but it might be
helpful in terms of thinking about how the whole thing works.

# Emacs cheat sheet

*A note on Emacs keyboard shortcuts:* with a shortcut like `C-x C-f`,
press `C-x` and *then* `C-f`, so that the `x` is released before the `f`
is pressed (although the `ctrl` key *may be* held down the whole time).
In contrast, with a key sequence like `C-M-x`, press the `ctrl`, `meta`
and `x` keys simultaneously.

## File navigation

-   open file: `C-x C-f`
-   save file: `C-x C-s`
-   switch to buffer: `C-x b`, then the buffer’s name
-   split window horizontally: `C-x 2`
-   split window vertically: `C-x 3`
-   jump to other window (in split window setup): `C-x o`

## Cursor movement

In most Emacs situations, you can use *either* the regular arrow keys to
navigate, or the default Emacs navigation commands:

-   `C-f`: forward one character
-   `C-b`: backward one character
-   `C-n`: forward one line
-   `C-p`: backward one line

There are also lots of other ways to move around, including (but not
limited to)

-   beginning of line: `C-a`
-   end of line of line: `C-e`
-   search forward: `C-s`
-   search backward: `C-r`
-   set/unset mark (for highlighting): `C-<space>`

Mastering Emacs has a great post on [effective
editing](http://www.masteringemacs.org/reading-guide/).

## Editing

-   kill (cut): `C-w`
-   copy: `M-w`
-   yank (paste): `C-y`, then `M-y` to cycle through previous kills
-   kill rest of line: `C-k`

## Getting out of (Emacs) trouble

-   cancel: `C-g` (if you get into trouble)
-   help (on a *function*): `C-h f`, then function name
-   help (on a *variable*): `C-h v`, then variable name
-   info: `C-h i`, then browse through the menus

For further reading, I can recommend the [Emacs reading
guide](http://www.masteringemacs.org/reading-guide/) at
[masteringemacs.org](http://masteringemacs.org).
