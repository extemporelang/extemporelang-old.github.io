---
title: Extempore & Sublime Text 2
categories: tools sublime-text
---

[Sublime Text 2](http://www.sublimetext.com) (ST2) is a cross-platform
text editor which runs on OS X, Linux and Windows. It’s the ‘spiritual
successor’ to Textmate, and it has some cool features, a large variety
of plugins for different languages and tasks, and a great community.

I won’t cover all the basics of ST2 here, because the [(unofficial)
documentation](http://docs.sublimetext.info/en/latest/) does that pretty
well. Think of this post as a ‘cheat sheet’, for dipping back into to
refresh your memory when you just can’t remember how to do something off
the top of your head.

# Installing ST2

Installing ST2 is a piece of cake—there are binaries for all platforms
on the [download](http://www.sublimetext.com/2) page.

You’ll also need the [ST2 Extempore
plugin](https://github.com/benswift/extempore-sublime), which provides
syntax highlighting and some commands for connecting to and working with
a running Extempore process. To install the plugin, download the [plugin
files](https://github.com/benswift/extempore-sublime/zipball/master) (or
clone the repo) and unzip them into your [ST2 packages
directory](http://docs.sublimetext.info/en/latest/basic_concepts.html#the-packages-directory)
(put it in the top level, not in the `User` subdirectory).

# Hacking on Extempore code in ST2

These are the steps youll need to take to start hacking on a piece of
Extempore code in ST2:

1.  start ST2
2.  open up your favourite shell (e.g. Terminal on OS X or cmd.exe
    on Windows)
3.  `cd` into your Extempore directory and run `extempore`, e.g.
    `./extempore`
4.  open up an ST2 buffer with an Extempore file (the Extempore plugin
    should be loaded automatically when ST2 sees the `.xtm`
    file extension)
5.  connect to the running Extempore process with the `extempore
     connect` command, which you can call through the command palette
    (`Ctrl+Shift+P` on Windows/Linux or `Cmd+Shift+P` on OSX) or the
    menu bar (`Tools > Extempore > Connect`)

Then, to evaluate Extempore code, highlight the code you want to
evaluate and hit `evaluate region` (which by default is mapped to
`ctrl+e`, but you can remap it to some other keyinding by editing the ).

To restart the Extempore process, just `ctrl+c` in the terminal where
`extempore` is running to kill it, then start it up again.

For more detail on how to ‘program’ in Extempore, have a look at this
post on [interacting with the Extempore
compiler](2012-09-26-interacting-with-the-extempore-compiler.org).

# Known issues with the Extempore ST2 plugin

The syntax highlighting currently doesn’t cover a few edge cases—so if
you end up tinkering with `Extempore.JSON-tmLanguage` to fix anything
then I’d love it if you submitted a patch.

Also, `extempore_evaluate` currently requires **highlighting** the code
to evaluate. It would be nice if it would eval the top-level
s-expression if no region was highlighted. This is hopefully not too
tricky to add if you know a bit about how ST2 works.
