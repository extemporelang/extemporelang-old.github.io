---
title: Hacking Extempore in vim
alias: ["./2014-11-07-hacking-extempore-in-vim.html"]
categories: vim tools
---

Extempore’s [vim](http://www.vim.org/) plugin can be found in the
`extras/` subdirectory. The plugin uses python to get the job done, so
it won’t work in `vi`. To install it, you can either `:source` it each
time, or put it in your vim plugin directory (probably a better option
if you’re going to be using it often).

The `extempore.vim` plugin file has a full list of the commands up the
top, but the most important ones (and their default keybindings) are:

-   `ExtemporeOpenConnection` (`<Leader>o`) connect this vim session to
    a running Extempore process
-   `ExtemporeCloseConnection` (`<Leader>O`) close the connection
-   `ExtemporeSendEnclosingBlock` (`<Leader>w`) send the current (i.e.
    where the cursor is) definition to Extempore
-   `ExtemporeSendSelection` (`<Leader>s`) send the current selection to
    Extempore
-   `ExtemporeSendEntireFile` (`<Leader>a`) send the current file to
    Extempore

Remember to have your terminal (where Extempore is running) somewhere
you can see it, since Extempore’s `stdout` will show up there (and not
in vim).

# Hacking on Extempore code in vim

These are the steps youll need to take to start hacking on a piece of
Extempore code in vim:

1.  start vim
2.  open up your favourite shell (e.g. Terminal on OS X or cmd.exe
    on Windows)
3.  `cd` into your Extempore directory and run `extempore`, e.g.
    `./extempore --device 2`
4.  in vim, create a new file or open an existing one (e.g. from the
    `examples/` subdirectory) and `:source` the extempore plugin (which
    is located by default in `extras/extempore.vim`)
5.  connect to the running Extempore process with
    `:ExtemporeSendEnclosingBlock` (or the `<Leader>w` keybinding)

Then, to evaluate Extempore code, position the cursor in (or highlight)
the code you want to evaluate and use the `ExtemporeSendEnclosingBlock`
command (which by default is mapped to `<Leader>w`).

To restart the Extempore process, just `ctrl+c` in the shell where
`extempore` is running to kill it, then start it up again (you’ll have
to reconnect vim to this new Extempore process).

For more detail on how to ‘program’ in Extempore, have a look at this
post on [interacting with the Extempore
compiler](2012-09-26-interacting-with-the-extempore-compiler.org).

# Known issues

The vim mode doesn’t yet support multiple connections or user-specified
host/port, but pull requests are welcome!

A big thankyou to Tim Mellor and others (including Garett Shulman) who
have contributed to the vim plugin.
