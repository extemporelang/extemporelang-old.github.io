---
title: Using xtlang modules for faster startup
alias: ["./2013-08-06-using-xtlang-modules-for-faster-startup.html"]
categories: xtlang modules
---

If you’ve done any Extempore programming which relies on large
libraries, such as the OpenGL library `libs/external/opengl.xtm` or the
instrument library `libs/external/instruments_ext.xtm`, you’ll know that
re-compiling these xtlang libraries every time you start Extempore can
be a pain. Wouldn’t it be awesome if you could save the compiled code to
a shared library and then just load it up without having to re-compile
each time?

# Creating a shared lib from compiled xtlang code

Well, good news—you can. Let’s make a basic xtlang function

``` {.extempore}
  (bind-val global_var i64 5)

  (bind-func triple_int
    (lambda (a:i64)
      (* a 3)))

  (bind-func test
    (lambda ()
      (printf "result = %lld\n" (triple_int global_var))))

  (test) ;; prints "result = 15"
```

If we compile this, we get the usual signatures printed to the log:
`Compiled triple_int >>> [i64,i64]*`, `Bound global_var >>> i64`, etc.
Hang on to these signature—we’ll need them later.

When the `triple_int` function is compiled, it’s stored in an LLVM
module which is maintained by the Extempore runtime. This is where all
the xtlang function definitions and global variables go after they’re
JIT-compiled by LLVM, ready to be called/accessed. However, this module
is in memory, and so it gets blown away when the Extempore process
finishes. What we want to do is export the definitions from Extempore’s
LLVM module and write them to a file so that we can load them up again
in the future.

Luckily, there’s a function in Extempore which does exactly that:

``` {.extempore}
  (llvm:export-module "/tmp/libtriple.bc")
```

This writes the LLVM bitcode for the current module (all currently
defined xtlang functions and variables) to the file `/tmp/libtriple.bc`.
I’ve given it the `.bc` extension because it’s LLVM bitcode, but you can
call it (and put it) anywhere you like.

The bitcode file is not a loadable shared library, though. To create
that, we need the `llc` compiler which is part of the LLVM compiler
suite. This is done outside Extempore using a shell command. To produce
a shared library from the bitcode file (make sure you replace
`/path/to/llvm-3.4.1/` with the right path for your machine)

``` {.bash}
  # Linux (with gcc):
  /path/to/llvm-3.4.1/bin/llc -O3 -relocation-model=pic -tailcallopt /tmp/libtriple.bc && gcc --shared -g /tmp/libtriple.s -o ./libtriple.so

  # OSX (with clang):
  /path/to/llvm-3.4.1/bin/llc -O3 -relocation-model=pic -tailcallopt /tmp/libtriple.bc && clang -O3 -dynamiclib -undefined dynamic_lookup /tmp/libtriple.s -o ./libtriple.dylib
```

The `llc` compiler produces an assembler (`.s`) file, which `gcc` or
`clang` then turns into a shared lib (`.so` on Linux or `.dylib` on
OSX).

# Telling extempore about the xtlang code in the shared lib

One final step remains—we need some way of telling Extempore about the
xtlang definitions in the shared lib. To do this, we create a header
file of sorts.

``` {.extempore}
  ;; load the shared lib
  (define libtriple
    (let ((platform (sys:platform)))
      (cond ((string=? platform "Linux") (sys:open-dylib "/path/to/libtriple.so"))
            ((string=? platform "OSX") (sys:open-dylib "/path/to/libtriple.dylib"))
            (else (print "Unknown platform: " platform)))))

  ;; tell Extempore about the types of the variables and closures
  (bind-lib-val libtriple global_var i64)
  (bind-lib-func libtriple triple_int [i64,i64]*)
  (bind-lib-func libtriple test [i32]*)
```

The definitions of `global_var`, `triple_int` and `test` are now given
with `bind-lib-func` and `bind-lib-val`. These special forms tell
Extempore about the types of the variables (including closures) which
are present in the `libtriple` lib we created earlier. You don’t have to
provide the xtlang source code (or values, in the case of
`bind-lib-val`, because the compiled definitions are present in the
shared lib.

Now, let’s see if it worked. Kill and restart Extempore, and evaluate
the ‘header’ code above (either manually or put it in a file and load it
with `sys:load`).

``` {.extempore}
  (test) ;; prints "result = 15"
```

Hooray!

Just a quick note for clarity: be aware that the dynamic library is
still made up of xtlang closures—not C ABI function calls. So you cannot
just bind to it like a ‘normal’ standard C library (i.e. with
`bind-lib`). Thus the need for `bind-lib-func`. This also means that you
can’t link to compiled extempore libraries from non-xtlang code.

# The xtlang standard library

For a real-world example of this type of pre-compilation of xtlang code,
have a look in `libs/xtm.xtm`. This is the xtlang ‘standard library’,
which we’re putting together. It includes all the essential libraries
required for working with music and graphics, and it loads *really* fast
(compared to re-compiling the xtlang libraries every time you start
Extempore).

We’re working towards a binary distribution for Extempore which includes
the `xtm.so` shared lib for each platform, which will make getting up
and running heaps easier. For the moment, if you’d like to make your own
standard library (just to make your life easier on startup), there are
instructions in the top of `xtm.xtm` on how to compile the standard lib
for yourself.

As always, if you get stuck, reach out on the mailing list. You might
even get a blog post as a reply :)
