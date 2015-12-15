---
layout: docs
title: Binding to C libs
categories: xtlang C-interop
---

There’s a *lot* of useful C library code out there. Sometimes there
*are* good reasons to write things from scratch, but other times you
find the exact thing you’re looking for on GitHub and you just want to
link against it and then go home to watch the footy.

Most languages (both high and low-level) provide some sort of [foreign
function
interface](http://en.wikipedia.org/wiki/Foreign_function_interface)
(FFI) to C. Different languages provide this functionality in different
ways, but at the end of the day the aim is to be able to call external C
code from within the language, and xtlang provides a way to do this.

The reasons for binding to C library code are different in xtlang than
they are in other languages, particularly ‘scripting’ languages like
Perl or Ruby. In those languages, it’s often for performance
reasons—there’s a certain hot loop of the code, and rewriting it in C
can give a huge performance win. xtlang, on the other hand, is *already*
high performance, because of its native compilation via LLVM, so
rewriting bits in C isn’t usually of any benefit. The main reason you’ll
want to call C code from xtlang, then, is to take advantage of existing
libraries.

# xtlang-C interaction

In some ways, mixing xtlang code and C code is easy. The type system is
quite similar: all of xtlang’s floats and ints have a C counterpart
which is exactly the same. Tuples are the same as C structs, and
xtlang’s arrays are the same as C arrays. These type equivalences aren’t
just conceptual or semantic—they’re the exact same bit patterns in
memory.

Also, both languages have pointer types, and deal with manual memory
management via pointers. Both have a static type system which allows the
compiler to throw errors at compile time if the types don’t all match
up. So there are some good reasons why C and xtlang should play nicely
together.

Having said that, there are some key differences between C and xtlang. C
is the archetype of the ‘static language’, while xtlang is designed to
allow the programmer to redefine core parts of the program *while it is
executing* (see the post on [Extempore’s
philosophy](2012-08-07-extempore-philosophy.org) for more details).
Extempore supports REPL-style development, with the programmer
interacting with the source code, evaluating and compiling parts of it
in a non-linear fashion, and then modifying and recompiling it as
necessary. There are a few quirky projects which allow this type of
development with C, but in general you build the whole project, then
ship the resulting binary.

So how *does* xtlang support binding and calling C code dynamically from
xtlang code? The basic answer is though shared (dynamic) libraries.[^1]
To call a C library from xtlang involves creating an xtlang
‘header’,[^2] which lets the xtlang compiler know about the types and
function signatures in C library’s header. Extempore can then load the
shared library, xtlang can call functions in the library, and it should
all be peaches.

# Foolib: the world’s most useless C library

Let’s consider a really simple example. Say we have a C library which
only defines *one function* called `foo`. This library (foolib) will
have a header

``` {.c}
  /* foolib.h */

  int foo(int bar);
```

and an implementation

``` {.c}
  /* foolib.c */

  int foo(int bar){
    return bar + 42;
  }
```

Not the most useful library in the world, to be sure, but let’s compile
it as a shared library anyway. Shared libraries have different file
extensions on the different platforms that Extempore runs on:

-   **OSX**: `sharedlib.dylib`
-   **Linux**: `sharedlib.so`
-   **Windows**: `sharedlib.dll`

If you compile the library yourself, in general you should get the right
type of binary for your platform.[^3] If you’ve just downloaded the
`.dylib` (or `.so`, or `.dll`) from the interwebs, though, you need to
be careful that the binary file was compiled for the platform you’re on.
It’s not just a matter of renaming the file to right file extension,
either: the guts of the file are different between the platforms as
well.

I’m writing this blog post on OSX, so I’ll refer to libraries with the
`.dylib` extension, but just substitute in the appropriate extension for
your platform. To build the shared library on OSX, move into foolib’s
directory and build it with the `-dynamiclib` flag:

``` {.bash}
clang foolib.c -dynamiclib -o foolib.dylib
```

`clang` is a C compiler that’s part of the LLVM project. I could also
have used `gcc` or some other compiler.

After running the above command, the file `foolib.dylib` will appear in
the directory—a binary file which contains the instructions for how to
perform the functions provided by the library (in this case just the
function `foo`). This is the *shared* or *dynamic* library.

Once the shared library is compiled, the only thing to do before it’s
callable from xtlang code is to tell xtlang compiler about the type
signature of the functions in the library. To do this, we use
`bind-lib`.

``` {.extempore}
  ;; foolib.xtm -- an xtm header for foolib

  ;; load the shared lib
  (define foolib (sys:open-dylib "foolib.dylib"))

  ;; define foo as a function
  (bind-lib foolib foo [i64,i64]*)

  ;; test that everything worked ok
  (bind-func foo_test
    (lambda (x)
      (printf "foo(x) = %lld\n" (foo x))))

  (foo_test 6) ;; prints "foo(x) = 48"
```

`sys:open-dylib` is the Extempore interface for loading shared
libraries. To find the library, it first looks for one of that name in
the directory in which the Extempore process is running. After that,
it’ll look on your system’s library path.[^4] `sys:open-dylib` has a
return value, which in the example above is bound to the symbol
`foolib`. It’s important to capture this return value, because we’ll
need it shortly with `bind-lib`.

In `foolib.xtm` (above), `bind-lib` is really only declaring that “there
is a C function called `foo` in the shared library `foolib`, and it
takes one `i64` argument and returns an `i64`.

But hang on a sec—if `foo` is a C *function*, why does it have the type
signature (square brackets) of an xtlang *closure*?[^5] Well, this is a
bit of a cheat on xtlang’s part—the bound function `foo` *is* just the
plain C function from the library. But we do have to specify its type
signature (argument and return types), and because xtlang doesn’t
provide a syntax for functions (only closures), then `bind-val` just
takes a closure signature and interprets it as a function signature
(which are the same).

It really is just a C function, though, and there is **no performance
penalty** for calling C functions in xtlang code. This is because
there’s no wrapper functions or anything like that that have to operate
as a bridge between the xtlang code, and the argument and return types
have exact (bit-identical) xtlang counterparts, so there’s really no
hard work to do (in contrast to higher level languages, which have to
worry about boxing/unboxing numeric types, for example).

# KissFFT: a more useful library

As a more useful example, let’s look at the library `fft.xtm` in the
`libs/external` directory which comes with Extempore. `fft.xtm` uses the
excellent [KissFFT](http://sourceforge.net/projects/kissfft/) library
for doing [Fourier
transforms](http://en.wikipedia.org/wiki/Fourier_transform). The library
is quite small and clean, and is spread over only a few source files—the
main ones being `kiss_fft.h` & `kiss_fft.c`. There’s gonna be a bit of C
in this section. Nothing too complicated, but if you’re rusty it might
be worth picking up a copy of
[K&R](http://www.iu.hio.no/~mark/CTutorial/CTutorial.html) or your to
flip through if necessary.

If you’re playing along at home, then you’ll need to download the
[source](http://sourceforge.net/projects/kissfft/files/kissfft/), build
the `kiss_fft.dylib` library and put it somewhere that `sys:open-dylib`
will find it. The `fft.xtm` header has some instructions on how to do
this.

After that’s done, then it’s a matter of providing `bind-lib` xtlang
definitions which tell Extempore about the functions in
`kiss_fft.dylib`. But how do we know what those functions are? Well, we
need to look at the `kiss_fft.h` header file.

A Fourier transform (FT) “expresses a mathematical function of time as a
function of frequency, known as its frequency spectrum” (from
[Wikipedia](http://en.wikipedia.org/wiki/Fourier_transform)). But don’t
worry if you don’t understand the maths behind the FT for the purposes
of this example, just know that we want to give it a buffer of input
values and have it give us back a buffer of transformed output values.
Looking through the header, it’s clear that the function we call to do
this is `kiss_fft`.

``` {.c}
  /*
   * kiss_fft(cfg,in_out_buf)
   *
   * Perform an FFT on a complex input buffer.
   * for a forward FFT,
   * fin should be  f[0] , f[1] , ... ,f[nfft-1]
   * fout will be   F[0] , F[1] , ... ,F[nfft-1]
   * Note that each element is complex and can be accessed like
      f[k].r and f[k].i
   * */

  void kiss_fft(kiss_fft_cfg cfg,const kiss_fft_cpx *fin,kiss_fft_cpx *fout);
```

The function `kiss_fft` returns `void` (doesn’t return a useful value)
and takes three arguments:

-   `cfg` (of type `kiss_fft_cfg`)
-   `fin` (of type `kiss_fft_cpx*`)
-   `fout` (also of type `kiss_fft_cpx*`)

This header file is well commented, and it’s clear that

-   `cfg` is some configuration data for the algorithm
-   `fin` should be a pointer to our input buffer
-   `fout` should be a pointer to the output buffer

Why do we pass a pointer to the output buffer in to the function? If we
already know what the output is, why are we calling the function at all?
The answer (and the clue is in the fact that the function returns
`void`) is that `fout` should point to a buffer where `kiss_fft` will
store the output values. Whatever data is in that buffer before the
function is called will be overwritten, so it had better not be
important.

Why is the library written this way? Well, one of the key benefits of
this “pass in a location for the answer to be written to” approach is
that the memory with the answer in it can be managed by the calling
function (that is, the function which calls `kiss_fft`). As discussed in
the [memory management
post](2012-08-17-memory-management-in-extempore.org), the explicit
nature of memory allocation and deallocation in xtlang (and in C) gives
the programmer great control over the lifetime of any memory the program
allocates. The function which *calls* `kiss_fft` will have a much better
idea of what it wants to do with the output values than `kiss_fft` does,
so it makes sense to have this calling function allocate some memory of
the appropriate size and type, and then just pass in a pointer to this
memory in `fout`.

So now we can just go ahead and turn the signature of `kiss_fft` into a
`bind-lib` and we’re done, right? Something like ([remembering
that](2012-08-09-xtlang-type-reference.org) xtlang uses `i8*` in place
of C’s `void` type)

``` {.extempore}
  (define kissfft (sys:open-dylib "kiss_fft.dylib"))

  (bind-lib kissfft kiss_fft [i8*,kiss_fft_cfg,kiss_fft_cpx*,kiss_fft_cpx*]*)
```

But then when we try and evaluate the `bind-lib`, the compiler throws an
error:

``` {.bash}
Compiler Error: cannot find type for "kiss_fft_cfg"
```

Ah, Extempore can’t recognise the type signature for `kiss_fft` without
knowing about all its argument and return types as well. So, let’s dive
back into the `kiss_fft.h` header file to find the declaration of
`kiss_fft_cfg`.

``` {.c}
  /* in kiss_fft.h */

  typedef struct kiss_fft_state* kiss_fft_cfg;
```

So it seems that `kiss_fft_cfg` is actually `typedef`’ed as a pointer to
the struct `kiss_fft_state`. A `typedef` is just like a `bind-alias` in
xtlang: the compiler doesn’t know anything about it, it just looks like
the type it points to. So the function `kiss_fft` is really expecting
`kiss_fft_state*` to be the type of its first argument. We need to find
the definition of *this* type.

Hmm, it’s not in `kiss_fft.h`. A look in *all* the header files in the
KissFFT source directory (with `grep kiss_fft_state *.h`) reveals that
it’s actually defined in `_kiss_fft_guts.h`.

``` {.c}
  /* in _kiss_fft_guts.h */

  struct kiss_fft_state{
      int nfft;
      int inverse;
      int factors[2*MAXFACTORS];
      kiss_fft_cpx twiddles[1];
  };
```

So the `kiss_fft_state` struct has four members:

-   `nfft` (an `int`)
-   `inverse` (an `int`)
-   `factors` (an `int` array of length `2` ×=MAXFACTORS=)
-   `twiddles` (a `kiss_fft_cpx` array of length `1`)

Earlier in that header `MAXFACTORS` is defined to be 32, so the `factos`
array will be of length `64`. Also, in `twiddles`, the `kiss_fft_cpx`
type is new—we haven’t found a definition for it yet. So we need to do
that before we can tell the xtlang compiler about the `kiss_fft_state`
struct.

The `kiss_fft_cpx` definition is back in `kiss_fft.h`

``` {.c}
  /* in kiss_fft.h */

  #ifdef FIXED_POINT
  #include <sys/types.h>  
  # if (FIXED_POINT == 32)
  #  define kiss_fft_scalar int32_t
  # else  
  #  define kiss_fft_scalar int16_t
  # endif
  #else
  # ifndef kiss_fft_scalar
  /*  default is float */
  #   define kiss_fft_scalar float
  # endif
  #endif

  typedef struct {
      kiss_fft_scalar r;
      kiss_fft_scalar i;
  }kiss_fft_cpx;

  typedef struct kiss_fft_state* kiss_fft_cfg;
```

`kiss_fft_cpx` is itself a struct with two values, `r` and `i`, which
are both of type `kiss_fft_scalar`. Looking at the top part of that
code, the type of `kiss_fft_scalar` depends on how the library was
compiled (all those `#ifdef` checks are performed at compile time). In
this case (and you can either trust me or check for yourself), we didn’t
pass any options for a fixed-point version of the library or anything
special, so `kiss_fft_scalar` will have the ‘default’ type of `float`.

`kiss_fft_cpx` is therefore a struct of two floats. This makes sense
given our knowledge of what the struct is designed to represent: a
complex number. The two `float` members are for the real (`r`) and
imaginary (`i`) part of the complex number.

Now, finally, we know all the types we need to call `kiss_fft`. We just
need to tell the xtlang compiler about them.

``` {.extempore}
  ;; in fft.xtm

  (bind-type kiss_fft_cpx <float,float>)
  (bind-type kiss_fft_state <i32,i32,|64,i32|,|1,kiss_fft_cpx|>)
  (bind-alias kiss_fft_cfg kiss_fft_state*)

  (bind-lib kissfft kiss_fft [i8*,kiss_fft_cfg,kiss_fft_cpx*,kiss_fft_cpx*]*)
```

See how each struct in C gets bound as a type in xtlang? If you don’t
believe me, go and have a look at the struct definitions above—they
should match up perfectly. We can now create tuples of type
`kiss_fft_cpx` in xtlang just like we would any other tuple, and in fact
we’ll *have to* if we want to actually call the functions from the
library.

So after all this detective work, finding and declaring the appropriate
type signatures, the above code finally compiles:

``` {.bash}
Bound kiss_fft_cpx >>> <float,float>
Bound kiss_fft_state >>> <i32,i32,|64,i32|,|1,kiss_fft_cpx|>
Aliased kiss_fft_cfg >>> kiss_fft_state*
Bound kiss_fft >>> [i8*,kiss_fft_cfg,kiss_fft_cpx*,kiss_fft_cpx*]*
```

There are a few more functions in the actual `fft.xtm` file which I
haven’t included in this post: helper functions for setting up the
`kiss_fft_cfg` struct, determining efficient FFT stride lengths and
other things like that. You don’t have to `bind-lib` all the functions
in the library, just the ones you need, although knowing which ones
sometimes more of an art than a science. If the library has a well
defined
[API](http://en.wikipedia.org/wiki/Application_programming_interface)
then it might be clear exactly how to get what you want out of the
library, but sometimes it just takes a bit of digging around and looking
at the code. In general, the approach I’ve taken here of “find the
function you want to call first, then work backwards to define all the
necessary types and helper functions” is probably not a bad one.

# The external directory

If you’ve looked around the extempore `examples` or `libs` directory,
you might have noticed that there are `core`, `external` and `contrib`
subdirectories in each one. The reason for the core/external distinction
is that any `.xtm` file which doesn’t require binding to an external C
library goes in `core`, and any `.xtm` file that *does* call into a
shared library goes in `external`. `contrib` is for platform-dependent
things, such as the Kinect library.

Everything in these folders is honest-to-goodness xtlang code just like
you could write yourself, and if you want to change anything in these
libraries you can do it on the fly, just as you can with any other
xtlang code. This is pretty cool—there’s something exciting about being
able to hack on the standard library while your code is running.

They’re also a great place to explore and get ideas for your own xtlang
code. And if you do end up writing a cool library (or xtlang bindings
for a cool C shared library) then submit a pull request and we’ll see if
we can get it included in the main Extempore distribution.

[^1]: To recap, C libraries can either be statically compiled into an
    application, or dynamically linked in at run-time. There are pros
    and cons to both approaches, and so C libraries can be compiled
    either statically or dynamically (by setting a compiler flag).

[^2]: I’m using the term *header* in quotes because it doesn’t have to
    be its own source file, there are no restrictions on naming, etc.
    It’s just regular xtlang code that needs to be evaluated before you
    can use the functions in the library.

[^3]: It is possible to compile on one platform for a different target
    platform—this is called *cross-compiling*. But if you know how to do
    that then you don’t need these instructions anyway.

[^4]: =sys:open-dylib= doesn’t do anything clever, just tries to find
    and load a shared library of the name you gave it. So it’s up to you
    to make sure that the library exists, and is of the right type for
    your platform.

[^5]: See [this post](2012-08-09-xtlang-type-reference.org) for more
    detail on the differences between closures and functions.
