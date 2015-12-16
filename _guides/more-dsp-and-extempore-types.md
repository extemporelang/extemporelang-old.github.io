---
title: More DSP and more Extempore types
categories: xtlang audio
---

So this blog post builds on [the previous
one](http://benswift.github.com/blog/2012/06/07/dsp-basics-in-extempore/)
in covering Extempore’s real-time DSP capability. That post showed that
by writing our own `dsp` sink we could make any sound we
wanted—essentially writing raw audio samples to `/dev/audio`. It also
introduced some of the primitive types supported by the xtlang compiler:
ints and floats of various lengths, and showed the syntax for typing
variables in xtlang and interpreting the type signatures of xtlang
closures.

This post expands on that stuff by showing some of the ‘compound’
[types](2012-08-09-xtlang-type-reference.org) and data structures that
xtlang supports. These include tuples, arrays, and even vector types.
Just like in C, [xtlang can allocate and return
pointers](2012-08-13-understanding-pointers-in-xtlang.org) to chunks of
memory, which in a DSP context can be used for, say, delay buffers or
wavetable synthesis, but also allows for number crunching with speed
comparable to C (so *orders of magnitude* faster than dynamically typed
interpreted language like scheme).

If you’ve just stumbled in from the interwebs, I suggests reading the
previous post linked above—it covers a lot of the basics of xtlang which
I’ll assume in this post. And again, if you’re the type of person who
isn’t satisfied until you’ve seen all the gory details, the full source
for the Extempore project is [up on
github](http://github.com/digego/extempore) for your enjoyment.

# Beyond pure tones

But playing a single sine tone is boring. Remember how [last
time](http://benswift.github.com/blog/2012/06/07/dsp-basics-in-extempore/)
we made some oscillator closures? Sure you do. Well, instead of just
using the *one* oscillator, let’s use a few of them to generate a whole
bunch of sine tones of different frequencies:

``` {.extempore}
  (bind-func osc_c ; osc_c is the same as last time
    (lambda (phase)
      (lambda (amp freq)
        (let ((inc:SAMPLE (* 3.141592 (* 2.0 (/ freq 44100.0)))))
          (set! phase (+ phase inc))
          (* amp (sin phase))))))

  ; remember that the dsp closure is called for every sample
  ; also, for convenience, let's make a type signature for the
  ; DSP closure

  (bind-alias DSP [SAMPLE,SAMPLE,i64,i64,SAMPLE*]*)

  (bind-func dsp:DSP ; note the use of the type signature 'DSP'
    (let ((osc1 (osc_c 0.0))
          (osc2 (osc_c 0.0))
          (osc3 (osc_c 0.0)))
      (lambda (in time channel data)
        (cond ((= channel 1) 
               (+ (osc1 0.5 220.0)
                  (osc2 0.5 350.0)))
              ((= channel 0)
               (osc3 0.5 210.0))
              (else 0.0)))))
```

See how the `let` ‘outside’ the `lambda` sets up the three oscillators,
then the `lambda` closes over them and so each time the oscillator is
called increments its `phase` value appropriately?

Any number of oscillators (think of them as unit generators) can be
bound and added in this way—this allows us to do additive synthesis.
Having to define and refer to each osc individually doesn’t scale up
very well, though, so it would be great if we could create and
initialise them programmatically. This brings us to a couple of new
(compound) types: tuples, and arrays.

# Tuples in xtlang

Tuples in xtlang are similar to tuples in other languages. They are
heterogeneous groupings of any xtlang types (just like a C struct).
They’re still statically typed, either explicitly or with the types
inferred from the types of other variables and literals. Syntactically,
tuples use angle brackets (`<>`).

When programming in xtlang you don’t really ever deal with tuples
directly—you deal with them by *reference* through pointers. There are
no ‘literals’ for tuples either—you can’t just go

``` {.extempore}
  (bind-func tuple_maker
    (lambda (a:i64)
      (let ((tup:<i64,i64> <a,a>))
        tup)))

  ; Compiler Error: unbound symbol: <a,a>
```

Instead, this time in the `let` we get a pointer to a tuple through a
call to `alloc`.

``` {.extempore}
  (bind-func tuple_maker
    (lambda (a:i64)
      (let ((tup:<i64,i64>* (alloc)))
        (tset! tup 0 a)
        (tset! tup 1 a)
        tup)))
```

This post goes into more detail about [memory management in
Extempore](2012-08-17-memory-management-in-extempore.org), but for now
the key point is that the call to `alloc` returns a *pointer* to a tuple
of the specified type.

Notice the `tset!` function, which is an alias for `tuple-set`. This
function takes three arguments: a pointer to a tuple (in the case above,
that’s `tup`), an `i64` (0-based) index for specifying which ‘slot’ in
the tuple we’re setting, and finally the value to set it to (which must
be of the appropriate type, otherwise you’ll get a type error).

This new version of `tuple_maker` compiles—hooray! The type signature
printed by the compiler is `Compiled tuple_maker >>>
[<i64,i64>*,i64]*` and the type of `tuple_maker` is a pointer (denoted
by the `*`) to a closure (denoted by the `[]`) which takes one `i64`
argument and returns a pointer to a tuple of two `i64` values.

Just to check that everything’s working properly, let’s write a little
`test` function

``` {.extempore}
  (bind-func test
      (lambda (a:i64)
        (let ((tup (tuple_maker a)))
          (printf "<%d,%d>\n"
                  (tref tup 0)
                  (tref tup 1))
          tup)))

  (test 4) ; prints <4,4> (as it should!)
```

Tuples come in handy in lots of places, for instance we can use them to
rewrite one of the `dsp` functions from earlier (the one with the three
oscs)

``` {.extempore}
  (bind-alias osc_t [SAMPLE,SAMPLE,SAMPLE]*)

  (bind-func dsp:DSP
    (let ((osc_tuple:<osc_t,osc_t,osc_t>* (alloc)))
      (tfill! osc_tuple (osc_c 0.0) (osc_c 0.0) (osc_c 0.0))
      (lambda (in time channel data)
        (cond ((= channel 1) 
               (+ ((tref osc_tuple 0) 0.5 300.0)
                  ((tref osc_tuple 1) 0.5 420.0)))
              ((= channel 0)
               ((tref osc_tuple 2) 0.5 600.0))
              (else 0.0)))))
```

This time, instead of binding each osc to its own symbol (`osc1`, `osc2`
and `osc3`), we created `osc_tuple`, a (pointer to a) tuple, which held
all the oscs. We filled it with `tfill!`, which takes as a first
argument the pointer to the tuple, and then enough additional arguments
to fill out the tuple. Equivalently, we could have set each element in
the tuple manually with `(tset! osc_tuple 0 (osc_c
0.0))` etc.

Also, the use of `bind-alias` is helpful here, because it allows us to
condense the verbose type of the closure oscs
(`[SAMPLE,SAMPLE,SAMPLE]*`) down to the more manageable `osc_t`, handy
when we then need to type the `osc_tuple` with three of them.

There’s no reason why the types in the tuple have to be the same.
Indeed, usually they won’t be—tuples allow us to define more complex
data structures which are suitable for the task at hand.

# Arrays in xtlang

If tuples are xtlang’s structs, then arrays are (funnily enough)
xtlang’s arrays. Unlike tuples, which can be composed of heterogeneous
xtlang types, arrays are homogeneous (like a C array). The elements of
the array can be tuples, closures, or any valid xtlang type.
Syntactically, arrays are marked by pipes (`|`). Again, we access and
manipulate arrays through pointers returned by calls to the various
memory allocation functions (e.g. `alloc`). Instead of `tref` and
`tset!` (which we used for tuples), we use `aref` and `aset!`.

So, to bring this discussion back to the practical art of noise-making,
let’s create a `dsp` function which makes use of arrays and tuples to do
some additive synthesis. We’ll make an array `osc_array`, and then two
more arrays (`amp_array` and `freq_array`) to keep track of the
amplitude and frequency values.

``` {.extempore}
  (bind-func dsp:DSP
    (let ((osc_array:|30,[SAMPLE,SAMPLE,SAMPLE]*|* (alloc))
          (amp_array:|30,SAMPLE|* (alloc))
          (freq_array:|30,SAMPLE|* (alloc))
          (i 0))
      ; initialise the arrays
      (dotimes (i 30)
        (aset! osc_array i (osc_c 0.0))
        (aset! amp_array i (+ 0.2 (* 0.2 (random))))
        (aset! freq_array i (+ 110.0 (* 1000.0 (random)))))
      ; this is the dsp closure
      (lambda (in time chan data)
        (cond ((= chan 0) ; left channel
               (let ((suml 0.0))
                 (dotimes (i 15) ; sum over the first 15 oscs
                   (set! suml (+ suml ((aref osc_array i)
                                       (aref amp_array i)
                                       (aref freq_array i)))))
                 (/ suml 15.0))) ; normalise over all oscs
              ((= chan 1) ; left channel
               (let ((sumr 0.0))
                 (dotimes (i 15 15) ; sum over the first 15 oscs
                   (set! sumr (+ sumr ((aref osc_array i)
                                       (aref amp_array i)
                                       (aref freq_array i)))))
                 (/ sumr 15.0)))
              (else 0.0))))) ; any remaining channels
```

This code is a bit more complex than the previous examples. Initially,
pointers to the three arrays (for the oscs, the amps and the freqs) are
set up in the `let`, then a `dotimes` goes through and sets them up with
the relevant data. The amplitudes and frequencies are chosen at random
(within sensible ranges). After the arrays have all been initialised in
the `dotimes`, the dsp `lambda` sums the output from the oscillators
(the first 15 oscs for the left channel and the last 15 oscs for the
right channel). That’s why the second `dotimes` takes an extra value in
the parens, this is an initial value (which defaults to zero) for the
loop variable to be bound to.

Remember that everything can be JIT-compiled whenever you like, so each
time the `dsp` closure is re-evaluated new random values will go into
the amp and freq arrays, and the additive `dsp` function will make a
different sound which you’ll hear straight away.

Now, choosing these values at random doesn’t necessarily lead to the
most musical results, so it’s a good idea to choose them in some sort of
systematic way. In our last example, we’ll play only the *even*
harmonics of a given base frequency (I’ve also simplified the output to
one channel for clarity).

``` {.extempore}
  (bind-func dsp:DSP
    (let ((osc_array:|30,[SAMPLE,SAMPLE,SAMPLE]*|* (alloc))
          (amp_array:|30,SAMPLE|* (alloc))
          (freq_array:|30,SAMPLE|* (alloc))
          (base_freq 110.0)
          (i 0))
      ; initialise the arrays
      (dotimes (i 30)
        (aset! osc_array i (osc_c 0.0))
        (aset! amp_array
               i
               (if (= (/ i 2) 0)
                   0.3
                   0.0))
        (aset! freq_array i (* (convert (+ i 1) SAMPLE) base_freq)))
      (lambda (in time chan data)
        (let ((sum 0.0))
          (dotimes (i 30)
            (set! sum (+ sum ((aref osc_array i)
                              (aref amp_array i)
                              (aref freq_array i)))))
          (/ sum 30.0))))) ; normalise over all oscs
```

See how we’re using the same arrays as last time (for osc, amp and freq)
but instead of randomly picking frequencies and amplitudes, we’re
generating a harmonic series with a fundamental of 110Hz, and only
playing the even harmonics (check the equality test in the
initialisation of `amp_array`). For fun, change that equality test to an
inequality test (`<>`) and listen to the result!

# Knock yourselves out

So the examples in this post are hopefully beginning to flesh out the
claims I made [last
time](http://benswift.github.com/blog/2012/06/07/dsp-basics-in-extempore/)
about being able to do real-time DSP in Extempore. Again, I know that
this might seem like reinventing the wheel, building all the oscillators
from scratch. There are xtlang libraries for all of this, so there’s no
need to mess around with the low-level synthesis stuff if you don’t want
to. But the point is that you *can*, and it’s all hot-swappable, and
written in the same language and environment that you use even if you
just want to trigger pre-made instruments. These examples show how to do
things from first principles, but feel free to mess around at whatever
level of abstraction tickles your creative fancy.
