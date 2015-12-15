---
title: Changing from doubles to floats in audio_dsp
alias: ["./2013-11-15-changing-from-doubles-to-floats-in-audio_dsp.html"]
categories: xtlang audio
---

Up till now, the type signature of the `dsp` closure (which we give the
alias `DSP` for convenience) has been

``` {.extempore}
(bind-alias DSP [double,double,double,double,double*]*)

(bind-func dsp:DSP
  (lambda (in time chan dat)
    (cos (/ (* TWOPI time 440.0) SR))))
```

The `dsp` function here takes four arguments:

-   `in`: a `double` representing audio input (if your hardware has an
    input device)
-   `time`: a `double` representing the global time value (in the `dsp`
    function above we use this time argument to generate a sine wave
-   `chan`: a `double` indicating the channel (`0.0` for the first
    channel, `1.0` for the second, etc.)
-   `data`: a `double*` (pointer to a buffer of doubles) which can be
    used to pass arbitrary data into the `dsp` function

The `dsp` function then returns a `double` value (in the range
\[-1.0,1.0\] which is basically sent straight out through the speakers.
When `dsp` is called once per audio sample (e.g. at 44100kHz) this
generates the audio output that you hear through your speakers.

Since Extempore is a 64-bit environment, up until this point all the
audio and DSP infrastructure has used 64-bit (double precision) floats
for each audio sample. Although the extra precision isn’t strictly
necessary[^1], Extempore’s numerical performance is good enough that
there hasn’t been a real cost to the extra precision, coupled with the
fact that the 32-bit vs 64-bit precision performance issue is [kindof
complicated](http://stackoverflow.com/questions/4584637/double-or-float-which-is-faster)
anyway on a 64-bit CPU.

# Changing from doubles to floats

There is one scenario where the smaller 32-bit floats are clearly
beneficial is with [SIMD operations](http://en.wikipedia.org/wiki/SIMD)
through xtlang’s vector type. Going into the details is a job for
another time, but basically it allows us to vectorise the audio stuff to
be more efficient, which is handy for computationally expensive things
like convolution reverb.

So we’ve made the decision to change extempore’s `dsp` callback to use
`floats` by default. Actually, we’ve added a new type alias in
`runtime/xtlang.xtm`

``` {.extempore}
(bind-alias SAMPLE float)
```

So the `DSP` alias in `libs/core/audio_dsp.xtm` is now

``` {.extempore}
;;;;;;;;;;;;; this for audio ;;;;;;;;;;;;;;;;;;
;; THESE ALL NEED TO BE CHANGED TOGETHER!!!!
(bind-alias SAMPLE float)
(bind-func audio_64bit (lambda () #f))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
```

You’ll notice that the second (`time`) and third (`chan`)arguments are
now `i64`, rather than `double` as they were before. This is because
`time` has always been an `i64` under the covers, but was cast to a
`double` to allow it to be easily used in signal processing stuff (as in
the `cos` example earlier at the beginning of this post). `chan` was
initially a double for this reason as well, but in practice you’re
usually testing for equality, and testing for equality on floats
(particularly single precision ones) is always a bit dicey, so we ended
up doing things like `(< chan 0.5)` to select channel 1.

So, we figured that while we were breaking things anyway with the `DSP`
type signature, we might as well fix up those other things too.

# Polymorphic math.h functions

Part of the reason that the old `DSP` signature was all doubles was
because `cos`, `sin`, `pow` etc. all took `double` arguments (the float
versions were `cosf`, `sinf` and `powf`), so it was nice if everything
was a `double`. To alleviate this pain, we’ve now made all those math.h
functions polymorphic over `float` and `double`. The plain un-poly’d
ones are still there in e.g. `cosf` and `cosd`, but if you call `cos`
with *either* a `float` or `double` argument then the xtlang compiler
will figure out which of `cosf` and `cosd` to call.

There’s also now a special `convert` function which takes a variable and
a type to convert *to*, and tries to figure out which specific
conversion to make based on the variable’s type.

``` {.extempore}
  (bind-func convert_test
    (lambda ()
      (let ((double_var 4.0))
        (cos (convert double_var SAMPLE)))))

  ;; note that convert_test returns a float
  ;; Compiled convert_test >>> [float]*
```

See how we used the `SAMPLE` alias, and the `cos` function knew to apply
the `dtof` conversion so that the output from the closure is a `float`?
Armed with `convert`, much of the pain of mixing `double` and `float`
vars can be taken away.

Sometimes the compiler doesn’t have enough information to figure out
what the type of the variable is, and in those cases you can still to
cast from `float` to `double` with `dtof` & `ftod`.

# What this means for Extempore users

You can change the `SAMPLE` alias back to `double`, and make sure you
update the the `audio_64bit` closure as well. Even then, though, your
existing code will be broken because of the `double` to `i64` change to
`time` and `chan`. So unless you’ve got a good reason to stick with
`doubles`, moving to `floats` is a good idea. It’ll mean that you can
take advantage of the new vectorised audio stuff, including the cool
`examples/external/convolution_reverb.xtm`.

We’ve updated all the libs and examples to use the `SAMPLE` alias, so
they should be all up-to-date. If you have any issues let us know on the
[mailing list](mailto:extemporelang@googlegroups.com). If you have old
audio code (basically anything which uses the `dsp` callback) then it’ll
be broken, but should be easy to update.

I’m also going back through all my old blog posts to make the change as
well. I’m probably going to miss things, though, so if there’s stuff
that’s now broken I apologise. Feel free to leave a comment and I’ll fix
it up.

[^1]: although it does help in some cases with numerical stability in
    filters, etc.
