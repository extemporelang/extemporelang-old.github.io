---
title: DSP basics in Extempore
alias: ["./2012-06-07-dsp-basics-in-extempore.html"]
categories: xtlang audio
---

In Extempore you can write dynamic, hot-swappable DSP code. There is a
special function in the environment called (funnily enough) `dsp`. This
function is the audio output ‘sink’—the value returned by this function
is (basically) written directly to the audio driver and output as sound
through the computer speakers. Every audio sample (that is, at a rate of
44100Hz) this function is called with a few arguments:

``` {.extempore}
  (bind-func dsp
    (lambda (in:SAMPLE time:i64 chan:i64 data:SAMPLE*)
      (* .1 (sin (/ (* 2.0 3.1415               ;; 2pi(ish)
                       440.0                    ;; frequency (Hz)
                       (i64tof (% time 44100))) ;; time mod samplerate
                    44100.0)))))

  ; to let Extempore know that this function is the one 
  ; it should call to get the output audio samples
  (dsp:set! dsp)
```

This dsp takes as input:

-   `in`: the input audio sample, e.g. from the microphone
-   `time`: an `i64` representing the time
-   `chan`: another `i64` which represents the channel index (`0` for L,
    `1` for R, etc.). Extempore can handle any number of channels.
-   `data`: this is a *pointer* to a `SAMPLE` type (which is `float` by
    default), and can be used to pass arbitrary data into the
    `dsp` function.

By default, the type of `SAMPLE` is `float`, it’s just a type alias:

``` {.extempore}
(bind-alias SAMPLE float)
```

but it can also be `double`. See [this
post](./2013-11-15-changing-from-doubles-to-floats-in-audio_dsp.org) for
more info.

In the example above, I’m ignoring all of these arguments except for the
`time` argument, which I’m using to generate a simple sinusoid at 440Hz.
Note the use of the `convert` function, which converts `time` (which is
an `i64`) to whatever type `SAMPLE` is. But the cool thing is that like
all functions in Extempore, this `dsp` function can be redefined
on-the-fly, as long as the type signature stays the same. So, if I
change the `dsp` function to

``` {.extempore}
  (bind-func dsp
    (lambda (in:SAMPLE time:i64 chan:i64 data:SAMPLE*)
      (* 0.1 (convert (random) SAMPLE))))
```

then the output changes to white noise. This is the real power of xtlang
(and Extempore)—everything’s dynamic and modifiable at runtime, but it’s
also performant enough to do sample-level manipulation in the same
language and environment. So instead of the ugens (unit generators, e.g.
oscillators) being locked up in a different language to the control
language, it’s all mixed in together.

# Abstraction and higher-order functions

Let’s create some oscillators:

``` {.extempore}
  (bind-func osc_c
    (lambda (phase)
      (lambda (amp freq:SAMPLE)
        (let ((incr (* 2.0 3.1415 (/ freq 44100.))))
          (set! phase (% (+ phase incr) (* 2.0 3.1415)))
          (* amp (sin phase))))))
```

This `osc_c` function doesn’t return a primitive (int/float) value.
Rather, it returns a (pointer to a) closure, which is our ‘oscillator’
and takes an amplitude and a phase argument. This idiom is a useful one,
and comes up so much in xtlang code that by convention we give any
closure which returns another closure a `_c` suffix.

The type message printed by the compiler when we evaluate `osc_c` is
`Compiled osc_c >>> [[float,float,float]*,float]*`.[^1] See that the
return type of the `osc_c` function is `[float,float,float]*`: a pointer
to a closure which takes two `float` arguments and returns a `float`.
This is our oscillator, and we can use our `osc_c` function to create as
many oscillators as we need:

``` {.extempore}
  (bind-func dsp
    (let ((osc1 (osc_c 0.0))
          (osc2 (osc_c 0.0)))
      (lambda (in:float time:i64 channel:i64 data:float*)
        (cond ; play a 200Hz tone in the left ear
              ((= channel 0) (osc1 0.25 200.0)) 
              ; play a 300Hz tone in the right ear
              ((= channel 1) (osc2 0.25 300.0))
              (else 0.0)))))
```

The `phase` variable in each of our oscillator closures is how we
maintain state between calls to `osc1` or `osc2`. Each time the closure
is called, `phase` gets incremented (see the definition of `osc_c`
above), and because `phase` is bound within a let that is local to the
returned closure, each osc has its *own* `phase` value, so the
oscillators created by `osc_c` are independent. In the case above, they
are each called with different frequencies to produce sine tones of
different pitch for each ear. This is closures in action, and it’s an
example of how the ‘scheme-like’ aspect of xtlang can simplify the job
of maintaining state.

# This is not the end…

It doesn’t take much imagination to see that *much* cooler stuff can be
done in `dsp` than just playing two sine tones. AM synthesis, FM
synthesis, granular and wavetable synthesis, as well as sampling and
sample manipulation—these are all possible. I’ll explain in future blog
posts how some of those things could be done in xtlang. Also, I haven’t
even *touched* on the graphics capabilities of Extempore, but I’ll get a
chance to cover those soon.

Also, I should point out that there are heaps better/easier ways to
achieve a lot of this stuff in Extempore: named constants for samplerate
& 2pi, syntactic sugar, library support etc. If you’re interested, jump
on in and have a poke around (especially in `libs/core/audio_dsp.xtm`)
or even fork it and contribute a patch. The docs are a bit sparse at the
moment, but I’m hoping that this blog post (and more that will follow)
will be a helpful resource for those starting out. And if you *do* make
something cool, then throw it up on the interwebs and let me know :)

[^1]: again, this is because `SAMPLE` is aliased to `float` by default
