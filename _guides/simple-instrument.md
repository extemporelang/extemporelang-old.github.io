---
layout: docs
title: A really simple instrument
categories: xtlang audio
---

In this post we’ll make a really basic `saw_synth` instrument. An
*instrument* in Extempore allows you to trigger ‘notes’ like a MIDI soft
synth. [This post](./2012-09-28-making-an-instrument.org) goes into a
lot more detail about how Extempore’s instrument infrastructure works,
but this is more of a ‘quick and dirty’ example instrument just to get a
feel for things.

All the instrument code is just regular
[xtlang](./2012-08-09-xtlang-type-reference.org), and can be found in
`libs/core/instruments.xtm` and `libs/external/instruments_ext.xtm`.

# A saw-synth note kernel

An instrument is basically two xtlang closures: a **note kernel
closure** and an **fx closure**. These closures must have specific type
signatures to play nice with the instrument signal chain.

First, let’s examine the note kernel closure. This closure takes *zero*
arguments, and returns another closure which takes four arguments:

-   `time`: the current (sample) time in Extempore
-   `chan`: the channel number
-   `freq`: the frequency (pitch) of the note as type `SAMPLE`
-   `amp`: the volume/loudness of the note as type `SAMPLE`

In Extempore, `SAMPLE` is aliased to `float` by default, but could also
be `double` (see [this
post](./2013-11-15-changing-from-doubles-to-floats-in-audio_dsp.org) for
more info).

The *returned* closure will be called to provide the basic audio signal
for the note, so that’s where we put our code to generate the saw wave

``` {.extempore}
  (sys:load "libs/core/instruments.xtm")

  (bind-func saw_synth_note_c
    (lambda (data:NoteInitData* nargs:i64 dargs:SAMPLE*)
      (let ((saw (saw_c 0.)))
        (lambda (time:i64 chan:i64 freq:SAMPLE amp:SAMPLE)
          (if (= chan 0)
              (saw amp freq)
              0.0)))))

  ;; when we evaluate saw_synth_note_c, the compiler prints:
  ;; Compiled:  saw_synth_note_c >>> [[float,i64,i64,float,float]*,NoteInitData*,i64,float*]*
```

Notice that the saw
[unit-generator](http://en.wikipedia.org/wiki/Unit_generator) (ugen)
`saw` is bound (by calling `saw_c` [^1]) *outside* the inner `lambda`
form. This inner `lambda` defines the closure which will be *returned*
by `saw_synth_note_c`. In this returned closure, the ugen `saw` (which
is itself an xtlang closure) is called with the amplitude and frequency
values which are passed in as arguments to the `lambda` form. The value
returned by the `saw` closure (as it is called repeatedly, once per
audio sample) will trace out a [sawtooth
wave](http://en.wikipedia.org/wiki/Sawtooth_wave).

This is just a mono note kernel at this stage, because `saw` is only
called when `chan` is equal to `0`. The note kernel closure will
actually be called one for *each* output channel, and the `chan`
argument will range from `0` for the first output channel to `n - 1` for
the nth output channel (the number of output channels you have will
depend on your audio device). It’s therefore easy to generalise our note
kernel to multiple channels, so let’s make it a stereo note kernel

``` {.extempore}
  (bind-func saw_synth_note_c
    (lambda (data:NoteInitData* nargs:i64 dargs:SAMPLE*)
      (let ((sawl (saw_c 0.))
            (sawr (saw_c 0.)))
        (lambda (time:i64 chan:i64 freq:SAMPLE amp:SAMPLE)
          (cond ((= chan 0)
                 (sawl amp freq))
                ((= chan 1)
                 (sawr amp freq))
                (else 0.0))))))
```

Now we make two saw ugens (`sawl` and `sawr`), and call the appropriate
one depending on the `chan` argument. Our stereo saw note kernel is now
ready to play!

# Adding fx to the instrument

Often, you’ll want to add an audio effect to the instrument’s
output—maybe a delay, a reverb, or some more outlandish audio
processing. But we don’t want to apply the fx processing to each note
individually, but rather to the total audio output of the instrument.
And that’s where the **fx closure** comes in.

<div class="ui image segment">
  <img src="/img/simple-instrument/fx.png" width="300px" alt="">
</div>

The most important argument to the fx closure is the `in` argument,
which represents the (dry) input signal that you want to process. It
*is* necessary to have an fx closure in your Extempore instrument,
although it may just pass its input through untouched:

``` {.extempore}
  (bind-func saw_synth_fx
    (lambda (in:SAMPLE time:i64 chan:i64 dat:SAMPLE*)
      in))

  ;; when we evaluate saw_synth_fx, the compiler prints:  
  ;; Compiled saw_synth_fx >>> [i64,i64,i64,float,float*]*
```

Let’s add a stereo delay to make things a bit more interesting

``` {.extempore}
  (bind-func saw_synth_fx 200000 ;; extra memory for the delay lines
    (let ((delayl (delay_c 22050))
          (delayr (delay_c 22050)))
      (lambda (in:SAMPLE time:i64 chan:i64 dat:SAMPLE*)
        (cond ((= chan 0)
               (delayl in 0.3 0.2))
              ((= chan 1)
               (delayr in 0.3 0.2))
              (else 0.0)))))
```

Nice one. Also, remember that you change the fx closure at any time
(just edit the code and [re-evaluate
it](./2012-09-26-interacting-with-the-extempore-compiler.org)).

# Putting it all together

Finally, to complete the instrument, we use a special `bind-instrument`
macro

``` {.extempore}
  (bind-instrument saw_synth saw_synth_note_c saw_synth_fx)
```

<div class="ui image segment">
  <img src="/img/simple-instrument/whole-instrument.png" alt="">
</div>

As long as your kernel (`saw_synth_note_c`) and fx (`saw_synth_fx`)
closures have the right signature, then evaluating the above line should
print for you

``` {.bash}
  Compiled saw_synth >>> [float,float,i64,i64,float*]*
```

…and now your instrument is ready to play.

What—is that the end? Well, that’s a bit frustrating: we haven’t even
got to *play* our instrument yet! Don’t worry, we’ll use our `saw_synth`
instrument in [this post](2012-10-15-playing-an-instrument-part-i.org)
about ‘making it go bing’.

There are a couple of things to note which might be helpful for when you
want to build your *own* instruments

-   The note kernel closure (in this example `saw_synth_note_c`) returns
    a closure for each note: multiple notes may be playing
    simultaneously (polyphony), so you want to make sure that each
    closure keeps track of the state it needs and doesn’t leak that
    state to any of the other notes which are playing simultaneously.
-   Each note kernel returns it’s output *one sample at a time*. So it’s
    up to you to make sure that these samples (when streamed to the
    audio hardware as an audio signal) make the audio waveform
    you’re after.

And again, if you’re interested in a more in-depth explanation of
Extempore’s instrument infrastructure, then you can [go and build your
own tonewheel organ](./2012-09-28-making-an-instrument.org).

[^1]: By [convention](./2012-10-15-xtlang-naming-conventions.org),
    xtlang closures which *return* closures have a `_c` suffix.
