---
title: Playing an instrument (part I)
categories: audio time musicmaking
---

*This post is a modified version of [a
tutorial](http://impromptu.moso.com.au/tutorials/bing.html) that Andrew
wrote for Impromptu.*

Extempore (like [Impromptu](http://impromptu.moso.com.au) before it)
supports playing ‘instruments’ at a note-level. This post covers the
basics of how to play instruments in Extempore. If you’re satisfied with
just playing Extempore’s built-in instruments (which can be found in
`libs/core/instruments.xtm`) then you can just start at this post. The
note-level approach to music generation will be very familiar to
Impromptu users.

If you want a deeper understanding of what’s going on ‘under the hood’,
there are [brief](2012-10-16-a-really-simple-instrument.org) or
[in-depth](2012-09-28-making-an-instrument.org) posts looking at how the
instrument infrastructure works (including how to write your own in
xtlang). Still, it’s not necessary to understand any of that to make
music, as we’ll cover in this tutorial.

# Setting up an instrument

This is about the simplest program you can write in Extempore. It loads
an instrument and plays a single note.

``` {.extempore}
  ;; load the instruments file 
  (sys:load "libs/core/instruments.xtm")

  ;; define a synth using the provided components
  ;; synth_note_c and synth_fx
  (bind-instrument synth synth_note_c synth_fx)

  ;; add the instrument to the DSP output sink closure
  (bind-func dsp:DSP
    (lambda (in time chan dat)
      (synth in time chan dat)))
  (dsp:set! dsp)

  ;; play a note on our synth
  (play-note (now) synth (random 60 80) 80 (* 1.0 *second*))
```

To run this simple program copy the code into your editor and evaluate
it—either one line at a time or all at once. [This
post](2012-09-26-interacting-with-the-extempore-compiler.org) has some
instructions on how to set up your editor and evaluate code in
Extempore, which might be helpful if you haven’t done it before.

When you eval the final line (the call to `play-note`) you should hear a
single note play for one second. You can re-evaluate that line as many
times as you like—you should hear a sound each time. Notice that
`random` chooses a different pitch each time you evaluate.

**Extra credit:** if you made a `saw_synth` in the [really simple
instrument](2012-10-16-a-really-simple-instrument.org) post, then see if
you can change the code above to play notes on your `saw_synth` instead!

Notice that Extempore is responsive, in fact you should be able to
evaluate `play-note` in time with a song playing on the radio. Extempore
is a ‘live performance instrument’ so it is designed to be responsive.
This is what I mean by interactive—we can evaluate code and view/hear
results straight away.

We can also create Scheme functions to trigger more complex musical
structures. Evaluate the following expression to define a function
`chord` that, when called, will play a chord on the `synth` instrument.

``` {.extempore}
  (define chord
     (lambda ()
        (play-note (now) synth 60 80 *second*)
        (play-note (now) synth 64 80 *second*)
        (play-note (now) synth 67 80 *second*)))
```

Once `chord` is defined you can then call it as many times as you like
by evaluating the following expression:

``` {.extempore}
  (chord)
```

Congratulations, you have successfully written a Scheme function to play
a C major chord. Try changing the pitch arguments to make different
chords.

# Writing loops

In keeping with traditions laid down in ages past by folks much smarter
than me, I should also provide a “Hello World” example. Because we’re in
Extempore, though, let’s add a bit of a twist: we’ll *listen* to the
string “Hello World” instead of printing it to the log.

Following on from the code we evaluated before to set up the `synth`

``` {.extempore}
  ; hello world as a list of note pitches
  ; transposed down two octaves (24 semitones)
  (define melody (map (lambda (c)
                         (- (char->integer c) 24))
                      (string->list "Hello World!")))

  ; Define a recursive function to cycle through the pitches in melody
  (define loop
     (lambda (time pitch-list)
        (cond ((null? pitch-list) (println 'done))
              (else (play-note time synth (car pitch-list) 80 10000)
                    (loop (+ time (* *second* 0.5))
                          (cdr pitch-list))))))

  ; start playing melody
  (loop (now) melody)
```

Note that `loop` is a recursive function—it calls itself. If you look
carefully at the code for `loop` you’ll see that it takes a list of
pitches, schedules the playing of the first pitch, then calls itself
back with the remaining pitches (if there are any). When we call `loop`
in the last line with time and pitch-list arguments, we should hear a
sequence of pitches—and it turns out that “Hello World” doesn’t make
such a good musical example :)

Try evaluating the final `(loop (now) melody)` expression
repeatedly—don’t wait until the sequence has finished playing before you
trigger another one. Pretty cool, huh. Extempore is dynamic and
interactive and was developed for use in live performance.

Because coding in Extempore is so dynamic, if you re-evaluate a whole
buffer you may not get the results you expect! In particular, remember
that if you have multiple Extempore (`*.xtm`) file buffers connected to
the same `extempore` process then any evaluations you make will all go
to the same place. For example, there is only one `dsp` audio sink, so
if you try to evaluate two examples with different audio chain
configurations you will almost certainly not get what you expect. If in
doubt, a good idea (particularly when getting started) is to restart
Extempore each time you want to run a new example or start a new
project.

The [second part of this
tutorial](2012-10-15-playing-an-instrument-part-ii.org) will gives a
much better idea of how you can build and evaluate code in a more
Extempore-friendly manner. It also shows how you can use pitch-classes
and other music-theoretic notions to build complex (and cool-sounding)
music in a relatively small amount of code. Check it out!
