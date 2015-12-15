---
layout: docs
title: Memory management in Extempore
categories: xtlang memory
---

The two languages hosted by the Extempore compiler, xtlang and Scheme,
have different approaches to dealing with memory allocation and
management. Both languages ultimately share the same memory—the stack
and heap associated with the Extempore process, Extempore gives access
to this memory to both languages via different mechanisms. Broadly
speaking, with Scheme code Extempore manages memory for you, while in
xtlang you have to do it yourself. This is a common trade-off, and each
has its advantages (in performance, programmer productivity, etc.) and
disadvantages. To work effectively in Extempore it’s helpful to know a
bit more about how to work with memory in Extempore, and that’s what
I’ll try to cover in this post.

# Automatic garbage collection in Scheme

Scheme objects (lists, closures, numbers, etc.) are automatically
[garbage
collected](http://en.wikipedia.org/wiki/Garbage_collection_(computer_science))
by the Extempore run-time garbage collector (GC)[^1]. This means that
when new objects are created, memory is automatically allocated to store
those objects, and as objects are destroyed or go out of scope (that is,
there are no longer any references to them) the memory is automatically
freed up for re-use.

Let’s do the most basic memory allocation imaginable: just binding a
numerical value to a symbol.

``` {.extempore}
  (define a 5)

  (println 'a '= a)  ;; prints a = 5
```

The fact that we can use the symbol `a` and have it evaluate to `5` (as
it should) means that the value (`5`) must be stored in memory
somewhere.[^2] It doesn’t matter *where* in memory (what the address
is), because we can always refer to the value using the symbol `a`. But
it’s good to remember that the `define` form is allocating some memory,
storing the value `5` in that memory, and binding a reference to the
value in the symbol `a`.

We can redefine the symbol `a` to be some other Scheme object, say, a
list.

``` {.extempore}
  (define a '(1 2 3))

  (println 'a '= a)  ;; prints a = (1 2 3)
```

The three-element list `(1 2 3)` takes up more memory than the number
`5`. So `define` can’t just write the new value of `a` over the top of
the old one. What it does (and in fact what re-defining things *always*
does) is allocate some new memory to store the new value into, and
change the variable `a` to point to that new value.

But what happens to the old value of `5` in memory? Well, it sits there
unmolested, at least for a while. But we can’t reach it—the only
‘handle’ we had to refer to it with was the symbol `a`, and that’s now
bound to some other value instead. The value `5` in memory is
‘unreachable’. So there’s no point having it sitting around, taking up
space like some freeloading relative.

That’s where the garbage collector comes in. Every now and then the
garbage collector checks all the Scheme objects in the world,[^3]
determines which of them are no longer reachable, and then frees up that
memory to be used for other things. While I don’t recommend this harsh
utilitarian approach to dealing with relatives who are down on their
luck, it *is* good idea in a computer program. Memory is a finite
resource, and the more efficiently we can get rid of memory that’s not
being used the better.[^4]

Basically, having a GC means that when you’re writing Scheme code, you
don’t have to worry about memory. The GC takes care of all the
allocation/deallocation bookkeeping for you. The cost is that this
bookkeeping requires CPU cycles—cycles which you could be using to do
other cool things. Also, every now and then the GC has to briefly ‘stop
the world’ (freeze the execution of all Scheme code) to do its job. This
takes time, and introduces an element of uncertainty (non-determinism)
to the execution of your code—you never know exactly when the GC is
going to freeze things to do it’s job, and there’s a risk that it’ll
happen at a really inconvenient time as far as your program is concerned
(Murphy’s law and all that). This is particularly problematic in domains
where timing is critical, such as real-time audio and video.

# Manual memory management in xtlang

Hang on a sec—isn’t working with real-time audio and video xtlang (and
therefore Extempore’s) *raison d’etre?* Well, yes is is—the sluggishness
(and non-determinism) of Impromptu’s Scheme interpreter was the spark
which ignited the development of xtlang (as mentioned in [this
post](../2012-08-07-Extempore-philosophy.org)). Again, this isn’t a
knock on Scheme in general as slow—there are some very sprightly Scheme
compilers, but Impromptu’s one was slow. The non-determinism was even
more of a problem, because the last thing you want when you’re
generating audio or video is a ‘stop the world’ GC pause, which will
lead to clicks and pops in audio or dropped frames in video. Real-time
systems and garbage collection are uneasy bedfellows.

So, xtlang requires [manual memory
management](http://en.wikipedia.org/wiki/Manual_memory_management). In
general, this means that when you want some memory you ask the compiler
for it, it’s yours for a time and you can do whatever you want with it,
and then you know when it’s going to be ‘given back’. It doesn’t
necessarily mean that you have it forever (in fact in many cases the
memory is quite short-lived), but it does mean that there are no
surprises—you specify exactly how much memory you’ll get and you know
it’s going to hang around for. This determinism an important benefit of
manual memory management in xtlang—especially in a real-time systems
context.

Zooming out for a second, a running program has access to and uses two
main regions of memory: the **stack** and the **heap**. There’s lots of
material on the web about the differences between these two a ([here’s
an explanation at
stackoverflow](http://stackoverflow.com/questions/79923/what-and-where-are-the-stack-and-heap)),
but I’ll give a quick summary here.

-   The **stack** is for dealing with function arguments and
    local variables. Each function call ‘pushes’ some new data onto the
    stack, and when the function returns it ‘pops’ off any local
    variables and leaves its return value. The stack is therefore
    generally changing pretty quickly.
-   The **heap**, on the other hand, is for longer-lived data. Buffers
    of audio, video, or any data which you want to have around for a
    while: these are the sort of things you’ll generally want to store
    on the heap.

I should also point out that the stack and heap aren’t actually
different types of memory in the computer—they’re just different areas
in the computer’s RAM. The difference is in the way the program *uses*
the different regions. Each running process has its own stack[^5] and
heap, and they are just regions of memory given to the process by the
OS.

So, that’s the stack and the heap, but there’s actually one other type
of memory in Extempore: **zone** memory. A zone is a
[region](http://en.wikipedia.org/wiki/Region-based_memory_management) of
memory which can be easily deallocated all at once. So, if you have some
data that you need to hang around longer than a function call (so a
stack allocation is no good), but want to be able to conveniently
deallocate all at once, then use a zone. There can be multiple zones in
existence at once, and they don’t interfere (or have anything to do
with) each other.

# The three flavours of memory in Extempore

So, in accordance with the three different memory ‘types’ (the stack,
the heap, and zones) there are three memory allocation functions in
xtlang: `salloc`, `halloc` and `zalloc`. They all return a pointer to
some allocated memory, but they differ in *where* that memory is
allocated from, and there are no prizes in guessing which function is
paired with which type of memory :)

Also, `alloc` in xtlang is an alias for `zalloc`. So if you ever see an
`alloc` in xtlang code just remember that it’s grabbing memory from a
zone.

## Stack allocation with salloc

As I mentioned above, the stack is associated with function calls, their
arguments and local variables. Because xtlang uses (in general)
[function *closures*](2012-08-09-xtlang-type-reference.org) rather than
just plain functions, stack allocation and `salloc` in xtlang is used in
the body of a closure. Remember that closures are just functions with
their enclosing scope: think of a function which has packaged up any
variables it references and carries them around in its saddlebags.

Well, that’s as clear as mud. Let’s have an example.

``` {.extempore}
  (bind-func simple_stack_alloc
    (lambda ()
      (let ((a 2)
            (b 3.5))
        (printf "a x b = %f\n"
                (* (i64tod a) b)))))

  (simple_stack_alloc) ;; prints "a x b = 7.000000"
```

Even though there was no explicit call to `salloc`, the local variables
which are bound in the `let` (in this case the integer `a` and the float
`b`) are allocated on the stack. This is always where the memory for
`let`-bound float and int literals is allocated from in xtlang. String
literals are bound globally (more on this shortly), but that’s the
exception to the rule—everything else which is bound in a `let` inside
an xtlang `lambda` will be stack allocated, unless you explicitly
request otherwise with `halloc` or `zalloc`.

String literals are the exception to the “all literals are on the stack”
rule. String literals are actually stored as `i8*` on the heap (as
though they were *halloced*). If you capture a pointer to one of these
strings (e.g. with `pref-ptr`), then you can pass it around and
dereference it from anywhere. [This
post](2012-08-09-xtlang-type-reference.org) has more details on strings
in Extempore.

This ‘implicit stack allocation’ works for int and float literals, but
how about aggregate and other higher-order types? In those cases, we
call `salloc` explicitly.

``` {.extempore}
  (bind-func double_tuple
    (lambda (a:i64)
      (let ((tup:<i64,i64>* (salloc)))
        (printf "input: %lld, " a)
        (tfill! tup a (* 2 a))
        (printf "output: <%lld,%lld>\n"
                (tref tup 0)
                (tref tup 1))
        tup)))

  (double_tuple 3) ;; prints "input: 3, output: <3,6>"
```

This `double_tuple` closure takes an `i64` argument, and creates a
2-tuple which contains the input value and also its double. Think of it
as creating input-output pairs for the function *f(x) = 2x*.

Notice how the tuple pointer `tup:<i64,i64>*` was `let`-bound to the
return value of the call to `salloc`. Initially, the memory was
uninitialised (see [this
post](2012-08-13-understanding-pointers-in-xtlang.org) for more
background about pointers), then two `i64` values were filled into it
with `tfill!`. This is basically all the closure does, apart from the
`printf` calls which are just reading and printing out what’s going on.

The printout confirms that the doubling is working correctly: `6` is
indeed what you get when you double `3`, so the output value of `<3,6>`
is spot on. The pointer (and memory) returned by `(salloc)` is obviously
working fine. And this pointer is also the return value of the closure
(so `double_tuple` has type signature `[<i64,i64>*,i64]*`).

What happens if we try and dereference this returned pointer?

``` {.extempore}
  (bind-func double_tuple_test
    (lambda ()
      (let ((tup (double_tuple 6)))
        (printf "tup* = <%lld,%lld>\n"
                (tref tup 0)
                (tref tup 1)))))

  (double_tuple_test)

  ;; prints:

  ;; input: 6, output: <6,12>
  ;; tup* = <6,12>
```

Well, that seems to work OK. What about if we call `double_tuple` again
in the body of the `let`, ignoring its return value?

``` {.extempore}
  (bind-func double_tuple_test2
    (lambda ()
      (let ((tup (double_tuple 6)))
        (double_tuple 2)
        (printf "tup* = <%lld,%lld>\n"
                (tref tup 0)
                (tref tup 1)))))

  (double_tuple_test2)

  ;; prints:

  ;; input: 6, output: <6,12> (in the 1st call to double_tuple)
  ;; input: 2, output: <2,4>  (in the 2nd call to double_tuple)
  ;; tup* = <2,4>
```

This isn’t right: `tup*` should still be the original tuple `<6,12>`,
because we’ve bound it the `let`. But somewhere in the process of
calling `double_tuple` again (with a different argument: `2`), the
values in our original tuple (which we have a pointer to in `tup`) have
been overwritten.

Finally, consider this example:

``` {.extempore}
  (bind-func double_tuple_test3
    (lambda ()
      (let ((tup (double_tuple 6))
            (test_closure
             (lambda ()
               (printf "tup* = <%lld,%lld>\n"
                       (tref tup 0)
                       (tref tup 1)))))
        (test_closure))))

  (double_tuple_test3)

  ;; prints:

  ;; input: 6, output: <6,12>
  ;; tup* = <0,4508736416>
```

Wow. That’s not just wrong, that’s *super wrong*. What’s going on is
that the call to `salloc` inside the closure `double_tuple` doesn’t keep
the memory after the closure returns, because at this point all the
local variables get popped off the stack. Subsequent calls to *any*
closure will push new arguments and local variables *onto* the stack and
overwrite the memory that `tup` points to.

That’s what deallocating memory *means*: it doesn’t mean that the memory
gets set to zero, or that new values will be written in straight away,
but it means that the memory *might* be overwritten at any stage. Which,
from a programming perspective, is just as bad as having new data
written into it, because if you can’t trust that your pointer still
points to the value(s) you think it does then it’s pretty useless.

So, what we need in this case is to allocate some memory which will
still hang around after the closure returns. `salloc` isn’t up to the
task, but `zalloc` is.

## Zone allocation with zalloc

Zone allocation is kindof like stack allocation, except with user
control over when the memory is freed (as opposed it happening at the
end of function execution, as with memory on the stack). Essentially
this means that we can push and pop zones off of a stack of memory zones
of user-defined size.

A memory zone can be created using the special `memzone` form. `memzone`
takes as a first argument a zone size in bytes, and then an arbitrary
number of other forms (s-expressions) which make up the body of the
`memzone`. The *extent* of the zone is defined by `memzone`’s
s-expression. Anything within the body of the `memzone` s-expression is
*in scope*.

Say we want to fill a memory region with `i64` values which just count
from `0` up to the length of the region (`region_length`). We’ll need to
allocate the memory for this region, and get a pointer to the start of
the region. We can do this using `zalloc` inside a `memzone`.

``` {.extempore}
  (bind-func fill_buffer_memzone
    (lambda ()
      (memzone 100000  ;; size of memzone (in bytes)
               (let ((region_length 1000)
                     (int_buf:i64* (zalloc region_length))
                     (i:i64 0))
                 (dotimes (i region_length)
                   (pset! int_buf i i))
                 (printf "int_buf[366] = %lld\n"
                         (pref int_buf 366))))))

  (fill_buffer_memzone) ;; prints "int_buf[366] = 366"
```

The code works as it should: as confirmed by the print statement. Notice
how the call to `zalloc` took an argument (`region_length`). This tells
`zalloc` how much memory to allocate from the zone. If we hadn’t passed
this argument (and it *is* optional), the default length is `1`, to
allocate enough memory for *one* `i64`. All of the alloc functions
(`salloc`, `halloc` and `zalloc`) can take this optional size argument,
and they all default to `1` if no argument is passed.

Let’s try another version of this code `fill_buffer_memzone2`, but with
a much longer buffer of `i64` values.

``` {.extempore}
  (bind-func fill_buffer_memzone2
    (lambda ()
      (memzone 100000  ;; size of memzone (in bytes)
               (let ((region_length 1000000)
                     (int_buf:i64* (zalloc region_length))
                     (i:i64 0))
                 (dotimes (i region_length)
                   (pset! int_buf i i))
                 (printf "int_buf[366] = %lld\n"
                         (pref int_buf 366))))))

  (fill_buffer_memzone2) ;; prints "int_buf[366] = 366"
```

This time, with a region length of one million, the code still works (at
least, the 367Th element is still correct), but the compiler also prints
a warning message to the log:

``` {.bash}
Zone:0x7ff7ac99a100 size:100000 is full ... leaking 8000000 bytes
Leaving a leaky zone can be dangerous ... particularly for concurrency
```

So what’s wrong? Well, remember that the `memzone` has a size (in bytes)
which is specified by its first argument. We can calculate how much
space `int_buf` will need (`region_length` multiplied by 8, because
there are 8 bytes per `i64`) and therefore how much of the zone’s memory
will be allocated with the call to `(zalloc
region_length)`. If this number is *greater* than the memzone size, then
we’ll get the “Zone is full, leaking *n* bytes” warning—as we did with
`fill_buffer_memzone2`.

When zones leak, the Extempore run-time will scramble to find extra
memory for you, but it will be from the heap—which is time-consuming and
it will never be deallocated. This is bad, so it’s always worth making
sure that the zones are big enough to start with.

`memzone` calls can also be nested inside one another. When a new zone
is created (pushed) any calls to `zalloc` will be allocated from the new
zone (which is the **top** zone). When the extent of the zone is reached
it is **popped** and its memory is reclaimed. The new **current** zone
is then the next **top** zone. The zones are in a stack in the ‘stack
*data structure*’ sense of the term, but this is not the stack that I
was talking about earlier with `salloc`. Hopefully that’s not too
confusing. So we’ll talk about pushing and popping zones from the *zone
stack*, but it’s still all done with `memzone` and `zalloc`.

By default each process has an initial **top** zone with 1M of memory.
If no user defined zones are created (i.e. no uses of `memzone`) then
any and all calls to zalloc will slowly (or quickly) use up this 1M of
memory—you’ll know when it runs out as you’ll get about a gazillion
memory leak messages.

In general this is the zone story. But to complicate things slightly
there are two special zones.

1.  The **audio zone**: there is a zone allocated for each audio frame
    processed, be that sample by sample, or buffer by buffer. The zones
    extent is for the duration of the audio frame (i.e. is deallocated
    at the end of the frame). The [DSP
    basics](2012-06-07-dsp-basics-in-extempore.org) post covers audio
    processing in Extempore.

2.  **Closure zones**: all ‘top level’ closures (any closure created
    using `bind-func`) has an associated zone created at compile time
    (not at run-time, although this distinction is quite blurry
    in Extempore). The `bind-func` zone default size is 8KB, however,
    `bind-func` has an optional argument to specify any arbitrary
    `bind-func` zone size.

To allocate memory from a closure’s zone, we need a `let` outside the
`lambda`. Anything `zalloc`’ed from there will come from the closure’s
zone. Anything `zalloc`’ed from *inside* the closure will come from
whatever the top zone is at the time—usually the default zone (unless
you’re in an enclosing `memzone`).

As an example, let’s revisit our ‘fill buffer’ examples from earlier.
With a region length of one thousand:

``` {.extempore}
  (bind-func fill_buffer_closure_zone
    (let ((region_length 1000)
          (int_buf:i64* (zalloc region_length))
          (i:i64 0))
      (lambda ()
        (dotimes (i region_length)
          (pset! int_buf i i))
        (printf "int_buf[366] = %lld\n"
                (pref int_buf 366)))))  
```

The `let` where `int_buf` is allocated is outside the `lambda` form, so
the memory will be coming from the zone associated with the closure
`fill_buffer_closure_zone`. When we try and compile that, we get the
warning:

``` {.bash}
Zone:0x7fb8b3a4a610 size:8192 is full ... leaking 32 bytes
Leaving a leaky zone can be dangerous ... particularly for concurrency
```

Let’s try it again, but with a ‘zone size’ argument to `bind-func`

``` {.extempore}
  (bind-func fill_buffer_closure_zone2 10000 ;; zone size: 10KB
    (let ((region_length 1000)
          (int_buf:i64* (zalloc region_length))
          (i:i64 0))
      (lambda ()
        (dotimes (i region_length)
          (pset! int_buf i i))
        (printf "int_buf[366] = %lld\n"
                (pref int_buf 366)))))

  (fill_buffer_closure_zone2) ;; prints "int_buf[366] = 366"
```

Sweet—no more warnings, and the buffer seems to be getting filled
nicely.

This type of thing is very useful for holding data closed over by the
top level closure. For example, an audio delay closure might specify a
large `bind-func` zone size and then allocate an audio buffer to be
closed over. The example file `examples/core/audio-dsp.xtm` has lots of
examples of this.

The `bind-func` zone will live for the extent of the top level closure,
and will be refreshed if the closure is rebuilt (i.e. the old zone will
be destroyed and a new zone allocated).

## Heap allocation with halloc

Finally, we meet `halloc`, the Extempore function for allocating memory
from the heap. The heap is for long-lived memory, such as data that you
want to keep hanging around for the life of the program.

You can use `halloc` anywhere you would use `salloc` or `zalloc` and it
will give you a pointer to some memory on the heap. So, let’s revisit
the `double_tuple_test3` example from earlier, which didn’t work because
the memory for `tup` on the stack went out of scope when the closure
returned. If we replace the `salloc` with a `halloc`:

``` {.extempore}
  (bind-func double_tuple_halloc
    (lambda (a:i64)
      (let ((tup:<i64,i64>* (halloc))) ;; halloc instead of salloc
        (tfill! tup a (* 2 a))
        tup)))

  (bind-func double_tuple_halloc_test
    (lambda ()
      (let ((tup (double_tuple_halloc 4))
            (test_closure
             (lambda ()
               (printf "tup* = <%lld,%lld>\n"
                       (tref tup 0)
                       (tref tup 1)))))
        (test_closure))))

  (double_tuple_halloc_test) ;; prints "tup* = <4,8>"
```

Now, the returned tuple pointer `tup` is a heap pointer, so we can refer
to it from *anywhere* without any issues. In fact, the only way to
deallocate memory which has been `halloc`’ed and free it up for re-use
is to use the xtlang function `free` (which is the same as calling
`free` in C).

In practice, a lot of the times where you want long-lived memory you’ll
want it to be associated with a closure anyway, so the closure’s zone is
a better option than the heap for memory allocation, as in the
`fill_buffer_closure_zone2` example above. This has the added advantage
that if you re-compile the closure, because you’ve changed the
functionality or whatever, all the memory in the zone is freed and
re-bound, which is often what you want.

Where you *may* want to use `halloc` to allocate memory on the heap, is
in binding global data structures which you want to have accessible from
anywhere in your xtlang code. Binding global xtlang variables is the job
of `bind-val`.

*Note:* `bind-val` *is currently undergoing some reworking, so watch
this space for best practices.*

# Choosing the right memory for the job

Each different alloc function is good for different things, and the
general idea to keep in mind is that you want your memory to hang around
for as long as you need it to—and *no longer*. Sometimes you only need
data in the body of a closure—then `salloc` is the way to go. Other
times you want it to be around for as long as the closure remains
unchanged, then `zalloc` is the right choice. Also, if you’re going to
be alloc’ing a whole lot of objects for a specific algorithmic task and
want to be able to conveniently let go of them all when you’re done,
then creating a new zone with `memzone` and using `zalloc` is a good way
to go. Finally, if you know that a particular buffer of data is going to
hang around for the life of the program, then use `halloc`.

It’s worth acknowledging that memory management in xtlang is a ‘training
wheels off’ scenario. It’s a joy to have the low level control and
performance of direct memory access, but there are also opportunities to
really mess things up in a way that’s trickier to do in higher-level
languages. Remember that memory is a finite resource. Don’t try and
allocate a memory region of 10^15^ 8-byte `i64`:

``` {.extempore}
  (bind-func fill_massive_buffer
    (lambda ()
      (let ((region_length 1000000000000000)
            (int_buf:i64* (zalloc region_length))
            (i:i64 0))
        (dotimes (i region_length)
          (pset! int_buf i i))
        (printf "int_buf[366] = %lld\n"
                (pref int_buf 366)))))

  (fill_massive_buffer)
```

When I call `(fill_massive_buffer)` on my computer (with 8GB of RAM),
disaster strikes.

``` {.bash}
Zone:0x7fc5cbc268c0 size:100000 is full ... leaking 8000000000000000 bytes
Leaving a leaky zone can be dangerous ... particularly for concurrency
extempore(21386,0x11833d000) malloc: *** mmap(size=8000000000000000) failed (error code=12)
error: can't allocate region
set a breakpoint in malloc_error_break to debug
Segmentation fault: 11
```

If you’re not used to working directly with memory, you’ll almost
certainly crash (segfault) Extempore when you start out. In fact, be
prepared to crash things *a lot* at first. Don’t be discouraged: once
you get your head around the three-fold memory model and where each
allocation function is getting its memory from, it’s much easier to
write clean and performant code in xtlang. And from there, the
performance and control of working with ‘bare metal’ types opens up lots
of cool possibilities.

[^1]: Extempore uses a tri-color (quad treadmill extension)
    mark-and-sweep garbage collector for those who are into that sort of
    thing.

[^2]: [This post](2012-08-13-understanding-pointers-in-xtlang.org)
    covers in more detail how computers store data in memory.

[^3]: Well, at least the world of your Extempore process, which *is* the
    world as far as the GC is concerned.

[^4]: I guess it also shows the danger of anthromorphising bit patterns
    in memory. Lots of life lessons in this blog post

[^5]: actually each *thread* has its own stack
