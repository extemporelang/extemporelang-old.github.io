---
title: Understanding pointers in xtlang
categories: xtlang memory
---

xtlang’s pointer types may cause some confusion for those who aren’t
used to (explicitly) working with reference types. That’s nothing to be
ashamed of—the whole [pass by
value](http://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_value)/[pass
by
reference](http://en.wikipedia.org/wiki/Evaluation_strategy#Call_by_reference)
thing can take a bit to get your head around.

So what does it mean to say that xtlang supports pointer types? Simply
put, this means that we can use variables in our program to store not
just values, but the *addresses* of values in memory. A few examples
might help to clarify things.

The `let` form in xtlang (as in Scheme) is a way of binding or assigning
variables: giving a name to a particular value. If we want to keep track
of the number of cats you have, then we can create a variable `num_cats`

``` {.extempore}
  (bind-func print_num_cats
    (lambda ()
      (let ((num_cats:i64 4))
        ;; the i64 printf format specifier is %lld
        (printf "You have %lld cats!\n" num_cats))))

  (print_num_cats) ;; prints "You have 4 cats!"
```

What’s happening here is that the `let` assigns the value `4` to the
variable `num_cats`, so that whenever the program sees the variable
`num_cats` it’ll look in the `num_cats` ‘place’ in memory and use
whatever value is stored there. The computer’s memory is laid out like a
row of little boxes, and each box has an address (the location of the
box) and also a value (what’s *in* the box).

<div class="ui image segment">
  <img src="/img/pointer-tut-1.png" alt="">
</div>

In this image the computer’s memory is represented by the blue boxes.
Each box has an address (the number below the box), an in this picture
you can see that this is only a subset of the total number of memory
boxes (in a modern computer there are millions of memory boxes).

The variable `num_cats` keeps track of the value that we’re interested
in. In this case the address of that value is ‘memory location 26’, but
it could easily be any other location (and indeed will almost certainly
be different if the closure `print_num_cats` is called again).

Once a variable exists, we can change its value with `set!`:

``` {.extempore}
  (bind-func print_num_cats2
    (lambda ()
      (let ((num_cats:i64 4))
        (printf "You have %lld cats... " num_cats)
        (set! num_cats 13)
        (printf "and now you have %lld cats!\n" num_cats))))

  (print_num_cats2)
  ;; prints "You have 4 cats... and now you have 13 cats!"
```

The `set!` function changes the value of `num_cats`: it sets a new value
into the memory location that `num_cats` refers to. In `print_num_cats2`
the value of `num_cats` starts out as `4`, so the first `printf` call
prints “You have 4 cats…”. The memory at this point might look like
this:

<div class="ui image segment">
  <img src="/img/pointer-tut-2a.png" alt="">
</div>

But then a new value (`13`) is set into `num_cats` with the call to
`set!`, so the second call to `printf` prints “and now you have 13
cats!”. After the call to `set!`, this is what the memory looks like:

<div class="ui image segment">
  <img src="/img/pointer-tut-2b.png" alt="">
</div>

Notice how this time the memory address for `num_cats` is different to
what it was the previous time (28 rather than 26). This is because the
`let` rebinds all its variable-value pairs each time it is entered, and
then forgets them when it is exited (that is, when the paren matching
the opening paren is reached).

# Pointers: storing memory addresses as values

What we’ve done so far is store the value (how many cats we have) into
the variable `num_cats`. The value has an address in memory, but as a
programmer we don’t necessarily know what that address is, just that we
can refer to the value using the name `num_cats`. It’s important to note
that the *compiler* knows what the address is—in fact as far as the
compiler is concerned every variable is just an address. But the
compiler allows us to give these variables names, which makes the code
much easier to write and understand.

Pointer types in xtlang are indicated with an asterisk (`*`), for
example the type `i64*` represents a pointer to a 64-bit integer
(sometimes called an `i64`-pointer). With pointers, we actually assign
the *address itself* in a variable. That’s the reason it’s called a
pointer: because it points to (is a reference to) the value.

Let’s update our code for printing the number of cats to use a pointer
to the value, rather than the value itself. Notice how the type of
`num_cats_ptr` is `i64*` (a pointer to an `i64`) rather than just an
`i64` like it was before.

``` {.extempore}
  (bind-func print_num_cats3
    (lambda ()
      (let ((num_cats_ptr:i64* (zalloc)))
        (printf "You have %lld cats!\n" num_cats_ptr))))

  (print_num_cats3) ;; prints "You have 4555984976 cats!"
```

There are a couple of other changes to the code. Firstly, we no longer
bind the value straight away (as we were doing with `(num_cats:i64
4)`), but instead we make a call to `zalloc`. This is the way to get
pointers in xtlang: through a call to an ‘alloc’ function.[^1] `zalloc`
is a function which ‘allocates’ and returns the *address* (i.e. a
pointer) of some memory which can be used to store the value in. This
address is the assigned to the variable `num_cats_ptr`, just like the
number `4` was assigned to `num_cats` in the earlier examples. The
orange bar on the variable name indicates that it’s a pointer.

So why does `print_num_cats3` print such a weird (on my machine:
4555984976 cats!) answer? Well, it’s because we’re trying to print it as
an `i64` *value* (using `%lld` in the `printf` format string), but it’s
not an `i64` value—it’s the *address* of a memory location where an
`i64` value is located. On a 64-bit system (such as the laptop I’m
writing this blog post on) the pointers *are* actually 64-bit integers,
because an integer is a pretty sensible way to store an address.

Incidentally, this is one of the key benefits (and driving forces
behind) the switch from 32 to 64 bit architectures—the need for more
memory addresses. If a pointer is a 32 bit integer, then you can only
‘address’ about 4.3 billion (2^32^) different memory locations. This
might seem like a lot, but as more and more computers came with more
than 4.3Gb of RAM installed, so the need for 64-bit pointers became more
pressing. There are workarounds, but having a larger addressable space
is a key benefit of 64-bit architectures[^2]. And it helps to remember
that pointers *are* just integers, but they’re not like the int types
that we use to store and manipulate data.

In `print_num_cats3` we don’t set any value into that location, we only
deal with the address. In fact, the memory this address points to is
referred to as *uninitialised*, which is a name for memory that has been
allocated but hasn’t had any values set into it. In Extempore,
uninitialised memory will be ‘zeroed out’, meaning all of the bits will
be set to `0`. So for an `i64` this will be the integer value `0`.

After the call to `zalloc`, the memory therefore will look like this
(the value is now shown in a different coloured box, to indicate it’s an
`i64*` pointer type and not an `i64` value type)

<div class="ui image segment">
  <img src="/img/pointer-tut-3.png" alt="">
</div>

This is cool, we can see that the value in memory location 27 is
actually the address 29, and the value of 29 is `0` because we haven’t
initialised it yet. So, remember how in `print_num_cats2` we used `set!`
to set a value into the variable `num_cats`? Well, we can do a similar
thing with the pointer `num_cats_ptr` using the function `pset!`.
`pset!` takes three arguments: a pointer, an index (which is zero in
this next example, but I’ll get to what the index means in the next
section) and a value. The value must be of the right type: e.g. if the
pointer is a pointer to a double (a `double*`) then the value must be a
`double`.

``` {.extempore}
  (bind-func print_num_cats4
    (lambda ()
      (let ((num_cats_ptr:i64* (zalloc)))
        (pset! num_cats_ptr 0 5)
        (printf "You have %lld cats!\n" (pref num_cats_ptr 0)))))

  (print_num_cats4) ;; prints "You have 5 cats!"
```

Great—the function now prints the right number of cats (in this case
`5`), so things are working properly again. After the `pset!` call, the
memory will look like this (the only difference from last time is that
the value 5 is stored in address 29, just as it should be).

<div class="ui image segment">
  <img src="/img/pointer-tut-4.png" alt="">
</div>

Notice also that in `print_num_cats4` we don’t pass `num_cats_ptr`
directly to `printf`, we do it through a call to `pref`. Whereas `pset!`
is for writing values into memory locations, `pref` is for reading them
out. Like `pset!`, pref takes a pointer as the first argument and an
offset for the second argument. In this way, we can read *and* write
`i64` values to the memory location without actually having a variable
of type `i64` (which we did with `num_cats` in the `print_num_cats` and
`print_num_cats2`). All this is possible because we have a pointer
variable (`num_cats_ptr`) which gives us a place to load and store the
data.

# Buffers and pointer arithmetic

In all the examples so far, we’ve only used a pointer to a single value.
This has worked fine, but you might have been wondering why we bothered,
because assigning values directly to variables (as we did in the first
couple of examples) seemed to work just fine.

One thing that pointers and alloc’ing allows us to do is work with whole
regions in memory, in which we can store *lots* of values. Say we want
to be able to determine the mean (average) of 3 numbers. One way to do
this is to store each of the different numbers with its own name.

``` {.extempore}
  (bind-func mean1
    (lambda ()
      (let ((num1:double 4.5)
            (num2:double 3.3)
            (num3:double 7.9))
        (/ (+ num1 num2 num3)
           3.0))))

  ;; call the function
  (mean1) ;; returns 5.233333
```

The `let` form binds the (`double`) values `4.5`, `3.3` and `7.9` to the
names `num1`, `num2` and `num3`. Then, all three values are added
together (with `+`) and then divided by `3.0` (with `/`) [^3]. Now, this
code does give the right answer, but it’s easy to see how things would
get out of hand if we wanted to find the mean of 5, 20 or one million
values. What we really want is a way to give *one* name to all the
values we’re interested in, rather than having to refer to all the
values by name individually. And to do that, we can use a pointer.

``` {.extempore}
  (bind-func mean2
    (lambda ()
      (let ((num_ptr:double* (zalloc 3)))
        ;; set the values into memory
        (pset! num_ptr 0 4.5)
        (pset! num_ptr 1 3.3)
        (pset! num_ptr 2 7.9)
        ;; read the values back out, add them
        ;; together, and then divide  by 3
        (/ (+ (pref num_ptr 0)
              (pref num_ptr 1)
              (pref num_ptr 2))
           3.0))))

  (mean2) ;; returns 5.233333
```

In `mean2`, we pass an integer argument (in this case `3`) to `zalloc`.
`zalloc` then allocates enough memory to fit 3 `double` values. The
pointer that gets returned is still only a pointer to the first of these
memory slots. And this is where the second ‘offset’ argument to `pref`
and `pset!` come in.

<div class="ui image segment">
  <img src="/img/pointer-tut-5.png" alt="">
</div>

See how the repeated calls to `pset!` and `pref` above have different
offset values? Well, that’s because the offset argument allows you to
get and set values ‘further into’ the memory returned by `(zalloc 3)`.
This isn’t anything magical, they just add the offset to the memory
address.

There is a helpful function called `pfill!` for filling multiple values
into memory (multiple calls to `pset!`) as we did in the above example.
Rewriting `mean2` to use `pfill!`:

``` {.extempore}
  (bind-func mean3
    (lambda ()
      (let ((num_ptr:double* (zalloc 3)))
        ;; set the values into memory
        (pfill! num_ptr 4.5 3.3 7.9)
        ;; read the values back out, add them
        ;; together, and then divide  by 3
        (/ (+ (pref num_ptr 0)
              (pref num_ptr 1)
              (pref num_ptr 2))
           3.0))))

  (mean3) ;; returns 5.233333
```

Finally, one more useful way to fill values into a chunk of memory is
using a `dotimes` loop. To do this, we need to bind a helper value `i`
to use as an index for the loop. This function allocates enough memory
for 5 `i64` values, and just fills it with ascending numbers:

``` {.extempore}
  (bind-func ptr_loop
    (lambda ()
      (let ((num_ptr:i64* (zalloc 5))
            (i:i64 0))
        ;; loop from i = 0 to i = 4
        (dotimes (i 5)
          (pset! num_ptr i i))
       (pref num_ptr 3))))

  (ptr_loop) ;; returns 3
```

After the `dotimes` the memory will look like this:

<div class="ui image segment">
  <img src="/img/pointer-tut-6.png" alt="">
</div>

There’s one more useful function for working with pointers: `pref-ptr`.
Where `(pref num_ptr 3)` returns the *value* of the 4th element of the
chunk of memory pointed to by `num_ptr`, `(pref-ptr
num_ptr 3)` returns the address of that value (a pointer to that value).
So, in the example above, `num_ptr` points to memory address 27, so
`(pref num_ptr 2)` would point to memory address 29. `(pref
(pref-ptr num_ptr n) 0)` is the same as `(pref (pref-ptr num_ptr 0)
n)` for any integer *n*.

# Pointers to higher-order types

The xtlang type system is covered in [this
post](2012-08-09-xtlang-type-reference.org), but as a quick recap there
are primitive types (floats and ints) there are higher-order types like
tuples, arrays and closures. Higher-order in this instance just means
that they are made up of other types, although these component types may
be themselves higher-order types.

As an example of an aggregate type, consider a 2 element tuple. Tuples
are (fixed-length) n-element structures, and are declared with angle
brackes (`<>`). So a tuple with an `i64` as the first element and a
double as the second element would have the type signature
`<i64,double>`. Getting and setting tuple elements is done with `tref`
and `tset!` respectively, which both work exactly like `pref=/=pset!`
except the first argument has to be a pointer to a tuple.

``` {.extempore}
  (bind-func print_tuples
    (lambda ()
      ;; step 1: allocate memory for 2 tuples
      (let ((tup_ptr:<i64,double>* (zalloc 2)))
        ;; step 2: initialise tuples
        (tset! (pref-ptr tup_ptr 0) 0 2)         ; tuple 1, element 1
        (tset! (pref-ptr tup_ptr 0) 1 2.0)       ; tuple 1, element 2
        (tset! (pref-ptr tup_ptr 1) 0 6)         ; tuple 2, element 1
        (tset! (pref-ptr tup_ptr 1) 1 6.0)       ; tuple 2, element 2
        ;; step 3: read & print tuple values
        (printf "tup_ptr[0] = <%lld,%f>\n"
                (tref (pref-ptr tup_ptr 0) 0)    ; tuple 1, element 1
                (tref (pref-ptr tup_ptr 0) 1))   ; tuple 1, element 2
        (printf "tup_ptr[1] = <%lld,%f>\n"
                (tref (pref-ptr tup_ptr 1) 0)    ; tuple 2, element 1
                (tref (pref-ptr tup_ptr 1) 1))))); tuple 2, element 2

  (print_tuples) ;; prints
  ;; tup_ptr[0] = <2,2.000000>
  ;; tup_ptr[1] = <6,6.000000>
```

This `print_tuples` example works in 3 basic steps:

1.  **Allocate memory** for two (uninitialised) `<i64,double>` tuples,
    bind pointer to this memory to `tup_ptr`.
2.  **Initialise tuples with values** (in this case `2` and `2.0` for
    the first tuple and `6` and `6.0` for the second one). Notice the
    nested `tset!` and `pref-ptr` calls: `pref-ptr` returns a pointer to
    the tuple at offset 0 (for the first) and 1 (for the second). This
    pointer is then passed as the first argument to `tset!`, which fills
    it with a value at the appropriate element.
3.  **Read (& print) values** back out of the tuples. These should be
    the values we just set in step 2—and they are.

Let’s have a look at what the memory will look like during the execution
of `print_tuples`. After the call to `(zalloc)` (step 1), we have a
pointer to a chunk of memory, but the tuples in this memory are
uninitialised (indicated by u).

<div class="ui image segment">
  <img src="/img/pointer-tut-7.png" alt="">
</div>

After using `pref` and `tset!` in step 2, the values get set into the
tuples. Step 3 simply reads these values back out—it doesn’t change the
memory.

<div class="ui image segment">
  <img src="/img/pointer-tut-8.png" alt="">
</div>

There are a couple of other things worth discussing about this example.

-   We used `pref_ptr` rather than `pref` in both step 2 and step 3.
    That’s because `tset!` and `tref` need a *pointer to* a tuple as
    their first argument, and if we had used regular `pref` we would
    have got the tuple itself. This means that we could have just used
    `tup_ptr` directly instead of `(pref-ptr tup_ptr 0)` in a couple of
    places, because these two pointers will always be equal (have a
    think about why this is true).
-   There are a few bits of repeated code, for example `(pref-ptr
     tup_ptr 1)` gets called 4 times. We could have stored this pointer
    in a temporary variable to prevent these multiple dereferences, how
    could we have done that (hint: create the new ‘tmp’ pointer in the
    `let`—make sure it’s of the right type).

There’s one final thing worth saying about pointers in xtlang. Why do
pointers even *have* types? Isn’t the address the same whether it’s an
int, a float, a tuple, or some complex custom type stored at that memory
address? The reason is to do with something all this talk of memory
locations as ‘boxes’ has glossed over: that different types require
different amounts of memory to store.

A more accurate (though still simplified) picture of the computer’s
memory is to think of the boxes as 8-bit bytes. One bit (a binary digit)
is just a `0` or a `1`, and a byte is made up of 8 bits, for example
`11001011`. These are just [base-2
numerals](http://en.wikipedia.org/wiki/Binary_numeral_system), so `5` in
decimal is `101`, and although they are difficult for humans to read
(unless you’re used to them), computers *live and breathe* binary
digits.

This is why the integer types all have numbers associated with them—the
number represents the number of bytes used to store the integer. So
`i64` requires 64 bits, while an `i8` only requires 8. The reason for
having different sizes is that larger sizes take up more room (more
bytes) in memory, but can also store larger values (n bits can store
2^n^ different numbers). All the other types have sizes, too: a `float`
is 32 bits for instance, and the number of bits required to represent an
aggregate type like a tuple or an array is (at least) the sum of the
sizes of their components.

So, reconsidering our very first example, where we stored an `i64` value
of `4` to represent how many cats we had, a more accurate diagram of the
actual memory layout in this situation is:

<div class="ui image segment">
  <img src="/img/pointer-tut-9.png" alt="">
</div>

See how each `i64` value takes up 8 bytes? Also, each byte has a memory
addresses, so the start of each `i64` in memory is actually 8 bytes
along from the previous one.

Now, consider the layout of an aggregate type like a tuple:

<div class="ui image segment">
  <img src="/img/pointer-tut-10.png" alt="">
</div>

Each tuple contains (and therefore takes up the space of) an `i64` and a
`double`. So the actual memory address offset between the beginning of
consecutive tuples is 16 bytes. But `pref` still works the same as in
the `i64*` case. `(pref tup_ptr 1)` gets the second tuple—it doesn’t try
and read a tuple from ‘half way in’.

This is one reason why pointers have types: the type of the pointer
tells `pref` how far to jump to get between consecutive elements (this
value is called the stride). This becomes increasingly helpful when
working with pointers to compound types: no-one wants figure out (and
keep track of) the size of a tuple like `<i32,i8,|17,double|*,double>`
and calculate the stride manually.

# Other benefits of using pointers

There are a few other situations where being able to pass pointers
around is really handy.

-   When the chunks of memory we’re dealing with are large, copying them
    around in memory becomes expensive (in the ‘time taken’ sense). So,
    if lots of different functions need to work on the same data,
    instead of copying it around so that each function has its own copy
    of the data, they can just pass around pointers to the same chunk
    of data. This means that each function needs to be a good citizen
    and not stuff up things for the others, but if you’re careful this
    can be a huge performance benefit.
-   You can programatically determine the amount of memory to allocate,
    which is something you can’t to with xtlang’s array types.

[^1]: There are 3 types of alloc in xtlang: `salloc`, `zalloc` and
    `halloc`. They all return a pointer of the appropriate type, but
    they differ in *where* that memory is allocated from. In order of
    how ‘long-lived’ the memory will be: `salloc` allocates memory on
    the stack, `zalloc` allocates memory from the current zone, and
    `halloc` allocates memory from the heap. Finally, `alloc` is an
    alias for `zalloc`.

[^2]: The exact size of the int used for pointers will depend on the CPU
    and OS you’re using. Most desktop/laptop machines and OSes these
    days are 64-bit, but many ARM processors in smartphones are 32-bit,
    embedded systems sometimes use even smaller pointer sizes. The OS
    will take care of this for you, though, and will always know how to
    deal with the pointers it gives you.

[^3]: Remember that xtlang (like Scheme) uses infix notation for its
    function calls, so the syntax is `(func_name arg1 arg2 ...)`.
