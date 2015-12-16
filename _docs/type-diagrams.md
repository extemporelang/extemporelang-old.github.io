---
title: xtlang type diagrams
categories: xtlang
---

It can sometimes be hard to get a picture of the types and data
structures used in a particular piece of code, and even trickier to make
sense of how the data flows through and is modified by a given bit of
code. The whole ‘source code visualisation’ thing is a super tricky
problem.[^1]

[xtlang’s built-in types](2012-08-09-xtlang-type-reference.org) may not
be too confusing in simple cases, but the use of compound (and even
recursive types) means that things can get complicated fairly quickly.
For this reason, it’s nice to be able to visualise the types involved a
particular bit of xtlang source code.

And the types *can* get pretty hairy: tuples with elements which are
arrays of closure pointers, pointers to pointers to pointers, not to
mention the generics stuff which is coming in Extempore version 0.5.
Defining your own custom types, which then get all the safety benefits
of compile-time type checking, is one of the nice things about the
xtlang type system. When you’re writing this sort of xtlang code, be
prepared to see the compiler spit out all sorts of complex type
signatures. And they do get easier to read at a glance in time—at least
in my experience—but it’s cool to be able to represent them visually as
well. And that’s where xtlang type diagrams come in.

This blog post is a companion piece to the [xtlang type
system](2012-08-09-xtlang-type-reference.org) post, so if you haven’t
read that then it might be worth jumping across to do that first. That
post does *use* type diagrams, though, so don’t worry if you don’t
understand them as you go through, they’ll become clear after you come
back and read *this* post. Ah, if only learning were an acyclic graph!

If this is the first time you’ve seen xtlang code then the diagrams
might not make a lot of sense, and you might want to read up on
[Extempore](2012-08-07-extempore-philosophy.org) and the [xtlang type
system](2012-08-09-xtlang-type-reference.org).

# The xtlang type key

<div class="ui image segment">
  <img src="/img/type-key.png" alt="">
</div>

To start, cast your eye over the type key: these are the visual
representations of all of Extempore’s built-in types, and the basis of
all compound and custom types.

## Int and float values

<div class="ui image segment">
  <img src="/img/int-float-examples.png" alt="">
</div>

The primitive float and int types of different widths are all
represented by the same colour, i.e. `i1` to `i64` values are the same
blue colour and `float` and `double` values are the same green
colour.[^2] To avoid confusion, if necessary the type can be written
below the box (as is the case in the diagrams above). However, for
clarity the exact width of the int or float will be omitted, especially
when ints and floats are members of compound types like tuples or are
taken as arguments to closures.

If a value is bound to a name, for instance in the case of a global
variable

``` {.extempore}
  (bind-val global_int i64 42)
  (bind-val global_float double 3.6)
```

then the name will be written inside the box:

<div class="ui image segment">
  <img src="/img/named-int-float-vars.png" width="350px" alt="">
</div>

The names can be bound either through global assignment (as above), or
in a `let`, or through being passed as an argument to a closure. If a
variable has no name, then the box is left blank.

## Pointers

Pointer types are indicated with a (half-height) orange bar, and the
number of orange bars indicates the pointer ‘depth’.

<div class="ui image segment">
  <img src="/img/pointer-examples.png" width="350px" alt="">
</div>

Pointers are used frequently to deal with aggregate types: tuples,
arrays, vectors and closures.

## Tuples

Tuples are an aggregate type: they are composed of elements which may be
any other type. In xtlang type diagrams, this is indicated by splitting
the tuple’s type box in half horizontally. The top half (coloured in
dark blue) represents the tuple (and potentially its name if it has one)
while the bottom half indicates the number and types of the elements in
the tuple. Some examples are probably the clearest way to explain this:

<div class="ui image segment">
  <img src="/img/tuple-examples.png" alt="">
</div>

Notice that all of the examples show tuple pointers: this is the
standard way of dealing with tuples in xtlang. The third example shows
the way tuples can be nested: the second element of the outer
(2-element) tuple is itself a tuple.

It’s also worth pointing out that the fact that tuples are graphically
‘larger’ than the equivalent element types on their own, although that
doesn’t mean that they take up more room in memory or anything like
that. As an example, the comparison between

<div class="ui image segment">
  <img src="/img/size-comparison.png" width="400px" alt="">
</div>

both the pair of primitives and the tuple take up the exact same room in
memory—the *compiler* knows about the tuple, not the run-time. Tuples
are a convenient way to deal with data which needs to be kept together,
e.g. the *x* and *y* co-ordinates of a point in the (2D) cartesian
plane. They also allow for extra type checking, and potentially even
some extra optimisations.

## Arrays

Arrays are another compound type. Like tuples, arrays are represent in
xtlang type diagrams in two layers—the top half indicates that it is an
array type, and the bottom half indicates the length (number of
elements) and the type of the elements. Like tuples, arrays are normally
handled in xtlang via pointers.

<div class="ui image segment">
  <img src="/img/array-examples.png" alt="">
</div>

The third example shows how the elements of an xtlang array can be of
any type. If you can look at that type signature and diagram and
understand it then you’re well on the way to understanding the xtlang
type system.

## Vectors

Vectors are exactly the same as arrays from a type diagram
perspective—the only difference is the colour.

<div class="ui image segment">
  <img src="/img/vector-examples.png" width="350px" alt="">
</div>

It’s worth keeping in mind, though, that vectors and arrays are very
different from a programming perspective. Arrays are much more ‘general
purpose’, while vectors support only limited types and operations but
utilise the SIMD registers on the CPU for blazing speed. However,
because the performance of xtlang is in general pretty great anyway,
it’s probably best to start working with arrays, and to only switch to
vectors if you really need to and are aware of the trade-offs.

## Closures

The final built-in xtlang is the closure type, and it’s a super
important one. The [type reference
post](2012-08-09-xtlang-type-reference.org) has more info on what
closures are and what they can be used for, but suffice to say that
they’re a key part of many xtlang idioms (in part due to xtlang’s Scheme
heritage) and come up all the time in xtlang code.

Closures are the most complicated of all the base types because they can
take arguments and return values (although they don’t *have* to take
arguments, and can return `void`). The visual representation of the
closure type therefore has to represent the both the argument type(s)
and the return types of the closure (which is often referred to in C
terminology as its *signature*).

Like tuples and arrays/vectors, closures are represented in layers—in
this case three layers. The top layer is for the argument types, the
middle (black) layer indicates the closure type itself, and the bottom
layer indicates the return type. The closure may be named or anonymous,
and the arguments may also be named and the argument names are
represented in the diagram.

As an example, consider the simple closure

``` {.extempore}
  (bind-func closure1
    (lambda ()
      25))

  ;; Compiled closure1 >>> [i64]*
```

The closure is given a name (`closure1`), takes no arguments, and
returns an `i64`.

<div class="ui image segment">
  <img src="/img/named-closure-1.png" width="150px" alt="">
</div>

Notice that the top ‘layer’ of the closure is blank (white) to indicate
that the closure takes no arguments. The bottom layer is ‘integer blue’
to indicate that the return type is an integer. Closures may have many
arguments (of many different types), but a closure will only have one
return type.

Some more complex examples:

``` {.extempore}
  (bind-func closure2
    (lambda (arg1 arg2)
      (dtoi64 (+ arg1 arg2))))

  ;; Compiled closure2 >>> [i64,double,double]*

  ;; closure2 takes one argument (an array of doubles) and returns a
  ;; tuple containing the first and last elements of this array, coerced
  ;; to i64s

  (bind-func closure3
    (lambda (inarray:|8,double|*)
      (let ((int_ends:<i64,i64>* (zalloc)))
        ;; set (i64 version of) first element of inarray into first
        ;; element of int_ends
        (tset! int_ends 0 (dtoi64 (aref inarray 0)))
        ;; set (i64 version of) last element of inarray into second
        ;; element of int_ends
        (tset! int_ends 1 (dtoi64 (aref inarray 7)))
        ;; return int_ends tuple pointer
        int_ends)))

  ;; Compiled closure3 >>> [<i64,i64>*,|8,double|*]*

  ;; closure4 is a higher-order closure which takes a closure, and
  ;; returns another closure (which adds its input to the output of the
  ;; first closure)

  (bind-func closure4
    (lambda (in_cls:[i64]*)
      (lambda (a)
        (+ a (in_cls)))))

  (bind-func closure4_test
    (lambda ()
      ((closure4 closure1) 5)))

  (closure4_test) ;; returns 30 (25 + 5)
```

And here are the xtlang type diagrams of `closure1`, `closure2` and
`closure3`

<div class="ui image segment">
  <img src="/img/named-closure-2.png" alt="">
</div>

See how `closure1` (a simple closure which takes no arguments) and
`closure4` are ‘composed’ together in the test closure `closure4_test`?
The closure that’s returned by `closure4` doesn’t even have a name—it’s
an anonymous. Closures can be used as elements of tuples and arrays,
too. That’s the power of higher-order closures—and they’re used all over
the place in Extempore.

# On the usefulness of type diagrams

xtlang type diagrams aren’t a full ‘source code visualisation’ solution.
I’m still trying to figure out a nice way to visualise a closure’s side
effects (as distinct from its return value), and `let`-binding is also
hard to depict clearly.

Still, in any source code (not just xtlang) certain functions and
variables are more important than others, at least in terms of
understanding what the code *does*. Type diagrams can be used to
visualise *key variables* in xtlang code rather than trying to represent
every variable and type in the code. This means that they can’t be
generated automatically, and they are time consuming to put together.
However, judicious use of type diagrams to illustrate the important
types and flow of xtlang code can really help in making xtlang code
scrutable, especially for all the visual learners out there :)

Type diagrams which are used throughout the Extempore documentation.

And again, you can grab the [source from
github](https://github.com/digego/extempore) (which includes an
`examples` directory) and have a hack around yourself. A great way to
learn is to dive in, change things and see what breaks.

[^1]: There are lots of proprietary and FOSS packages out there which
    are designed to take source code as input and produce some visual
    representation of the code (see [this stackexchange
    thread](http://stackoverflow.com/questions/517589/tools-to-get-a-pictorial-function-call-graph-of-code)
    for a few suggestions).

[^2]: This is to avoid cluttering the graph with too many colours—it’s
    already inaccessible enough for colourblind folks as it is.
