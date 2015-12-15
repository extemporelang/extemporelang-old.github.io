---
layout: docs
title: Extempore Generics
categories: xtlang generics
---

Those of you who are on the Extempore mailing list will have seen
Andrew’s update on [Extempore’s
generics](http://extempore.moso.com.au/xtlang_update.html) the other
day. I recommend you check it out, it’s more detailed than this post.
But to distil the goodies into a couple of examples, let’s take a look
at higher-order functions in xtlang through `map`, `filter` and `foldl`
/ `foldr` from the abstract data types library `libs/core/adt.xtm`.

[Filter](http://en.wikipedia.org/wiki/Filter_%2528higher-order_function%2529),
[map](http://en.wikipedia.org/wiki/Map_%2528higher-order_function%2529)
and
[fold/reduce](http://en.wikipedia.org/wiki/Fold_%2528higher-order_function%2529)
are cornerstones of functional programming, and any language with
functional aspirations has to provide them in some form.

In xtlang, the trick is getting the types right: you don’t want to have
a separate map for functions which take and return `i64`, a separate one
for `double` not to mention the combinatorial explosion of other
input/output possibilities. Extempore handles this by using generic
types, which written in the type signatures as usual but preceded with a
‘bang’ (`!`).

There are lots of other generic goodies in here (xtlang’s lists for a
start), but I’m going to skip straight to `map`. If you’re interested in
how the `List` type is defined, as well as `cons`, `car` / `cdr`,
`length`, `nth`, `range` etc., then have a look in `libs/core/adt.xtm`.

**Note:** some of these changes have only been pushed to the `master`
branch recently, so if you have problems then make sure you’re
up-to-date.

# A generic cube function

Say we want to find the sum of all the even cubes between 0 and 9
(inclusive). There are lots of ways to do this, but the functional way
is as elegant as any other. We’ll start by writing a basic cube function
which takes a value *x* and returns the cube *x^3^*.

``` {.extempore}
  (bind-func cube:[!a,!a]*
    (lambda (x)
      (* x x x)))

  ;; when compiled, prints Generic:   cube >>> [!a,!a]*
```

Notice that the type signature of the `cube` function is `[!a,!a]*`, a
(pointer to a) closure which takes one argument of type `!a` and returns
a value of type `!a`.

What happens when we call this function?

``` {.extempore}
  (bind-func test_cube
    (lambda ()
      (println "the cube of" 3 "is" (cube 3))))

  (test_cube)  ;; prints "the cube of 3 is 27"
```

That seems to work as it should. Let’s try calling it with a float value

``` {.extempore}
  (bind-func test_cube
    (lambda ()
      (println "the cube of" 3.0 "is" (cube 3.0))))

  (test_cube)  ;; prints "the cube of 3.000000 is 27.000000"
```

That works too. But hang on, normally when I try and call xtlang
functions with ints rather than floats (or vice-versa) I get a compile
error! Well, the secret to `cube`’s flexibility is in the `!a` *generic*
types in the closure’s signature. This tells the compiler to find a
‘typing’ for `cube` where the return type is the same as the argument.
The compiler won’t always be able to find a typing of the variables
which satisfies these constraints, for instance in the case of `cube`
the type `!a` has to work with the `*` function in the body of the
function as well. But in the example above, the compiler can figure out
the type of `!a` as `i64` in the first case and `double` in the second.
When that happens it compiles a specialised `i64` and `double` version
of the `cube` function under the covers and uses *that* function in the
body of `test_cube`.

# Mapping over lists

Cool, that’s handy. The productivity drain of having to write separate
`cube_i64` and `cube_d` was the *only* thing getting in the way of me
writing the next software product which would [get acquired by Facebook
for 16 billion
dollars](http://techcrunch.com/2014/02/19/facebook-buying-whatsapp-for-16b-in-cash-and-stock-plus-3b-in-rsus/).

Well, not quite. But let’s get back to our goal of finding the sum of
all the even cubes between 0 and 9 (inclusive). We can use the `range`
function[^1] to generate the list of integers from 0 to 9, which seems
like a start.

``` {.extempore}
  (bind-func sum_even_cubes_below_ten
    (lambda ()
      (let ((start_list (range 10)))
        (list_print start_list))))

  (sum_even_cubes_below_ten) ;; prints (0 1 2 3 4 5 6 7 8 9)
```

What we want to do is find the cube of all those numbers. We could do a
`dotimes` loop, storing the results somewhere as we went, but `map` is
much nicer:

``` {.extempore}
  (bind-func sum_even_cubes_below_ten
    (lambda ()
      (let ((start_list (range 10))
            (cube_list (map (lambda (x) (cube x)) start_list)))
        (list_print cube_list))))

  (sum_even_cubes_below_ten) ;; prints (0 1 8 27 64 125 216 343 512 729)
```

Zooming in on the call to `map`, we passed in two arguments:

-   an anonymous closure (see the `lambda`) which took one argument and
    applied the `cube` function we wrote earlier
-   our original `start_list` of integers below ten.

``` {.extempore}
  (cube_list (map (lambda (x) (cube x)) start_list))
```

Notice the lack of type annotations (e.g. `:i64`), how does the compiler
know the types of all these things? Well, the secret is in the type
signature for `map`

``` {.extempore}
  ;; from libs/core/adt.xtm
  (bind-func map:[List:<!a,List*>*,[!a,!b]*,List:<!b,List*>*]*

    ...
```

Even if that long type signature is a bit confronting, note that there
are two generic types in there: `!a` and `!b`. Basically, this `map` has

-   **return value**: a list with elements of type `!a`
-   **first argument**: a closure which takes a `!b` and returns an `!a`
-   **second argument**: a list with elements of type `!b`

In our `sum_even_cubes_below_ten` example, the argument type and return
type of the mapped function (`cube`) were the same (both `i64`)—but they
don’t have to be. If you’re feeling adventurous, try writing a version
which returns a tuple containing the original value and it’s cube (e.g.
`<2,8>`).

`map` is also polymorphic: there are version of `map` for mapping 2 and
3 argument functions `adt.xtm` as well. You might be able to guess what
type signatures they have, involving `!c` in the 2 arg case and both
`!c` and `!d` in the 3 arg case.

# Filtering

What about sorting the wheat from the chaff? We’ll use `filter`. Again,
filter takes a closure as a first argument which will be called on all
the members of the input list: if it returns `bool` true, then keep it
in the returned list, if it returns `bool` false then keep it out.

``` {.extempore}
  (bind-func sum_even_cubes_below_ten
    (lambda ()
      (let ((start_list (range 10))
            (even_cube_list (filter
                             ;; filtering function: true for even numbers
                             (lambda (x) (= (% (convert x i64) 2) 0))
                             ;; our original map to return the cubes
                             (map (lambda (x) (cube x)) start_list))))
        (list_print even_cube_list))))

  (sum_even_cubes_below_ten) ;; prints (0 8 64 216 512)
```

# Folding lists

The final step is to take the sum of all the numbers in this filtered
list. We can use `foldl` to ‘fold’ an addition function over the list (0
8 64 216 512), effectively giving us (+ (+ (+ (+ (+ 0 0) 8) 64) 216)
512) [^2]

``` {.extempore}
  (bind-func sum_even_cubes_below_ten
    (lambda ()
      (let ((start_list (range 10))
            (even_cube_list (filter
                             (lambda (x) (= (% (convert x i64) 2) 0))
                             (map (lambda (x) (cube x)) start_list))))
        (println "the sum is"
                 (foldl
                  (lambda (a:i64 b) (+ a b)) ;; folding function
                  0                     ;; initial value
                  even_cube_list)       ;; list to fold over
                 ))))

  (sum_even_cubes_below_ten) ;; prints "the sum is 800"
```

It looks like the sum is 800. Success! But we can even skip the
filtering step by providing a smarter ‘reducing’ function which only
adds the cube to the total if it’s even.

``` {.extempore}
  (bind-func sum_even_cubes_below_ten
    (lambda ()
      (let ((start_list (range 10))
            (cube_list (map (lambda (x) (cube x)) start_list)))
        (println "the sum is"
                 (foldl
                  ;; folding function: only add to total if x is even
                  (lambda (total x) (if (= (% (convert x i64) 2) 0) (+ x total) total))
                  (convert 0) ;; initial value
                  cube_list))))))

  (sum_even_cubes_below_ten) ;; prints "the sum is 800"
```

Woo—Both versions are in agreement! Notice that there are a couple of
`convert` calls in there, in the ‘modulo 2 even check’ part of the
folding function and also in the initial value. The reason they’re there
is so that I can do this:

``` {.extempore}
  (bind-func sum_even_cubes_below_ten
    (lambda ()
      (let ((start_list (range 10.0))
            (cube_list (map (lambda (x) (cube x)) start_list)))
        (println "the sum is"
                 (foldl
                  ;; folding function: only add to total if x is even
                  (lambda (total x) (if (= (% (convert x i64) 2) 0) (+ x total) total))
                  (convert 0) ;; initial value
                  cube_list))))))

  (sum_even_cubes_below_ten) ;; prints "the sum is 800.000000"
```

There’s only one tiny change: in this version `range` is called with a
floating point `10.0` instead of an integer `10`. And the result is also
now a floating point `800.000000`. All the rest of the code is exactly
the same as before, but `cube`, `map` and `foldl` still worked as
expected. The `convert` calls were added to make sure the literal values
were automatically converted to the right type—the starting value needs
to be a `0` in the int case and a `0.0` in the float case.

# Wrapping up

Well, this didn’t turn out to be quite as bite-sized an example of
xtlang’s generics as I’d hoped. I’ll have to have a crack at a more
succinct explanation soon. Also, writing generic float/int code barely
scratches the surface of what is possible. Things get even more
interesting when dealing with more complex user-defined generic types:
`Points`, `HashTables`, `BTree`, etc. `map`, `filter`, `foldl` and
`foldr` can all be used to great effect with these types. The unit tests
in `tests/generics.xtm` have some examples of this in action.

One other thing to remember is that all this type specialisation happens
at compile time (which is one of the reasons that compilation with
generics can be *slow*), but the runtime performance should be
blazing—in general a pretty worthwhile tradeoff. When the compiler
rewrite in xtlang happens, we’ll all get a pony. And by that I mean
*much* faster compilation.

Enjoy messing around, and if you’ve got any questions [hit us up on the
list](mailto:extemporelang@googlegroups.com).

[^1]: the range function can also generate lists of doubles, or take a
    `start` and `step` argument, see `libs/core/adt.xtm`

[^2]: note that there’s an extra 0 in the innermost brackets—this is the
    initial value (second argument) supplied to the `foldr` function
