---
title: xtlang naming conventions
alias: ["./2012-10-15-xtlang-naming-conventions.html"]
categories: xtlang
---

These conventions aren’t (currently) used consistently throughout the
Extempore codebase, but we’re hoping to bring everything into line
eventually—pull requests welcome :)

# Scheme conventions

-   **Spaces** in names should use dashes (`-`), e.g.
    `my-scheme-variable`
-   **Predicates** (functions that *test* their arguments and return
    `#t` or `#f`) should end in `?`, e.g. `integer?`
-   **Destructive functions** (which modify their arguments in-place)
    should end in a `!`, e.g. `set-cdr!`
-   **Global variables** should be surrounded by `*`, e.g.
    `*my-scheme-global*`

# xtlang conventions

Some of the differences between the xtlang and the Scheme conventions
are for technical reasons (xtlang’s identifiers have the same
limitations as C, e.g. no `!`), while others are to help differentiate
between Scheme and xtlang visually (e.g. dashes in Scheme vs underscores
in xtlang).

-   **Closures** should use “snake case”, e.g. `my_xtlang_closure`
-   **Types** (except for the [built-in
    types](2012-08-09-xtlang-type-reference.org)) should use “camel
    case”, with a capitalised first letter e.g. `MyType`
-   Closures associated with a named type should start with the name of
    the type (e.g. `MyType_create`, `MyType_do_all_the_things`)
-   If a closure allocates [zone
    memory](2012-08-17-memory-management-in-extempore.org) (i.e. if it
    calls `alloc` or `zalloc`) then there should be another version of
    the closure with the suffix `_h` which allocates from the heap (i.e.
    `halloc`) instead
-   **Global variables** should be uppercased, e.g. `MY_XTLANG_GLOBAL`

## Memory

### Allocation

-   For named type `TypeName`, the “naive” constructors (which just take
    the elements of the type as arguments) will be compiled
    automatically, and called `TypeName_z` (the new type will
    be zone-allocated) and `TypeName_h` (the new type will
    be heap-allocated).

<!-- -->

-   In addition, the `_z` version will be poly’d to `TypeName`. If
    additional constructors are added, e.g. “smart” constructors which
    take advantage of relationships between the elements of the tuple or
    perform bounds checks on the arguments, these may also be poly’d to
    `TypeName` (assuming they have a different signature to the naive
    constructor `TypeName_z`).

### Copying

### Deallocation

-   `free` in xtlang is the same as free in `stdlib.h` - when passed a
    `TypeName*` it will free the memory for the actual tuple, but will
    leak any resources managed by the fields

# Capitalisation conventions

-   In prose *Extempore* should be capitalised, while *xtlang* should
    remain uncapitalised.

