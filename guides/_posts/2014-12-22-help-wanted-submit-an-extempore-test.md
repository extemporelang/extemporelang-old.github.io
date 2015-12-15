---
title: Help wanted: submit an Extempore test
tags: contributing
---

The Extempore community[^1] has done some cool stuff recently, including
Cody’s “Extempore in a virtualbox” work, Giora’s flurry of pull
requests, not to mention many others.

One thing that becomes increasingly necessary as the pull requests start
coming in is good test coverage. Extempore has an automated test suite,
and the tests themselves are in the `tests/` directory. The general idea
is that this directory structure will grow to mirror the `libs/` one, so
that `tests/core/adt.xtm` has tests for the code in `libs/core/adt.xtm`,
etc. Some of these test files exist already (although more tests are
necessary in these cases), while others need to be created as new tests
are written.

# The xtmtest API

The `xtmtest` macro takes up to three arguments:

1.  the xtlang function to test (e.g. a `bind-func`)
2.  a call to said function
3.  (optional) the expected return value

Here’s an example:

``` {.extempore}
  ;; load the test harness
  (sys:load "libs/core/test.xtm")

  ;; note the quote (') mark
  (xtmtest '(bind-func add_two_integers
              (lambda (a:i64 b)
                (+ a b)))
           (add_two_integers 2 3)
           5)
  ;; prints:
  ;;; xtmtest  add_two_integers
  ;;; result: correct
```

If things don’t work as you’d expect, make sure you’ve quoted the
`bind-func` form.

If you want to test the function (e.g. `add_two_integers`) with multiple
calls to check that it returns the right results with various arguments,
use the `xtmtest-result` function (call this as many times as you like
with different arguments and return values).

``` {.extempore}
  (xtmtest-result (add_two_integers 1 5) 6)
  (xtmtest-result (add_two_integers 10 5000) 5010)
```

# Running a test suite

Like I mentioned, the tests are organised into files in the Extempore
`tests/` directory. There are also a couple of “aggregate” test files,
`tests/all-core.xtm` (tests for the `libs/core/`),
`tests/all-external.xtm` (tests for `libs/external/`) and
`tests/all.xtm` (test all the things!).

To run a file of tests, use the `sys:run-tests` function. For example,
to run all the tests in `tests/core/xtlang.xtm`

``` {.extempore}
  (sys:run-tests "tests/core/xtlang.xtm" ;; test file
                 #t                      ;; print results?
                 #t                      ;; exit when finished?
                 )
```

The second argument is `#t` (or any value other than `#f`) then the test
results will be pretty-printed to the log as they are run, and a summary
will be printed at the end.

If the third argument (`quit-on-exit?`) is not `#f`, then Extempore will
exit once the test file has been run. The `extempore` process will
return a meaningful error code (`0` for success, non-zero for failure),
and so this should play nice with automated tools—we’ll put together a
TravisCI test bot at some point.

Finally, there’s a helper bash script called `test-all.sh` in the
top-level Extempore directory which will run all the tests.

# All I want for Christmas is a new Extempore test

We really appreciate bug reports, and the best way to submit them these
days is in the form of a failing test.

You can just paste the failing test into an email to the [mailing
list](mailto:extemporelang@googlegroups.com), or you can submit a pull
request with the test in it. If you’re not sure where your test should
go, you can still submit it as a pull request—just add the test code to
the bottom of the `tests/failing.xtm` file.

Remember to run all the *passing* tests (e.g. with `test-all.sh`) before
you submit the pull request, just to make sure you haven’t committed
anything else by mistake.

# Merry Christmas

I’m going to be off work between Christmas and new year, so to all those
who follow this blog from the livecoding community and beyond—have a
great Christmas and a safe New Year, or 春节 (Spring Festival) or
whatever you celebrate at this time of year. See you on the mailing list
in 2015.

[^1]: i.e. everyone who’s not me or Andrew :)
