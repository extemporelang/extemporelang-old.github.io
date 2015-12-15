---
title: A distributed package manager for the Extempore live programming environment
categories: tools contributing
---

This is a possible (group) project for [ANU COMP 3100, 3500, 3550, 4500
or 8715](http://cs.anu.edu.au/courses/COMP3100/) in 2015. There are
sub-projects in here if you’re interested in doing it as an individual
project, too. [Send me an email](mailto:ben.swift@anu.edu.au) if you’re
interested.

## Project overview

These days, a good [package
manager](http://en.wikipedia.org/wiki/Package_manager) is an essential
part of a programming language ecosystem. The goal of this project is to
design and build a package manager (both the server infrastructure and a
reference client) for the [Extempore](http://extempore.moso.com.au) live
programming environment.

If you’ve never seen Extempore before, here’s a short video of it in
action [in my livecoding](https://vimeo.com/78788032). You could also
have a look around the other posts on this blog—the [docs
page](../extempore-docs/index.org) would be a good place to start.

## Prior art

Package managers have been around for a long time in various forms, from
Perl’s venerable [CPAN](http://www.cpan.org/) to more recent
incarnations such as Rust’s [Cargo](https://crates.io/). The first stage
of the project will be to examine existing package managers to see how
they do things.

### Language-specific package managers

-   Perl - [CPAN](http://www.cpan.org/)
-   R - [CRAN](http://cran.r-project.org/)
-   Java - [ANT](http://ant.apache.org/),
    [Maven](http://maven.apache.org/)
-   Ruby - [Gems](https://rubygems.org/)
-   Python - [PyPI](https://pypi.python.org/pypi/pip)
-   Javascript - [npm](https://www.npmjs.com/)
-   Rust - [Cargo](https://crates.io/)
-   Go - [godep](https://github.com/tools/godep)

### OS-Level package managers

-   Debian/Ubuntu - [APT](https://wiki.debian.org/Apt)
-   Arch Linux - [Pacman](https://wiki.archlinux.org/index.php/pacman)
-   OSX - [Homebrew](http://brew.sh)
-   Windows [Chocolatey](https://chocolatey.org/),
    [Scoop](http://scoop.sh)

## Requirements gathering

The next stage of the project will to gather requirements from the
Extempore developer community, investigating issues such as:

-   which tasks must the tool perform: search, download/install, upload,
    uninstall, etc.
-   how will it perform dependency resolution?
-   how will it handle packages which require both native Extempore code
    and foreign libs (C/C++/Fortran)?
-   what challenges/opportunities does the *live* nature of Extempore
    bring to the task of package management?

## Designing an API

The package manager client and server infrastructure should communicate
through a well-designed application programming interface (API). From
the examination of existing package managers and the interaction with
the Extempore developer community, the API must be designed to balance
usability and accessability for new programmers with the flexibility and
power demanded by more specialised users, such as compile-time options,
cross-platform support, and package versioning.

The design must also balance the needs of package
contributors/maintainers and package users.

## Building the package distribution infrastructure

Once a (draft) design is put together, the next step is to build out the
client and server infrastructure.

It is anticipated that the API design will also change through the build
process as unforseen issues arise. The project will therefore proceed
with an agile-style feedback loop between the design and implementation
steps, with working code (rather than an inert specification) being the
end goal of the project.

### Server infrastructure

There are several design considerations in building the package serving
infrastructure

-   centralised (e.g. [Amazon S3](http://aws.amazon.com/s3/)) vs
    decentralised
-   pre-built packages vs source only (local compilation)
-   how to ensure high-availability (mirroring, fault-tolerance)

### Building a reference client

The project also requires the implementation of a reference client in
Extempore, which allows the Extempore programmer to search for and
install packages which have been contributed to the global package
repository.

``` {.extempore}
(xpm-search "sndfile")
;;
;; Search results
;; --------------
;;
;; pkg name: libsndfile
;;  version: 1.0.25
;;      url: http://www.mega-nerd.com/libsndfile/
;;
;;  details:
;;
;;    libsndfile provides tools for reading and
;;    writing audio files in various different formats
(xpm-install "libsndfile")
;;
;; Installing package "libsndfile"
;; -------------------------------
;;
;; Downloading...done
;;
;; Using system C compiler: gcc
;;
;;   ./configure -- prefix=/usr/local/lib/xpm && make && make install
;;
;; Successfully installed package "libsndfile"
;;
```

The public API will also allow for other clients, for example a web
interface for browsing available packages.

## Packaging existing Extempore libraries

Finally, the project will involve taking the existing Extempore library
situation (which is mostly ad-hoc) and converting it to use the new
package management infrastructure. This includes libraries for linear
algebra, FFTs, audio signal processing, reading/writing sound files in
various formats, and both 2D and 3D graphics.

## Outcomes

In successfully completing this project, the group will

-   engage with current Extempore programmers/users at every stage of
    the process
-   design and deliver a high-availability distributed (server & client)
    package management infrastructure
-   provide a meaningful real-world contribution to the Extempore
    project, a home-grown ANU programming environment for live coding

## More info

As with any project, there are aspects which can be tailored/tweaked to
suit the needs of the individuals and groups involved. Again, [get in
touch](mailto:ben.swift@anu.edu.au) if you want to chat about it more.
