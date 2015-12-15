---
title: Building Extempore through Homebrew on OSX and Linux
categories: setup osx linux
---

If you’ve tried to install Extempore in the past, you’ll know that
building from source is a (moderately) painful process with about half a
dozen steps. We’ve tried to make Extempore binaries, but making sure
they work across the variety of platforms/processors is a massive pain,
especially for all the shared lib dependencies.

# Enter Homebrew

[Homebrew](http://brew.sh) has been my go-to package manager on OSX for
several years now. In fact, over the last couple of years I’ve gradually
been contributing packages for various Extempore dependencies to make my
own life easier.

Well, now there’s a homebrew formula for Extempore. The reason there
hasn’t been one in the past is the need to patch and use a patched
version of LLVM, and since homebrew doesn’t really allow for multiple
versions of a package side-by-side, I haven’t been able to get a patched
LLVM for Extempore into the homebrew repo.

However, it’s super easy in homebrew for me to maintain my *own* package
repo (which homebrew calls a **tap**), and I can have whatever packages
I like in it. So, assuming you have homebrew installed, make sure
everything is up-to-date

``` {.bash}
brew update

# check if anything is outdated
brew outdated

# upgrade all outdated packages
brew upgrade
```

Then, tap my homebrew repo

``` {.bash}
brew tap benswift/extempore
```

After that, homebrew knows about all the packages in my tap, which
includes Extempore. You can install it with

``` {.bash}
brew install extempore
```

which will install the latest stable version of Extempore, as well as
(by default) all of the shared libs you need for the Extempore standard
library. If you want to grab the latest version from the `master`
branch, you can tell `brew` to do that with an additional `--HEAD`
argument.

Your extempore directory will be in
`/usr/local/Cellar/extempore/<VERSION_NUMBER>`, unless you installed
homebrew somewhere other than `/usr/local`. `cd` into that directory,
run `./extempore` and then you’re ready to connect up an editor and
[start evaluating Extempore
code](./2012-09-26-interacting-with-the-extempore-compiler.org). You
might also want to think about [pre-building the Extempore standard
library](2013-12-16-building-the-extempore-standard-library.org), since
that will make loading the audio and graphics libraries on startup
*much* quicker (\~20 seconds vs several minutes).

# Homebrew on Linux

There’s a fork of [homebrew for
Linux](https://github.com/Homebrew/linuxbrew) as well. Homebrew on Linux
lives in a `.linuxbrew` folder inside your home directory, so it
shouldn’t interfere with your ‘regular’ package manager (`apt-get`,
`yum`, `pacman`, etc.). This (homebrew) installation method works pretty
reliably on Ubuntu 14.04, and should also work for older Ubuntus.
Linuxbrew nominally also works on Fedora, but things are a bit hairier
there—[let me know](mailto:extemporelang@googlegroups.com) if you get it
to work and I’ll add any tips here.

If you’d rather not install homebrew on your linux box then you can
still [build from
source](./2013-03-20-building-extempore-on-osx-linux.org) as before.

You can install homebrew on Ubuntu (or Debian) with

``` {.bash}
sudo apt-get install build-essential curl git m4 ruby texinfo libbz2-dev python-setuptools libcurl4-openssl-dev libexpat-dev libncurses-dev zlib1g-dev
ruby -e "$(wget -O- https://raw.github.com/Homebrew/linuxbrew/go/install)"
```

For other Linux distros, see the [Linuxbrew
readme](https://github.com/Homebrew/linuxbrew#dependencies) for
installation instructions.

Once you’ve installed homebrew on your Linux box, to install Extempore
you’ll need one or two other packages from your ‘regular’ package
manager. On Ubuntu 14.04 I needed these packages:

``` {.bash}
sudo apt-get install libasound2-dev libgl1-mesa-dev libglu1-mesa-dev
```

After that, you can tap my *Linux* Extempore homebrew repo and install
Extempore:

``` {.bash}
brew tap benswift/extemporelinux
brew install extempore
```

## If you want Jack support

If you want to build Portaudio with Jack support (through homebrew),
then you have to do a bit more mucking around.

**After** doing a full install (including portaudio and extempore) as
described above, then you need to remove portaudio and extempore

``` {.bash}
brew remove portaudio extempore
```

Then, rebuild portaudio while telling it where the Jack files are:

``` {.bash}
JACK_LIBS="-L/usr/lib/x86_64-linux-gnu -ljack" JACK_CFLAGS="-I/usr/include/jack" brew install --verbose portaudio
```

where `JACK_LIBS` and `JACK_CFLAGS` reflect where Jack is on **your**
platform—also check the output to see that Jack was indeed compiled
properly.

Finally, you can reinstall extempore

``` {.bash}
brew install extempore 
```

## Troubleshooting on Linux

Here are some additional tips if you get stuck:

-   If you’ve already installed portaudio through your system package
    manager, then that can cause problems—in particular Extempore will
    crash on startup. If `extempore --print-devices` prints no devies,
    this is probably the problem. To fix it, the best solution we’ve
    found is to uninstall the portaudio through the package manager,
    build extempore through homebrew, and then reinstall portaudio
    through the package manager. Messy, I know, and it may break other
    things, so if you find a better way then let me know.
-   if you’re missing the asound headers, you might need to `sudo
     apt-get install libasound2-dev`
-   Mesa is a good bet for OpenGL stuff, I’ve had the best experiences
    on Ubuntu, where the packages are `sudo apt-get install
     libgl1-mesa-dev libglu1-mesa-dev`
-   On ubuntu, make sure you’ve got the right OpenGL drivers—which you
    can select with the “Additional Drivers” tool

I’m working on smoothing out these issues, I’ll keep this post updated
as I fix them. If you find any other gotchas (and I’m sure there will be
some, there are lots of Linux distros out there) then send an email to
the [Extempore mailing list](mailto:extemporelang@googlegroups.com) and
I’ll add them here as well.

# Automatically building the standard library as well

As of Extempore v0.55, homebrew can [build the Extempore standard
library](2013-12-16-building-the-extempore-standard-library.org) for you
as part of the install/upgrade process:

``` {.bash}
brew install --with-stdlib extempore
```

Currently, this flag is off by default (since it does take a while), but
it’s a handy option to use if you know that you’re going to be tearing
down and starting Extempore regularly.

# using scoop.sh for easy building on Windows

[scoop.sh](http://scoop.sh) seems promising as a similar alternative to
homebrew on Windows. I haven’t looked into it in detail yet myself, but
I’ll hopefully have a crack at making an Extempore package for it at
some stage. If anyone has experience with it and wants to help out,
they’d be very welcome :)
