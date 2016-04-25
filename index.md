---
layout: default
---

An environment for cyberphysical programming.

## Documentation

See the
[documentation website](http://digego.github.io/extempore/index.html).

## Getting started

To get started, you can either download a binary release or build
Extempore from source yourself.

### Download a pre-built binary

Download a
[binary release](https://github.com/digego/extempore/releases), unzip
it and run `extempore.exe` from inside the `extempore` folder.

### Build from source

This will download and build all the dependencies you need (including
LLVM). So, if you've got a C++ compiler, git and CMake, here are some
one-liner build commands:

On **Linux/OSX**:

    git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake .. && make install

On **Windows**:

    git clone https://github.com/digego/extempore && mkdir extempore/cmake-build && cd extempore/cmake-build && cmake -G"Visual Studio 14 2015 Win64" .. && cmake --build . --target ALL_BUILD --config Release

## Examples

To see Extempore in action, check out these videos:

<iframe width="640" height="390" src="https://www.youtube.com/embed/yY1FSsUV-8c" frameborder="0" allowfullscreen></iframe>

- [The Concert Programmer](https://www.youtube.com/watch?v=yY1FSsUV-8c)
- [Interactive, distributed, physics simulation](https://vimeo.com/126577281)
- [Programming in Time](https://www.youtube.com/watch?v=Sg2BjFQnr9s)
- [The Physics Playroom - interactive installation](https://vimeo.com/58239256)
- [An *old* Graphics Demo](https://vimeo.com/37293927)
- [A Programmer's Guide to Western Music](https://www.youtube.com/watch?v=xpSYWd_aIiI)

## Community

Join the Extempore community:

- [Extempore google group](http://groups.google.com/group/extemporelang)
- [Extempore mailing list](mailto:extemporelang@googlegroups.com)

## Licence

Copyright (c) 2011-2016, Andrew Sorensen

All rights reserved.

Redistribution and use in source and binary forms, with or without 
modification, are permitted provided that the following conditions are met:

1. Redistributions of source code must retain the above copyright notice, 
   this list of conditions and the following disclaimer.

2. Redistributions in binary form must reproduce the above copyright notice,
   this list of conditions and the following disclaimer in the documentation 
   and/or other materials provided with the distribution.

Neither the name of the authors nor other contributors may be used to endorse
or promote products derived from this software without specific prior written 
permission.

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" 
AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE 
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE 
ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE 
LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR 
CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF 
SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS 
INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN 
CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) 
ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE 
POSSIBILITY OF SUCH DAMAGE.

<a href="https://github.com/digego/extempore" class="github-corner"><svg width="80" height="80" viewBox="0 0 250 250" style="fill:#151513; color:#fff; position: absolute; top: 0; border: 0; right: 0;"><path d="M0,0 L115,115 L130,115 L142,142 L250,250 L250,0 Z"></path><path d="M128.3,109.0 C113.8,99.7 119.0,89.6 119.0,89.6 C122.0,82.7 120.5,78.6 120.5,78.6 C119.2,72.0 123.4,76.3 123.4,76.3 C127.3,80.9 125.5,87.3 125.5,87.3 C122.9,97.6 130.6,101.9 134.4,103.2" fill="currentColor" style="transform-origin: 130px 106px;" class="octo-arm"></path><path d="M115.0,115.0 C114.9,115.1 118.7,116.5 119.8,115.4 L133.7,101.6 C136.9,99.2 139.9,98.4 142.2,98.6 C133.8,88.0 127.5,74.4 143.8,58.0 C148.5,53.4 154.0,51.2 159.7,51.0 C160.3,49.4 163.2,43.6 171.4,40.1 C171.4,40.1 176.1,42.5 178.8,56.2 C183.1,58.6 187.2,61.8 190.9,65.4 C194.5,69.0 197.7,73.2 200.1,77.6 C213.8,80.2 216.3,84.9 216.3,84.9 C212.7,93.1 206.9,96.0 205.4,96.6 C205.1,102.4 203.0,107.8 198.3,112.5 C181.9,128.9 168.3,122.5 157.7,114.1 C157.9,116.9 156.7,120.9 152.7,124.9 L141.0,136.5 C139.8,137.7 141.6,141.9 141.8,141.8 Z" fill="currentColor" class="octo-body"></path></svg></a><style>.github-corner:hover .octo-arm{animation:octocat-wave 560ms ease-in-out}@keyframes octocat-wave{0%,100%{transform:rotate(0)}20%,60%{transform:rotate(-25deg)}40%,80%{transform:rotate(10deg)}}@media (max-width:500px){.github-corner:hover .octo-arm{animation:none}.github-corner .octo-arm{animation:octocat-wave 560ms ease-in-out}}</style>
