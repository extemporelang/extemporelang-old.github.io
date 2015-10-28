---
layout: guide
title: Getting Started
---

## Introduction

The Extempore environment typically consists of two moving parts; the extempore process itself, and a 
text editor which can send code to the process in order to be evaluated. We support a number of different
<a href="/guides/getting-started/#text-editors">text editors</a>, and installing extempore on most platforms 
is straight forward.

## Installing Extempore

### OSX

OSX installation requires homebrew to be installed.

`brew tap benswift/extempore`

`brew install extempore`

Installation from source is also possible.

### Linux

#### Dependencies

__a C++ compiler toolchain__

On Ubuntu/Debian:

`sudo apt-get install g++`

On Fedora/CentOS/RHEL:
 
`sudo yum install gcc gcc-c++`

__git__

On Ubuntu/Debian:

`sudo apt-get install git` 

On Fedora/CentOS/RHEL:

`sudo yum install git` 

__CMake (version 3.1 or greater)___

On Fedora/CentOS/RHEL:
 
`sudo yum install cmake`

The Ubuntu 15.04 package archive only includes CMake v3.0, but you can get a more up-to-date version through a package archive

`sudo apt-get install software-properties-common && sudo add-apt-repository ppa:george-edison55/cmake-3.x && sudo apt-get update && sudo apt-get install cmake`

__ALSA__

To use the ALSA portaudio backend (which is probably what you want, unless you have a real reason to go with something else) you'll need the libasound package at build-time.

`sudo apt-get install libasound2-dev`


#### Building 

Extempore uses CMake for configuration. In your extempore directory (i.e. the one this INSTALL.md file is in)

`mkdir cmake-build && cd cmake-build && cmake ..`

On Linux/OSX CMake will generate a Makefile in cmake-build, with a few useful targets.

To build Extempore:
 
`make -j4` 

To install extempore into /usr/local/bin and the rest of the files (the "Extempore share directory") into /usr/local/share/extempore

`make install` 

To ahead-of-time compile the core/extended "standard library":

`make aot/make aot_extended` 

To remove the installed files:

`make uninstall` 

### Windows 7

#### Dependencies

We assume you have installed <a href="https://chocolatey.org/">Chocolatey</a> and 
<a href="https://www.nuget.org/">Nuget</a>, and so can successfully run the `choco` and `nuget` command from the terminal (cmd.exe).

__a C++ compiler toolchain__

<a href="https://www.visualstudio.com/en-us/downloads/download-visual-studio-vs.aspx"> 
Visual Studio on Windows (the Community 2015 version is now free)</a>

__CMake__ 

`choco install cmake`

__git__

`choco install git `

__Boost__

We still need one component of the Boost libs on Windows (specifically the ASIO component for TCP/UDP handling). If you've got the NuGet command line client installed, you can probably do:

`nuget install boost-vc140 & nuget install boost_system-vc140 & nuget install boost_regex-vc140 & nuget install boost_date_time-vc140`

It doesn't matter how you get these deps or where you put them, as long as you tell Extempore where they are through the BOOST_DIR cmake variable. The BOOST_DIR should have two subdirectories include and lib, which should contain the boost header directory and the libboost*.lib files respectively.


