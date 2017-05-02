More packages for the Guix package manager! This repository contains
recipes for packages that I am working on. They will eventually be sent
upstream. When this is the case, they will vanish from this repo.

How to use these packages?
==========================

```sh
export GUIX_PACKAGE_PATH=$HOME/guix-more
```

That's it!

What is in there?
=================

Binary analysis software
------------------------

* _capstone_: A disassembler framework with a lot of supported architectures
* _z3_: A constraint solver
* _angr_: A binary analysis tool
* _radare2_: A disassembler and analysis tool
* _bap_: A binary analysis platform

Games
-----

* _lugaru_: A third-person action game

Video
-----

* _aegisub_: A subtitle editor

Webservices
-----------

* _pootle_: A community translation server

Java
----

* _antlr3_: A parser generator
* _josm_: An openstreetmap editor

Non-free
--------

* _compcert_: A certified C compiler

Current work
============

Using skia with icecat
----------------------

Currently working on this. I have skia, but it requires some tricks to actually
have icecat build with it.

Adding gradle and groovy
------------------------

Gradle is partially written in groovy, which itself is partially written in
groovy. I currently am working on some dependencies, but I don't have a clear
plan for groovy.
