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

* _angr_: A binary analysis tool

Education
---------

* _morji_: A spaced-repetition program

Java
----

* _fop_: A print formatter driven by XSL formatting objects

Openstreetmap
-------------

* _josm_: An openstreetmap editor
* _imposm_: A script to import data from OSM to a postgis database
* _tegola_: A program to produce vector tiles from a postgis database

Non-free
--------

* _compcert_: A certified C compiler

Current work
============

Using skia with icecat
----------------------

Currently working on this. I have skia, but it requires some tricks to actually
have icecat build with it.

Adding gradle and scala/sbt
---------------------------

Gradle is a build system used mostly by android applications. I have packages
for 2/3 of gradle, but that's not enough. I'm currently blocked at a gradle
subproject that requires scala and sbt. Scala is written in scala, so it's hard
to bootstrap. I've packaged a binary version for now and building sbt (another
dependency) is my next target.

Adding a maven-build-system
---------------------------

Although the maven package is now in Guix proper, we do not have a maven-build-system
yet. Such a build system requires maven and some plugins. I'm planning to build
these plugins. They require a lot of new dependencies for which we can't use
the maven-build-system either... Such a pain :/

OCaml and Coq packages
----------------------

The more the better. They are being integrated.

TODO
====

Eiffel studio -> eiffel -> AutoProof and Eve -> boogie.

alt-ergo, CVC4
