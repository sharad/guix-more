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
* _angr_: A binary analysis tool, not yet complete