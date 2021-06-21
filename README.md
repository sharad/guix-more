More packages for the Guix package manager! This repository contains
recipes for packages that I am working on. They will eventually be sent
upstream. When this is the case, they will vanish from this repository.
Note that most of these packages are WIP and might not work very well, or might
not even build. Your help is very welcome!

This repository should only contain free software.  If you encounter non-free
software, please open an issue, email me or ping me on IRC (roptat on libera.chat).

How to use these packages?
==========================


This is a Guix channel. You will first need to install
[Guix](https://www.gnu.org/software/guix/download/) itself. Then, simply create
a new `~/.config/guix/channels.scm` file with this content, or update the
file with the additional channel if it already exists:

```scheme
(cons* (channel
        (name 'guix-android)
        (url "https://git.lepiller.eu/git/guix-more.git"))
       %default-channels)
```

Then run `guix pull` to pull the new channel.

### Important checks ###

Make sure your guix environment is set up properly. You need to have
`~/.config/guix/current` as the **first** item in your `$PATH` or you're going
to run into troubles. Additionally, after running `guix pull`, make sure you
run `hash guix` in any open terminal to make sure bash's cache is cleared of
the old guix binary location.

Current work
============

Adding Scala and SBT
--------------------

Scala is not bootstrappable, so it will not go into Guix easily.  In this repo,
I rebuild Scala using the official Scala binary, and manage to build SBT using
it.  Note that I only packaged sbt-launcher, which is what you get when you
download sbt from their website.  Its role is to download and execute the
full version of sbt from the repositories.

Adding Gradle
-------------

Gradle is a build system used mostly by android applications. I have packages
for 2/3 of gradle in the past, but the project evolved quickly since then, so
I will have to restart from the beginning.

Other Packages
--------------

As the name suggests, this channel contains more packages that I plan to upstream
at some point, when I have time.
