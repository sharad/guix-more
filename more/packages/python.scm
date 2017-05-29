;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017 Julien Lepiller <julien@lepiller.eu>
;;;
;;; This file is part of GNU Guix.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (more packages python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages zip)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system python))

(define-public python-cooldict
  (package
    (name "python-cooldict")
    (version "1.02")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cooldict" version))
              (sha256
               (base32
                "084if0s504576nph0f6glmg3mmvijq7nbnf65hh22gwwdwrjss83"))))
    (build-system python-build-system)
    (native-inputs
     `(("ana" ,python-ana)))
    (home-page "https://github.com/zardus/cooldict")
    (synopsis "Some useful dict-like structures")
    (description "Some useful dict-like structures.")
    (license license:bsd-2)))

(define-public python2-cooldict
  (package-with-python2 python-cooldict))
 
(define-public python-cachetools
  (package
    (name "python-cachetools")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cachetools" version))
              (sha256
               (base32
                "0a56ydsvsri1r19ny55g0x7jsgjl9n02vnxbhfz0vhhd4h174nki"))))
    (build-system python-build-system)
    (home-page "https://github.com/tkem/cachetools")
    (synopsis "Memoizing collections and decorators including lru_cache")
    (description "This module provides various memoizing collections and
decorators, including variants of the Python 3 Standard Library @code{lru_cache}
function decorator.")
    (license license:expat)))

(define-public python2-cachetools
  (package-with-python2 python-cachetools))

(define-public python-bintrees
  (package
    (name "python-bintrees")
    (version "2.0.7")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bintrees" version ".zip"))
              (sha256
               (base32
                "0w0js514awl9qwamcr71spi8mmv7q3n4mgrqrnmr9w6f09k5wrv0"))))
    (build-system python-build-system)
    (native-inputs `(("unzip" ,unzip)))
    (home-page "https://github.com/mozman/bintrees")
    (synopsis "Provides Binary- RedBlack- and AVL-Trees written in Python and Cython/C")
    (description "This package provides Binary- RedBlack- and AVL-Trees written
in Python and Cython/C.

This Classes are much slower than the built-in dict class, but all
iterators/generators yielding data in sorted key order. Trees can be uses as
drop in replacement for dicts in most cases.")
    (license license:expat)))

(define-public python2-bintrees
  (package-with-python2 python-bintrees))
 
(define-public python2-dpkt
  (package
    (name "python2-dpkt")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dpkt" version))
              (sha256
               (base32
                "0rr9ygczhxkfb61778jx0cxs0sq46zwlcj5l3wn6xmd3iy3yx9y6"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "setup.py"
                  (("os.system\\('py2dsc-deb ' + sdist_file\\)") "")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/kbandla/dpkt")
    (synopsis "Fast, simple network packet creation / parsing")
    (description "Fast, simple packet creation / parsing, with definitions for
the basic TCP/IP protocols.")
    (license license:bsd-3)))

(define-public python2-rpyc
  (package
    (name "python2-rpyc")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rpyc" version))
              (sha256
               (base32
                "0jwbxxf5f8l05pwh7ilg380y4pqv3nxibaszbwpl9gzh2i9q9yj3"))))
    (build-system python-build-system)
    (native-inputs
     `(("nose" ,python2-nose)))
    (propagated-inputs
     `(("plumbum" ,python2-plumbum)
       ("progressbar" ,python2-progressbar)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/tomerfiliba/rpyc")
    (synopsis "Remote procedure call for Python")
    (description "Remote Python Call is a transparent library for symmetrical
remote procedure calls, clustering, and distributed-computing.  RPyC makes use
of object-proxying, a technique that employs python's dynamic nature, to
overcome the physical boundaries between processes and computers, so that
remote objects can be manipulated as if they were local.")
    (license license:expat)))

(define-public python2-progressbar
  (package
    (name "python2-progressbar")
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "progressbar" version))
              (sha256
               (base32
                "0m0j93yfvbd8pw8cz2vdb9hyk9d0zkkd509k69jrw545jxr8mlxj"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/niltonvolpato/python-progressbar")
    (synopsis "Text progress bar library for Python")
    (description
      "A text progress bar is typically used to display the progress of a long
running operation, providing a visual cue that processing is underway.

The ProgressBar class manages the current progress, and the format of the line
is given by a number of widgets. A widget is an object that may display
differently depending on the state of the progress bar. There are three types
of widgets:

@enumerate
@item a string, which always shows itself
@item a ProgressBarWidget, which may return a different value every time its
update method is called
@item a ProgressBarWidgetHFill, which is like ProgressBarWidget, except it
expands to fill the remaining width of the line.
@end enumerate

The progressbar module is very easy to use, yet very powerful. It will also
automatically enable features like auto-resizing when the system supports it.")
    (license (list license:lgpl2.1+ license:bsd-3))))

(define-public python-progressbar2
  (package
    (name "python-progressbar2")
    (version "3.20.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "progressbar2" version))
              (sha256
               (base32
                "1xz5l3598bl2r1j8h6dqljbjf44f2d137ppi0l381adz4zd38vd1"))))
    (build-system python-build-system)
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)
       ("pytest" ,python-pytest)))
    (propagated-inputs
     `(("six" ,python-six)
       ("utils" ,python-utils)))
    (home-page "https://github.com/WoLpH/python-progressbar")
    (synopsis "A text progress bar for python")
    (description "A text progress bar is typically used to display the progress
of a long running operation, providing a visual cue that processing is underway.

The ProgressBar class manages the current progress, and the format of the line
is given by a number of widgets.  A widget is an object that may display
differently depending on the state of the progress bar.")
    (license license:bsd-2)))

(define-public python2-progressbar2
  (package-with-python2 python-progressbar2))
 
(define-public python-mulpyplexer
  (package
    (name "python-mulpyplexer")
    (version "0.07")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mulpyplexer" version))
              (sha256
               (base32
                "1j5gm913adc8f0mn9y6a9wm9h78jb7ykr8i00yysfcy6ah2ilp9v"))))
    (build-system python-build-system)
    (home-page "https://github.com/zardus/mulpyplexer")
    (synopsis "Multiplex interactions with lists of python objects")
    (description "Mulpyplexer is a piece of code that can multiplex interactions with lists of python objects.")
    (license license:bsd-2)))

(define-public python2-mulpyplexer
  (package-with-python2 python-mulpyplexer))

(define-public python-ana
  (package
    (name "python-ana")
    (version "0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/zardus/ana.git")
                    (commit "94928f773661eaa5a6c2dec40dad199c70daedab")))
              (sha256
               (base32
                "0f2wdhs0xwpnk9lznxl96b2yzcz1641wbqrh1aid7q2pm60v6dhv"))
              (file-name (string-append name "-" version))))
    (build-system python-build-system)
    (native-inputs
     `(("nose" ,python-nose)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-python3-import
           (lambda _
             (substitute* "ana/datalayer.py"
               (("import cPickle as pickle") "import pickle")))))))
    (home-page "https://github.com/zardus/ana")
    (synopsis "Provide easy distributed data storage for python objects")
    (description "ANA is a project to provide easy distributed data storage for
stuff.  It provides every object with a UUID and, when pickled, will first
serialize the object's state to a central location and then \"pickle\" the
object into just its UUID.  This is really handy when you have to distribute
objects in some distributed system, and you'd rather not pickle the whole
object every time you need to send it.")
    (license license:bsd-2)))

(define-public python2-ana
  (package
    (inherit (package-with-python2 python-ana))
    (arguments
     `(#:python ,python-2))))

(define-public python-plumbum
  (package
    (name "python-plumbum")
    (version "1.6.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "plumbum" version))
       (sha256
        (base32 "083kikr1f7qzpp5jllss97dy8d6249v7ia3wg9i0a6wz8l4ffj82"))))
    (build-system python-build-system)
    (native-inputs
     `(("pytest" ,python-pytest)))
    (home-page "https://plumbum.readthedocs.io/en/latest")
    (synopsis "Shell script-like programs in Python")
    (description
      "Plumbum (Latin for lead, which was used to create pipes back in the day)
is a small yet feature-rich library for shell script-like programs in Python.
The motto of the library is “Never write shell scripts again”, and thus it
attempts to mimic the shell syntax (“shell combinators”) where it makes sense,
while keeping it all Pythonic and cross-platform.")
    (license license:expat)))

(define-public python2-plumbum
  (package-with-python2 python-plumbum))
