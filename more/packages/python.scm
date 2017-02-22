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
  #:use-module (gnu packages python)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python)
  #:use-module (guix build-system trivial)
  #:use-module (srfi srfi-1))

(define-public python-setuptools-scm
  (package
    (name "python-setuptools-scm")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "setuptools_scm" version))
              (sha256
               (base32
                "0bwyc5markib0i7i2qlyhdzxhiywzxbkfiapldma8m91m82jvwfs"))))
    (build-system python-build-system)
    (home-page "https://github.com/pypa/setuptools_scm/")
    (synopsis "Manage Python package versions in SCM metadata")
    (description
     "Setuptools_scm handles managing your Python package versions in
@dfn{software configuration management} (SCM) metadata instead of declaring
them as the version argument or in a SCM managed file.")
    (license license:expat)))

(define-public python2-setuptools-scm
  (package-with-python2 python-setuptools-scm))

(define-public python-pytest-runner
  (package
    (name "python-pytest-runner")
    (version "2.11")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "pytest-runner" version))
       (sha256
        (base32
         "08rizp3zz8fdm2a5j97rcq3hddbhq5f6k0vr35amjj83zqpawk5x"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         ;; The fancy way of setting the version with setuptools_scm does not
         ;; seem to work here.
         (add-after 'unpack 'set-version
          (lambda _
            (substitute* "docs/conf.py"
              (("version = setuptools_scm\\.get_version\\(root='\\.\\.')")
               (string-append "version = \"" ,version "\"")))
            #t)))))
    (native-inputs
     `(("python-pytest" ,python-pytest)
       ("python-setuptools-scm" ,python-setuptools-scm)))
    (home-page "https://github.com/pytest-dev/pytest-runner")
    (synopsis "Invoke py.test as a distutils command")
    (description
     "This package provides a @command{pytest-runner} command that
@file{setup.py} files can use to run tests.")
    (license license:expat)))

(define-public python2-pytest-runner
  (package-with-python2 python-pytest-runner))

(define-public python-utils
  (package
    (name "python-utils")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "python-utils" version))
              (sha256
               (base32
                "1zvrc1rc06n89pycg969pcy30bff4sqzhff365sxh629ybnl8pwq"))))
    (build-system python-build-system)
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)))
    (home-page "https://github.com/WoLpH/python-utils")
    (synopsis "Convenient utilities not included with the standard Python install")
    (description
      "Python Utils is a collection of small Python functions and classes which
make common patterns shorter and easier.")
    (license license:bsd-2)))

(define-public python2-utils
  (package-with-python2 python-utils))

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
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bintrees" version))
              (sha256
               (base32
                "1q71md58i33qpjhwgi9ph9hpgch1dy2i6n4qljyl0x410rhikfvc"))))
    (build-system python-build-system)
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
    (version "1.8.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "dpkt" version))
              (sha256
               (base32
                "004qsqzg3fwkh623l1y8j62ai166hr02y192s7n1hs166kjjb5fr"))
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
 
(define-public python-rpyc
  (package
    (name "python-rpyc")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rpyc" version))
              (sha256
               (base32
                "0jwbxxf5f8l05pwh7ilg380y4pqv3nxibaszbwpl9gzh2i9q9yj3"))))
    (build-system python-build-system)
    (home-page "https://github.com/tomerfiliba/rpyc")
    (synopsis "Remote procedure call for Python")
    (description "Remote Python Call is a transparent library for symmetrical
remote procedure calls, clustering, and distributed-computing.  RPyC makes use
of object-proxying, a technique that employs python's dynamic nature, to
overcome the physical boundaries between processes and computers, so that
remote objects can be manipulated as if they were local.")
    (license license:expat)))

(define-public python2-rpyc
  (package-with-python2 python-rpyc))

(define-public python-progressbar2
  (package
    (name "python-progressbar2")
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "progressbar2" version))
              (sha256
               (base32
                "16r21cpjvv0spf4mymgpy7hx6977iy11k44n2w9kipwg4lhwh02k"))))
    (build-system python-build-system)
    (native-inputs
     `(("pytest-runner" ,python-pytest-runner)))
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

;; Not reproducible.
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
    (home-page "https://github.com/zardus/ana")
    (synopsis "Provide easy distributed data storage for python objects")
    (description "ANA is a project to provide easy distributed data storage for
stuff.  It provides every object with a UUID and, when pickled, will first
serialize the object's state to a central location and then \"pickle\" the
object into just its UUID.  This is really handy when you have to distribute
objects in some distributed system, and you'd rather not pickle the whole
object every time you need to send it.")
    (license license:bsd-2)))

;; Not reproducible.
(define-public python2-ana
  (package-with-python2 python-ana))
