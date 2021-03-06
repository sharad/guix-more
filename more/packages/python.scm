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
  #:use-module (gnu packages aspell)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages enchant)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages networking)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-compression)
  #:use-module (gnu packages python-crypto)
  #:use-module (gnu packages python-web)
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages sphinx)
  #:use-module (gnu packages time)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
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

;; According to pypi, should work with py3
(define-public python2-rpyc
  (package
    (name "python2-rpyc")
    (version "3.4.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "rpyc" version))
              (sha256
               (base32
                "1iw1nkyh8r55xqafl14lp7lih38b5fdqid05s6cp4zd62821v6d8"))))
    (build-system python-build-system)
    ;; (native-inputs
    ;;  `(("nose" ,python2-nose)))
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

;; According to pypi, should work with py3
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

(define-public python-mulpyplexer
  (package
    (name "python-mulpyplexer")
    (version "0.08")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "mulpyplexer" version))
              (sha256
               (base32
                "1zn5d1vyhfjp8x9z5mr9gv8m8gmi3s3jv3kqb790xzi1kqi0p4ya"))))
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
                    (commit "6d37cf9288839c5536ed2075f206d8d2a80c5906")))
              (sha256
               (base32
                "15mvylgfzmsj0n62m6r5xpqzp6qp4nmp9r3j93g0f64z894kqk0q"))
              (file-name (string-append name "-" version))))
    (build-system python-build-system)
    (native-inputs
     `(("nose" ,python-nose)))
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
  (package-with-python2 python-ana))

(define-public python-plumbum
  (package
    (name "python-plumbum")
    (version "1.6.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "plumbum" version))
              (sha256
               (base32
                "1vjbl9qy9fkl3vwiiwpaafmyxfks2sc3b3dhkp4vdgk2pdcv1ayq"))))
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

(define-public python-cymruwhois
  (package
    (name "python-cymruwhois")
    (version "1.6")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "cymruwhois" version))
          (sha256
           (base32
        "0m7jgpglkjd0lsyw64lfw6qxdm0fg0f54145f79kq4rk1vjqbh5n"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public python-ripe-atlas-sagan
  (package
    (name "python-ripe-atlas-sagan")
    (version "1.2.2")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "ripe.atlas.sagan" version))
          (sha256
           (base32
        "1pww7f4kh9cgd9qm7hbnkxg2cvj7mcmwhsan97cl5pd0xqxxnvw3"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("cryptography" ,python-cryptography)
       ("dateutil" ,python-dateutil)
       ("python-nose" ,python-nose)
       ("pytz" ,python-pytz)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python-socketio-client
  (package
    (name "python-socketio-client")
    (version "0.7.2")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "socketIO-client" version))
          (sha256
           (base32
        "1hfjfhyxgql1ndda1bagg8niy8m28byd2r0yq4l7zycwlzxq9kb4"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("websocket-client" ,python-websocket-client)
        ("requests" ,python-requests)))
    (native-inputs
      `(("coverage" ,python-coverage)
    ("nose" ,python-nose)))
    (arguments '(#:tests? #f)); requires network
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python-argparse
  (package
    (name "python-argparse")
    (version "1.4.0")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "argparse" version))
          (sha256
           (base32
        "1r6nznp64j68ih1k537wms7h57nvppq0szmwsaf99n71bfjqkc32"))))
    (build-system python-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python-unittest2-fix
  (package
    (inherit python-unittest2)
    (version "1.1.0")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "unittest2" version))
          (sha256
           (base32
        "0y855kmx7a8rnf81d3lh5lyxai1908xjp0laf4glwa4c8472m212"))))
    (arguments
      `(#:phases
    (modify-phases %standard-phases
      (add-before 'check 'disable-failures
        (lambda _
          (substitute* "unittest2/test/test_result.py"
        (("testGet") "dontTestGet"))
          (substitute* "unittest2/test/test_loader.py"
        (("test_loadTestsFromNames__relative_malformed_name") "dontTest")
        (("test_loadTestsFromName__relative_malformed_name") "dontTest2")))))))
    (propagated-inputs
      `(("traceback2" ,python-traceback2)
    ("six" ,python-six)
    ("argparse" ,python-argparse)))))

(define-public python-funcsigs
  (package
    (name "python-funcsigs")
    (version "1.0.2")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "funcsigs" version))
          (sha256
           (base32
        "0l4g5818ffyfmfs1a924811azhjj8ax9xd1cffr1mzd3ycn0zfx7"))))
    (build-system python-build-system)
    (native-inputs
      `(("unittest2" ,python-unittest2-fix)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public python-ripe-atlas-cousteau
  (package
    (name "python-ripe-atlas-cousteau")
    (version "1.4.1")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "ripe.atlas.cousteau" version))
          (sha256
           (base32
        "1964qllddqqh1sz9psmmb84ahqdy499vavm9wdn0k2v7q6y0vm0p"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("websocket-client" ,python-websocket-client)
        ("socketIO-client" ,python-socketio-client)
        ("dateutil" ,python-dateutil)
        ("jsonschema" ,python-jsonschema)
        ("requests" ,python-requests)))
    (native-inputs
      `(("mock" ,python-mock)
        ("nose" ,python-nose)
        ("funcsigs" ,python-funcsigs)
        ("coverage" ,python-coverage)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python-ripe-atlas-tools
  (package
    (name "python-ripe-atlas-tools")
    (version "2.2.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "ripe.atlas.tools" version))
              (sha256
               (base32
                "1afcf56fyvsxb0i15v43804rqnn0xdp33achds84axnd1rl1375g"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("pyopenssl" ,python-pyopenssl)
       ("sagan" ,python-ripe-atlas-sagan)
       ("cousteau" ,python-ripe-atlas-cousteau)
       ("pyaml" ,python-pyaml)
       ("ipy" ,python-ipy)
       ("tzlocal" ,python-tzlocal)))
    (native-inputs
     `(("mock" ,python-mock)
       ("coverage" ,python-coverage)))
    (arguments
      `(#:tests? #f; tests can't load dependencies
        #:phases
        (modify-phases %standard-phases
          (add-before 'check 'update-dependency
            (lambda _
              ;; Change dependency version to match what we have in guix
              (substitute* "setup.py"
                (("==1.2") "==1.2.2")
                (("==1.4") "==1.4.1"))
              #t)))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public transmon
  (package
    (name "transmon")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "transmon" version))
              (sha256
               (base32
                "1l7lxp4xwymyb1wqhycqg33ry6gza4660k12xkja04kmw4aqv0az"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (propagated-inputs
     `(("python-android-stringslib" ,python-android-stringslib)
       ("python-polib" ,python-polib)
       ("python-pygit2" ,python-pygit2)))
    (home-page "https://framagit.org/tyreunom/transmon")
    (synopsis "")
    (description "")
    (license license:agpl3+)))

(define-public python-neovim
  (package
    (name "python-neovim")
    (version "0.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pynvim" version))
              (sha256
               (base32
                "01dybk4vs452pljn1q3il5z2sd313ki0lgiglc0xmjc6wp290r6g"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-greenlet" ,python-greenlet)
       ("python-msgpack" ,python-msgpack)))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/neovim/pynvim")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
