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
 
(define-public python-cachetools
  (package
    (name "python-cachetools")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cachetools" version))
              (sha256
               (base32
                "0pdw2fr29pxlyn1g5fhdrrqbpn0iw062nv716ngdqvdx7hnizq7d"))))
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

(define-public python-progressbar2
  (package
    (name "python-progressbar2")
    (version "3.34.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "progressbar2" version))
              (sha256
               (base32
                "1gigwmr60bgvjg2b4w93nww065dc4af8bq40b4hr9n9f54jp3w5x"))))
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

(define-public python-trollius
  (package
    (name "python-trollius")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "trollius" version))
              (sha256
               (base32
                "146c60hgcmgjkbf2hmiag52f9i3hka6shwbfybdsmlvqjnfms5nd"))))
    (build-system python-build-system)
    (home-page "https://github.com/haypo/trollius")
    (propagated-inputs
     `(("mock" ,python-mock)
       ("six" ,python-six)))
    (arguments
     `(#:tests? #f))
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public python2-trollius
  (package-with-python2 python-trollius))

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

(define-public python2-neovim
  (let ((parent (package-with-python2 python-neovim)))
    (package
      (inherit parent)
      (propagated-inputs
       `(("trollius" ,python2-trollius)
         ,@(package-propagated-inputs parent))))))

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

(define-public python2-cymruwhois
  (package-with-python2 python-cymruwhois))

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

(define-public python2-ripe-atlas-sagan
  (package-with-python2 python-ripe-atlas-sagan))

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

(define-public python2-socketio-client
  (package-with-python2 python-socketio-client))

(define-public python-linecache2
  (package
    (name "python-linecache2")
    (version "1.0.0")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "linecache2" version))
          (sha256
           (base32
        "0z79g3ds5wk2lvnqw0y2jpakjf32h95bd9zmnvp7dnqhf57gy9jb"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)); circular dependency with unittest2
    (propagated-inputs
      `(("pbr" ,python-pbr)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python2-linecache2
  (package-with-python2 python-linecache2))

(define-public python-traceback2
  (package
    (name "python-traceback2")
    (version "1.4.0")
    (source (origin
          (method url-fetch)
          (uri (pypi-uri "traceback2" version))
          (sha256
           (base32
        "0c1h3jas1jp1fdbn9z2mrgn3jj0hw1x3yhnkxp7jw34q15xcdb05"))))
    (build-system python-build-system)
    (arguments '(#:tests? #f)); circular dependency with unittest2
    (propagated-inputs
      `(("linecache2" ,python-linecache2)
        ("pbr" ,python-pbr)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python2-traceback2
  (package-with-python2 python-traceback2))

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

(define-public python2-argparse
  (package-with-python2 python-argparse))

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

(define-public python2-unittest2-fix
  (package-with-python2 python-unittest2-fix))

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

(define-public python2-funcsigs
  (package-with-python2 python-funcsigs))

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

(define-public python2-ripe-atlas-cousteau
  (package-with-python2 python-ripe-atlas-cousteau))

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

(define-public python2-ripe-atlas-tools
  (package-with-python2 python-ripe-atlas-tools))

(define-public python-web.py
  (package
    (name "python-web.py")
    (version "0.40.dev0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "web.py" version))
              (sha256
               (base32
                "18v91c4s683r7a797a8k9p56r1avwplbbcb3l6lc746xgj6zlr6l"))))
    (build-system python-build-system)
    (home-page "http://webpy.org/")
    (synopsis "")
    (description "")
    (license license:public-domain)))

(define-public python2-web.py
  (package-with-python2 python-web.py))

(define-public python-bitstring
  (package
    (name "python-bitstring")
    (version "3.1.5")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "bitstring" version ".zip"))
              (sha256
               (base32
                "1algq30j6rz12b1902bpw7iijx5lhrfqhl80d4ac6xzkrrpshqy1"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://github.com/scott-griffiths/bitstring")
    (synopsis "Simple construction, analysis and modification of binary data")
    (description "Bitstring is a pure Python module designed to help make the
creation and analysis of binary data as simple and natural as possible.

Bitstrings can be constructed from integers (big and little endian), hex, octal,
binary, strings or files.  They can be sliced, joined, reversed, inserted into,
overwritten, etc.  They can also be read from, searched and replaced, and
navigated in, similar to a file or stream.")
    (license license:expat)))

(define-public python2-bitstring
  (package-with-python2 python-bitstring))

(define-public python-android-stringslib
  (package
    (name "python-android-stringslib")
    (version "0.1.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "android-stringslib" version))
              (sha256
               (base32
                "00k0nzjvym984805ikq22fzd81cr23j74lhamd50p2vf4yalw76n"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)); no tests
    (native-inputs
     `(("python-sphinx" ,python-sphinx)))
    (home-page "https://framagit.org/tyreunom/python-android-strings-lib")
    (synopsis "")
    (description "")
    (license license:expat)))

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

(define-public python-pyenchant
  (package
    (name "python-pyenchant")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyenchant" version))
              (sha256
               (base32
                "1872ckgdip8nj9rnh167m0gsj5754qfg2hjxzsl1s06f5akwscgw"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f; FIXME: Dictionary for language 'en_US' could not be found
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'setlib
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "enchant/_enchant.py"
               (("/opt/local/lib/libenchant.dylib\"")
                (string-append "/opt/local/lib/libenchant.dylib\"\n"
                               "    yield \"" (assoc-ref inputs "enchant")
                               "/lib/libenchant-2.so\""))))))))
             ;(setenv "PYENCHANT_LIBRARY_PATH"
             ;        (string-append (assoc-ref inputs "enchant") "/lib/libenchant.so")))))))
    (propagated-inputs
     `(("enchant" ,enchant)
       ("hunspell" ,hunspell)))
    (native-inputs
     `(("hunspell-dict-en-us" ,hunspell-dict-en-us)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public offlate
  (package
    (name "offlate")
    (version "0.1.dev")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://framagit.org/tyreunom/offlate.git")
                     (commit "28fa87a163aa40d6770390792ce17e583618fb80")))
              (sha256
               (base32
                "0hcg7fiwa51jfm8jgz65vjakid38kfsrrc805f4imldmv9j9gf3a"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-translations
           (lambda _
             (invoke "make" "update-langs")
             #t)))))
    (propagated-inputs
     `(("python-android-stringslib" ,python-android-stringslib)
       ("python-dateutil" ,python-dateutil)
       ("python-lxml" ,python-lxml)
       ("python-pyenchant" ,python-pyenchant)
       ("python-ruamel.yaml" ,python-ruamel.yaml)
       ("python-polib" ,python-polib)
       ("python-pyqt" ,python-pyqt)
       ("python-requests" ,python-requests)))
    (native-inputs
     `(("qt" ,qt)))
    (home-page "https://framagit.org/tyreunom/offlate")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public python-zope.interface
  (package
    (name "python-zope.interface")
    (version "4.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "zope.interface" version))
        (sha256
          (base32
            "0k67m60ij06wkg82n15qgyn96waf4pmrkhv0njpkfzpmv5q89hsp"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page
      "https://github.com/zopefoundation/zope.interface")
    (synopsis "Interfaces for Python")
    (description "Interfaces for Python")
    (license #f)))

(define-public python-hkdf
  (package
    (name "python-hkdf")
    (version "0.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "hkdf" version))
              (sha256
               (base32
                "1jhxk5vhxmxxjp3zj526ry521v9inzzl8jqaaf0ma65w6k332ak2"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page
      "https://github.com/casebeer/python-hkdf")
    (synopsis
      "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (description
      "HMAC-based Extract-and-Expand Key Derivation Function (HKDF)")
    (license #f)))

(define-public python-geoip
  (package
    (name "python-geoip")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "geoip" version))
              (sha256
               (base32
                "1rphxf3vrn8wywjgr397f49s0s22m83lpwcq45lm0h2p45mdm458"))))
    (build-system python-build-system)
    (home-page "http://www.maxmind.com/")
    (synopsis
      "MaxMind GeoIP Legacy Database - Python API")
    (description
      "MaxMind GeoIP Legacy Database - Python API")
    (license #f)))

(define-public python-hyperlink
  (package
    (name "python-hyperlink")
    (version "18.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "hyperlink" version))
        (sha256
          (base32
            "01m3y19arfqljksngy8grc966zdb4larysralb8cajzi8kvly6zh"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-idna" ,python-idna)))
    (home-page
      "https://github.com/python-hyper/hyperlink")
    (synopsis
      "A featureful, immutable, and correct URL for Python.")
    (description
      "A featureful, immutable, and correct URL for Python.")
    (license license:expat)))

(define-public python-twisted-for-wormhole
  (package
    (inherit python-twisted)
    (name "python-twisted")
    (version "18.9.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "Twisted" version ".tar.bz2"))
              (sha256
               (base32
                "15d3gmkrg8g27hyd6ihawv2y2dv5gnpyg67wy9npgbl4pz3f4jr9"))))
    (propagated-inputs
     `(("python-pyhamcrest" ,python-pyhamcrest)
       ("python-service-identity" ,python-service-identity)
       ("python-hyperlink" ,python-hyperlink)
       ,@(package-propagated-inputs python-twisted)))))

(define-public python-txaio
  (package
    (name "python-txaio")
    (version "18.8.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "txaio" version))
        (sha256
          (base32
            "1zmpdph6zddgrnkkcykh6qk5s46l7s5mzfqrh82m4b5iffn61qv7"))))
    (build-system python-build-system)
    (propagated-inputs
      `(;("python-[all]" ,#{python-\x5b;all\x5d;}#)
        ;("python-[asyncio]"
        ; ,#{python-\x5b;asyncio\x5d;}#)
        ;("python-[dev]" ,#{python-\x5b;dev\x5d;}#)
        ;("python-[twisted]"
        ; ,#{python-\x5b;twisted\x5d;}#)
        ("python-mock" ,python-mock)
        ("python-pep8" ,python-pep8)
        ("python-pyenchant" ,python-pyenchant)
        ("python-pytest" ,python-pytest)
        ("python-pytest-cov" ,python-pytest-cov)
        ("python-six" ,python-six)
        ("python-sphinx" ,python-sphinx)
        ;("python-sphinx-rtd-theme"
        ; ,python-sphinx-rtd-theme)
        ;("python-sphinxcontrib-spelling"
        ; ,python-sphinxcontrib-spelling)
        ("python-tox" ,python-tox)
        ("python-twine" ,python-twine)
        ("python-twisted" ,python-twisted)
        ("python-wheel" ,python-wheel)
        ("python-zope.interface" ,python-zope.interface)))
    (home-page "https://github.com/crossbario/txaio")
    (synopsis
      "Compatibility API between asyncio/Twisted/Trollius")
    (description
      "Compatibility API between asyncio/Twisted/Trollius")
    (license #f)))

(define-public python-txtorcon
  (package
    (name "python-txtorcon")
    (version "18.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "txtorcon" version))
              (sha256
               (base32
                "1c7qfpr1zz34whz66lk4xpwdn7d5jqk6ccgas5n54li479mra0an"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(;("python-codecov" ,python-codecov)
       ;("python-coverage" ,python-coverage)
       ;("python-coveralls" ,python-coveralls)
       ;("python-cuvner" ,python-cuvner)
       ;("python-geoip" ,python-geoip)
       ("python-ipaddress" ,python-ipaddress)
       ("python-mock" ,python-mock)
       ("python-pycodestyle" ,python-pycodestyle)
       ("python-pyflakes" ,python-pyflakes)
       ;("python-readme-renderer" ,python-readme-renderer)
       ;("python-repoze.sphinx.autointerface" ,python-repoze.sphinx.autointerface)
       ("python-setuptools" ,python-setuptools)
       ("python-sphinx" ,python-sphinx)
       ("python-tox" ,python-tox)
       ("python-twine" ,python-twine)
       ("python-wheel" ,python-wheel)))
    (home-page "")
    (synopsis
      "Twisted-based Tor controller client, with state-tracking and configuration abstractions. https://txtorcon.readthedocs.org https://github.com/meejah/txtorcon")
    (description
      "Twisted-based Tor controller client, with state-tracking and configuration abstractions. https://txtorcon.readthedocs.org https://github.com/meejah/txtorcon")
    (license #f)))

(define-public python-spake2
  (package
    (name "python-spake2")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "spake2" version))
              (sha256
               (base32
                "1x16r7lrbklvfzbacb66qv9iiih6liq1y612dqh2chgf555n2yn1"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-hkdf" ,python-hkdf)))
    (home-page
      "https://github.com/warner/python-spake2")
    (synopsis
      "SPAKE2 password-authenticated key exchange (pure python)")
    (description
      "SPAKE2 password-authenticated key exchange (pure python)")
    (license license:expat)))

(define-public python-humanize
  (package
    (name "python-humanize")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "humanize" version))
              (sha256
               (base32
                "06dvhm3k8lf2rayn1gxbd46y0fy1db26m3h9vrq7rb1ib08mfgx4"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (home-page "http://github.com/jmoiron/humanize")
    (synopsis "python humanize utilities")
    (description "python humanize utilities")
    (license license:expat)))

(define-public python-service-identity
  (package
    (name "python-service-identity")
    (version "17.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "service_identity" version))
        (sha256
          (base32
            "1aq24cn3nnsjr9g797dayhx4g653h6bd41ksqhidzq0rvarzn0a0"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-attrs" ,python-attrs)
        ("python-pyasn1" ,python-pyasn1)
        ("python-pyasn1-modules" ,python-pyasn1-modules)
        ("python-pyopenssl" ,python-pyopenssl)))
    (home-page
      "https://service-identity.readthedocs.io/")
    (synopsis
      "Service identity verification for pyOpenSSL.")
    (description
      "Service identity verification for pyOpenSSL.")
    (license license:expat)))

(define-public python-pyhamcrest
  (package
    (name "python-pyhamcrest")
    (version "1.9.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pyhamcrest" version))
        (sha256
          (base32
            "1kan3nyxs1dz333s7mfvjj47l8j6qxd1imyf2kg8jzm57njs1ylg"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
      `(("python-setuptools" ,python-setuptools)
        ("python-six" ,python-six)))
    (home-page
      "https://github.com/hamcrest/PyHamcrest")
    (synopsis
      "Hamcrest framework for matcher objects")
    (description
      "Hamcrest framework for matcher objects")
    (license #f)))

(define-public python-autobahn
  (package
    (name "python-autobahn")
    (version "18.9.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "autobahn" version))
              (sha256
               (base32
                "1mhj64rsnbi6rc0hskmllw280rvd99z045p6dq8h0mw60r7r52yr"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
     `(;("python-[accelerate]"
     ;,#{python-\x5b;accelerate\x5d;}#)
     ;("python-[all]" ,#{python-\x5b;all\x5d;}#)
     ;("python-[asyncio]"
     ;,#{python-\x5b;asyncio\x5d;}#)
     ;("python-[compress]"
     ;,#{python-\x5b;compress\x5d;}#)
     ;("python-[dev]" ,#{python-\x5b;dev\x5d;}#)
     ;("python-[encryption]"
     ;,#{python-\x5b;encryption\x5d;}#)
     ;("python-[nvx]" ,#{python-\x5b;nvx\x5d;}#)
     ;("python-[scram]" ,#{python-\x5b;scram\x5d;}#)
     ;("python-[serialization]"
     ;,#{python-\x5b;serialization\x5d;}#)
     ;("python-[twisted]"
     ;,#{python-\x5b;twisted\x5d;}#)
     ;("python-argon2-cffi" ,python-argon2-cffi)
     ;("python-awscli" ,python-awscli)
     ("python-cbor" ,python-cbor)
     ("python-cffi" ,python-cffi)
     ("python-flake8" ,python-flake8)
     ("python-lz4" ,python-lz4)
     ("python-mock" ,python-mock)
     ("python-passlib" ,python-passlib)
     ;("python-pep8-naming" ,python-pep8-naming)
     ("python-py-ubjson" ,python-py-ubjson)
     ("python-pyenchant" ,python-pyenchant)
     ("python-pyflakes" ,python-pyflakes)
     ("python-pynacl" ,python-pynacl)
     ("python-pyopenssl" ,python-pyopenssl)
     ("python-pyqrcode" ,python-pyqrcode)
     ;("python-pytest" ,python-pytest)
     ;("python-pytest-aiohttp" ,python-pytest-aiohttp)
     ;("python-pytest-asyncio" ,python-pytest-asyncio)
     ;("python-pytrie" ,python-pytrie)
     ;("python-qualname" ,python-qualname)
     ("python-service-identity" ,python-service-identity)
     ("python-six" ,python-six)
     ;("python-snappy" ,python-snappy)
     ("python-sphinx" ,python-sphinx)
     ("python-sphinx-rtd-theme" ,python-sphinx-rtd-theme)
     ;("python-sphinxcontrib-spelling" ,python-sphinxcontrib-spelling)
     ("python-twine" ,python-twine)
     ("python-twisted" ,python-twisted-for-wormhole)
     ("python-txaio" ,python-txaio)
     ;("python-u-msgpack-python" ,python-u-msgpack-python)
     ("python-wheel" ,python-wheel)
     ;("python-wsaccel" ,python-wsaccel)
     ("python-zope.interface" ,python-zope.interface)))
    (home-page "http://crossbar.io/autobahn")
    (synopsis
      "WebSocket client & server library, WAMP real-time framework")
    (description
      "WebSocket client & server library, WAMP real-time framework")
    (license #f)))

(define-public magic-wormhole
  (package
    (name "magic-wormhole")
    (version "0.10.5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "magic-wormhole" version))
        (sha256
          (base32
            "1vhp97pdnqb8nd08pk9fn7mk5jwihdfcsqilxkg3brai6lgyln4m"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
      `(("python-attrs" ,python-attrs)
        ("python-autobahn" ,python-autobahn)
        ("python-automat" ,python-automat)
        ("python-click" ,python-click)
        ("python-hkdf" ,python-hkdf)
        ("python-humanize" ,python-humanize)
        ("python-ipaddress" ,python-ipaddress)
        ("python-pynacl" ,python-pynacl)
        ("python-six" ,python-six)
        ("python-spake2" ,python-spake2)
        ("python-tqdm" ,python-tqdm)
        ("python-twisted" ,python-twisted-for-wormhole)
        ("python-txtorcon" ,python-txtorcon)))
    (home-page
      "https://github.com/warner/magic-wormhole")
    (synopsis
      "Securely transfer data between computers")
    (description
      "Securely transfer data between computers")
    (license license:expat)))

(define-public python-pathlib2
  (package
    (name "python-pathlib2")
    (version "2.3.3")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pathlib2" version))
        (sha256
          (base32
            "0hpp92vqqgcd8h92msm9slv161b1q160igjwnkf2ag6cx0c96695"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-scandir" ,python-scandir)))
    (native-inputs
      `(("python-six" ,python-six)))
    (home-page
      "https://pypi.python.org/pypi/pathlib2/")
    (synopsis "Object-oriented filesystem paths")
    (description "Object-oriented filesystem paths")
    (license license:expat)))

(define-public python-check-manifest
  (package
    (name "python-check-manifest")
    (version "0.37")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "check-manifest" version))
        (sha256
          (base32
            "0lk45ifdv2cpkl6ayfyix7jwmnxa1rha7xvb0ih5999k115wzqs4"))))
    (build-system python-build-system)
    (native-inputs
      `(("python-mock" ,python-mock)
        ("git" ,git)))
    (home-page
      "https://github.com/mgedmin/check-manifest")
    (synopsis
      "Check MANIFEST.in in a Python source package for completeness")
    (description
      "Check MANIFEST.in in a Python source package for completeness")
    (license license:expat)))

(define-public python-codacy-coverage
  (package
    (name "python-codacy-coverage")
    (version "1.3.11")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "codacy-coverage" version))
        (sha256
          (base32
            "1g0c0w56xdkmqb8slacyw5qhzrkp814ng3ddh2lkiij58y9m2imr"))))
    (build-system python-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("python-check-manifest" ,python-check-manifest)))
    (home-page
      "https://github.com/codacy/python-codacy-coverage")
    (synopsis "Codacy coverage reporter for Python")
    (description
      "Codacy coverage reporter for Python")
    (license license:expat)))

(define-public python-translation-finder
  (package
    (name "python-translation-finder")
    (version "1.6")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "translation-finder" version))
        (sha256
          (base32
            "0lq9441ziiq8aw8ldippkcvzhyw12lfra72kc6f5ik3rvw612m2a"))))
    (build-system python-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
	 (add-before 'build 'remove-failing-test
	   (lambda _
	     (delete-file "translation_finder/test_api.py")
	     #t)))))
    (propagated-inputs
      `(("python-chardet" ,python-chardet)
        ("python-pathlib2" ,python-pathlib2)
	("python-ruamel.yaml" ,python-ruamel.yaml)
        ("python-six" ,python-six)))
    (native-inputs
     `(("python-codecov" ,python-codecov)
       ("python-codacy-coverage" ,python-codacy-coverage)
       ("python-pytest-cov" ,python-pytest-cov)
       ("python-pytest-runner" ,python-pytest-runner)
       ("python-twine" ,python-twine)))
    (home-page "https://weblate.org/")
    (synopsis
      "A translation file finder for Weblate, translation tool with tight version control integration")
    (description
      "A translation file finder for Weblate, translation tool with tight version control integration")
    (license license:gpl3+)))

(define-public python-translate-toolkit
  (package
    (name "python-translate-toolkit")
    (version "2.3.1")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "translate-toolkit" version))
        (sha256
          (base32
            "1fsfhqyhdwjbb0ljmkzaibjr3s7q7ipg776rwk14l9c7chvnn58x"))))
    (build-system python-build-system)
    (home-page "http://toolkit.translatehouse.org/")
    (arguments
     ;; Require old versions
     `(#:tests? #f))
    (synopsis
      "Tools and API for translation and localization engineering.")
    (description
      "Tools and API for translation and localization engineering.")
    (license license:gpl2+)))

(define-public python-httmock
  (package
    (name "python-httmock")
    (version "1.3.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "httmock" version))
        (sha256
          (base32
            "1zj1fcm0n6f0wr9mr0hmlqz9430fnr5cdwd5jkcvq9j44bnsrfz0"))))
    (build-system python-build-system)
    (arguments
     ;; Tests can't be run?
     `(#:tests? #f))
    (propagated-inputs
      `(("python-requests" ,python-requests)))
    (home-page "https://github.com/patrys/httmock")
    (synopsis "A mocking library for requests.")
    (description "A mocking library for requests.")
    (license license:asl2.0)))

(define-public python-gitlab
  (package
    (name "python-gitlab")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "python-gitlab" version))
        (sha256
          (base32
            "0n2s4cmmrhx1yxpfa6xfkncairgrcvcmvhq2sx4k0k65cy9ipsf4"))))
    (build-system python-build-system)
    (propagated-inputs
      `(("python-requests" ,python-requests)
        ("python-six" ,python-six)))
    (native-inputs
     `(("python-httmock" ,python-httmock)
       ("python-mock" ,python-mock)))
    (home-page
      "https://github.com/python-gitlab/python-gitlab")
    (synopsis "Interact with GitLab API")
    (description "Interact with GitLab API")
    (license license:lgpl3+)))
