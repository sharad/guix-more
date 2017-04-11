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
     `(("pytest-runner" ,python-pytest-runner)
       ("pytest" ,python-pytest)
       ("six" ,python-six)))
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
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "progressbar2" version))
              (sha256
               (base32
                "16r21cpjvv0spf4mymgpy7hx6977iy11k44n2w9kipwg4lhwh02k"))))
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

(define-public python-webassets
  (package
    (name "python-webassets")
    (version "0.12.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "webassets" version))
       (sha256
        (base32
         "1nrqkpb7z46h2b77xafxihqv3322cwqv6293ngaky4j3ff4cing7"))))
    (build-system python-build-system)
    (home-page "http://github.com/miracle2k/webassets")
    (synopsis "Media asset management")
    (description "Merges, minifies and compresses Javascript and CSS files,
supporting a variety of different filters, including YUI, jsmin, jspacker or
CSS tidy.  Also supports URL rewriting in CSS files.")
    (license license:bsd-2)))


(define-public python-sphinx-me
  (package
    (name "python-sphinx-me")
    (version "0.3")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "sphinx-me" version))
       (sha256
        (base32
         "06jzgp213zihnvpcy2y5jy3ykid3apc2ncp2pg6a2g05lhiziglq"))))
    (build-system python-build-system)
    (home-page "https://github.com/stephenmcd/sphinx-me")
    (synopsis "Create a Sphinx documentation shell")
    (description
      "Create a Sphinx documentation shell for your project and include the
README file as the documentation index.  It handles extracting the required
meta data such as the project name, author and version from your project for
use in your Sphinx docs.")
    (license license:bsd-2)))

(define-public python2-sphinx-me
  (package-with-python2 python-sphinx-me))

(define-public python-rq
  (package
    (name "python-rq")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "rq" version))
       (sha256
        (base32 "0gaq5pnh0zy46r8jvygi0ifbvz3pq6i7xla78ijcgjw0x77qzsdh"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("python-redis" ,python-redis)))
    (home-page "http://python-rq.org/")
    (synopsis "Simple job queues for Python")
    (description
     "RQ (Redis Queue) is a simple Python library for queueing jobs and
processing them in the background with workers.  It is backed by Redis and it
is designed to have a low barrier to entry.")
    (license license:bsd-2)))

(define-public python2-rq
  (package-with-python2 python-rq))

(define-public python-cssmin
  (package
    (name "python-cssmin")
    (version "0.2.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "cssmin" version))
        (sha256
         (base32
          "1dk723nfm2yf8cp4pj785giqlwv42l0kj8rk40kczvq1hk6g04p0"))))
    (build-system python-build-system)
    (home-page "https://github.com/zacharyvoase/cssmin")
    (synopsis "Python port of the YUI CSS Compressor")
    (description "Python port of the YUI CSS Compressor.")
    (license (list license:expat license:bsd-3))))

(define-public python2-cssmin
  (package-with-python2 python-cssmin))
 
(define-public python-diff-match-patch
  (package
    (name "python-diff-match-patch")
    (version "20121119")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "diff-match-patch" version))
        (sha256
         (base32
          "0k1f3v8nbidcmmrk65m7h8v41jqi37653za9fcs96y7jzc8mdflx"))))
    (build-system python-build-system)
    (home-page "https://code.google.com/p/google-diff-match-patch")
    (synopsis "Synchronize plain text")
    (description "Diff Match and Patch libraries offer robust algorithms to
perform the operations required for synchronizing plain text.")
    (license license:asl2.0)))

(define-public python2-diff-match-patch
  (package-with-python2 python-diff-match-patch))
 
(define-public python-dirsync
  (package
    (name "python-dirsync")
    (version "2.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append
               "https://pypi.python.org/packages/15/7c/2d4986c0bd927fcc496d1c19"
               "3fdd2c0aafbd30ba8928ec92b79f3abf2bd7/dirsync-" version ".zip"))
        (sha256
         (base32
          "1hcdvmkwd5512zbxpin0k7bx5bkgzy3swjx7d0kj1y45af6r75v2"))))
    (build-system python-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (propagated-inputs
     `(("six" ,python-six)))
    (home-page "https://bitbucket.org/tkhyn/dirsync")
    (synopsis "Advanced directory tree synchronisation tool")
    (description "Advanced directory tree synchronisation tool.")
    (license license:expat)))

(define-public python2-dirsync
  (package-with-python2 python-dirsync))
 
(define-public python-elasticsearch
  (package
    (name "python-elasticsearch")
    (version "1.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "elasticsearch" version))
        (sha256
         (base32
          "1sdw1r05cw7ihnmng8ra9v968fj7bq6sji8i1dikymsnkcpgc69g"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("urllib3" ,python-urllib3)))
    (home-page "https://github.com/elastic/elasticsearch-py")
    (synopsis "Low-level client for Elasticsearch")
    (description "Official low-level client for Elasticsearch.  Its goal is to
provide common ground for all Elasticsearch-related code in Python; because of
this it tries to be opinion-free and very extendable.")
    (license license:expat)))

(define-public python2-elasticsearch
  (package-with-python2 python-elasticsearch))

(define-public python-dateutil-2
  (package
    (name "python-dateutil")
    (version "2.6.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "python-dateutil" version))
      (sha256
       (base32
        "1lhq0hxjc3cfha101q02ld5ijlpfyjn2w1yh7wvpiy367pgzi8k2"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-six" ,python-six)))
    (home-page "https://dateutil.readthedocs.io/en/stable/")
    (synopsis "Extensions to the standard datetime module")
    (description
     "The dateutil module provides powerful extensions to the standard
datetime module, available in Python 2.3+.")
    (license license:bsd-3)))

(define-public python2-dateutil-2
  (package-with-python2 python-dateutil-2))
 
(define-public python-levenshtein
  (package
    (name "python-levenshtein")
    (version "0.12.0")
    (source
     (origin
      (method url-fetch)
      (uri (pypi-uri "python-Levenshtein" version))
      (sha256
       (base32
        "1c9ybqcja31nghfcc8xxbbz9h60s9qi12b9hr4jyl69xbvg12fh3"))))
    (build-system python-build-system)
    (home-page "https://github.com/ztane/python-Levenshtein")
    (synopsis "Fast computation of Levenshtein distance and string similarity")
    (description
     "The Levenshtein Python C extension module contains functions for fast computation of
@enumerate
@item Levenshtein (edit) distance, and edit operations
@item string similarity
@item approximate median strings, and generally string averaging
@item string sequence and set similarity
@end enumerate
It supports both normal and Unicode strings.")
    (license license:gpl2+)))

(define-public python2-levenshtein
  (package-with-python2 python-levenshtein))
 
(define-public python-scandir
  (package
    (name "python-scandir")
    (version "1.4")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "scandir" version))
       (sha256
        (base32 "0yjrgp0mxp3d8bjkq2m1ac2ys8n76wykksvgyjrnil9gr3fx7a5d"))))
    (build-system python-build-system)
    (home-page "https://github.com/benhoyt/scandir")
    (synopsis "Directory iteration function")
    (description
     "Directory iteration function like os.listdir(), except that instead of
returning a list of bare filenames, it yields DirEntry objects that include
file type and stat information along with the name.  Using scandir() increases
the speed of os.walk() by 2-20 times (depending on the platform and file
system) by avoiding unnecessary calls to os.stat() in most cases.")
    (license license:bsd-3)))

(define-public python2-scandir
  (package-with-python2 python-scandir))
 
(define-public python-stemming
  (package
    (name "python-stemming")
    (version "1.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "stemming" version))
       (sha256
        (base32 "0ldwa24gnnxhniv0fhygkpc2mwgd93q10ag8rvzayv6hw418frsr"))))
    (build-system python-build-system)
    (home-page "https://bitbucket.org/mchaput/stemming/overview")
    (synopsis "Python implementations of various stemming algorithms")
    (description
     "Python implementations of the Porter, Porter2, Paice-Husk, and Lovins
stemming algorithms for English. These implementations are straightforward and
efficient, unlike some Python versions of the same algorithms available on the
Web. This package is an extraction of the stemming code included in the Whoosh
search engine.")
    (license license:public-domain)))

(define-public python2-stemming
  (package-with-python2 python-stemming))
 
(define-public python-translate-toolkit
  (package
    (name "python-translate-toolkit")
    (version "2.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "translate-toolkit" version ".tar.bz2"))
       (sha256
        (base32 "1648y76sxg11m31hvvnsps1yvmiz51dnn8ir24q9mjfw1qarx3wp"))))
    (build-system python-build-system)
    (home-page "http://toolkit.translatehouse.org")
    (synopsis "Tools and API for translation and localization engineering")
    (description
     "Tools and API for translation and localization engineering.  It contains
several utilities, as well as an API for building localization tools.")
    (license license:gpl2+)))

(define-public python2-translate-toolkit
  (package-with-python2 python-translate-toolkit))

(define-public python-mysqlclient
  (package
    (name "python-mysqlclient")
    (version "1.3.9")
    (source
     (origin
       (method url-fetch)
       (uri (pypi-uri "mysqlclient" version))
       (sha256
        (base32
         "16ccq9hvsrc6nf0nakn19dqncr913kf97j8ip28s5f8m3wgcy34r"))))
    (build-system python-build-system)
    (native-inputs
     `(("mariadb" ,mariadb)))
    (inputs
     `(("mysql" ,mysql)
       ("libz" ,zlib)
       ("openssl" ,openssl)))
    (propagated-inputs
     `(("nose" ,python-nose)
       ("mock" ,python-mock)
       ("py.test" ,python-pytest)))
    (home-page "https://github.com/PyMySQL/mysqlclient-python")
    (synopsis "MySQLdb is an interface to the popular MySQL database server for Python")
    (description "MySQLdb is an interface to the popular MySQL database server
for Python.  The design goals are:
@enumerate
@item Compliance with Python database API version 2.0 [PEP-0249],
@item Thread-safety,
@item Thread-friendliness (threads will not block each other).
@end enumerate")
    (license license:gpl2)))

(define-public python2-mysqlclient
  (package-with-python2 python-mysqlclient))

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
