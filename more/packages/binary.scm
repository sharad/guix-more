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

(define-module (more packages binary)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (more packages python)
  #:use-module (more packages smt))

(define-public python-pyelftools
  (package
    (name "python-pyelftools")
    (version "0.24")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eliben/pyelftools/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iw47b20brg0ah86s9a2dn1f70qfmdv20p04q131vmnwa9g066f4"))))
    (build-system python-build-system)
    (home-page "https://github.com/eliben/pyelftools")
    (synopsis "Parsing and analyzing ELF files and DWARF debugging information")
    (description
      "Python library for parsing and analyzing ELF files and DWARF debugging information.")
    (license license:public-domain)))

(define-public python2-pyelftools
  (package-with-python2 python-pyelftools))

(define-public capstone
  (package
    (name "capstone")
    (version "3.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aquynh/capstone/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1whl5c8j6vqvz2j6ay2pyszx0jg8d3x8hq66cvgghmjchvsssvax"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (home-page "http://www.capstone-engine.org")
    (synopsis "Disassembler")
    (description
     "Capstone can disassemble machine code for many supported architectures
such as x86, x86_64, arm, arm64, mips, ppc, sparc, sysz and xcore.  It provides
bindings for Python, Java, OCaml and more.")
    (license (list license:bsd-3 license:expat))))

;; This package has a timestamp embedded in
;; lib/python3.5/site-packages/capstone/__pycache__/__iti__.cpython-35.pyc
(define-public python-capstone
  (package
    (inherit capstone)
    (name "python-capstone")
    (propagated-inputs
     `(("capstone" ,capstone)))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-and-fix-setup-py
           (lambda _
             (chdir "bindings/python")
             (substitute* "setup.py" (("data_files=.*") ""))
             (substitute* "capstone/__init__.py"
               (("_lib_path =.*")
                (string-append "_lib_path = '"
                               (assoc-ref %build-inputs "capstone")
                               "/lib'\n")))
             #t)))))))

(define-public python2-capstone
  (package-with-python2 python-capstone))
 
(define-public python-pefile
  (package
    (name "python-pefile")
    (version "2016.3.28")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "pefile" version))
        (sha256
         (base32
          "0ysz17ci0nhc5gi6j9si0fg87lzc7vcz3ccbi6qgfgjwbc422h7j"))))
    (build-system python-build-system)
    (home-page "https://github.com/erocarrera/pefile")
    (synopsis "Parse and work with Portable Executable (aka PE) files")
    (description "Pefile is a multi-platform Python module to parse and work
with Portable Executable (aka PE) files.  Most of the information contained in
the PE headers is accessible as well as all sections' details and their data.
The structures defined in the Windows header files will be accessible as
attributes in the PE instance.  The naming of fields/attributes will try to
adhere to the naming scheme in those headers.  Only shortcuts added for
convenience will depart from that convention.")
    (license license:expat)))

(define-public python2-pefile
  (package-with-python2 python-pefile))

(define-public python-archinfo
  (package
    (name "python-archinfo")
    (version "6.7.1.13")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "archinfo" version))
              (sha256
               (base32
                "0x896mk98r6g9h3rxpqq9ri0s6v9n937jx0fzn7i61zn61n7whzw"))))
    (build-system python-build-system)
    (home-page "https://github.com/angr/archinfo")
    (synopsis "Collection of classes that contain architecture-specific information")
    (description "Archinfo is a collection of classes that contain
architecture-specific information.  It is useful for cross-architecture tools
(such as pyvex).")
    (license license:bsd-2)))

(define-public python2-archinfo
  (package-with-python2 python-archinfo))

(define-public angr-vex
  (package
    (name "angr-vex")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/angr/vex.git")
                    (commit "058410ede7ee74231255f6ae77cae8476c8a3ef4")))
              (sha256
               (base32
                "02wi1705pa0xbwfqx3jj6g7nnvzi8whgmnd29fp1i7n4qz20gcgb"))
              (file-name (string-append name "-" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                   (lib (string-append out "/lib"))
                   (include (string-append out "/include")))
               (mkdir-p lib)
               (mkdir-p include)
               (copy-recursively "pub" include)
               (copy-file "libvex.a" (string-append lib "/libvex.a"))))))))
    (home-page "https://github.com/angr/vex")
    (synopsis "Fork of libVEX for PyVEX")
    (description "This is a mirror of libVEX (of the Valgrind project:
valgrind.org) for use with PyVEX.")
    (license license:gpl2+)))

(define-public python2-pyvex
  (package
    (name "python2-pyvex")
    (version "6.7.1.13.post2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyvex" version))
              (sha256
               (base32
                "1x9s88hgrw9xz8v9x9njjz0jq4fxkwyn479074bg4wbqjsp9n7qd"))))
    (build-system python-build-system)
    (inputs `(("angr-vex" ,angr-vex)))
    (propagated-inputs
     `(("archinfo" ,python2-archinfo)
       ("pycparser" ,python2-pycparser)
       ("cffi" ,python2-cffi)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-setup.py
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "setup.py"
               (("VEX_PATH = .*")
                (string-append "VEX_PATH = '" (assoc-ref inputs "angr-vex") "'"))
               ((".*self.execute\\(_build_vex.*")
                "")
               (("e\\['VEX_LIB_PATH'\\] = .*")
                "e['VEX_LIB_PATH'] = os.path.join(VEX_PATH, 'lib')\n")
               (("'pub'")
                "'include'")))))))
    (home-page "https://github.com/angr/pyvex")
    (synopsis "PyVEX exposes VEX into Python")
    (description "VEX is an intermediate representation that is useful to carry
analyses on binary.  PyVEX exposes VEX into Python.")
    (license license:bsd-2)))

(define-public unicorn
  (package
    (name "unicorn")
    (version "1.0-rc3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/unicorn-engine/unicorn/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18sf8vbmf08ss27qhiv7la492k39q0ci8kpjx836bv7rq3cbgb2q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (native-inputs
     `(("python" ,python-2)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.unicorn-engine.org/")
    (synopsis "CPU emulator")
    (description "Unicorn is a lightweight multi-platform, multi-architecture
CPU emulator framework.")
    (license (list license:gpl2 license:lgpl2.0))))

;; Not reproducible
(define-public python-unicorn
  (package
    (inherit unicorn)
    (name "python-unicorn")
    (propagated-inputs
     `(("unicorn" ,unicorn)))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases (modify-phases %standard-phases
                  (add-after 'unpack 'chdir-and-fix-setup-py
                    (lambda _
                      (chdir "bindings/python")
                      (substitute* "setup.py" (("build_libraries\\(\\)\n") "\n"))
                      (substitute* "unicorn/unicorn.py"
                        (("'',")
                         (string-append "'"
                                        (assoc-ref %build-inputs "unicorn")
                                        "/lib',")))
                      #t)))))))

(define-public python2-unicorn
  (package
    (inherit (package-with-python2 python-unicorn))
    (propagated-inputs
     `(("unicorn" ,unicorn)
       ("pyvex" ,python2-pyvex)))))

;; TODO: Requires dpkt-fix for testing
(define-public python2-simuvex
  (package
    (name "python2-simuvex")
    (version "6.7.1.31")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "simuvex" version))
              (sha256
               (base32
                "150jwf55pib7ndz7bjb4fxifqqgdxx7n1f5qa87mz6349qvi1xji"))))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (propagated-inputs
     `(("pyvex" ,python2-pyvex)
       ("bintrees" ,python2-bintrees)
       ("dpkt" ,python2-dpkt)
       ("cooldict" ,python2-cooldict)
       ("cachetools" ,python2-cachetools)
       ("claripy" ,python2-claripy)
       ("unicorn" ,python2-unicorn)
       ("glib" ,glib)))
    (inputs
     `(("zlib" ,zlib)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/angr/cle")
    (synopsis "Abstraction of process memory")
    (description "CLE loads binaries and their associated libraries, resolves
imports and provides an abstraction of process memory the same way as if it was
loaded by the OS's loader.")
    (license license:bsd-2)))

(define-public python-cle
  (package
    (name "python-cle")
    (version "6.7.1.31")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cle" version))
              (sha256
               (base32
                "0llk54f9p3b73f1pk19axlhw8yw80fdv07jkghqmqwd6xrnpnmmc"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("pyelftools" ,python-pyelftools)
       ("cffi" ,python-cffi)
       ("archinfo" ,python-archinfo)
       ("pefile" ,python-pefile)))
    (home-page "https://github.com/angr/cle")
    (synopsis "Abstraction of process memory")
    (description "CLE loads binaries and their associated libraries, resolves
imports and provides an abstraction of process memory the same way as if it was
loaded by the OS's loader.")
    (license license:bsd-2)))

(define-public python2-cle
  (package
    (inherit (package-with-python2 python-cle))
    (propagated-inputs
     `(("pyelftools" ,python2-pyelftools)
       ("cffi" ,python2-cffi)
       ("pefile" ,python2-pefile)
       ("pyvex" ,python2-pyvex)))))

(define-public python2-angr
  (package
    (name "python2-angr")
    (version "6.7.1.31")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "angr" version))
              (sha256
               (base32
                "19msllsjwc869824sx1qah6vnb03z22s71fph215ykbbb2843p1k"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("cle" ,python2-cle)
       ("capstone" ,python2-capstone)
       ("six" ,python2-six)
       ("utils" ,python2-utils)
       ("mulpyplexer" ,python2-mulpyplexer)
       ("rpyc" ,python2-rpyc)
       ("networkx" ,python2-networkx)
       ("futures" ,python2-futures)
       ("progressbar" ,python2-progressbar2)
       ("simuvex" ,python2-simuvex)))
    (home-page "https://github.com/angr/angr")
    (synopsis "Angr is a python framework for analyzing binaries")
    (description "angr is a python framework for analyzing binaries.  It
focuses on both static and dynamic symbolic (\"concolic\") analysis, making it
applicable to a variety of tasks.")
    (license license:bsd-2)))
