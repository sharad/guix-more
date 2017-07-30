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
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tls)
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
    (native-inputs
     `(("utils" ,python-utils)))
    (arguments
     `(#:tests? #f))
    (home-page "https://github.com/eliben/pyelftools")
    (synopsis "Parsing and analyzing ELF files and DWARF debugging information")
    (description
      "Python library for parsing and analyzing ELF files and DWARF debugging information.")
    (license license:public-domain)))

(define-public python2-pyelftools
  (package
    (inherit (package-with-python2 python-pyelftools))
    (arguments
     `(#:tests? #t
       #:python ,python-2))))
    
;; rc required by python2-angr
(define-public capstone
  (package
    (name "capstone")
    (version "3.0.5-rc2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/aquynh/capstone/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cqms9r2p43aiwp5spd84zaccp16ih03r7sjhrv16nddahj0jz2q"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" %output)
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-cstool-ldflags
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "cstool/Makefile"
               (("LDFLAGS =")
                (string-append "LDFLAGS = -Wl,-rpath=" (assoc-ref outputs "out")
                               "/lib"))))))))
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
             (substitute* "setup.py" ((".*   build_libraries.*") ""))
             (substitute* "capstone/__init__.py"
               (("pkg_resources.resource_filename.*")
                (string-append "'" (assoc-ref %build-inputs "capstone") "/lib',\n")))
             #t)))))))

(define-public python2-capstone
  (package-with-python2 python-capstone))

(define-public capstone-git
  (package
    (inherit capstone)
    (version "3.0.5-rc2")
    (name "capstone-git")
    (source (origin
              (method git-fetch)
              (uri (git-reference (url "https://github.com/aquynh/capstone.git")
                                  (commit "b6c4c3f5c79684b02d0672b50b4db494f6ce60f9")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0kqdfp0flx5czzwr490pzn9mzsxcw8qpcfz4y7bpf2cklsr4mh25"))))
    (arguments
     `(#:tests? #f
       #:make-flags (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
                          "CC=gcc")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-cstool
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "cstool/Makefile"
               (("LDFLAGS =")
                (string-append "LDFLAGS = -Wl,-rpath=" (assoc-ref outputs "out") "/lib"))))))))))

(define-public python-capstone-git
  (package
    (inherit capstone-git)
    (name "python-capstone-git")
    (propagated-inputs
     `(("capstone" ,capstone-git)))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir-and-fix-setup-py
           (lambda _
             (chdir "bindings/python")
             (substitute* "setup.py" (("   *build_libraries.*") "\n"))
             (substitute* "capstone/__init__.py"
               (("pkg_resources.resource_filename.*")
                (string-append "\"" (assoc-ref %build-inputs "capstone") "/lib\",\n")))
             #t)))))))

(define-public python2-capstone-git
  (package-with-python2 python-capstone-git))

 
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
    (arguments
     `(#:tests? #f)); no test
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

(define-public python2-archinfo
  (package
    (name "python2-archinfo")
    (version "6.7.7.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "archinfo" version))
              (sha256
               (base32
                "1amcavk8x9xch18sfzbla5sgas955clj06g6hx20n47q87bw22vz"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/angr/archinfo")
    (synopsis "Collection of classes that contain architecture-specific information")
    (description "Archinfo is a collection of classes that contain
architecture-specific information.  It is useful for cross-architecture tools
(such as pyvex).")
    (license license:bsd-2)))

(define-public angr-vex
  (package
    (name "angr-vex")
    (version "20170130")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/angr/vex.git")
                    (commit "fc6a0b1187cd614e97e204046b4d4be482e7ab3f")))
              (sha256
               (base32
                "1qfv5j2hpvh5mv8mschrcd5sga4h910iggppr2g8jr6k9r3x725i"))
              (file-name (string-append name "-" version))))
    (build-system gnu-build-system)
    (arguments
     `(#:make-flags
       (list "CC=gcc" "CC_NATIVE=gcc")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'get-Makefile
           (lambda _
             (copy-file "Makefile-gcc" "Makefile")))
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
    (version "6.7.7.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "pyvex" version))
              (sha256
               (base32
                "1bqag7hb1ysrq9hb31cn8l7b8ad91rfw52bm3kh9gbma8rmxi0hl"))))
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
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/unicorn-engine/unicorn/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0z01apwmvhvdldm372ww9pjfn45awkw3m90c0h4v0nj0ihmlysis"))))
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
     `(#:phases (modify-phases %standard-phases
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

(define-public python2-simuvex
  (package
    (name "python2-simuvex")
    (version "6.7.7.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "simuvex" version))
              (sha256
               (base32
                "1awc078861x7nj44g3x1p3mjdc3fhz85gqgx7rfk6918s6nbx848"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "setup.py"
                  (("dpkt-fix") "dpkt")))))
    (build-system python-build-system)
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("enum34" ,python2-enum34)))
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

(define-public python2-cle
  (package
    (name "python2-cle")
    (version "6.7.7.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "cle" version))
              (sha256
               (base32
                "0x4cyl1qkhwj18860nhxdylzaxq45264jv4449cl0vl6y23lbk8v"))
              (modules '((guix build utils)))
              (snippet
               '(substitute* "setup.py"
                  ((", \"idalink\"") ""))))); Idalink is not acceptable
    (build-system python-build-system)
    (propagated-inputs
     `(("pyelftools" ,python2-pyelftools)
       ("cffi" ,python2-cffi)
       ("archinfo" ,python2-archinfo)
       ("future" ,python2-future)
       ("pyvex" ,python2-pyvex)
       ("pefile" ,python2-pefile)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/angr/cle")
    (synopsis "Abstraction of process memory")
    (description "CLE loads binaries and their associated libraries, resolves
imports and provides an abstraction of process memory the same way as if it was
loaded by the OS's loader.")
    (license license:bsd-2)))

(define-public python2-angr
  (package
    (name "python2-angr")
    (version "6.7.7.27")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "angr" version))
              (sha256
               (base32
                "1k9vr9kds956jqyv126fc57ygly400jhhga98ms8clr4m3k3xlzn"))))
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
       ("enum34" ,python2-enum34)
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

(define-public radare2
  (package
    (name "radare2")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://radare.mikelloc.com/get/" version "/"
                                  name "-" version ".tar.gz"))
              (sha256
               (base32
                "16ggsk40zz6hyvclvqj1r4bh4hb78jf0d6ppry1jk4r0j30wm7cm"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                  (substitute* "libr/asm/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  (substitute* "libr/parse/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))
                  (substitute* "libr/bin/p/Makefile"
                    (("LDFLAGS\\+=") "LDFLAGS+=-Wl,-rpath=$(LIBDIR) "))))))
    (build-system gnu-build-system)
    (arguments
     '(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'mklibdir
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref %outputs "out") "/lib")))))
       #:configure-flags
       (list "--with-sysmagic" "--with-syszip" "--with-openssl"
             "--without-nonpic" "--with-rpath" "--with-syscapstone")
       #:make-flags
       (list "CC=gcc")))
    (inputs
     `(("openssl" ,openssl)
       ("zip" ,zip)
       ("gmp" ,gmp)
       ("capstone" ,capstone)))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "https://rada.re/")
    (synopsis "Binary analysis tool")
    (description
      "Radare2 is a tool for reversing binaries.")
    (license license:gpl3+)))
