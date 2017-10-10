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

(define-module (more packages ocaml)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages texinfo)
  #:use-module (more packages smt))

;; Janestreet packages are found in a similar way and all need the same patch.
(define (janestreet-origin name version hash)
  (origin (method url-fetch)
          (uri (string-append "https://ocaml.janestreet.com/ocaml-core/"
                              (version-major+minor version) "/files/"
                              name "-" version ".tar.gz"))
          (sha256 (base32 hash))
          (modules '((guix build utils)))
          (snippet
           (let ((pattern (string-append "lib/" name)))
             `(begin
                ;; install.ml contains an invalid reference to the ppx file and
                ;; propagates this error to the generated META file.  It
                ;; looks for it in the "lib" directory, but it is installed in
                ;; "lib/ocaml/site-lib/package".  This substitute does not change
                ;; this file for non ppx packages.
                (substitute* "install.ml"
                  ((,pattern) (string-append "lib/ocaml/site-lib/" ,name)))
                ;; The standard Makefile would try to install janestreet modules
                ;; in OCaml's directory in the store, which is read-only.
                (substitute* "Makefile"
                  (("--prefix")
                   "--libdir $(LIBDIR) --prefix")))))))

;; They also require almost the same set of arguments
(define janestreet-arguments
  `(#:use-make? #t
    #:make-flags
    (list (string-append "CONFIGUREFLAGS=--prefix "
                         (assoc-ref %outputs "out")
                         " --enable-tests")
          (string-append "LIBDIR="
                         (assoc-ref %outputs "out")
                         "/lib/ocaml/site-lib")
          ;; for ocaml-bin-prot, otherwise ignored
          (string-append "OCAML_TOPLEVEL_PATH="
                         (assoc-ref %build-inputs "findlib")
                         "/lib/ocaml/site-lib"))
    #:phases (modify-phases %standard-phases (delete 'configure))))

(define-public ocaml-zed
  (package
    (name "ocaml-zed")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/zed/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0pvfq9ikhbkv4ksn3k3vzs6wiwkihjav3n81lhxm54z9931gfwnz"))))
    (build-system ocaml-build-system)
    (propagated-inputs
     `(("camomile" ,ocaml-camomile)
       ("react" ,ocaml-react)))
    (home-page "https://github.com/diml/zed")
    (synopsis "Abstract engine for text edition in OCaml")
    (description "Zed is an abstract engine for text edition. It can be used to
write text editors, edition widgets, readlines, ...

Zed uses Camomile to fully support the Unicode specification, and implements an
UTF-8 encoded string type with validation, and a rope datastructure to achieve
efficient operations on large Unicode buffers. Zed also features a regular
expression search on ropes.

To support efficient text edition capabilities, Zed provides macro recording
and cursor management facilities.")
    (license license:bsd-3)))

(define-public ocaml-lambda-term
  (package
    (name "ocaml-lambda-term")
    (version "1.10.1")
    (home-page "https://github.com/diml/lambda-term")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/" version ".tar.gz"))
              (sha256
               (base32
                "1449glcsavcwbcsxbd7wcjz50y8vvin4zwpmkhq8i6jca3f3sknj"))))
    (build-system ocaml-build-system)
    (propagated-inputs
     `(("zed" ,ocaml-zed)
       ("lwt" ,ocaml-lwt)
       ("react" ,ocaml-react)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
        (add-after 'install 'link-stubs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                   (lib (string-append out "/lib/ocaml/site-lib/lambda-term")))
              (mkdir-p stubs)
              (symlink (string-append lib "/dlllambda-term_stubs.so")
                       (string-append stubs "/dlllambda-term_stubs.so"))))))))
    (synopsis "Terminal manipulation library for OCaml")
    (description "Lambda-term is a cross-platform library for manipulating the
terminal. It provides an abstraction for keys, mouse events, colors, as well as
a set of widgets to write curses-like applications.

The main objective of lambda-term is to provide a higher level functional
interface to terminal manipulation than, for example, ncurses, by providing a
native OCaml interface instead of bindings to a C library.

Lambda-term integrates with zed to provide text edition facilities in console
applications.")
    (license license:bsd-3)))

(define-public ocaml-utop
  (package
    (name "ocaml-utop")
    (version "1.19.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/utop/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16z02vp9n97iax4fqpbi7v86r75vbabxvnd1rirh8w2miixs1g4x"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("cppo" ,ocaml-cppo)))
    (propagated-inputs
     `(("lambda-term" ,ocaml-lambda-term)
       ("lwt" ,ocaml-lwt)
       ("react" ,ocaml-react)))
    (home-page "https://github.com/diml/utop")
    (synopsis "Universal toplevel for OCaml")
    (description "utop is an improved toplevel for OCaml.  It can run in a
terminal or in Emacs.  It supports line edition, history, real-time and context
sensitive completion, colors, and more.

It integrates with the tuareg mode in Emacs.")
    (license license:bsd-3)))

(define-public proof-general2
  (package
    (name "proof-general2")
    (version "4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/ProofGeneral/PG/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0zif2fv6mm4pv75nh10q3p37n293495rvx470bx7ma382zc3d8hv"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("which" ,which)
       ("emacs" ,emacs-minimal)
       ("texinfo" ,texinfo)))
    (inputs
     `(("host-emacs" ,emacs)
       ("perl" ,perl)
       ("coq" ,coq)))
    (arguments
     `(#:tests? #f  ; no check target
       #:make-flags (list (string-append "PREFIX=" %output)
                          (string-append "DEST_PREFIX=" %output)
                          "-j1")
       #:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (guix build emacs-utils))
       #:imported-modules (,@%gnu-build-system-modules
                           (guix build emacs-utils))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-after 'unpack 'disable-byte-compile-error-on-warn
                    (lambda _
                      (substitute* "Makefile"
                        (("\\(setq byte-compile-error-on-warn t\\)")
                         "(setq byte-compile-error-on-warn nil)"))
                      #t))
         (add-after 'unpack 'patch-hardcoded-paths
                    (lambda* (#:key inputs outputs #:allow-other-keys)
                      (let ((out   (assoc-ref outputs "out"))
                            (coq   (assoc-ref inputs "coq"))
                            (emacs (assoc-ref inputs "host-emacs")))
                        (define (coq-prog name)
                          (string-append coq "/bin/" name))
                        (substitute* "pgshell/pgshell.el"
                          (("/bin/sh") (which "sh")))
                        ;(emacs-substitute-variables "coq/coq.el"
                        ;  ("coq-prog-name"           (coq-prog "coqtop"))
                        ;  ("coq-compiler"            (coq-prog "coqc"))
                        ;  ("coq-dependency-analyzer" (coq-prog "coqdep")))
                        (substitute* "Makefile"
                          (("/sbin/install-info") "install-info"))
                        (substitute* "bin/proofgeneral"
                          (("^PGHOMEDEFAULT=.*" all)
                           (string-append all
                                          "PGHOME=$PGHOMEDEFAULT\n"
                                          "EMACS=" emacs "/bin/emacs")))
                        #t))))))
         ;(add-after 'unpack 'clean
         ;           (lambda _
         ;             ;; Delete the pre-compiled elc files for Emacs 23.
         ;             (zero? (system* "make" "clean"))))
         ;(add-after 'install 'install-doc
         ;           (lambda* (#:key make-flags #:allow-other-keys)
         ;             ;; XXX FIXME avoid building/installing pdf files,
         ;             ;; due to unresolved errors building them.
         ;             (substitute* "Makefile"
         ;               ((" [^ ]*\\.pdf") ""))
         ;             (zero? (apply system* "make" "install-doc"
         ;                           make-flags)))))))
    (home-page "http://proofgeneral.inf.ed.ac.uk/")
    (synopsis "Generic front-end for proof assistants based on Emacs")
    (description
     "Proof General is a major mode to turn Emacs into an interactive proof
assistant to write formal mathematical proofs using a variety of theorem
provers.")
    (license license:gpl2+)))

(define-public ocaml-menhir-fix
  (package
    (inherit ocaml-menhir)
    (version "20170607")
    (name "ocaml-menhir-fix")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://gallium.inria.fr/~fpottier/menhir/"
                    "menhir-" version ".tar.gz"))
              (sha256
               (base32
                "0qffci9qxgfabzyalx851q994yykl4n9ylr4vbplsm6is1padjh0"))))))

(define-public compcert
  (package
    (name "compcert")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://compcert.inria.fr/release/compcert-"
                                  version ".tgz"))
              (sha256
               (base32
                "0dgrj26dzdy4n3s9b5hwc6lm54vans1v4qx9hdp1q8w1qqcdriq9"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "./configure" "x86_64-linux" "-prefix"
                             (assoc-ref outputs "out"))))))
       #:tests? #f))
    (native-inputs
     `(("ocaml" ,ocaml)
       ("coq" ,coq-fix)))
    (inputs
     `(("menhir" ,ocaml-menhir-fix)))
    (home-page "http://compcert.inria.fr")
    (synopsis "Certified C compiler")
    (description "CompCert is a certified (with coq) C compiler.  Warning: this
package is not free software!")
    ;; actually the "INRIA Non-Commercial License Agreement"
    ;; a non-free license.
    (license (license:non-copyleft "file:///LICENSE"))))

;; yet another build system <3
;; In this one, the autoconf-generated configure script configures the build and
;; builds remake from source, a make-like system specific to this package.
(define-public coq-flocq
  (package
    (name "coq-flocq")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/file/36199/flocq-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0h5mlasirfzc0wwn2isg4kahk384n73145akkpinrxq5jsn5d22h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq-fix)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Flocq"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
             ;; TODO: requires coq-gappa and coq-interval.
             ;(zero? (system* "./remake" "check-more"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
    (home-page "http://flocq.gforge.inria.fr/")
    (synopsis "Floating-point formalization for the Coq system")
    (description "Flocq (Floats for Coq) is a floating-point formalization for
the Coq system.  It provides a comprehensive library of theorems on a multi-radix
multi-precision arithmetic.  It also supports efficient numerical computations
inside Coq.")
    (license license:lgpl3+)))

(define-public coq-gappa
  (package
    (name "coq-gappa")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/file/36351/gappa-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0924jr6f15fx22qfsvim5vc0qxqg30ivg9zxj34lf6slbgdl3j39"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq-fix)
       ("bison" ,bison)
       ("flex" ,flex)))
    (inputs
     `(("gmp" ,gmp)
       ("mpfr" ,mpfr)
       ("boost" ,boost)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Gappa"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
    (home-page "http://gappa.gforge.inria.fr/")
    (synopsis "Verify and formally prove properties on numerical programs")
    (description "Gappa is a tool intended to help verifying and formally proving
properties on numerical programs dealing with floating-point or fixed-point
arithmetic.  It has been used to write robust floating-point filters for CGAL
and it is used to certify elementary functions in CRlibm.  While Gappa is
intended to be used directly, it can also act as a backend prover for the Why3
software verification plateform or as an automatic tactic for the Coq proof
assistant.")
    (license (list license:gpl2 license:cecill))))

(define-public coq-mathcomp
  (package
    (name "coq-mathcomp")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/math-comp/math-comp/archive/mathcomp-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1j9ylggjzrxz1i2hdl2yhsvmvy5z6l4rprwx7604401080p5sgjw"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq-fix)))
    (arguments
     `(#:tests? #f; No need to test formally-verified programs :)
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'chdir
           (lambda _
             (chdir "mathcomp")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "COQLIB" (string-append (assoc-ref outputs "out") "/lib/coq/"))
             (zero? (system* "make" "-f" "Makefile.coq"
                             (string-append "COQLIB=" (assoc-ref outputs "out")
                                            "/lib/coq/")
                             "install")))))))
    (home-page "https://math-comp.github.io/math-comp/")
    (synopsis "Mathematical Components for Coq")
    (description "Mathematical Components for Coq has its origins in the formal
proof of the Four Colour Theorem.  Since then it has grown to cover many areas
of mathematics and has been used for large scale projects like the formal proof
of the Odd Order Theorem.

The library is written using the Ssreflect proof language that is an integral
part of the distribution.")
    (license license:cecill-b)))

(define-public coq-coquelicot
  (package
    (name "coq-coquelicot")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/36537/coquelicot-" version ".tar.gz"))
              (sha256
               (base32
                "0fx99bvsbdizj00gx2im8939y4wwl05f4qhw184j90kcx5vjxxv9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq-fix)))
    (propagated-inputs
     `(("mathcomp" ,coq-mathcomp)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Coquelicot"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
    (home-page "http://coquelicot.saclay.inria.fr/index.html")
    (synopsis "Coq library for Reals")
    (description "Coquelicot is an easier way of writing formulas and theorem
statements, achieved by relying on total functions in place of dependent types
for limits, derivatives, integrals, power series, and so on.  To help with the
proof process, the library comes with a comprehensive set of theorems that cover
not only these notions, but also some extensions such as parametric integrals,
two-dimensional differentiability, asymptotic behaviors.  It also offers some
automations for performing differentiability proofs.  Moreover, Coquelicot is a
conservative extension of Coq's standard library and provides correspondence
theorems between the two libraries.")
    (license license:lgpl3+)))

(define-public coq-interval
  (package
    (name "coq-interval")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/"
                                  "file/36538/interval-" version ".tar.gz"))
              (sha256
               (base32
                "16ir7mizl18kwa1ls8fwjih6r87894bvc1r6lh85cd43la7nriq3"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)
       ("coq" ,coq-fix)))
    (propagated-inputs
     `(("flocq" ,coq-flocq)
       ("coquelicot" ,coq-coquelicot)
       ("mathcomp" ,coq-mathcomp)))
    (arguments
     `(#:configure-flags
       (list (string-append "--libdir=" (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib/Gappa"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-remake
           (lambda _
             (substitute* "remake.cpp"
               (("/bin/sh") (which "sh")))))
         (replace 'build
           (lambda _
             (zero? (system* "./remake"))))
         (replace 'check
           (lambda _
             (zero? (system* "./remake" "check"))))
         (replace 'install
           (lambda _
             (zero? (system* "./remake" "install")))))))
    (home-page "http://coq-interval.gforge.inria.fr/")
    (synopsis "Coq tactics to simplify inequality proofs")
    (description "Interval provides vernacular files containing tactics for
simplifying the proofs of inequalities on expressions of real numbers for the
Coq proof assistant.")
    (license license:cecill-c)))

(define-public coq-fix
  (package
    (inherit coq)
    (name "coq-fix")
    (version "8.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://coq.inria.fr/distrib/V" version
                                  "/files/coq-" version ".tar.gz"))
              (sha256
               (base32
                "1pw1xvy1657l1k69wrb911iqqflzhhp8wwsjvihbgc72r3skqg3f"))))
    (native-search-paths
      (list (search-path-specification
              (variable "COQPATH")
              (files (list "lib/coq/user-contrib")))))))

(define-public cubicle
  (package
    (name "cubicle")
    (version "1.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://cubicle.lri.fr/cubicle-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1sny9c4fm14k014pk62ibpwbrjjirkx8xmhs9jg7q1hk7y7x3q2h"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)
       ("which" ,which)))
    (propagated-inputs
     `(("z3" ,z3)))
    (arguments
     `(#:configure-flags (list "--with-z3")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'configure-for-release
           (lambda _
             (substitute* "Makefile.in"
               (("SVNREV=") "#SVNREV="))))
         (add-before 'configure 'fix-/bin/sh
           (lambda _
             (substitute* "configure"
               (("/bin/sh") (which "sh")))))
         (add-before 'configure 'fix-smt-z3wrapper.ml
           (lambda _
             (substitute* "Makefile.in"
               (("\\\\n") "")))))))
    (home-page "http://cubicle.lri.fr/")
    (synopsis "Model checker for array-based systems")
    (description "Cubicle is an open source model checker for verifying safety
properties of array-based systems.  This is a syntactically restricted class of
parametrized transition systems with states represented as arrays indexed by an
arbitrary number of processes.  Cache coherence protocols and mutual exclusion
algorithms are typical examples of such systems.")
    (license license:asl2.0)))

(define-public ocaml-c2newspeak
  (package
    (name "ocaml-c2newspeak")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/airbus-seclab/c2newspeak")
                     (commit "6f7adf13fefb7f8d4dc668b8290226e3c6a30063")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1apaz0b84865xfba0mxbskbnaq6llqsn3qhy8b0sssbdxzw5w1x4"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "check"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'install 'modify-installed-file-list
           (lambda _
             (substitute* "src/newspeak.Makefile"
               (("c2newspeak/typedC.cmi")
                "c2newspeak/typedC.cmi c2newspeak/typedC.cmx c2newspeak/typedC.o"))))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "bin/c2newspeak" (string-append (assoc-ref outputs "out") "/bin")))))))
    (home-page "https://github.com/airbus-seclab/c2newspeak")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public ocaml-bincat
  (package
    (name "ocaml-bincat")
    (version "0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/airbus-seclab/bincat/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1762wrvf7fv16kxfvpblj4b0pwbwny1b39263q4jnqni12474djl"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; some failures for unknown reasons
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "LDCONFIG=true"
             (string-append "CFLAGS+=-I " (assoc-ref %build-inputs "ocaml")
                            "/lib/ocaml"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'python-path
           (lambda _
             (setenv "PYTHONPATH" (string-append (getenv "PYTHONPATH")
                                                 ":../python"))))
         (add-before 'build 'fix-makefile
           (lambda _
             (substitute* "ocaml/src/Makefile"
               (("GITVERSION:=.*") "GITVERSION:=0.6\n")
               ;; typedC library is embedded in newspeak.cmxa
               (("typedC.cmx") ""))))
         (add-before 'check 'fix-test
           (lambda _
             (setenv "PATH" (string-append (getenv "PATH") ":" (getcwd) "/ocaml/src"))
             (chmod "test/eggloader_x86" #o755))))))
    (inputs
     `(("c2newspeak" ,ocaml-c2newspeak)
       ("zarith" ,ocaml-zarith)
       ("menhir" ,ocaml-menhir)
       ("ocamlgraph" ,ocaml-graph)
       ("gmp" ,gmp)))
    (native-inputs
     `(("python" ,python-2)
       ("pytest" ,python2-pytest)
       ("sphinx" ,python2-sphinx)
       ("nasm" ,nasm)))
    (home-page "https://github.com/airbus-seclab/bincat")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

