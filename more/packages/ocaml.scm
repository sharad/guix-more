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
  #:use-module (gnu packages m4)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tex)
  #:use-module (gnu packages texinfo)
  #:use-module (more packages smt))

(define-public ocaml-fix
  (package
    (inherit ocaml)
    (version "4.05.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://caml.inria.fr/pub/distrib/ocaml-"
                    (version-major+minor version)
                    "/ocaml-" version ".tar.xz"))
              (sha256
               (base32
                "1y9fw1ci9pwnbbrr9nwr8cq8vypcxwdf4akvxard3mxl2jx2g984"))))
    (arguments
     `(#:modules ((guix build gnu-build-system)
                  (guix build utils)
                  (web server))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-/bin/sh-references
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((sh (string-append (assoc-ref inputs "bash")
                                       "/bin/sh"))
                    (quoted-sh (string-append "\"" sh "\"")))
               (with-fluids ((%default-port-encoding #f))
                 (for-each
                  (lambda (file)
                    (substitute* file
                      (("\"/bin/sh\"")
                       (begin
                         (format (current-error-port) "\
patch-/bin/sh-references: ~a: changing `\"/bin/sh\"' to `~a'~%"
                                 file quoted-sh)
                         quoted-sh))))
                  (find-files "." "\\.ml$"))
                 #t))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man")))
               ;; Custom configure script doesn't recognize
               ;; --prefix=<PREFIX> syntax (with equals sign).
               (zero? (system* "./configure"
                               "--prefix" out
                               "--mandir" mandir)))))
         (replace 'build
           (lambda _
             (zero? (system* "make" "-j1" ;; fails to build otherwise
                             "world.opt"))))
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "testsuite"
               (zero? (system* "make" "all"))))))))))

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

(define-public ocaml-build
  (package
    (name "ocaml-build")
    (version "0.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/ocamlbuild/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1vh30731gv1brr4ljfzd6m5lni44ifyb1w8hwir81ff9874fs5qp"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f; FIXME: tests fail to find Findlib
       #:make-flags
       (list (string-append "OCAMLBUILD_PREFIX=" (assoc-ref %outputs "out"))
             (string-append "OCAMLBUILD_BINDIR=" (assoc-ref %outputs "out") "/bin")
             (string-append "OCAMLBUILD_LIBDIR=" (assoc-ref %outputs "out") "/lib")
             (string-append "OCAMLBUILD_MANDIR=" (assoc-ref %outputs "out") "/share/man"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         ;(replace 'configure
         ;  (lambda* (#:key outputs #:allow-other-keys)
         ;    (let ((out (assoc-ref %outputs "out")))
         ;      (zero? (system* "make" "-f" "configure.make" "all")))))
         (add-before 'build 'findlib-environment
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out")))
               (setenv "OCAMLFIND_DESTDIR" (string-append out "/lib/ocaml/site-lib"))
               (setenv "OCAMLFIND_LDCONF" "ignore")
               #t))))))
    (native-inputs
     `(("ocaml" ,ocaml-fix)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public camlp4-fix
  (package
    (inherit camlp4)
    (name "camlp4")
    (version "4.05+2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/camlp4/archive/"
								  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
			    "0dd9scf50y0928syvxflljwry2dzm35n903fgpfdkpcn907jq96v"))))
    (inputs `(("ocaml" ,ocaml-fix)))
    (native-inputs
     `(("ocaml" ,ocaml-fix)
       ("which" ,which)
       ("build" ,ocaml-build)))))

(define-public ocaml-findlib-fix
  (package
    (inherit ocaml-findlib)
    (native-inputs
     `(("camlp4" ,camlp4-fix)
       ("ocaml" ,ocaml-fix)
       ("m4" ,m4)))))

(define-public camlp5-fix
  (package
    (inherit camlp5)
    (name "camlp5")
    (version "7.03")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/camlp5/camlp5/archive/rel"
                                  (string-delete #\. version) ".tar.gz"))
              (sha256
               (base32
                "06pj7l75r586gngam7nspd1a13ay7cj2bjh035z64w4fgaahlgf1"))))
    (inputs
     `(("ocaml" ,ocaml-fix)))))

(define-public lablgtk-fix
  (package
    (inherit lablgtk)
    (native-inputs
     `(("ocaml" ,ocaml-fix)
       ("build" ,ocaml-build)
       ("camlp4" ,camlp4-fix)
       ("findlib" ,ocaml-findlib-fix)
       ("pkg-config" ,pkg-config)))))

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
                "0qffci9qxgfabzyalx851q994yykl4n9ylr4vbplsm6is1padjh0"))))
    (inputs
     `(("ocaml" ,ocaml-fix)
       ("ocamlbuild" ,ocaml-build)))))

(define-public coq-fix
  (package
    (inherit coq)
    (native-inputs
     `(("ocamlbuild" ,ocaml-build)
       ("hevea" ,hevea)
       ("texlive" ,texlive)))
    (inputs
     `(("lablgtk" ,lablgtk-fix)
       ("python" ,python-2)
       ("camlp5" ,camlp5-fix)))
    (arguments
     `(#:ocaml ,ocaml-fix
       #:findlib ,ocaml-findlib-fix
       ,@(package-arguments coq)))))

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
     `(("ocaml" ,ocaml-fix)
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

