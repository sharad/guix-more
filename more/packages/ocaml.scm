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
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages coq)
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
  #:use-module (more packages smt)
  #:use-module (ice-9 match))

(define (ocaml-forge-uri name version file-number)
  (string-append "https://forge.ocamlcore.org/frs/download.php/"
                 (number->string file-number) "/" name "-" version
                 ".tar.gz"))

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

(define-public ocaml4.02-camlp5
  (package
    (inherit camlp5)
    (inputs
     `(("ocaml" ,ocaml-4.02)))))

(define-public coq-8.6
  (package
    (inherit coq)
    (name "coq")
    (version "8.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/coq/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02nm5sn79hrb9fdmkhyclk80jydadf4jcafmr3idwr5h4z56qbms"))))
    (arguments
     `(#:ocaml ,ocaml-4.02
       #:findlib ,ocaml4.02-findlib
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man"))
                    (browser "icecat -remote \"OpenURL(%s,new-tab)\""))
               (invoke "./configure"
                       "-prefix" out
                       "-mandir" mandir
                       "-browser" browser
                       "-coqide" "opt"))
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "make" "-j" (number->string
                                  (parallel-job-count))
                     "world")
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "test-suite"
               (invoke "make"))
             #t)))))
    (native-inputs '())
    (inputs
     `(("lablgtk" ,ocaml4.02-lablgtk)
       ("python" ,python-2)
       ("camlp5" ,ocaml4.02-camlp5)))))

(define-public coq-8.7
  (package
    (inherit coq)
    (name "coq")
    (version "8.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/coq/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lkqvs7ayzv5kkg26y837pg0d6r2b5hbjxl71ba93f39kybw69gg"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (mandir (string-append out "/share/man"))
                    (browser "icecat -remote \"OpenURL(%s,new-tab)\""))
               (invoke "./configure"
                       "-prefix" out
                       "-mandir" mandir
                       "-browser" browser
                       "-coqide" "opt"))
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "ide/ideutils.ml"
               (("Bytes.unsafe_to_string read_string") "read_string"))
             (invoke "make" "-j" (number->string
                                  (parallel-job-count))
                     (string-append
                       "USERFLAGS=-I "
                       (assoc-ref inputs "ocaml-num")
                       "/lib/ocaml/site-lib")
                     "world")
             #t))
         (delete 'check)
         (add-after 'install 'check
           (lambda _
             (with-directory-excursion "test-suite"
               ;; These two tests fail.
               ;; This one fails because the output is not formatted as expected.
               (delete-file-recursively "coq-makefile/timing")
               ;; This one fails because we didn't build coqtop.byte.
               (delete-file-recursively "coq-makefile/findlib-package")
               (invoke "make"))
             #t)))))))

(define-public coq-8.9
  (package
    (inherit coq)
    (name "coq")
    (version "8.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/coq/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1w0g0w2ps3v17g0nkf9zrnlhzrfvvzxyp248kgqnvybrinyf5mlb"))))
    (native-inputs
     `(("ocaml-ounit" ,ocaml-ounit)
       ,@(package-native-inputs coq)))))

(define-public coq-bignums-8.7
  (package
    (inherit coq-bignums)
    (name "coq-bignums")
    (version "8.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/bignums/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "03iw9jiwq9jx45gsvp315y3lxr8m9ksppmcjvxs5c23qnky6zqjx"))))
    (native-inputs
     `(("ocaml" ,ocaml)
       ("coq-8.7" ,coq-8.7)))
    (inputs
     `(("camlp5" ,camlp5)))))

(define-public ppsimpl
  (package
    (name "ppsimpl")
    (version "8.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://scm.gforge.inria.fr/anonscm/git/ppsimpl/ppsimpl.git")
                     (commit "86255a47568df58767d1d8e0b9e2da31cf73a5fc")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0h509w43j2wd2pyx04k3xfd0bsbmqscwqvhf8whzc3cxzl4j6vvq"))))
              ;(uri "https://gforge.inria.fr/frs/download.php/file/37615/ppsimpl-09-07-2018.tar.gz")
              ;(sha256
              ; (base32
              ;  "010zgskc1wd5v6wmmyxaapvwxjlgbdqqiks2dvf6llx03b07ak59"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:configure-flags
       (list "-R" (string-append (assoc-ref %build-inputs "compcert") "/lib/coq/user-contrib/compcert") "compcert")
       #:make-flags (list "COQC=coqc -R src PP -I src"
                          (string-append
                            "COQLIBINSTALL="
                            (assoc-ref %outputs "out")
                            "/lib/coq/user-contrib"))))
    (inputs
     `(("coq-bignums-8.7" ,coq-bignums-8.7)
       ("compcert" ,compcert)))
    (native-inputs
     `(("coq-8.7" ,coq-8.7)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)
       ("camlp4" ,camlp4)
       ("camlp5" ,camlp5)
       ("which" ,which)))
    (home-page "")
    (synopsis "")
    (description "")
    ;; No declared license -> all rights reserved
    (license #f)))

(define-public compcert
  (package
    (name "compcert")
    (version "3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://compcert.inria.fr/release/compcert-"
                                  version ".tgz"))
              (sha256
               (base32
                "16xrqcwak1v1fk5ndx6jf1yvxv3adsr7p7z34gfm2mpggxnq0xwn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((system ,(match (or (%current-target-system) (%current-system))
                              ("x86_64-linux" "x86_64-linux")
                              ("i686-linux" "x86_32-linux")
                              ("armhf-linux" "arm-linux"))))
               (format #t "Building for ~a~%" system)
               (invoke "./configure" system "-prefix"
                       (assoc-ref outputs "out")))
             #t))
         (add-after 'install 'install-lib
           (lambda* (#:key outputs #:allow-other-keys)
             (for-each
               (lambda (file)
                 (install-file
                   file
                   (string-append
                     (assoc-ref outputs "out")
                     "/lib/coq/user-contrib/compcert/" (dirname file))))
               (find-files "." ".*.vo$"))
             #t)))
       #:tests? #f))
    (native-inputs
     `(("ocaml" ,ocaml)
       ("coq" ,coq-8.7)))
    (inputs
     `(("menhir" ,ocaml-menhir)))
    (home-page "http://compcert.inria.fr")
    (synopsis "Certified C compiler")
    (description "CompCert is a certified (with coq) C compiler.  Warning: this
package is not free software!")
    ;; actually the "INRIA Non-Commercial License Agreement"
    ;; a non-free license.
    (license (license:non-copyleft "file:///LICENSE"))))

(define-public ocaml-c2newspeak
  (package
    (name "ocaml-c2newspeak")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/airbus-seclab/c2newspeak")
                     (commit "c97fd380111a49fa7baeb9e49c45238fca627492")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0fxh868s5jraq61mnig9ilhyjzsq4iw32f20zh3982naanp4p8r6"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:test-target "check"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
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
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/airbus-seclab/bincat/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1ncwm1h428x1bs4sq7ql1isrkhw0angglsa9hnsvhhw2i1jsdk7j"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; disabled for now
       #:validate-runpath? #f; disabled for now
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
             "LDCONFIG=true"
             (string-append "CFLAGS+=-I " (assoc-ref %build-inputs "ocaml")
                            "/lib/ocaml"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'python-path
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "PYTHONPATH" (string-append (getenv "PYTHONPATH")
                                                 ":../python:"
                                                 (assoc-ref outputs "out")
                                                 "/lib/python2.7/site-packages/"))
             #t))
         (add-before 'build 'fix-makefiles
           (lambda _
             (substitute* "ocaml/src/Makefile"
               (("GITVERSION:=.*") "GITVERSION:=0.8.1\n"))
             (substitute* "python/Makefile"
               (("./setup.py install") "./setup.py install --prefix=$(PREFIX)"))
             #t))
         (add-before 'check 'fix-test
           (lambda _
             (setenv "PATH" (string-append (getenv "PATH") ":" (getcwd) "/ocaml/src"))
             ;; Remove tests that require an armv8 compiler
             (substitute* "test/Makefile"
               (("eggloader_armv8 eggloader_armv7 eggloader_armv7thumb") ""))
             (chmod "test/eggloader_x86" #o755)
             #t))
         (add-before 'install 'install-python-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out")
                                     "/lib/python2.7/site-packages/")))))))
    (inputs
     `(("c2newspeak" ,ocaml-c2newspeak)
       ("zarith" ,ocaml-zarith)
       ("menhir" ,ocaml-menhir)
       ("ocamlgraph" ,ocaml-graph)
       ("ocaml-cppo" ,ocaml-cppo)
       ("ocaml-ppx-tools" ,ocaml-ppx-tools)
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

(define-public ocaml-ocplib-simplex
  (package
    (name "ocaml-ocplib-simplex")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OCamlPro-Iguernlala/"
                                  "ocplib-simplex/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0y6q4bgly7fisdklriww48aknqf2vg4dphr7wwnd1wh80l4anzg1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; Compilation error
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'autoreconf
           (lambda _
             (invoke "autoreconf" "-fiv")
             #t))
         (add-before 'install 'mkdir
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib")))
               (mkdir-p lib)
               #t))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("ocaml" ,ocaml)
       ("ocaml-findlib" ,ocaml-findlib)
       ("which" ,which)))
    (home-page "")
    (synopsis "")
    (description "")
    ; lgpl2.1+ with linking exception
    (license license:lgpl2.1+)))

(define-public frama-c
  (package
    (name "frama-c")
    (version "20171101")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://frama-c.com/download/frama-c-Sulfur-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1vwjfqmm1r36gkybsy3a7m89q5zicf4rnz5vlsn9imnpjpl9gjw1"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f; for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'export-shell
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CONFIG_SHELL" (string-append (assoc-ref inputs "bash")
                                                   "/bin/sh")))))))
    (inputs
     `(("gmp" ,gmp)
       ("ocaml-graph" ,ocaml-graph)
       ("ocaml-zarith" ,ocaml-zarith)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public coq-io
  (package
    (name "coq-io")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq-io/io/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k1z8kav3wz5n04g3imm1hqjimb9cf12ga5wkj1skz8l5ccjxprw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:make-flags
       (list (string-append "COQLIB=" (assoc-ref %outputs "out") "/lib/coq/"))
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda _
             (invoke "./configure.sh")
             #t)))))
    (native-inputs
     `(("coq" ,coq-8.6)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))
