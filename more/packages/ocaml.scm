;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2019 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix build-system dune)
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
  #:use-module (gnu packages compression)
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
  #:use-module (gnu packages python-xyz)
  #:use-module (gnu packages sphinx)
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
       #:imported-modules (,@%default-gnu-imported-modules
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
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-latest-ocaml
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "Makefile.build"
               (("MLINCLUDES=") (string-append
                                  "MLINCLUDES=-I "
                                  (assoc-ref inputs "ocaml-num")
                                  "/lib/ocaml/site-lib ")))
             (substitute* "configure.ml"
               (("CAMLFLAGS=") "CAMLFLAGS=-unsafe-string -package num "))
             (substitute* "ide/ideutils.ml"
               (("String.blit") "Bytes.blit"))
             (substitute* "tools/coqmktop.ml"
               (("nums") (string-append (assoc-ref inputs "ocaml-num")
                                        "/lib/ocaml/site-lib/nums"))
               (("\"-linkall\"") "\"-linkall\" :: \"-package\" :: \"num\""))
             #t))
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
     `(("lablgtk" ,lablgtk)
       ("python" ,python-2)
       ("camlp5" ,camlp5)
       ("ocaml-num" ,ocaml-num)))))

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

(define-public coq-8.8
  (package
    (inherit coq)
    (name "coq")
    (version "8.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/coq/coq/archive/V"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0i2hs0i6rp27cy8zd0mx7jscqw5cx2y0diw0pxgij66s3yr47y7r"))))
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

;(define-public ocaml-bincat
;  (package
;    (name "ocaml-bincat")
;    (version "0.8.1")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/airbus-seclab/bincat/archive/v"
;                                  version ".tar.gz"))
;              (file-name (string-append name "-" version ".tar.gz"))
;              (sha256
;               (base32
;                "1ncwm1h428x1bs4sq7ql1isrkhw0angglsa9hnsvhhw2i1jsdk7j"))))
;    (build-system ocaml-build-system)
;    (arguments
;     `(#:tests? #f; disabled for now
;       #:validate-runpath? #f; disabled for now
;       #:make-flags
;       (list (string-append "PREFIX=" (assoc-ref %outputs "out"))
;             "LDCONFIG=true"
;             (string-append "CFLAGS+=-I " (assoc-ref %build-inputs "ocaml")
;                            "/lib/ocaml"))
;       #:phases
;       (modify-phases %standard-phases
;         (delete 'configure)
;         (add-before 'build 'python-path
;           (lambda* (#:key outputs #:allow-other-keys)
;             (setenv "PYTHONPATH" (string-append (getenv "PYTHONPATH")
;                                                 ":../python:"
;                                                 (assoc-ref outputs "out")
;                                                 "/lib/python2.7/site-packages/"))
;             #t))
;         (add-before 'build 'fix-makefiles
;           (lambda _
;             (substitute* "ocaml/src/Makefile"
;               (("GITVERSION:=.*") "GITVERSION:=0.8.1\n"))
;             (substitute* "python/Makefile"
;               (("./setup.py install") "./setup.py install --prefix=$(PREFIX)"))
;             #t))
;         (add-before 'check 'fix-test
;           (lambda _
;             (setenv "PATH" (string-append (getenv "PATH") ":" (getcwd) "/ocaml/src"))
;             ;; Remove tests that require an armv8 compiler
;             (substitute* "test/Makefile"
;               (("eggloader_armv8 eggloader_armv7 eggloader_armv7thumb") ""))
;             (chmod "test/eggloader_x86" #o755)
;             #t))
;         (add-before 'install 'install-python-dir
;           (lambda* (#:key outputs #:allow-other-keys)
;             (mkdir-p (string-append (assoc-ref outputs "out")
;                                     "/lib/python2.7/site-packages/")))))))
;    (inputs
;     `(("c2newspeak" ,ocaml-c2newspeak)
;       ("zarith" ,ocaml-zarith)
;       ("menhir" ,ocaml-menhir)
;       ("ocamlgraph" ,ocaml-graph)
;       ("ocaml-cppo" ,ocaml-cppo)
;       ("ocaml-ppx-tools" ,ocaml-ppx-tools)
;       ("gmp" ,gmp)))
;    (native-inputs
;     `(("python" ,python-2)
;       ("pytest" ,python2-pytest)
;       ("sphinx" ,python2-sphinx)
;       ("nasm" ,nasm)))
;    (home-page "https://github.com/airbus-seclab/bincat")
;    (synopsis "")
;    (description "")
;    (license license:lgpl2.1+)))

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

(define-public ocaml-optint
  (package
    (name "ocaml-optint")
    (version "0.0.2")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/dinosaure/optint/releases/download/v0.0.2/optint-v0.0.2.tbz")
        (sha256
          (base32
            "1lmb7nycmkr05y93slqi98i1lcs1w4kcngjzjwz7i230qqjpw9w1"))))
    (build-system dune-build-system)
    (arguments
     `(#:test-target "."))
    (home-page "https://github.com/dinosaure/optint")
    (synopsis
      "Abstract type on integer between x64 and x86 architecture")
    (description
      "This library provide one module `Optint` which use internally an `int` if you
are in a x64 architecture or an `int32` (boxed value) if you are in a x86
architecture. This module is __really__ unsafe and does not care some details
(like the sign bit) for any cast.

## Goal

The main difference between an `int` and an `int32` is the second is boxed.
About performance this is not the best. However, you can not ensure to be in an
x64 architecture where you can use directly an `int` instead an `int32` (and
improve performance).

So, this library provide an abstraction about a real `int32`. In a x64
architecture, internally, we use a `int` and in a x86 architure, we use a
`int32`. By this way, we ensure to have in any platform 32 free bits in
`Optint.t`.")
    (license #f)))

(define-public ocaml-checkseum
  (package
    (name "ocaml-checkseum")
    (version "0.0.3")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/dinosaure/checkseum/releases/download/v0.0.3/checkseum-v0.0.3.tbz")
        (sha256
          (base32
            "12j45zsvil1ynwx1x8fbddhqacc8r1zf7f6h576y3f3yvbg7l1fm"))))
    (build-system dune-build-system)
    (propagated-inputs
      `(("ocaml-optint" ,ocaml-optint)
        ("ocaml-fmt" ,ocaml-fmt)
        ("ocaml-rresult" ,ocaml-rresult)
        ("ocaml-cmdliner" ,ocaml-cmdliner)))
    (native-inputs
      `(("ocaml-alcotest" ,ocaml-alcotest)))
    (home-page
      "https://github.com/dinosaure/checkseum")
    (synopsis
      "Adler-32, CRC32 and CRC32-C implementation in C and OCaml")
    (description
      "Checkseum is a library to provide implementation of Adler-32, CRC32 and CRC32-C in C and OCaml.

This library use the linking trick to choose between the C implementation (checkseum.c) or the OCaml implementation (checkseum.ocaml).
This library is on top of optint to get the best representation of an int32.
")
    (license #f)))

; not the latest but imagelib requires 0.7
(define-public ocaml-decompress
  (package
    (name "ocaml-decompress")
    (version "0.7")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/mirage/decompress/releases/download/v0.7/decompress-0.7.tbz")
        (sha256
          (base32
            "1q96q4bhrlz13c33jj82qn6706m8dbn4azc6yja8lbavpy4q5zpy"))))
    (build-system ocaml-build-system)
    (arguments
     ;; Tets need some path modification
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "ocaml" "pkg/pkg.ml" "build")))
         (add-before 'build 'set-topfind
           (lambda* (#:key inputs #:allow-other-keys)
             ;; add the line #directory ".." at the top of each file
             ;; using #use "topfind";; to be able to find topfind
             (let* ((findlib-path (assoc-ref inputs "findlib"))
                    (findlib-libdir
                     (string-append findlib-path "/lib/ocaml/site-lib")))
               (substitute* '("pkg/pkg.ml")
                 (("#use       \"topfind\";;" all)
                  (string-append "#directory \"" findlib-libdir "\"\n"
                                 all))))
             #t)))))
    (propagated-inputs
     `(("ocaml-optint" ,ocaml-optint)
        ("ocaml-checkseum" ,ocaml-checkseum)
        ("ocaml-cmdliner" ,ocaml-cmdliner)))
    (native-inputs
      `(("camlzip" ,camlzip)
        ("ocaml-re" ,ocaml-re)
        ("ocaml-topkg" ,ocaml-topkg)
        ("ocamlbuild" ,ocamlbuild)
        ("ocaml-alcotest" ,ocaml-alcotest)
        ("opam" ,opam)))
    (inputs
     `(("zlib" ,zlib)))
    (home-page
      "https://github.com/mirage/decompress")
    (synopsis "Implementation of Zlib in OCaml")
    (description
      "Decompress is an implementation of Zlib in OCaml

It provides a pure non-blocking interface to inflate and deflate data flow.
")
    (license #f)))

(define-public ocaml-imagelib
  (package
    (name "ocaml-imagelib")
    (version "20180522")
    (source
      (origin
        (method url-fetch)
        (uri "https://github.com/rlepigre/ocaml-imagelib/archive/ocaml-imagelib_20180522.tar.gz")
        (sha256
          (base32
            "06l724fj8yirp5jbf782r2xl3lrcff2i1b3c3pf80kbgngw6kakg"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda _
             (invoke "make")))
         (replace 'install
           (lambda _
             (invoke "make" "install"))))))
    (propagated-inputs
      `(("ocaml-decompress" ,ocaml-decompress)
        ("which" ,which)))
    (native-inputs
      `(("ocamlbuild" ,ocamlbuild)))
    (home-page "http://lepigre.fr")
    (synopsis
      "The imagelib library implements image formats such as PNG and PPM")
    (description
      "The imagelib library implements image formats such as PNG and PPM in
OCaml, relying on only one external dependency: 'decompress'.

Supported image formats:
 - PNG (full implementation of RFC 2083),
 - PPM, PGM, PBM, ... (fully supported),
 - JPG (only image size natively, conversion to PNG otherwise),
 - GIF (only image size natively, conversion to PNG otherwise),
 - XCF (only image size natively, conversion to PNG otherwise),
 - Other formats rely on 'convert' (imagemagick).

As imagelib only requires 'decompress', it can be used together with
js_of_ocaml to compile projects to Javascript. Note that some of the
features of imagelib require the convert binary  (and thus cannot be
used from Javascript).")
    (license #f)))


; Require earley and sqlite3 to be up-to-date
;(define-public patoline
;  (package
;    (name "patoline")
;    (version "0.2")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/patoline/patoline/archive/"
;                                  version ".tar.gz"))
;              (file-name (string-append name "-" version ".tar.gz"))
;              (sha256
;               (base32
;                "1qlxcf8k83lcyamyg19838j3f1js068skxgab94axv2gv4ylhhfb"))))
;    (build-system dune-build-system)
;    (arguments
;     `(#:test-target "."
;       #:phases
;       (modify-phases %standard-phases
;         (add-before 'build 'set-dirs
;           (lambda* (#:key outputs #:allow-other-keys)
;             (let ((out (assoc-ref outputs "out")))
;               (substitute* '("unicodelib/config.ml"
;                              "patconfig/patDefault.ml")
;                 (("/usr/local/share") (string-append out "/share"))))
;             #t)))))
;    (propagated-inputs
;     `(("camlzip" ,camlzip)
;       ("ocaml-earley" ,ocaml-earley)
;       ("ocaml-imagelib" ,ocaml-imagelib)
;       ("ocaml-sqlite3" ,ocaml-sqlite3)))
;    (inputs
;     `(("zlib" ,zlib)))
;    (home-page "")
;    (synopsis "")
;    (description "")
;    (license license:gpl2+)))

(define-public coq-tlc
  (package
    (name "coq-tlc")
    (version "20181116")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.inria.fr/charguer/tlc")
                     (commit version)))
              (sha256
               (base32
                "1fkb4z92m04wdic4im4fd7dypbr1lnz285f06ci52kxgv2w4bkjz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-timestamp
           (lambda _
             (substitute* "GNUmakefile"
               (("\\$\\(shell /bin/date.*") (string-append ,version "\n")))
             #t))
         (add-before 'build 'fix-timestamp
           (lambda _
             (substitute* "src/Makefile.coq"
               (("/usr/bin/env bash") (which "bash")))
             #t))
         (add-before 'build 'fix-install-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/Makefile"
               (("TARGET *:=.*") (string-append "TARGET := " (assoc-ref outputs "out")
                                               "/lib/coq/user-contrib/TLC")))
             #t)))))
    (native-inputs
     `(("coq" ,coq)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))
