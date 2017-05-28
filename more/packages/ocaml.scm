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
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ocaml)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages emacs)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages multiprecision)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages texinfo))

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

(define-public ocaml-ppx-sexp-value
  (package
    (name "ocaml-ppx-sexp-value")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_value" version
              "0m3ag23mbqm0i2pv1dzilfks15ipa5q60mf57a0cd3p0pvarq10g"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_sexp_value/")
    (synopsis "Simplify building s-expressions from ocaml values")
    (description "A ppx rewriter that simplifies building s-expressions from
ocaml values.")
    (license license:asl2.0)))

(define-public ocaml-ppx-pipebang
  (package
    (name "ocaml-ppx-pipebang")
    (version "113.33.03")
    (source (janestreet-origin "ppx_pipebang" version
              "1965c7hymp26ncmjs0pfxi2s5jlj60z2c9b194lgcwxqiav56pcw"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_pipebang/")
    (synopsis "Inline reverse application operators `|>` and `|!`")
    (description "A ppx rewriter that inlines reverse application operators
`|>` and `|!`.")
    (license license:asl2.0)))

(define-public ocaml-ppx-bin-prot
  (package
    (name "ocaml-ppx-bin-prot")
    (version "113.33.03")
    (source (janestreet-origin "ppx_bin_prot" version
              "173kjv36giik11zgfvsbzwfbpr66dm2pcha9vf990jgzh8hqz39h"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("bin-prot" ,ocaml-bin-prot)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_bin_prot/")
    (synopsis "Generation of bin_prot readers and writers from types")
    (description "Generation of binary serialization and deserialization
functions from type definitions.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fail
  (package
    (name "ocaml-ppx-fail")
    (version "113.33.03")
    (source (janestreet-origin "ppx_fail" version
              "1dwgad0f05gqp5rnwf9dcasidpfi7q3mrpazsw3a2vijjblbhjgn"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_fail/")
    (synopsis "Add location to calls to failwiths")
    (description "Syntax extension that makes [failwiths] always include a
position.")
    (license license:asl2.0)))

(define-public ocaml-ppx-custom-printf
  (package
    (name "ocaml-ppx-custom-printf")
    (version "113.33.03")
    (source (janestreet-origin "ppx_custom_printf" version
              "11jlx0n87g2j1vyyp343dibx7lvvwig5j5q0nq0b80kbsq0k6yr8"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-driver" ,ocaml-ppx-driver)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_custom_printf/")
    (synopsis "Printf-style format-strings for user-defined string conversion")
    (description "Extensions to printf-style format-strings for user-defined
string conversion.")
    (license license:asl2.0)))

(define-public ocaml-ppx-sexp-message
  (package
    (name "ocaml-ppx-sexp-message")
    (version "113.33.03")
    (source (janestreet-origin "ppx_sexp_message" version
              "084w1l3gnyw4ri9vbn7bv9b2xkw1520qczfxpxdarfivdrz8xr68"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_sexp_message/")
    (synopsis "A ppx rewriter for easy construction of s-expressions")
    (description "The aim of ppx_sexp_message is to ease the creation of
s-expressions in OCaml.  This is mainly motivated by writing error and debugging
messages, where one needs to construct a s-expression based on various element
of the context such as function arguments.")
    (license license:asl2.0)))

(define-public ocaml-ppx-fields-conv
  (package
    (name "ocaml-ppx-fields-conv")
    (version "113.33.03")
    (source (janestreet-origin "ppx_fields_conv" version
              "1vzbdz27g5qhhfs7wx6rjf979q4xyssxqbmp6sc1sxknbghslbdv"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)
       ("ppx-core" ,ocaml-ppx-core)))
    (propagated-inputs
     `(("fieldslib" ,ocaml-fieldslib)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-type-conv" ,ocaml-ppx-type-conv)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_fields_conv/")
    (synopsis "Generation of accessor and iteration functions for ocaml records")
    (description "Ppx_fields_conv is a ppx rewriter that can be used to define
first class values representing record fields, and additional routines, to get
and set record fields, iterate and fold over all fields of a record and create
new record values.")
    (license license:asl2.0)))

(define-public ocaml-re
  (package
    (name "ocaml-re")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ocaml/ocaml-re//archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1s3rcr76cgm4p1xmaazc58arkg2lz3zfcp1icm00m6s5ccnkh67b"))))
    (build-system ocaml-build-system)
    (native-inputs `(("ounit" ,ocaml-ounit)))
    (home-page "https://github.com/ocaml/ocaml-re/")
    (synopsis "Regular expression library for OCaml")
    (description "Pure OCaml regular expressions with:
enumerate
@item Perl-style regular expressions (module Re_perl)
@item Posix extended regular expressions (module Re_posix)
@item Emacs-style regular expressions (module Re_emacs)
@item Shell-style file globbing (module Re_glob)
@item Compatibility layer for OCaml's built-in Str module (module Re_str)
@end enumerate")
    (license license:expat)))

(define-public ocaml-ppx-expect
  (package
    (name "ocaml-ppx-expect")
    (version "113.33.03")
    (source (janestreet-origin "ppx_expect" version
              "03sbs4s5i8l9syr45v25f5hzy7msd2b47k2a9wsq9m43d4imgkrc"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("fieldslib" ,ocaml-fieldslib)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("ppx-assert" ,ocaml-ppx-assert)
       ("ppx-compare" ,ocaml-ppx-compare)
       ("ppx-core" ,ocaml-ppx-core)
       ("ppx-custom-printf" ,ocaml-ppx-custom-printf)
       ("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-fields-conv" ,ocaml-ppx-fields-conv)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-variants-conv" ,ocaml-ppx-variants-conv)
       ("re" ,ocaml-re)
       ("sexplib" ,ocaml-sexplib)
       ("variantslib" ,ocaml-variantslib)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_expect/")
    (synopsis "Cram like framework for OCaml")
    (description "Expect-test is a framework for writing tests in OCaml, similar
to Cram.  Expect-tests mimic the existing inline tests framework with the
let%expect_test construct.  The body of an expect-test can contain
output-generating code, interleaved with %expect extension expressions to denote
the expected output.")
    (license license:asl2.0)))

(define-public ocaml-ppx-jane
  (package
    (name "ocaml-ppx-jane")
    (version "113.33.03")
    (source (janestreet-origin "ppx_jane" version
              "0bjxkhmzgm6x9dcvjwybbccn34khbvyyjimcbaja30fp6qcqk5yl"))
    (build-system ocaml-build-system)
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("opam" ,opam)))
    (propagated-inputs
     `(("ppx-assert" ,ocaml-ppx-assert)
       ("ppx-bench" ,ocaml-ppx-bench)
       ("ppx-bin-prot" ,ocaml-ppx-bin-prot)
       ("ppx-compare" ,ocaml-ppx-compare)
       ("ppx-custom-printf" ,ocaml-ppx-custom-printf)
       ("ppx-deriving" ,ocaml-ppx-deriving)
       ("ppx-enumerate" ,ocaml-ppx-enumerate)
       ("ppx-expect" ,ocaml-ppx-expect)
       ("ppx-fail" ,ocaml-ppx-fail)
       ("ppx-fields-conv" ,ocaml-ppx-fields-conv)
       ("ppx-here" ,ocaml-ppx-here)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("ppx-let" ,ocaml-ppx-let)
       ("ppx-pipebang" ,ocaml-ppx-pipebang)
       ("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("ppx-sexp-message" ,ocaml-ppx-sexp-message)
       ("ppx-sexp-value" ,ocaml-ppx-sexp-value)
       ("ppx-typerep-conv" ,ocaml-ppx-typerep-conv)
       ("ppx-variants-conv" ,ocaml-ppx-variants-conv)))
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/ppx_jane/")
    (synopsis "Standard Jane Street ppx rewriters")
    (description "Ppx_jane is a ppx_driver including all standard ppx rewriters.")
    (license license:asl2.0)))

(define-public ocaml-core-kernel
  (package
    (name "ocaml-core-kernel")
    (version "113.33.03")
    (source (janestreet-origin "core_kernel" version
               "0fl23jrwivixawhxinbwaw9cabqnzn7fini7dxpxjjvkxdc8ip5y"))
    (native-inputs
     `(("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("bin_prot" ,ocaml-bin-prot)
       ("ppx-assert" ,ocaml-ppx-assert)
       ("ppx-bench" ,ocaml-ppx-bench)
       ("ppx-driver" ,ocaml-ppx-driver)
       ("ppx-expect" ,ocaml-ppx-expect)
       ("ppx-inline-test" ,ocaml-ppx-inline-test)
       ("typerep" ,ocaml-typerep)
       ("sexplib" ,ocaml-sexplib)
       ("variantslib" ,ocaml-variantslib)
       ("result" ,ocaml-result)
       ("fieldslib" ,ocaml-fieldslib)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/core_kernel/")
    (synopsis "Portable standard library for OCaml")
    (description "Core is an alternative to the OCaml standard library.

Core_kernel is the system-independent part of Core. It is aimed for cases when
the full Core is not available, such as in Javascript.")
    (license license:asl2.0)))

(define-public ocaml-async-kernel
  (package
    (name "ocaml-async-kernel")
    (version "113.33.03")
    (source (janestreet-origin "async_kernel" version
              "04bjsaa23j831r09r38x6xx9nhryvp0z5ihickvhxqa4fb2snyvd"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("core-kernel" ,ocaml-core-kernel)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_kernel/")
    (synopsis "Monadic concurrency library")
    (description "Monadic concurrency library.")
    (license license:asl2.0)))

(define-public ocaml-async-rpc-kernel
  (package
    (name "ocaml-async-rpc-kernel")
    (version "113.33.03")
    (source (janestreet-origin "async_rpc_kernel" version
             "0y97h9pkb00v7jpf87m8cbb0ffkclj9g26ph6sq97q8dpisnkjwh"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-kernel" ,ocaml-async-kernel)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_rpc_kernel/")
    (synopsis "Platform-independent core of Async RPC library")
    (description "Platform-independent core of Async RPC library.")
    (license license:asl2.0)))

(define-public ocaml-core
  (package
    (name "ocaml-core")
    (version "113.33.03")
    (source (janestreet-origin "core" version
              "1znll157qg56g9d3247fjibv1hxv3r9wxgr4nhy19j2vzdh6a268"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("core-kernel" ,ocaml-core-kernel)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/core/")
    (synopsis "Alternative to OCaml's standard library")
    (description "The Core suite of libraries is an alternative to OCaml's
standard library that was developed by Jane Street.")
    (license license:asl2.0)))

(define-public ocaml-async-unix
  (package
    (name "ocaml-async-unix")
    (version "113.33.03")
    (source (janestreet-origin "async_unix" version
              "1fwl0lfrizllcfjk8hk8m7lsz9ha2jg6qgk4gssfyz377qvpcq4h"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-kernel" ,ocaml-async-kernel)
       ("core" ,ocaml-core)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_unix")
    (synopsis "Monadic concurrency library")
    (description "Monadic concurrency library.")
    (license license:asl2.0)))

(define-public ocaml-async-extra
  (package
    (name "ocaml-async-extra")
    (version "113.33.03")
    (source (janestreet-origin "async_extra" version
              "1si8jgiq5xh5sl9f2b7f9p17p7zx5h1pg557x2cxywi2x7pxqg4f"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-rpc-kernel" ,ocaml-async-rpc-kernel)
       ("async-unix" ,ocaml-async-unix)
       ("core" ,ocaml-core)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async_extra")
    (synopsis "Monadic concurrency library")
    (description "Monadic concurrency library.")
    (license license:asl2.0)))

(define-public ocaml-async
  (package
    (name "ocaml-async")
    (version "113.33.03")
    (source (janestreet-origin "async" version
              "0210fyhcs12kpmmd26015bgivkfd2wqkyn3c5wd7688d0f872y25"))
    (native-inputs
     `(("oasis" ,ocaml-oasis)
       ("js-build-tools" ,ocaml-js-build-tools)
       ("ppx-jane" ,ocaml-ppx-jane)
       ("opam" ,opam)))
    (propagated-inputs
     `(("async-extra" ,ocaml-async-extra)))
    (build-system ocaml-build-system)
    (arguments janestreet-arguments)
    (home-page "https://github.com/janestreet/async")
    (synopsis "Monadic concurrency library")
    (description "Monadic concurrency library.")
    (license license:asl2.0)))

(define-public ocaml-ocplib-endian
  (package
    (name "ocaml-ocplib-endian")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/OCamlPro/ocplib-endian/"
                                  "archive/" version ".tar.gz"))
              (sha256
               (base32
                "0hwj09rnzjs0m0kazz5h2mgs6p95j0zlga8cda5srnzqmzhniwkn"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs `(("cppo" ,ocaml-cppo)))
    (home-page "https://github.com/OCamlPro/ocplib-endian")
    (synopsis "Optimised functions to read and write int16/32/64 from strings
and bigarrays")
    (description "Optimised functions to read and write int16/32/64 from strings
and bigarrays, based on new primitives added in version 4.01. It works on
strings, bytes and bigstring (Bigarrys of chars), and provides submodules for
big- and little-endian, with their unsafe counter-parts.")
    (license license:lgpl2.1)))

(define-public ocaml-cstruct
  (package
    (name "ocaml-cstruct")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-cstruct/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "15qpdc8421shq4pprdas9jznpva45229wkfqbwcxw9khaiiz7949"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:configure-flags
       (list "--enable-lwt" "--enable-async")
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'link-stubs
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                    (lib (string-append out "/lib/ocaml/site-lib/cstruct")))
               (mkdir-p stubs)
               (symlink (string-append lib "/dllcstruct_stubs.so")
                        (string-append stubs "/dllcstruct_stubs.so"))))))))
    (native-inputs
     `(("ounit" ,ocaml-ounit)
       ("ppx-tools" ,ocaml-ppx-tools)
       ("camlp4" ,camlp4)))
    (propagated-inputs
     `(("ocplib-endian" ,ocaml-ocplib-endian)
       ("lwt" ,ocaml-lwt)
       ("async" ,ocaml-async)
       ("sexplib" ,ocaml-sexplib)))
    (home-page "https://github.com/mirage/ocaml-cstruct")
    (synopsis "Access C structures via a camlp4 extension")
    (description "Cstruct is a library and syntax extension to make it easier
to access C-like structures directly from OCaml.  It supports both reading and
writing to these structures, and they are accessed via the Bigarray module.")
    (license license:isc)))

(define-public ocaml-hex
  (package
    (name "ocaml-hex")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-hex/"
                                  "archive/" version ".tar.gz"))
              (sha256
               (base32
                "0s63g0b8gfv2xm6fv6xg7bva8h76b5pcjb0zw3f8cygs0lq9072v"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (propagated-inputs `(("cstruct" ,ocaml-cstruct)))
    (home-page "https://github.com/mirage/ocaml-hex/")
    (synopsis "Minimal library providing hexadecimal converters")
    (description "Minimal library providing hexadecimal converters.")
    (license license:isc)))

(define-public ocaml-ezjsonm
  (package
    (name "ocaml-ezjsonm")
    (version "0.4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ezjsonm/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kag0z2xlk4rw73a240dmkxh9rj6psxxcxkm7d7z0rrj6hzjajgq"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("alcotest" ,ocaml-alcotest)))
    (propagated-inputs
     `(("hex" ,ocaml-hex)
       ("jsonm" ,ocaml-jsonm)
       ("lwt" ,ocaml-lwt)
       ("sexplib" ,ocaml-sexplib)))
    (arguments
     `(#:configure-flags (list "--enable-lwt")
       ;; dllcstruct_stubs.so: cannot open shared object file: No such file
       ;; or directory. May be fixed?
       #:tests? #f))
    (home-page "https://github.com/mirage/ezjsonm/")
    (synopsis "An easy interface on top of the Jsonm library")
    (description "This version provides more convenient (but far less flexible)
input and output functions that go to and from [string] values.  This avoids
the need to write signal code, which is useful for quick scripts that
manipulate JSON.")
    (license license:isc)))

(define-public ocaml-uri
  (package
    (name "ocaml-uri")
    (version "1.9.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mirage/ocaml-uri/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "02bzrag79prx261rxf9mlak749pwf4flpfl8p012x1xznv9m0clc"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (native-inputs
     `(("ounit" ,ocaml-ounit)))
    (propagated-inputs
     `(("ppx-sexp-conv" ,ocaml-ppx-sexp-conv)
       ("re" ,ocaml-re)
       ("ppx-deriving" ,ocaml-ppx-deriving)
       ("sexplib" ,ocaml-sexplib)
       ("stringext" ,ocaml-stringext)))
    (home-page "https://github.com/mirage/ocaml-uri")
    (synopsis "RFC3986 URI/URL parsing library")
    (description "RFC3986 URI/URL parsing library.")
    (license license:isc)))

(define-public ocaml-easy-format
  (package
    (name "ocaml-easy-format")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mjambon/easy-format/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1zcz682y9figa84k7lgdjcab5qbzk3yy14ygfqp2dhhrvjygm252"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:tests? #f))
    (home-page "https://github.com/mjambon/easy-format")
    (synopsis "High-level and functional interface to the Format module of the
OCaml standard library")
    (description "High-level and functional interface to the Format module of
the OCaml standard library.")
    (license license:bsd-3)))

(define-public ocaml-optcomp
  (package
    (name "ocaml-optcomp")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/diml/optcomp/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0hhhb2gisah1h22zlg5iszbgqxdd7x85cwd57bd4mfkx9l7dh8jh"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:use-make? #t
	   #:make-flags
       (list (string-append "BUILDFLAGS=\"-cflags -I,"
                            (assoc-ref %build-inputs "camlp4")
                            "/lib/ocaml/site-lib/camlp4/Camlp4Parsers\""))))
    (native-inputs `(("camlp4" ,camlp4)))
    (propagated-inputs `(("camlp4" ,camlp4)))
    (home-page "https://github.com/diml/optcomp")
    (synopsis "Optional compilation with cpp-like directives")
    (description "Optional compilation with cpp-like directives.")
    (license license:bsd-3)))

(define-public ocaml-piqilib
  (package
    (name "ocaml-piqilib")
    (version "0.6.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alavrik/piqi/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1whqr2bb3gds2zmrzqnv8vqka9928w4lx6mi6g244kmbwb2h8d8l"))
              (file-name (string-append name "-" version ".tar.gz"))
              (patches (search-patches "ocaml-piqilib-fix-makefile.patch"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (substitute* "make/OCamlMakefile"
                 (("/bin/sh") (which "bash")))
               (zero? (system* "./configure" "--prefix" out "--ocaml-libdir"
                               (string-append out "/lib/ocaml/site-lib"))))))
       (add-after 'build 'build-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* "make" "ocaml")))) 
       (add-after 'install 'install-ocaml
         (lambda* (#:key outputs #:allow-other-keys)
           (zero? (system* "make" "ocaml-install"))))
       (add-after 'install-ocaml 'link-stubs
         (lambda* (#:key outputs #:allow-other-keys)
           (let* ((out (assoc-ref outputs "out"))
                  (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                  (lib (string-append out "/lib/ocaml/site-lib/piqilib")))
             (mkdir-p stubs)
             (symlink (string-append lib "/dllpiqilib_stubs.so")
                      (string-append stubs "/dllpiqilib_stubs.so"))))))))
    (native-inputs
     `(("which" ,which)
       ("camlp4" ,camlp4)))
    (propagated-inputs
     `(("xmlm" ,ocaml-xmlm)
       ("ulex" ,ocaml-ulex)
       ("optcomp" ,ocaml-optcomp)
       ("easy-format" ,ocaml-easy-format)
       ("base64" ,ocaml-base64)))
    (home-page "http://piqi.org")
    (synopsis "Data serialization and conversion library")
    (description "Common library used by piqi command-line tool and piqi-ocaml.")
    (license license:asl2.0)))

(define-public ocaml-uuidm
  (package
    (name "ocaml-uuidm")
    (version "0.9.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://erratique.ch/software/uuidm/"
                                  "releases/uuidm-" version ".tbz"))
              (sha256
               (base32
                "0hz4fdx0x16k0pw9995vkz5d1hmzz6b16wck9li399rcbfnv5jlc"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:tests? #f
       #:build-flags
       (list "build" "--with-cmdliner" "true")
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("opam" ,opam)))
    (propagated-inputs
     `(("cmdliner" ,ocaml-cmdliner)
       ("topkg" ,ocaml-topkg)))
    (home-page "http://erratique.ch/software/uuidm")
    (synopsis "Universally unique identifiers (UUIDs) for OCaml")
    (description "Uuidm is an OCaml module implementing 128 bits universally
unique identifiers version 3, 5 (named based with MD5, SHA-1 hashing) and 4
(random based) according to RFC 4122.")
    (license license:isc)))

(define-public ocamlgraph
  (package
    (name "ocamlgraph")
    (version "1.8.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://ocamlgraph.lri.fr/download/"
                                  "ocamlgraph-" version ".tar.gz"))
              (sha256
               (base32
                "1845r537swjil2fcj7lgbibc2zybfwqqasrd2s7bncajs83cl1nz"))
              (patches (search-patches "ocamlgraph-honor-source-date-epoch.patch"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:install-target "install-findlib"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-/bin/sh
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("-/bin/sh") (string-append "-" (assoc-ref inputs "bash")
                                            "/bin/sh"))))))))
    (inputs `(("lablgtk" ,lablgtk)))
    (home-page "http://ocamlgraph.lri.fr/")
    (synopsis "A generic graph library for OCaml")
    (description "A generic graph library for OCaml.")
    (license license:lgpl2.1)))

(define-public ocaml-piqi
  (package
    (name "ocaml-piqi")
    (version "0.7.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alavrik/piqi-ocaml/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0ngz6y8i98i5v2ma8nk6mc83pdsmf2z0ks7m3xi6clfg3zqbddrv"))))
    (build-system ocaml-build-system)
    (arguments
     `(#:make-flags
       (list (string-append "DESTDIR=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-/bin/sh
           (lambda _
             (substitute* "make/OCamlMakefile"
               (("/bin/sh") (which "sh")))
             #t)))))
    (native-inputs
     `(("which" ,which)
       ("protobuf" ,protobuf))) ; for tests
    (propagated-inputs
     `(("piqilib" ,ocaml-piqilib)))
    (home-page "https://github.com/alavrik/piqi-ocaml")
    (synopsis "Protocol serialization system for OCaml")
    (description "Piqi is a multi-format data serialization system for OCaml.
It provides a uniform interface for serializing OCaml data structures to JSON,
XML and Protocol Buffers formats.")
    (license license:asl2.0)))

(define-public bap
  (package
    (name "bap")
    (version "1.1.0")
    (home-page "https://github.com/BinaryAnalysisPlatform/bap")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1ms95m4j1qrmy7zqmsn2izh7gq68lnmssl7chyhk977kd3sxj66m"))
              (file-name (string-append name "-" version ".tar.gz"))))
   (build-system ocaml-build-system)
   (native-inputs
    `(("oasis" ,ocaml-oasis)
      ("clang" ,clang)
      ("ounit" ,ocaml-ounit)))
   (propagated-inputs
    `(("core-kernel" ,ocaml-core-kernel)
      ("ppx-driver" ,ocaml-ppx-driver)
      ("uri" ,ocaml-uri)
      ("llvm" ,llvm)
      ("gmp" ,gmp)
      ("clang-runtime" ,clang-runtime)
      ("fileutils" ,ocaml-fileutils)
      ("cmdliner" ,ocaml-cmdliner)
      ("zarith" ,ocaml-zarith)
      ("uuidm" ,ocaml-uuidm)
      ("camlzip" ,camlzip)
      ("frontc" ,ocaml-frontc)
      ("ezjsonm" ,ocaml-ezjsonm)
      ("ocurl" ,ocaml-ocurl)
      ("piqi" ,ocaml-piqi)
      ("ocamlgraph" ,ocamlgraph)
      ("bitstring" ,ocaml-bitstring)
      ("ppx-jane" ,ocaml-ppx-jane)
      ("re" ,ocaml-re)))
   (inputs `(("llvm" ,llvm)))
   (arguments
    `(#:use-make? #t
      #:phases
      (modify-phases %standard-phases
        (replace 'configure
          (lambda* (#:key outputs #:allow-other-keys)
            (zero? (system* "./configure" "--prefix"
                            (assoc-ref outputs "out")
                            "--libdir"
                            (string-append
                              (assoc-ref outputs "out")
                              "/lib/ocaml/site-lib")
                            "--with-llvm-version=3.8"
                            "--with-llvm-config=llvm-config"
                            "--enable-everything"))))
        (add-before 'install 'fix-ocamlpath
          (lambda* (#:key outputs #:allow-other-keys)
            (setenv "OCAMLPATH"
                    (string-append
                      (getenv "OCAMLPATH") ":"
                      (assoc-ref outputs "out")
                      "/lib/ocaml/site-lib"))
            (setenv "PATH"
                    (string-append (getenv "PATH") ":"
                                   (assoc-ref outputs "out") "/bin"))))
        (add-after 'install 'link-stubs
          (lambda* (#:key outputs #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (stubs (string-append out "/lib/ocaml/site-lib/stubslibs"))
                   (lib (string-append out "/lib/ocaml/site-lib/bap-plugin-llvm")))
              (mkdir-p stubs)
              (symlink (string-append lib "/dllllvm_plugin_stubs.so")
                       (string-append stubs "/dllllvm_plugin_stubs.so"))))))))
   (synopsis "Binary Analysis Platform")
   (description "Binary Analysis Platform is a framework for writing program
analysis tools, that target binary files.  The framework consists of a plethora
of libraries, plugins, and frontends.  The libraries provide code reusability,
the plugins facilitate extensibility, and the frontends serve as entry points.")
   (license license:expat)))

(define-public ocaml-camomile
  (package
    (name "ocaml-camomile")
    (version "0.8.5")
    (home-page "https://github.com/yoriyuki/Camomile")
    (source (origin
              (method url-fetch)
              (uri (string-append home-page "/releases/download/rel-" version
                                  "/camomile-" version ".tar.bz2"))
              (sha256
               (base32
                "003ikpvpaliy5hblhckfmln34zqz0mk3y2m1fqvbjngh3h2np045"))))
    (build-system ocaml-build-system)
    (native-inputs `(("camlp4" ,camlp4)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-bin/sh
           (lambda* (#:key #:allow-other-keys)
             (substitute* "configure"
               (("CONFIG_SHELL-/bin/sh")
                (string-append "CONFIG_SHELL-" (which "bash")))))))))
    (synopsis "Comprehensive Unicode library")
    (description "Camomile is a Unicode library for OCaml.  Camomile provides
Unicode character type, UTF-8, UTF-16, UTF-32 strings, conversion to/from about
200 encodings, collation and locale-sensitive case mappings, and more.  The
library is currently designed for Unicode Standard 3.2.")
    (license license:lgpl2.0+))) ; with an exception

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

(define-public compcert
  (package
    (name "compcert")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://compcert.inria.fr/release/compcert-"
                                  version ".tgz"))
              (sha256
               (base32
                "03fxf01acvy0akzb1czk33jsfmv2rka0m0jc1a2gmzs9i192rr7m"))))
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
       ("coq" ,coq)))
    (inputs
     `(("menhir" ,ocaml-menhir)))
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
       ("coq" ,coq)))
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
       ("coq" ,coq)
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
       ("coq" ,coq)))
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

;; TODO: coquelicot

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
       ("coq" ,coq)))
    (propagated-inputs
     `(("flocq" ,coq-flocq)
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
