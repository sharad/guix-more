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

(define-module (more packages smt)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages ocaml)
  #:use-module (gnu packages python)
  #:use-module (more packages python))

(define-public python2-z3-solver
  (package
    (inherit z3)
    (name "python2-z3-solver")
    (build-system python-build-system)
    (propagated-inputs
     `(("z3" ,z3)))
    (arguments
     `(#:python ,python-2
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
             (system* "python" "scripts/mk_make.py")
             (copy-file "build/python/z3/z3core.py" "src/api/python/z3/z3core.py")
             (copy-file "build/python/z3/z3consts.py" "src/api/python/z3/z3consts.py")
             (chdir "src/api/python")
             (substitute* "z3/z3core.py"
               (("_dirs = \\[")
                (string-append "_dirs = ['" (assoc-ref inputs "z3")
                                            "/lib', ")))
             (substitute* "MANIFEST.in"
               ((".*") ""))
             (substitute* "setup.py"
               (("self.execute\\(.*") "\n")
               (("scripts=.*") "\n")))))))))

(define-public python2-claripy
  (package
    (name "python2-claripy")
    (version "7.7.12.16")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "claripy" version))
              (sha256
               (base32
                "1jrnww0lpfi65g83pbdkig951r4jrhri67dn4n67viz897q73736"))
              (modules '((guix build utils)))
              (snippet
               `(substitute* "setup.py"
                  (("angr-only-z3-custom==9002") "z3-solver")))))
    (build-system python-build-system)
    (propagated-inputs
     `(("ana" ,python2-ana)
       ("z3" ,python2-z3-solver)))
    (arguments
     `(#:python ,python-2))
    (home-page "https://github.com/angr/claripy")
    (synopsis "Claripy is a abstracted constraint-solving wrapper")
    (description "Claripy is a abstracted constraint-solving wrapper.")
    (license license:bsd-2)))

(define-public why3
  (package
    (name "why3")
    (version "0.88.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://gforge.inria.fr/frs/download.php/file"
                                  "/37313/why3-" version ".tar.gz"))
              (sha256
               (base32
                "0limdqy9l5bjzwhdalcfdyh0b6laxgiphhvr4bby9p0030agssiy"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("ocaml" ,ocaml)))
    (propagated-inputs
     `(("menhir" ,ocaml-menhir)))
    (arguments
     `(#:tests? #f)) ;; Need Alt-Ergo
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (replace 'check
       ;    (lambda _
       ;      (zero? (system* "make" ;"test-ocaml-extraction"
       ;                      "test-runstrat" "test-coq-tactic.opt"
       ;                      "test-coq-tactic.byte" "test-session.opt"
       ;                      "test-session.byte" "test-api-mlw.opt"
       ;                      "test-api-mlw.byte" "test-api-mlw-tree.opt"
       ;                      "test-mlw-tree.byte" "test-api-logic.opt"
       ;                      "test-api-logic.byte")))))))
    (home-page "http://why3.lri.fr")
    (synopsis "")
    (description "")
    (license license:lgpl2.1)))
