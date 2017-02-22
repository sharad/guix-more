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

(define-module (gnu packages smt)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define-public z3-solver
  (package
    (name "z3-solver")
    (version "4.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/Z3Prover/z3/archive/z3-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "032a5lvji2liwmc25jv52bdrhimqflvqbpg77ccaq1jykhiivbmf"))
              (file-name (string-append name "-" version ".tar.gz"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'generate-make
           (lambda _
             (system* "python" "scripts/mk_make.py")
             (chdir "build"))))
       #:test-target "test"
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (native-inputs
     `(("python" ,python-2)))
    (home-page "https://github.com/Z3Prover/z3")
    (synopsis "SMT solver library")
    (description "Z3 is a theorem prover from Microsoft Research.")
    (license license:expat)))
