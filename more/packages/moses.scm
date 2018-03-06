;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages moses)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu))

(define-public cmph
  (package
    (name "cmph")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/cmph/cmph/cmph-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0xms1hii88wlihrr4766qmk26kvzhzcw3h6a489gp89xzxsrlv5d"))))
    (build-system gnu-build-system)
    (home-page "http://cmph.sourceforge.net/")
    (synopsis "C Minimal Perfect Hashing Library")
    (description "Perfect hash functions map a static set of n keys into a set
of m integer numbers without collisions, where m is greater than or equal to
n.  If m is equal to n, the function is called minimal.

Minimal perfect hash functions are widely used for memory efficient storage
and fast retrieval of items from static sets, such as words in natural
languages, reserved words in programming languages or interactive systems,
universal resource locations (URLs) in Web search engines, or item sets in
data mining techniques.  Therefore, there are applications for minimal perfect
hash functions in information retrieval systems, database systems, language
translation systems, electronic commerce systems, compilers, operating
systems, among others.")
    (license (list license:lgpl2.1+ license:mpl1.1))))

(define-public moses
  (package
    (name "moses")
    (version "4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/moses-smt/mosesdecoder/"
                                  "archive/RELEASE-" version ".tar.gz"))
              (sha256
               (base32
                "13wvxizbvzrklswf1s8751r0vqd71xfn55biy76ifni2pg6pcwrm"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:make-flags
       `(,(string-append "--with-boost=" (assoc-ref %build-inputs "boost"))
         ,(string-append "--with-cmph=" (assoc-ref %build-inputs "cmph"))
         "--with-mm" "--with-probing-pt" "--no-xmlrpc-c" "-q" "link=shared"
         ,(string-append "--prefix=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'patch-bin-sh
           (lambda _
             (substitute* "jam-files/engine/execunix.c"
               (("\"/bin/sh\"") (string-append "\"" (which "sh") "\"")))))
         (replace 'build
           (lambda* (#:key make-flags inputs #:allow-other-keys)
             (setenv "JAMSHELL" (string-append (which "sh") " -c"))
             (apply invoke "./bjam" make-flags)))
         (replace 'install
           (lambda* (#:key make-flags inputs #:allow-other-keys)
             (apply invoke "./bjam" "install" make-flags))))))
    (inputs
     `(("boost" ,boost)
       ("cmph" ,cmph)
       ("zlib" ,zlib)))
    (home-page "http://www.statmt.org/moses")
    (synopsis "Statistical machine translation")
    (description "Moses is an implementation of the statistical (or data-driven)
approach to machine translation (MT).  In statistical machine translation
(SMT), translation systems are trained on large quantities of parallel data
(from which the systems learn how to translate small segments), as well as
even larger quantities of monolingual data (from which the systems learn what
the target language should look like).  Parallel data is a collection of
sentences in two different languages, which is sentence-aligned, in that
each sentence in one language is matched with its corresponding translated
sentence in the other language.")
    (license license:asl2.0)))
