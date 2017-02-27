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

(define-module (more packages google)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages ninja)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public chromium
  (package
    (name "chromium")
    (version "56.0.2924.87")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://commondatastorage.googleapis.com/"
                            "chromium-browser-official/" name "-" version ".tar.xz"))
        (sha256
         (base32
          "1q2kg85pd6lv036w7lsss5mhiiva9rx4f0410sbn9bnazhghib4s"))
        (modules '((guix build utils)))
        (snippet
         '(begin
           (for-each delete-file-recursively
                     (find-files "." "third_party"))))))
    (build-system gnu-build-system)
    (home-page "https://chromium.googlesource.com/chromium/src")
    (synopsis "Google web browser")
    (description "Google web browser.")
    (license "bsd-3")))

(define-public google-gn
  (package
    (inherit chromium)
    (name "google-gn")
    (version "0")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "tools/gn")
             (setenv "CC" (which "gcc"))
             (zero? (system* "python" "bootstrap/bootstrap.py" "-s"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (copy-file "../../out/Release/gn" (string-append bin "/gn"))))))))
    (native-inputs
     `(("python" ,python-2)
       ("ninja" ,ninja)))
    (home-page "https://chromium.googlesource.com/chromium/buildtools.git")
    (synopsis "Google gn")
    (description "Google gn.")
    (license license:bsd-3)))
