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

(define-module (more packages android)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (gnu packages android)
  #:use-module (gnu packages xml))

(define-public fdroidserver-git
  (package
    (inherit fdroidserver)
    (name (package-name fdroidserver))
    (version "git")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.com/fdroid/fdroidserver.git")
                     (commit "ff578dae30dfa83330862a01a6cbd603330b71e0")))
              (file-name (string-append name "-" version "-source"))
              (sha256
               (base32
                "1ar41806wy0y8szrsv963mj9bkf5jlnwlk75xl9x0mykpnj279j7"))))
    (arguments
     (substitute-keyword-arguments (package-arguments fdroidserver)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'build 'compile-catalog
             (lambda _
               (invoke "python" "setup.py" "compile_catalog")
               #t))
           (add-after 'install 'install-scripts
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (site (string-append out "/lib/python3.6/site-packages")))
                 (for-each
                   (lambda (script)
                     (install-file script site))
                   '("gradlew-fdroid" "jenkins-build-all" "jenkins-setup-build-environment"
                     "jenkins-test" "makebuildserver")))
               #t))))))
    (native-inputs
     `(("python-defusedxml" ,python-defusedxml)
       ,@(package-native-inputs fdroidserver)))))
