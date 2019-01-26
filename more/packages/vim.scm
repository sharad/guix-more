;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Kei Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages vim)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (more packages python)
  #:use-module ((guix licenses) #:prefix license:))

(define-public neovim-coquille
  (package
    (name "neovim-coquille")
    (version "0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://framagit.org/tyreunom/coquille.git")
                     (commit "6a7833312fe6156df568815ff1b4bae9241fd4a3")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0gmlb9g29cky8acr0zq9fy7r7lp9jvp6sik3srmggrsbmf2wzng1"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (nvim (string-append out "/share/nvim/site/coquille")))
               (delete-file ".travis.yml")
               (delete-file ".gitignore")
               (for-each
                 (lambda (file)
                   (install-file file (string-append nvim "/" (dirname file))))
                 (find-files "." ".")))
             #t)))))
    (propagated-inputs
     `(("python-neovim" ,python-neovim)))
    (home-page "https://framagit.org/tyreunom/coquille")
    (synopsis "")
    (description "")
    (license license:expat)))
