;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages tex)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix utils)
  #:use-module (guix build-system texlive)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages tex))

;; (define-public texlive-generic-babel-french
;;   (package
;;     (name "texlive-generic-babel-french")
;;     (version (number->string 66594))    ;70951
;;     (source (origin
;;               (method svn-fetch)
;;               (uri (texlive-ref "generic" "babel-french"))
;;               (file-name (string-append name "-" version "-checkout"))
;;               (sha256
;;                (base32
;;                 "1rymnl34w4k5izw56g48s90wki2qyq8nz1aj6ijax2bnznm55ngl"))))
;;     (build-system texlive-build-system)
;;     (arguments '(#:tex-directory "generic/babel-french"))
;;     (home-page "https://www.ctan.org/pkg/babel-french")
;;     (synopsis "Babel support for French")
;;     (description
;;      "This package provides the language definition file for support of
;; French in @code{babel}.")
;;     (license license:lppl1.3+)))

;; (define-public texlive-latex-sauerj
;;   (package
;;     (name "texlive-latex-sauerj")
;;     (version (number->string 66594))    ;70951
;;     (source (origin
;;               (method svn-fetch)
;;               (uri (texlive-ref "latex" "sauerj"))
;;               (file-name (string-append name "-" version "-checkout"))
;;               (sha256
;;                (base32
;;                 "1rn3l0klhx1yhw37m20jj5aiss0qd7n30ivhss6x9ivsli938dk0"))))
;;     (build-system texlive-build-system)
;;     (arguments '(#:tex-directory "latex/sauerj"))
;;     (home-page "")
;;     (synopsis "")
;;     (description "sauerj, parcolumns")
;;     (license license:lppl1.3+)))

(define-public texlive-biber-old
  (package
    (inherit texlive-biber)
    (name (package-name texlive-biber))
    (version "2.11")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/plk/biber/")
                    (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              ;; TODO: Patch awaiting inclusion upstream (see:
              ;; https://github.com/plk/biber/issues/239).
              (patches (search-patches "biber-fix-encoding-write.patch"))
              (sha256
               (base32
                "0qgkc1k9n36yfmndwz879pak6mjphld0p85lzn9g2ng0vhxsifzz"))))))
