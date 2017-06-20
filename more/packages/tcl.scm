;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Kei Kebreau <kei@openmailbox.org>
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

(define-module (more packages tcl)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages)
  #:use-module (gnu packages image)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages tcl)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (guix licenses))

(define-public tcl-fix
  (package
    (inherit tcl)
    (name "tcl-fix")
    (native-search-paths
      (list (search-path-specification
              (variable "TCLLIBPATH")
              (separator " ")
              (files '("lib")))))))

(define-public tcllib-fix
  (package
    (inherit tcllib)
    (name "tcllib-fix")
    (native-search-paths
      (list (search-path-specification
              (variable "TCLLIBPATH")
              (separator " ")
              (files `("lib" ,(string-append "lib/tcllib" (package-version tcllib)))))))))

(define-public expect-fix
  (package
    (inherit expect)
    (name "expect-fix")
    (inputs
      `(("tcl" ,tcl-fix)))))
