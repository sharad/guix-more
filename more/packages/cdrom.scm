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

(define-module (more packages cdrom)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages acl)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages linux))

(define-public cdrtools
  (package
    (name "cdrtools")
    (version "3.02a07")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "mirror://sourceforge/cdrtools/alpha/cdrtools-"
                    version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "06s9y0z5s5011hgi3fps2540vd3pcv5ih7fl0l5pqgddlxzsdha9"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)))
    (inputs
     `(("acl" ,acl)
       ("libcap" ,libcap)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-binaries
           (lambda* (#:key #:allow-other-keys)
             (setenv "CONFIG_SHELL" (which "sh"))
             (substitute* "RULES/rules.prg"
               (("/bin/rm") (which "rm"))
               (("/bin/ln") (which "ln"))
               (("/bin/mv") (which "mv")))
             (substitute* "cdda2wav/configure"
               (("#! /bin/sh") (string-append "#!" (which "sh"))))
             (substitute* "autoconf/acgeneral.m4"
               (("#! /bin/sh") (string-append "#!" (which "sh"))))
             (substitute* "autoconf/configure"
               (("#! /bin/sh") (string-append "#!" (which "sh"))))))
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "DEFAULTS/Defaults.linux"
               (("/opt/schily") (assoc-ref outputs "out"))
               (("DEFINSGRP=.*") "DEFINSGRP=root"))))
         (add-after 'install 'create-compatibility-links
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin-dir (string-append (assoc-ref outputs "out") "/bin")))
               (symlink (string-append bin-dir "/cdrecord")
                        (string-append bin-dir "/wodim"))
               (symlink (string-append bin-dir "/readcd")
                        (string-append bin-dir "/readom"))
               (symlink (string-append bin-dir "/mkisofs")
                        (string-append bin-dir "/genisoimage"))
               (symlink (string-append bin-dir "/cdda2wav")
                        (string-append bin-dir "/icedax"))))))
       #:make-flags
       (list (string-append "INS_BASE=" (assoc-ref %outputs "out"))
             (string-append "INS_RBASE=" (assoc-ref %outputs "out"))
             "XCC_COM=gcc")
       #:parallel-build? #f
       #:tests? #f))
    (home-page "http://cdrtools.sourceforge.net/private/cdrecord.html")
    (synopsis "CD utilities collection")
    (description
     "CD utilities collection.")
    (license (list license:gpl2+ license:lgpl2.1))))
