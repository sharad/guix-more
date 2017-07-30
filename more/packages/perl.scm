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

(define-module (more packages perl)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages perl))

(define-public perl-glib
  (package
    (name "perl-glib")
    (version "1.324")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/sourceforge"
                                  "/gtk2-perl/Glib-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0klvmn8czm1bq0xv0rx474ln3zabjjhyy1yqhfvw25wzbd5j26kh"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("perl" ,perl)
       ("glib" ,glib)))
    (home-page "http://gtk2-perl.sourceforge.net")
    (synopsis "Perl wrappers for glib 2.x, including GObject")
    (description
      "Perl-glib is a collection of perl wrappers for glib 2.x, including GObject.")
    (license license:lgpl2.1+)))

(define-public perl-cairo
  (package
    (name "perl-cairo")
    (version "1.106")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/sourceforge"
                                  "/gtk2-perl/Cairo-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i25kks408c54k2zxskvg54l5k3qadzm8n72ffga9jy7ic0h6j76"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("perl" ,perl)
       ("cairo" ,cairo)))
    (home-page "http://gtk2-perl.sourceforge.net")
    (synopsis "Perl wrappers for cairo")
    (description
      "Perl-cairo is a collection of perl wrappers for cairo.")
    (license license:lgpl2.1+)))

(define-public perl-pango
  (package
    (name "perl-pango")
    (version "1.227")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/sourceforge"
                                  "/gtk2-perl/Pango-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wdcidnfnb6nm79fzfs39ivawj3x8m98a147fmcxgv1zvwia9c1l"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (propagated-inputs
     `(("perl-glib" ,perl-glib)
       ("perl-cairo" ,perl-cairo)
       ("pango" ,pango)))
    (home-page "http://gtk2-perl.sourceforge.net")
    (synopsis "Perl wrappers for pango")
    (description
      "Perl-pango is a collection of perl wrappers for pango.")
    (license license:lgpl2.1+)))

(define-public perl-gtk2
  (package
    (name "perl-gtk2")
    (version "1.2498")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.sourceforge.net/sourceforge"
                                  "/gtk2-perl/Gtk2-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0gs6lr4clz86838s3klrl37lf48j24zv0p37jlsvsnr927whpq3j"))))
    (build-system perl-build-system)
    (native-inputs
     `(("perl-extutils-depends" ,perl-extutils-depends)
       ("perl-extutils-pkgconfig" ,perl-extutils-pkgconfig)))
    (inputs
     `(("perl-pango" ,perl-pango)
       ("perl-glib" ,perl-glib)
       ("gtk" ,gtk+-2)))
    (home-page "http://gtk2-perl.sourceforge.net")
    (synopsis "Perl wrappers for GTK+ 2.x")
    (description
      "Perl-gtk2 is a collection of perl wrappers for GTK+ 2.x.")
    (license license:lgpl2.1+)))

;perl-gdgraph
;perl-mp3-info
;perl-net-snmp
;perl-sort-naturally
