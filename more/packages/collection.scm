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

(define-module (more packages collection)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gd)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages perl))

(define-public unrar
  (package
    (name "unrar")
    (version "0.0.1")
    (source (origin
              (method url-fetch)
              (uri "http://deb.debian.org/debian/pool/main/u/unrar-free/unrar-free_0.0.1+cvs20140707.orig.tar.gz")
              (sha256
               (base32
                "18yvsa6k3pc0axl7j089jf5d8hya1zb68s2igm5z3xxbcagrzfih"))))
    (build-system gnu-build-system)
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public gcstar
  (package
    (name "gcstar")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.gna.org/gcstar/gcstar-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gcz88slgm14rlsw84gpka7hpdmrdvpdvxp9qvs45gv1383r0b6s"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p %output)
             (format #t "calling install script\n")
             (zero? (system* "perl" "install" "--verbose"
                            (string-append "--prefix=" (assoc-ref outputs "out")))))))))
    (native-inputs
     `(("perl" ,perl)))
    (propagated-inputs
     `(("perl-gtk2" ,perl-gtk2)
       ("perl-pango" ,perl-pango)
       ("perl-archive-zip" ,perl-archive-zip)
       ("perl-date-calc" ,perl-date-calc)
       ("perl-datetime-format-strptime" ,perl-datetime-format-strptime)
       ("perl-libwww" ,perl-libwww)
       ("perl-switch" ,perl-switch)
       ("perl-gd" ,perl-gd)
       ("perl-http-cookies" ,perl-http-cookies)
       ("perl-http-date" ,perl-http-date)
       ("perl-http-message" ,perl-http-message)
       ("perl-xml-libxml" ,perl-xml-libxml)
       ("perl-xml-simple" ,perl-xml-simple)
       ("perl-xml-parser" ,perl-xml-parser)
       ("perl-glib" ,perl-glib)))
    (home-page "http://www.gcstar.org")
    (synopsis "Collection management")
    (description
      "GCstar is a free open source application for managing your collections.
Detailed information on each item can be automatically retrieved from the
internet and you can store additional data, such as the location or who you've
lent it to.  You may also search and filter your collection by many criteria.")
    (license license:gpl2+)))
