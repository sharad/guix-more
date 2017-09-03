;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2014 John Darrington <jmd@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2016 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2015 Ludovic Courtès <ludo@gnu.org>
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

(define-module (more packages flashing-tools)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages libusb)
  #:use-module (gnu packages qt))

(define-public heimdall
  (package
    (name "heimdall")
    (version "1.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/Benjamin-Dobell/Heimdall"
                                  "/archive/v" version ".tar.gz"))
              (sha256
               (base32
                "1y7gwg3lipyp2zcysm2vid1qg5nwin9bxbvgzs28lz2rya4fz6sq"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (copy-file "bin/heimdall" (string-append bin "/heimdall"))
               (copy-file "bin/heimdall-frontend"
                          (string-append bin "/heimdall-frontend"))))))))
    (inputs
     `(("libusb" ,libusb)
       ("qt" ,qtbase)
       ("zlib" ,zlib)))
    (home-page "http://glassechidna.com.au/heimdall/")
    (synopsis "Flash firmware onto Samsung mobile devices")
    (description "Heimdall is a tool suite used to flash firmware (aka ROMs)
onto Samsung mobile devices.  Heimdall connects to a mobile device over USB and
interacts with low-level software running on the device, known as Loke.  Loke
and Heimdall communicate via the custom Samsung-developed protocol typically
referred to as the 'Odin 3 protocol'.")
    (license license:expat)))
