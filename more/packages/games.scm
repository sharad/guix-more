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

(define-module (more packages games)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages xiph))

(define-public lugaru
  (package
    (name "lugaru")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/osslugaru/lugaru/downloads/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "0b5q73kwbiqin00iizjwzzwijrl45drq0da4cxn94mqqc7ygc02y"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DSYSTEM_INSTALL=ON")
       #:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("sdl2" ,sdl2)
       ("glu" ,glu)
       ("libjpeg" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("openal" ,openal)
       ("vorbis" ,libvorbis)
       ("zlib" ,zlib)))
    (home-page "https://osslugaru.gitlab.io")
    (synopsis "Cross-platform third-person action game")
    (description "The main character, Turner, is an anthropomorphic rebel bunny
rabbit with impressive combat skills.  In his quest to find those responsible
for slaughtering his village, he uncovers a far-reaching conspiracy involving
the corrupt leaders of the rabbit republic and the starving wolves from a
nearby den.  Turner takes it upon himself to fight against their plot and save
his fellow rabbits from slavery.")
    (license (list license:gpl2+ license:cc-by-sa3.0))))
