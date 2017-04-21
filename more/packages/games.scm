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
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tbb)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages zip))

(define-public lugaru
  (package
    (name "lugaru")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/osslugaru/lugaru/downloads/"
                                  name "-" version ".tar.xz"))
              (sha256
               (base32
                "15zgcshy22q51rm72zi6y9z7qlgnz5iw3gczjdlir4bqmxy4gspk"))))
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

(define-public ogre3d
  (package
    (name "ogre3d")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://bitbucket.org/sinbad/ogre/get/v"
                     (string-map (lambda (x) (if (char=? x #\.) #\- x)) version)
                     ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0p8gyn293qn3iyiy1smfmjd9zpnjb8h2zgvff8778fwh0ylbmlpa"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)))
    (inputs
     `(("freetype" ,freetype)
       ("boost" ,boost)
       ("sdl2" ,sdl2)
       ("cppunit" ,cppunit)
       ("freeimage" ,freeimage)
       ("glu" ,glu)
       ("libxt" ,libxt)
       ("libxaw" ,libxaw)
       ("libxxf86vm" ,libxxf86vm)
       ("libxrandr" ,libxrandr)
       ("mesa" ,mesa)
       ("tbb" ,tbb)
       ("tinyxml" ,tinyxml)
       ("zziplib" ,zziplib)))
    (arguments
     `(#:tests? #f
       #:configure-flags
       (list (string-append "-DFREETYPE_FT2BUILD_INCLUDE_DIR="
               (assoc-ref %build-inputs "freetype")
               "/include"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-doc
           (lambda* _
             (zero? (system* "make" "OgreDoc")))))))
    (home-page "http://www.ogre3d.org")
    (synopsis "3D graphics engine")
    (description "3D graphics engine")
    (license license:expat)))

(define-public cegui
  (package
    (name "cegui")
    (version "0.8.7")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://prdownloads.sourceforge.net/crayzedsgui/cegui-"
                     version ".tar.bz2"))
              (sha256
               (base32
                "067562s71kfsnbp2zb2bmq8zj3jk96g5a4rcc5qc3n8nfyayhldk"))))
    (build-system cmake-build-system)
    (arguments
     `(#:tests? #f))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (inputs
     `(("pcre" ,pcre)
       ("gl" ,mesa)
       ("freetype" ,freetype)
       ("fribidi" ,fribidi)
       ("glew" ,glew)
       ("sdl2" ,sdl2)
       ("irrlicht" ,irrlicht)
       ("ogre" ,ogre3d)
       ("epoxy" ,libepoxy)
       ("expat" ,expat)
       ("libxml2" ,libxml2)
       ("freeimage" ,freeimage)
       ("python" ,python)
       ("lua" ,lua-5.1)
       ("gtk" ,gtk+-2)
       ("boost" ,boost)
       ("minizip" ,minizip)
       ("tinyxml" ,tinyxml)))
    (home-page "http://cegui.org.uk/")
    (synopsis "Crazy Eddie's GUI system")
    (description "Crazy Eddie's GUI System is a free library providing windowing
and widgets for graphics APIs / engines where such functionality is not natively
available, or severely lacking.  The library is object-oriented, written in C++,
cross-platform, and targeted at game and application developers.  Additionally,
it offers a WYSIWYG editor for creating layouts and imagesets.")
    (license license:expat)))
