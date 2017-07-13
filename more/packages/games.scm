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
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages audio)
  #:use-module (more packages boost)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (more packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tbb)
  #:use-module (more packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

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
       ("boost" ,boost-fix)
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
       ("boost" ,boost-fix)
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

(define-public morji
  (package
    (name "morji")
    (version "0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://bardinflor.perso.aquilenet.fr/morji/morji-"
                     version ".tar.gz"))
              (sha256
               (base32
                "18givlgh10cg0a3gs3747ihhfm4hyj056cr3x7vqhcnrx6vgy06i"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "make" "install"
                             (string-append "PREFIX=" (assoc-ref outputs "out"))))))
         (replace 'check
           (lambda _
             (zero? (system* "tclsh" "test_expect.tcl")))))))
    (propagated-inputs
     `(("ncurses" ,ncurses) ; TODO: this should probably be a propagated-input of tcllib.
       ("sqlite" ,sqlite)
       ("tcl" ,tcl-fix)
       ("tcllib" ,tcllib-fix)))
    (native-inputs
     `(("expect" ,expect-fix)))
    (home-page "https://bardinflor.perso.aquilenet.fr/morji/intro-en")
    (synopsis "Simple flashcard program for the terminal")
    (description "Morji is a simple flashcard program for the terminal.  It
uses a modified version of the SM2 algorithm taking inspiration from mnemosyne
and anki.")
    (license license:isc)))

(define-public cpptest
  (package
    (name "cpptest")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/cpptest/cpptest/cpptest-"
                                  version "/cpptest-" version ".tar.gz"))
              (sha256
               (base32
                "09v070a9dv6zq6hgj4v67i31zsis3s96psrnhlq9g4vhdcaxykwy"))))
    (build-system gnu-build-system)
    (home-page "http://cpptest.sourceforge.net/")
    (synopsis "")
    (description "")
    (license license:lgpl2.1)))

(define-public khanat
  (package
    (name "khanat")
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.khaganat.net/khaganat/khanat-code.git")
                     (commit "90d9f6da6c367312ea856e1b8df67ec9ef1959c7")))
              (sha256
               (base32
                "0wh4k6k4213pm4bbynlsnbvpcmqiliny19v9sffgd011pzywy7cp"))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost-fix)
       ("cpptest" ,cpptest)
       ("curl" ,curl)
       ("giflib" ,giflib)
       ("libfreetype" ,freetype)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libvorbis" ,libvorbis)
       ("libxml2" ,libxml2)
       ("lua" ,lua-5.1)
       ("luabind" ,luabind)
       ("mesa" ,mesa)
       ("openal" ,openal)
       ("openssl" ,openssl)
       ("zlib" ,zlib)))
    (arguments
     `(#:out-of-source? #t
       #:tests? #f
       #:configure-flags (list "-DFINAL_VERSION=ON" "-DWITH_RYZOM_SERVER=OFF"
                               "-DWITH_RYZOM_TOOLS=OFF" "-DWITH_NEL_TESTS=OFF"
                               "-DWITH_RYZOM_CLIENT=ON" "-DWITH_NEL_TOOLS=OFF"
                               "-DWITH_NEL_SAMPLES=OFF" "-DWITH_STATIC=OFF"
                               "-DWITH_STATIC_EXTERNAL=OFF")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "code"))))))
    (home-page "https://khaganat.net/")
    (synopsis "")
    (description "")
    (license license:agpl3)))

(define-public khanat-assets
  (package
    (name "khanat-assets")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://git.khaganat.net/khaganat/khanat-assets"
                                  "/repository/archive.tar.bz2?ref="
                                  "22abb542c6b87637ccf24bfd79ccd762b35f8f19"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1f08s6682v6i909d0gp20pk599685gyhwivqxgs9cxxg6h132azz"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (output (assoc-ref %outputs "out"))
                (vardir (string-append output "/share/khanat")))
           (chdir (string-append source "/database"))
           (for-each
             (lambda (file)
               (mkdir-p (dirname (string-append vardir "/" file)))
               (copy-file file (string-append vardir "/" file)))
             (find-files "." "[^/]*"))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:cc-by-sa3.0)))

(define-public khanat-resources
  (package
    (name "khanat-resources")
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.khaganat.net/khaganat/khanat-ressources.git")
                     (commit "8db7ba5840ced758710696a401ee4a4246eb9b70")))
              (sha256
               (base32
                "010z3wvh0bkdbar0n0rvk0pqj87nnk9srgjh8pjx7mic8p517k8j"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder
       (begin
         (use-modules (guix build utils))
         (let* ((source (assoc-ref %build-inputs "source"))
                (output (assoc-ref %outputs "out"))
                (vardir (string-append output "/share/khanat")))
           (chdir source)
           (for-each
             (lambda (file)
               (mkdir-p (dirname (string-append vardir "/" file)))
               (copy-file file (string-append vardir "/" file)))
             (find-files "." "[^/]*"))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:cc-by-sa3.0)))
