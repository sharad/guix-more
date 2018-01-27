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
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (more packages lua)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (more packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages tbb)
  #:use-module (more packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

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
       ("ogre" ,ogre)
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

(define-public morji
  (package
    (name "morji")
    (version "0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://bardinflor.perso.aquilenet.fr/morji/morji-"
                     version ".tar.gz"))
              (sha256
               (base32
                "1icpqn7ypg4jbbn222zvgdg96x0k1nb9rbcfr5ky86ldhly1flq2"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; Tests don't run in our environment
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "make" "install"
                             (string-append "PREFIX=" (assoc-ref outputs "out")))))))))
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
     `(("boost" ,boost)
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

(define-public anki
  (package
    (name "anki")
    ; the latest stable version requires qt4 webkit which we don't have because
    ; of issues on arm and probably security reasons.
    (version "2.1.0beta25")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://apps.ankiweb.net/downloads/beta/anki-"
                                  version "-source.tgz"))
              (sha256
               (base32
                "1p42b395k3kny5c17na3sw95mya3cw2hg3nxyj3b3mdhwdcy677r"))))
    (build-system gnu-build-system)
    (propagated-inputs
     `(("python" ,python)
       ("python-pyaudio" ,python-pyaudio)
       ("python-pyqt" ,python-pyqt)
       ("python-sip" ,python-sip)
       ("python-decorator" ,python-decorator)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (substitute* "anki/__init__.py"
               (("< 6") "< 5"))
             (substitute* "aqt/qt.py"
               (("from PyQt5.QtWebEngineWidgets import QWebEnginePage") ""))
             (let* ((output (assoc-ref outputs "out"))
                    (bindir (string-append output "/bin"))
                    (libdir (string-append output "/lib/python3.5/site-packages")))
               (for-each
                 (lambda (file)
                   (mkdir-p (dirname (string-append libdir "/" file)))
                   (copy-file file (string-append libdir "/" file)))
                 (append (find-files "anki" ".*\\.py")
                         (find-files "aqt" ".*\\.py")))
               (mkdir-p bindir)
               (with-output-to-file (string-append bindir "/anki")
                 (lambda _
                   (display
                     (string-append
                       "#!" (assoc-ref inputs "python") "/bin/python3\n"
                       "import aqt\n"
                       "aqt.run()\n"))))
               (chmod (string-append bindir "/anki") #o755)))))))
    (home-page "https://apps.ankiweb.net")
    (synopsis "")
    (description "")
    (license license:gpl2)))
