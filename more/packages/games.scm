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
  #:use-module (guix build-system ant)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages audio)
  #:use-module (gnu packages base)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages fribidi)
  #:use-module (gnu packages games)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages java)
  #:use-module (gnu packages lua)
  #:use-module (more packages lua)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages python-xyz)
  #:use-module (more packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages tbb)
  #:use-module (more packages tcl)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages image)
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

;; (define-public khanat
;;   (package
;;     (name "khanat")
;;     (version "3.0")
;;     (source (origin
;;               (method git-fetch)
;;               (uri (git-reference
;;                      (url "https://git.khaganat.net/khaganat/mmorpg_khanat/khanat-opennel-code.git")
;;                      ;(commit "0ca90c49a5e1e6f8865ef15517bd25e388a2db96")))
;;                      (commit "302ee7e20ea93caded5d46637918ba0092f207fd")))
;;               (sha256
;;                (base32
;;                 ;"1rfrk43ya8mx576ycs4rln67kdrci37ryixk7abf1cqjdrz7a883"))))
;;                 "1w0bhspsgf3dg33xdyypc4rm935n6g5d7shid92jf4j6jb0cjahh"))))
;;     (build-system cmake-build-system)
;;     (inputs
;;      `(("boost" ,boost)
;;        ("cpptest" ,cpptest)
;;        ("curl" ,curl)
;;        ("giflib" ,giflib)
;;        ("libfreetype" ,freetype)
;;        ("libjpeg" ,libjpeg)
;;        ("libpng" ,libpng)
;;        ("libvorbis" ,libvorbis)
;;        ("libxml2" ,libxml2)
;;        ("lua" ,lua-5.1)
;;        ("luabind" ,luabind)
;;        ("mesa" ,mesa)
;;        ("openal" ,openal)
;;        ("openssl" ,openssl)
;;        ("zlib" ,zlib)))
;;     (arguments
;;      `(#:out-of-source? #t
;;        #:tests? #f
;;        #:configure-flags (list "-DFINAL_VERSION=ON" "-DWITH_RYZOM_SERVER=OFF"
;;                                "-DWITH_RYZOM_TOOLS=OFF" "-DWITH_NEL_TESTS=OFF"
;;                                "-DWITH_RYZOM_CLIENT=ON" "-DWITH_NEL_TOOLS=OFF"
;;                                "-DWITH_NEL_SAMPLES=OFF" "-DWITH_STATIC=OFF"
;;                                "-DWITH_STATIC_EXTERNAL=OFF")
;;        #:phases
;;        (modify-phases %standard-phases
;;          (add-before 'configure 'chdir
;;            (lambda _
;;              (chdir "code")))
;;          (add-after 'install 'link-khanat
;;            (lambda* (#:key outputs #:allow-other-keys)
;;              (let* ((out (assoc-ref outputs "out"))
;;                     (game (string-append out "/games/khanat_client"))
;;                     (bin (string-append out "/bin/khanat_client")))
;;              (symlink game bin)))))))
;;     (home-page "https://khaganat.net/")
;;     (synopsis "")
;;     (description "")
;;     (license license:agpl3)))

(define-public emojicode
  (package
    (name "emojicode")
    (version "0.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/emojicode/emojicode/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1l3f4i0vh09x8dz5fl1f4mb8wlgmi0j2bhjkfzrnmbgp09hi8wsl"))))
    (build-system cmake-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fixgcc7
           (lambda _
             (display (getenv "CXX_INCLUDE_PATH"))
             (setenv "CPATH" (getenv "C_INCLUDE_PATH"))
             (unsetenv "C_INCLUDE_PATH")
             (setenv "CMAKE_C_IMPLICIT_INCLUDE_DIRECTORIES" (getenv "CPATH"))
             (setenv "CMAKE_CXX_IMPLICIT_INCLUDE_DIRECTORIES" (getenv "CPATH")))))))
    (inputs
     `(("llvm" ,llvm)))
    (native-inputs
     `(("gcc" ,gcc-7)))
    (home-page "http://www.emojicode.org")
    (synopsis "World’s only programming language that’s bursting with emojis")
    (description "Emojicode is the only programming language consisting of
emojis.  Emojicode is a straightforward language to learn, whatever background
you have.")
    (license license:artistic2.0)))

(define-public torque3d
  (package
    (name "torque3d")
    (version "3.10.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/GarageGames/Torque3D/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qxaybdmir90ynfqs7l45di7vh0xa619abq53l9avj6yycihgw8b"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags
       (list "-DTORQUE_APP_NAME=Torque3D")
       #:tests? #f))
    (inputs
     `(("gtk+" ,gtk+)
       ("libxft" ,libxft)
       ("openal" ,openal)))
    (native-inputs
     `(("nasm" ,nasm)
       ("pkg-config" ,pkg-config)))
    (home-page "http://torque3d.org/")
    (synopsis "Game engine")
    (description "")
    (license license:expat)))

(define-public actor-framework
  (package
    (name "actor-framework")
    (version "0.16.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/actor-framework/actor-framework.git")
                     (commit version)))
              (sha256
               (base32
                "0nqw1cv7wxbcn2qwm08qffb6k4n3kgvdiaphks5gjgm305jk4vnx"))))
    (build-system cmake-build-system)
    (arguments
     `(#:configure-flags (list "-DCAF_NO_EXAMPLES=yes")))
    (home-page "http://actor-framework.org/")
    (synopsis "Concurrency library implementing the actor model")
    (description "CAF is a C++11 actor model implementation featuring
lightweight & fast actor implementations, pattern matching for messages,
network transparent messaging, and more.")
    (license (list license:boost1.0 license:bsd-3))))

(define-public mecab
  (package
    (name "mecab")
    (version "0.996")
    (source (origin
              (method url-fetch)
              (uri "https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7cENtOXlicTFaRUE")
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ncwlqxl1hdn1x4v4kr2sn1sbbcgnhdphp0lcvk74nqkhdbk4wz0"))
              (patches
                (search-patches
                  "mecab-variable-param.patch"))))
    (build-system gnu-build-system)
    (search-paths
      (list (search-path-specification
              (variable "MECAB_DICDIR")
              (separator #f)
              (files '("lib/mecab/dic")))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-mecab-dicdir-variable
           (lambda _
             (substitute* "mecabrc.in"
               (("dicdir = .*")
                "dicdir = $MECAB_DICDIR"))
             (substitute* "mecab-config.in"
               (("echo @libdir@/mecab/dic")
                "if [ -z \"$MECAB_DICDIR\" ]; then
  echo @libdir@/mecab/dic
else
  echo \"$MECAB_DICDIR\"
fi"))
             #t)))))
    (inputs
     `(("libiconv" ,libiconv)))
    (home-page "https://taku910.github.io/mecab")
    (synopsis "Morphological analysis engine for texts")
    (description "Mecab is a morphological analysis engine developped as a
collaboration between the Kyoto university and Nippon Telegraph and Telephone
Corporation.  The engine is independent of any language, dictionary or corpus.
")
    (license (list license:gpl2+ license:lgpl2.1+ license:bsd-3))))

(define-public mecab-ipadic
  (package
    (name "mecab-ipadic")
    (version "2.7.0")
    (source (origin
              (method url-fetch)
              (uri "https://drive.google.com/uc?export=download&id=0B4y35FiV1wh7MWVlSDBCSXZMTXM")
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08rmkvj0f0x6jq0axrjw2y5nam0mavv6x77dp9v4al0wi1ym4bxn"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list (string-append "--with-dicdir=" (assoc-ref %outputs "out")
                            "/lib/mecab/dic")
             "--with-charset=utf8")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-mecab-dir
           (lambda* (#:key outputs #:allow-other-keys)
             (setenv "MECAB_DICDIR" (string-append (assoc-ref outputs "out")
                                                   "/lib/mecab/dic"))
             #t)))))
    (native-inputs
     `(("mecab" ,mecab))); for mecab-config
    (home-page "")
    (synopsis "")
    (description "")
    (license (license:non-copyleft "COPYING"))))

(define-public uqm
  (package
    (name "uqm")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/sc2/UQM/0.7/uqm-"
                                  version "-1-source.tgz"))
              (sha256
               (base32
                "1rr8s25qsbqqbp3qsm2ndv11iqaxh72fc6fd8xdf80vb56piaq0k"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Configuration can only happen interactively, so change default
             ;; values instead.
             (substitute* "build/unix/build.config"
               (("/usr/local/games") (assoc-ref outputs "out")))
             (substitute* "build.sh"
               (("/bin/sh") (which "sh")))
             (setenv "CFLAGS"
                     (string-append
                       "-I" (assoc-ref inputs "sdl-image") "/include/SDL"
                       " -I" (assoc-ref inputs "glu") "/include"
                       " -O3 -DNDEBUG"))
             (chmod "build/unix/build_collect" #x755)
             (setenv "MAKE_VERBOSE" "1")
             (invoke (which "sh") "build.sh" "uqm")
             #t))
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             ;; Configuration can only happen interactively, so change default
             ;; values instead.
             (invoke (which "sh") "build/unix/build.sh" "uqm" "install")
             (mkdir-p (string-append (assoc-ref outputs "out")
                                     "/share/uqm/content/packages"))
             (copy-file (assoc-ref inputs "uqm-content")
                        (string-append (assoc-ref outputs "out")
                                       "/share/uqm/content/packages/uqm-"
                                       ,version "-content.uqm"))
             #t)))))
    (inputs
     `(("libmikmod" ,libmikmod)
       ("libvorbis" ,libvorbis)
       ("glu" ,glu)
       ("sdl" ,sdl)
       ("sdl-image" ,sdl-image)
       ("uqm-content"
        ,(origin
           (method url-fetch)
           (uri (string-append "mirror://sourceforge/sc2/UQM/0.7/uqm-"
                               version "-content.uqm"))
           (sha256
            (base32
             "1gx39ns698hyczd4nx73mr0z86bbi4q3h8sw3pxjh1lzla5xpxmq"))))))
    (native-inputs
     `(("pkg-config" ,pkg-config)))
    (home-page "http://sc2.sourceforge.net")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public java-arc-core
  (package
    (name "java-arc-core")
    (version "110")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/Anuken/Arc")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1iyckj4y47bcw15vsr104gysc9idx0m301qsx817mdgfdbjp9hmi"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "arc-core.jar"
       #:source-dir "arc-core/src/arc"
       #:test-dir "arc-core/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-native
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((stb (assoc-ref inputs "stb_image.h"))
                   (jdk (assoc-ref inputs "jdk"))
                   (lib (string-append (assoc-ref outputs "out") "/lib")))
               (copy-file stb "arc-core/csrc/stb_image.h")
               (invoke "gcc" "-shared" "-O2" "-fPIC"
                       ; symbols specify GLIBC_2.2.5 in memcpy_wrap.c
                       ;(if (member ,(%current-system)
                       ;            '("i686-linux" "armhf-linux" "i586-gnu"))
                       ;  "-Wl,-wrap,memcpy"
                       ;  "-Wl,-wrap,memcpy,-wrap,pow")
                       "-Wl,--no-undefined"
                       "-Iarc-core/csrc"
                       (string-append "-I" jdk "/include")
                       (string-append "-I" jdk "/include/linux")
                       "arc-core/csrc/pix.c"
                       "-lm"
                       ;"natives/memcpy_wrap.c"
                       "-o" "libarc.so")
               (mkdir-p lib)
               (install-file "libarc.so" lib)
               (install-file "libarc.so" "build/classes/"))
             #t))
         (add-before 'build 'fix-native-loading
           (lambda _
             ;; Ensure the name of the library is the same, independently from
             ;; the platform, as we don't perform a cross-build for every platform.
             (substitute* "arc-core/src/arc/util/SharedLibraryLoader.java"
               (("if\\(isLinux\\).*")
                "if(isLinux) return \"lib\" + libraryName + \".so\";\n"))
             #t))
         (add-after 'build-native 'repack
           (lambda _
             (invoke "jar" "-cf" "build/jar/arc-core.jar" "-C" "build/classes"
                     ".")
             #t))
         (add-before 'check 'fix-test-dir
           (lambda _
             (substitute* "build.xml"
               (("\\}/java") "}"))
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (inputs
     `(("stb_image.h"
        ,(origin (method url-fetch)
                 (uri (string-append
                        "https://raw.githubusercontent.com/nothings/stb/"
                        ;; latest commit, since there are no releases
                        "b42009b3b9d4ca35bc703f5310eedc74f584be58"
                        "/stb_image.h"))
                 (file-name "stb_image.h-b42009b")
                 (sha256
                  (base32
                   "132nblax05sqk0f77qc6jl03s24dwwh2s87gjx6872pwgmqhsnwf"))))))
    (home-page "https://github.com/Anuken/Arc")
    (synopsis "Java game development framework based off of libGDX")
    (description "This package contains a development framework for Java
games based on libGDX.  This is mostly used by Mindustry.")
    (license license:expat)))
