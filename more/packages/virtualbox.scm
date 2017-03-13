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

(define-module (more packages virtualbox)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix svn-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (more packages cdrom)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages image)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages m4)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages qt)
  #:use-module (gnu packages sdl)
  #:use-module (gnu packages texinfo)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg))

(define-public kbuild
  (package
    (name "kbuild")
    (version "0.1.9998")
    (source (origin
              ;(method url-fetch)
              ;(uri (string-append "ftp://ftp.netlabs.org/pub/kbuild/kBuild-"
              ;                    version "-src.tar.gz"))
              ;(sha256
              ; (base32
              ;  "19j2sswdqqjzjzmg0xam8nmwmxg422iii0fl9cwzcznjfigdn1c2"))))
              (method svn-fetch)
              (uri (svn-reference
                     (url "http://svn.netlabs.org/repos/kbuild/trunk")
                     (revision 3025)))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1k7y2lqqhsfwfzzi7rms7a2kakimm7g46qa2gypkvzdd3drbpanj"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bison" ,bison)
       ("flex" ,flex)
       ("perl" ,perl)
       ("texinfo" ,texinfo)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'patch-generated-file-shebangs 'reconfigure
           (lambda _
             (chdir "src/kmk")
             (system* "autoreconf" "-fiv")
             (chdir "../..")))
         (add-before 'build 'fix-binaries
           (lambda _
             (substitute* "Config.kmk"
               (("\\+= rt") "+= rt pthread"))
             (substitute* "src/ash/output.h"
               (("dprintf") "debugprintf"))
             (substitute* "src/ash/output.c"
               (("dprintf") "debugprintf"))
             (substitute* "bootstrap.gmk"
               ((" /bin/sh") (string-append " " (which "sh")))
               ((" /bin/echo") (string-append " " (which "echo"))))
             (substitute* "kBuild/env.sh"
               (("/bin/pwd") (which "pwd")))))
         (replace 'build
           (lambda _
             (zero? (system* "kBuild/env.sh" "--full" "make" "-f" "bootstrap.gmk"
                             "AUTORECONF=true"
                             (string-append "CONFIG_SHELL=" (which "sh"))
                             (string-append "AR=" (which "ar"))))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "kBuild/env.sh" "kmk" "PATH_INS=/"
                             (string-append "NIX_INSTALL_DIR="
                                            (assoc-ref outputs "out")))))))))
    (home-page "http://trac.netlabs.org/kbuild/wiki")
    (synopsis "Makefile framework")
    (description "kBuild is a makefile framework for writing simple makefiles
for complex tasks.")
    (license license:gpl3+)))

(define-public acpica-unix
  (package
    (name "acpica-unix")
    (version "20170303")
    (home-page "https://www.acpica.org")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://acpica.org/sites/acpica/files/acpica-unix-"
                     version ".tar.gz"))
              (sha256
               (base32
                "1dc933rr11gv1nlaf5j8ih1chdakbjbjkn34jgbm330zppmck4y0"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("m4" ,m4)
       ("bison" ,bison)
       ("flex" ,flex)))
    (arguments
     `(#:tests? #f; no check target
       #:make-flags
       (list (string-append "PREFIX=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'goto-dir
           (lambda _
             (chdir "generate/unix")
             (zero? (system* "make" "clean")))))))
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public virtualbox
  (package
    (name "virtualbox")
    (version "5.1.14")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.virtualbox.org/virtualbox/" version
                    "/VirtualBox-" version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "12i2kyn7svy2kd6j40fzzhy7173xfq884ygb6x9fbihpcw1bnrw2"))))
    (build-system gnu-build-system)
    (native-inputs
     `(;("kbuild" ,kbuild)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("acpica" ,acpica); for iasl
       ("alsa" ,alsa-lib)
       ("curl" ,curl)
       ("cdrtools" ,cdrtools)
       ("glu" ,glu)
       ("libidl" ,libidl)
       ("libpng" ,libpng)
       ("libvpx" ,libvpx)
       ("libxcursor" ,libxcursor)
       ("libxext" ,libxext)
       ("libxinerama" ,libxinerama)
       ("libxml2" ,libxml2)
       ("libxmu" ,libxmu)
       ("libxrandr" ,libxrandr)
       ("libxslt" ,libxslt)
       ("lvm2" ,lvm2)
       ("mesa" ,mesa)
       ("python" ,python-2)
       ("qt5" ,qt)
       ("openssl" ,openssl)
       ("sdl" ,sdl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("PYTHONDIR=.*")
                (string-append "PYTHONDIR=" (assoc-ref inputs "python") "\n")))))
         ;(add-before 'configure 'remove-binaries
           ;(lambda* _
             ;(substitute* "configure"
             ;  (("bin/\\$OS.\\$BUILD_MACHINE") "bin"))
             ;(delete-file-recursively "tools")
             ;(delete-file-recursively "kBuild/bin")))
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (zero? (system* "./configure" "--disable-java" "--disable-pulse"
                             "--disable-vmmraw"
                             (string-append "--with-makeself=" (which "echo"))))))
                             ;(string-append "--with-kbuild="
                             ;               (assoc-ref inputs "kbuild"))))))
         (replace 'build
           (lambda* _
             (zero? (system* "kmk")))))))
    (home-page "https://www.virtualbox.org")
    (synopsis "Virtual Machine manager")
    (description
     "VirtualBox is a powerful x86 and AMD64/Intel64 virtualization product for
enterprise as well as home use.")
    (license license:gpl2+)))
