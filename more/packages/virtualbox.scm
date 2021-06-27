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
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages admin)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cdrom)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages gettext)
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
    ;(version "0.1.5-p2")
    (source (origin
              ;(method url-fetch)
              ;(uri (string-append "ftp://ftp.netlabs.org/pub/kbuild/kBuild-"
              ;                    version "-src.tar.gz"))
              ;(sha256
              ; (base32
              ;  "19j2sswdqqjzjzmg0xam8nmwmxg422iii0fl9cwzcznjfigdn1c2"))))
              ;(method svn-fetch)
              ;(uri (svn-reference
              ;       (url "http://svn.netlabs.org/repos/kbuild/trunk")
              ;       (revision 3224)))
              ;(file-name (string-append name "-" version))
              ;(sha256
              ; (base32
              ;  "0dvp6j61v92afhkz392zbk0myx27mzqpqcfqr7yyaj3kigbnmal8"))))
              (method git-fetch)
              (uri (git-reference
                     (url "https://framagit.org/tyreunom/kbuild-snapshot.git")
                     (commit "f3a865c4671242524860097c819152447a03a7f7")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1xkirw4xdwpgcrvnwdqdr7d3fyg6yg2dzgyi83vxmvib6k4xwvxr"))))
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("bison" ,bison)
       ("flex" ,flex)
       ("gettext-minimal" ,gettext-minimal)
       ("perl" ,perl)
       ("pkg-config" ,pkg-config)
       ("texinfo" ,texinfo)))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'patch-generated-file-shebangs 'reconfigure
           (lambda _
             (chdir "src/kmk")
             (substitute* "Makefile.am"
               (("AM_CPPFLAGS =") "AM_CPPFLAGS = -I$(srcdir)/../lib//kStuff/include")
               (("kmkbuiltin\\.c")
                "kmkbuiltin.c kmkbuiltin/getopt_r.c kmkbuiltin/getopt1_r.c")
               (("kmk_redirect_SOURCES =")
                "kmk_redirect_SOURCES = output.c"))
             (system* "autoreconf" "-fiv")
             (chdir "../..")))
         (add-before 'build 'fix-binaries
           (lambda _
             (substitute* "Config.kmk"
               (("\\+= rt") "+= rt pthread"))
             (substitute* "src/kash/output.h"
               (("dprintf") "debugprintf"))
             (substitute* "src/kash/output.c"
               (("dprintf") "debugprintf"))
             (substitute* "bootstrap.gmk"
               ((" /bin/sh") (string-append " " (which "sh")))
               ((" /bin/echo") (string-append " " (which "echo"))))
             (substitute* "kBuild/env.sh"
               (("/bin/pwd") (which "pwd")))))
         (add-before 'build 'fix-errors
           (lambda _
             (copy-file "src/sed/po/Makefile.in.in" "src/kmk/po/Makefile.in.in")
             (substitute* "src/kmk/kmkbuiltin.c"
               (("int kmk_builtin_dircache.*")
                "int kmk_builtin_dircache(int argc, char **argv, char **envp, PKMKBUILTINCTX pCtx)"))
             (substitute* "src/kmk/kmkbuiltin.h"
               (("#include <fcntl.h>")
                "#include <fcntl.h>\n#include <stdint.h>"))
             (substitute* "src/kmk/output.h"
               (("#define INCLUDED_MAKE_OUTPUT_H")
                "#define INCLUDED_MAKE_OUTPUT_H\n#include <stdio.h>"))
             (substitute* "src/kmk/kmkbuiltin/install.c"
               (("\tuid") "\tThis.uid")
               (("\tgid") "\tThis.gid"))
             (substitute* "src/kmk/kmkbuiltin/rm.c"
               (("\\(vflag\\)") "(pThis->vflag)"))
             (substitute* "src/kmk/kmkbuiltin/getopt_r.h"
               (("typedef struct") "#include <stddef.h>\ntypedef struct"))
             (substitute* "src/kmk/output.c"
               (("unsigned int stdio_traced = 0;")
                "unsigned int stdio_traced = 0;\nssize_t output_write_text (struct output *out, int is_err, const char *src, size_t len){return len;}"))
             ;  (("/\\* write/fwrite like function, text mode. \\*/") "#endif")
             ;  (("return output_write_bin \\(out, is_err, src, len\\);")
             ;   "return output_write_bin (out, is_err, src, len);\n#if 1"))
             #t))
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
    (version "5.2.8")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://download.virtualbox.org/virtualbox/" version
                    "/VirtualBox-" version ".tar.bz2"))
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "1s44h0d2khclkgkfgcwpq09dpckzr22r8737icdwhjhbgga5j9zf"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; TODO: frmts contains a lot more bundled code.
                   (for-each delete-file-recursively
                     '("src/libs/libpng-1.2.54"
                       "src/libs/libxml2-2.9.4"
                       "src/libs/zlib-1.2.8"))))))
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
       ("qt" ,qtbase)
       ("openssl" ,openssl)
       ("sdl" ,sdl)))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-paths
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "configure"
               (("CXX=.*") "CXX=\"g++ -std=c++11\"\n")
               (("PYTHONDIR=.*")
                (string-append "PYTHONDIR=" (assoc-ref inputs "python") "\n"))
               (("QT5DIR=.*")
                (string-append "QT5DIR=" (assoc-ref inputs "qt") "\n")))))
         ;(add-before 'configure 'remove-binaries
           ;(lambda* _
             ;(substitute* "configure"
             ;  (("bin/\\$OS.\\$BUILD_MACHINE") "bin"))
             ;(delete-file-recursively "tools")
             ;(delete-file-recursively "kBuild/bin")))
         (replace 'configure
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (setenv "I_INCXML2" (string-append "-I" (assoc-ref inputs "libxml2")
                                                "/include/libxml2"))
             (setenv "I_INCSDL" (string-append "-I" (assoc-ref inputs "sdl")
                                               "/include/SDL"))
             (invoke "./configure" "--disable-java" "--disable-pulse"
                     "--disable-vmmraw"
                     (string-append "--with-makeself=" (which "echo")))
                     ;(string-append "--with-kbuild="
                     ;               (assoc-ref inputs "kbuild"))))
             #t))
         (replace 'build
           (lambda* _
             (invoke "kmk")
             #t)))))
    (home-page "https://www.virtualbox.org")
    (synopsis "Virtual Machine manager")
    (description
     "VirtualBox is a powerful x86 and AMD64/Intel64 virtualization product for
enterprise as well as home use.")
    (license license:gpl2+)))
