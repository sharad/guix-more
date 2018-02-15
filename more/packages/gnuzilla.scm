;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
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

(define-module (more packages gnuzilla)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module ((srfi srfi-26))
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix utils)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cargo)
  #:use-module (guix build-system gnu)
  #:use-module (more packages google)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bison)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages curl)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages flex)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages libreoffice)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages llvm)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages rust)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages xorg)
  #:use-module (ice-9 match))

(define-public icecat-skia
  (package
    (inherit icecat)
    (name "icecat-skia")
    (inputs
     `(("skia" ,skia)
       ,@(package-inputs icecat)))
    (arguments (substitute-keyword-arguments (package-arguments icecat)
                 ((#:configure-flags flags)
                  `(cons* "--enable-skia" ,flags))))))

(define-public icu4c-for-firefox
  (package
   (inherit icu4c)
   (name "icu4c")
   (version "59.1")
   (source (origin
            (method url-fetch)
            (uri (string-append
                  "http://download.icu-project.org/files/icu4c/"
                  version
                  "/icu4c-"
                  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                  "-src.tgz"))
            (sha256
             (base32 "1zkmbg2932ggvpgjp8pys0cj6z8bw087y8858009shkrjfpzscki"))))))

(define nss-for-firefox
  (package
    (inherit nss)
    (name "nss")
    (version "3.34.1")
    (source (origin
              (method url-fetch)
              (uri (let ((version-with-underscores
                          (string-join (string-split version #\.) "_")))
                     (string-append
                      "https://ftp.mozilla.org/pub/mozilla.org/security/nss/"
                      "releases/NSS_" version-with-underscores "_RTM/src/"
                      "nss-" version ".tar.gz")))
              (sha256
               (base32
                "186x33wsk4mzjz7dzbn8p0py9a0nzkgzpfkdv4rlyy5gghv5vhd3"))
              ;; Create nss.pc and nss-config.
              (patches (search-patches "nss-pkgconfig.patch"
                                       "nss-increase-test-timeout.patch"))))))
(define-public libpng-apng-for-firefox
  (package
    (inherit libpng-apng)
    (name "libpng-apng")
    (version "1.6.34")
    (source
     (origin
       (method url-fetch)
       (uri (list (string-append "mirror://sourceforge/libpng/libpng16/"
                                 version "/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src"
                   "/libpng16/libpng-" version ".tar.xz")
                  (string-append
                   "ftp://ftp.simplesystems.org/pub/libpng/png/src/history"
                   "/libpng16/libpng-" version ".tar.xz")))
       (sha256
        (base32
         "1xjr0v34fyjgnhvaa1zixcpx5yvxcg4zwvfh0fyklfyfj86rc7ig"))))
    (inputs
     `(("apng" ,(origin
                  (method url-fetch)
                  (uri
                   (string-append "mirror://sourceforge/libpng-apng/libpng16/"
                                  version "/libpng-" version "-apng.patch.gz"))
                  (sha256
                   (base32
                    "1ha4npf9mfrzp0srg8a5amks5ww84xzfpjbsj8k3yjjpai798qg6"))))))))

(define-public sqlite-for-firefox
  (package
   (inherit sqlite)
   (name "sqlite")
   (version "3.21.0")
   (source (origin
            (method url-fetch)
            (uri (let ((numeric-version
                        (match (string-split version #\.)
                          ((first-digit other-digits ...)
                           (string-append first-digit
                                          (string-pad-right
                                           (string-concatenate
                                            (map (cut string-pad <> 2 #\0)
                                                 other-digits))
                                           6 #\0))))))
                   (string-append "https://sqlite.org/2017/sqlite-autoconf-"
                                  numeric-version ".tar.gz")))
            (sha256
             (base32
              "1qxvzdjwzw6k0kqjfabj86rnq87xdbwbca7laxxdhnh0fmkm3pfp"))))))

(define-public firefox
  (package
    (name "firefox")
    (version "58.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.mozilla.org/pub/firefox/"
                                  "releases/" version "/source/firefox-"
                                  version ".source.tar.xz"))
              (sha256
               (base32
                "1zxapyir31zr1k7qrixqayzafh3kkvx0hh5vv1kp8kgrmg53j2hf"))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (use-modules (ice-9 ftw))
          ;; Remove bundled libraries that we don't use, since they may
          ;; contain unpatched security flaws, they waste disk space and
          ;; network bandwidth, and may cause confusion.
          (for-each delete-file-recursively
                    '(;; FIXME: Removing the bundled icu breaks configure.
                      ;;   * The bundled icu headers are used in some places.
                      ;;   * The version number is taken from the bundled copy.
                      ;;"intl/icu"
                      ;;
                      ;; FIXME: A script from the bundled nspr is used.
                      ;;"nsprpub"
                      ;;
                      ;; TODO: Use system media libraries.  Waiting for:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                      ;;   * libogg
                      ;;   * libtheora
                      ;;   * libvorbis
                      ;;   * libtremor (not yet in guix)
                      ;;   * libopus
                      ;;   * speex
                      ;;   * soundtouch (not yet in guix)
                      ;;
                      ;; TODO: Use system harfbuzz.  Waiting for:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=847568>
                      ;;
                      ;; TODO: Use system graphite2.
                      ;;
                      "modules/freetype2"
                      "modules/zlib"
                      "modules/libbz2"
                      "ipc/chromium/src/third_party/libevent"
                      "media/libjpeg"
                      "media/libvpx"
                      "security/nss"
                      "gfx/cairo"
                      "js/src/ctypes/libffi"
                      "db/sqlite3"))
          ;; Delete this file that has incorrect checksums
          (for-each delete-file (find-files "." "\\.cargo-checksum.json"))
          ;; Delete .pyc files, typically present in icecat source tarballs
          (for-each delete-file (find-files "." "\\.pyc$"))
          ;; Delete obj-* directories, sometimes present in icecat tarballs
          (for-each delete-file-recursively
                    (scandir "." (lambda (name)
                                   (string-prefix? "obj-" name))))
          #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:out-of-source? #t
       #:tests? #f
       #:configure-flags (list "--disable-necko-wifi"
                               "--disable-stylo"
                               "--disable-crashreporter"
                               "--disable-updater"
                               "--disable-tests"; Remove if we want to test
                               "--enable-application=browser"
                               "--enable-optimize=-O2"
                               "--with-pthreads"
                               ;; use system libraries
                               "--enable-system-hunspell"
                               "--enable-startup-notification"
                               "--enable-alsa" "--enable-pulseaudio"
                               "--enable-system-sqlite"
                               "--with-system-libevent"
                               "--with-system-libvpx"
                               "--with-system-nspr"
                               "--with-system-nss"
                               "--with-system-icu"
                               "--enable-system-cairo"
                               "--enable-system-ffi"
                               "--enable-system-pixman"
                               "--with-system-bz2"
                               "--with-system-jpeg"
                               "--with-system-png"
                               "--with-system-zlib")
                               ;; clang is not found because it is assumed to be in
                               ;; the same location as llvm.
                               ;(string-append "--with-clang-path="
                               ;               (assoc-ref %build-inputs "clang-3.9.1")
                               ;               "/bin/clang"))
                               ;(string-append "--with-libclang-path="
                               ;               (assoc-ref %build-inputs "clang-3.9.1")
                               ;               "/lib"))
       ;; Race condition in python?
       ;;   EOFError: EOF read where object expected
       #:parallel-build? #f
       #:phases
       (modify-phases %standard-phases
         (replace
          'configure
          ;; configure does not work followed by both "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bash (which "bash"))
                   (abs-srcdir (getcwd))
                   (srcdir (string-append "../" (basename abs-srcdir)))
                   (flags `(,(string-append "--prefix=" out)
                            ,(string-append "--with-l10n-base="
                                            abs-srcdir "/intl/l10n")
                            ,@configure-flags)))
              ;; We removed the embedded sqlite, so don't reference it.
              (substitute* '("storage/moz.build" "dom/indexedDB/moz.build")
                (("'/db/sqlite3/src',") ""))
              (setenv "SHELL" bash)
              (setenv "CONFIG_SHELL" bash)
              (setenv "AUTOCONF" (which "autoconf")) ; must be autoconf-2.13
              (mkdir "../build")
              (chdir "../build")
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (zero? (apply system* bash
                            (string-append srcdir "/configure")
                            flags))))))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gtk+" ,gtk+)
       ("gtk+-2" ,gtk+-2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("hunspell" ,hunspell)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libxft" ,libxft)
       ("libevent" ,libevent-2.0)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxcomposite" ,libxcomposite)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("ffmpeg" ,ffmpeg)
       ("libpng-apng" ,libpng-apng-for-firefox)
       ("libvpx" ,libvpx)
       ("icu4c" ,icu4c-for-firefox)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("nspr" ,nspr)
       ("nss" ,nss-for-firefox)
       ("sqlite" ,sqlite-for-firefox)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("yasm" ,yasm)
       ("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf-2.13)
       ("which" ,which)
       ("rust" ,rustc)
       ("cargo" ,cargo)
       ("clang-3.9.1" ,clang-3.9.1)
       ("llvm-3.9.1" ,llvm-3.9.1)))
    (home-page "https://mozilla.org")
    (synopsis "Web browser")
    (description "")
    (license (package-license icecat))))
