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

(define-module (more packages google)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages image)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages ninja)
  #:use-module (gnu packages nss)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages image)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (srfi srfi-1))

(define-public chromium
  (package
    (name "chromium")
    (version "56.0.2924.87")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://commondatastorage.googleapis.com/"
                            "chromium-browser-official/" name "-" version ".tar.xz"))
        (sha256
         (base32
          "1q2kg85pd6lv036w7lsss5mhiiva9rx4f0410sbn9bnazhghib4s"))
        (patches (search-patches "chromium-gn-remove-third-party.patch"))
        (modules '((guix build utils)))
        (snippet
         '(begin
           (delete-file-recursively "base/third_party/libevent")))))
    (build-system gnu-build-system)
    (home-page "https://chromium.googlesource.com/chromium/src")
    (synopsis "Google web browser")
    (description "Google web browser.")
    (license license:bsd-3)))

(define-public skia
  (package
    (name "skia")
    (version "0")
    (source
      (origin
        (method git-fetch)
        (uri (git-reference
               (url "https://skia.googlesource.com/skia.git")
               (commit "c746bc15c167bc2a22169c2211fd2e65d9e266f5")))
        (file-name (string-append name "-" version))
        (sha256
         (base32
          "1x5m8ri6hmj9pbq4amglhkwbkcvhqp4vh8njwlrnlrsaipfdy62c"))
        (modules '((guix build utils)))
        (snippet
         `(begin
            (chdir "third_party")
            (delete-file-recursively "expat")
            (delete-file-recursively "libjpeg-turbo")
            (delete-file-recursively "freetype2")
            (delete-file-recursively "libpng")
            (delete-file-recursively "libwebp")
            (delete-file-recursively "zlib")
            (for-each (lambda (dir)
                        (for-each (lambda (f)
                                    (mkdir-p (dirname (string-append "externals/" f)))
                                    (copy-file f (string-append "externals/" f)))
                                  (find-files dir)))
                      '("icu" "sfntly"))
            (chdir "..")
            (for-each (lambda (f)
                        (substitute* f
                          ((".*//third_party/expat.*") "")
                          ((".*//third_party/freetype2.*") "")
                          ((".*//third_party/libjpeg-turbo:libjpeg.*") "")
                          ((".*//third_party/libpng.*") "")
                          ((".*//third_party/libwebp.*") "")
                          ((".*//third_party/zlib.*") "")))
                      '("BUILD.gn" "third_party/dng_sdk/BUILD.gn"))
            #t))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system*
                      "gn" "gen" "out/Shared"
                      (string-append "--args="
                                     "cc=\"gcc\" "
                                     "cxx=\"g++\" "
                                     "extra_cflags=[\"-I"
                                      (assoc-ref inputs "freetype")
                                      "/include/freetype2"
                                     "\"]"
                                     "skia_use_sfntly=false "
                                     "skia_use_dng_sdk=false "
                                     ;"skia_use_system_expat=true "
                                     ;"skia_use_system_libjpeg_turbo=true "
                                     ;"skia_use_system_freetype2=true "
                                     ;"skia_use_system_icu=true "
                                     ;"skia_use_system_libpng=true "
                                     ;"skia_use_system_libwebp=true "
                                     ;"skia_use_system_zlib=true "
                                     "is_official_build=true "
                                     "is_component_build=true")))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "ninja" "-C" "out/Shared"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
              (let ((lib (string-append (assoc-ref outputs "out") "/lib"))
                    (include (string-append (assoc-ref outputs "out")
                                            "/include")))
                (mkdir-p lib)
                (mkdir-p include)
                (copy-recursively "include" include)
                (copy-file "out/Shared/libskia.so"
                           (string-append lib "/libskia.so"))))))))
    (native-inputs
     `(("gn" ,google-gn)
       ("ninja" ,ninja)
       ("python" ,python-2)))
    (inputs
     `(("expat" ,expat)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("glu" ,glu)
       ("icu" ,icu4c)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libwebp" ,libwebp)
       ("libz" ,zlib)
       ("mesa" ,mesa)))
    (home-page "https://skia.org")
    (synopsis "Graphics library")
    (description "Graphics library.")
    (license license:bsd-3)))

(define-public google-gn
  (package
    (inherit chromium)
    (name "google-gn")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'fix-include
           (lambda* (#:key inputs #:allow-other-keys)
             (let* ((libevent (assoc-ref inputs "libevent"))
                    (event-h (string-append libevent "/include/event.h")))
               (substitute* "base/message_loop/message_pump_libevent.cc"
                 (("base/third_party/libevent/event.h") event-h)))))
         (replace 'build
           (lambda* (#:key outputs #:allow-other-keys)
             (chdir "tools/gn")
             (setenv "CC" (which "gcc"))
             (zero? (system* "python" "bootstrap/bootstrap.py" "-s"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (copy-file "../../out/Release/gn" (string-append bin "/gn"))))))))
    (native-inputs
     `(("python" ,python-2)
       ("ninja" ,ninja)))
    (inputs
     `(("libevent" ,libevent)
       ("icu" ,icu4c)
       ("nspr" ,nspr)))
    (home-page "https://chromium.googlesource.com/chromium/buildtools.git")
    (synopsis "Google gn")
    (description "Google gn.")
    (license license:bsd-3)))
