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

(define-module (more packages video)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages algebra)
  #:use-module (more packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages libreoffice) ; hunspell
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages video)
  #:use-module (gnu packages wxwidgets)
  #:use-module (gnu packages xorg))

(define-public ffms2
  (package
   (name "ffms2")
   (version "2.23")
   (home-page "https://github.com/FFMS/ffms2/")
   (source (origin
             (method url-fetch)
             (uri (string-append home-page "archive/" version ".tar.gz"))
             (sha256
              (base32
               "1vbkab8vrplxz5xgag8ggzkwp4f7nf285pd0l2a7zy66n6i2m6xh"))))
   (build-system gnu-build-system)
   (arguments
    '(#:configure-flags
      (list "--enable-avresample")))
   (inputs
    `(("zlib" ,zlib)))
   (propagated-inputs
    `(("ffmpeg" ,ffmpeg)))
   (native-inputs
    `(("pkg-config" ,pkg-config)))
   (synopsis "Cross-plateform wrapper around ffmpeg/libav")
   (description
     "FFMpegSource is a wrapper library around ffmpeg/libav that allows
programmers to access a standard API to open and decompress media files")
   (license license:gpl2+))); inherits from ffmpeg
  ;; sources are distributed under a different license that the binary.
  ;; see https://github.com/FFMS/ffms2/blob/master/COPYING

(define-public aegisub
  (package
    (name "aegisub")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://ftp.aegisub.org/pub/archives/releases/source/"
                     name "-" version ".tar.xz"))
              (sha256
               (base32
                "11b83qazc8h0iidyj1rprnnjdivj1lpphvpa08y53n42bfa36pn5"))))
    (build-system gnu-build-system)
    (arguments
     `(#:configure-flags
       (list "--disable-update-checker"
             "--without-portaudio"
             "--without-openal"
             "--without-oss")
       ;; tests require busted, a lua package we don't have yet
       #:tests? #f))
    (inputs
     `(("boost" ,boost)
       ("desktop-file-utils" ,desktop-file-utils)
       ("ffms2" ,ffms2)
       ("fftw" ,fftw)
       ("hunspell" ,hunspell)
       ("mesa" ,mesa)
       ("libass" ,libass)
       ("alsa-lib" ,alsa-lib)
       ("pulseaudio" ,pulseaudio)
       ("libx11" ,libx11)
       ("freetype" ,freetype)
       ("wxwidgets-gtk2" ,wxwidgets-gtk2)))
    (native-inputs
     `(("intltool" ,intltool)
       ("pkg-config" ,pkg-config)))
    (home-page "http://www.aegisub.org/")
    (synopsis "Subtitle engine")
    (description "Aegisub helps translators create subtitles for video")
    (license (list license:bsd-3 ; the package is licensed under the bsd-3, except
                   license:mpl1.1 ; for vendor/universalchardet under the mpl1.1
                   license:expat)))) ; and src/gl that is under a license similar
   ; the the Expat license, with a rewording (Software -> Materials). (called MIT
   ; by upstream). See https://github.com/Aegisub/Aegisub/blob/master/LICENCE
   ; src/MatroskaParser.(c|h) is under bsd-3 with permission from the author
