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

(define-module (more packages opencv)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages graphics)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages image)
  #:use-module (gnu packages photo)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages video)
  #:use-module (gnu packages zip)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system python))

(define-public opencv
  (package
    (name "opencv")
    (version "3.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/opencv/opencv/archive/"
                                  version ".zip"))
              (file-name (string-append name "-" version ".zip"))
              (sha256
               (base32
                "0p0ppp3p7xnn4ah9g3a9nh7wav2jg2zq3mz1vnd50bk6aknhq06j"))))
    (build-system cmake-build-system)
    (native-inputs
     `(("doxygen" ,doxygen)
       ("unzip" ,unzip)
       ("pkg-config" ,pkg-config)))
    (inputs
     `(("ffmpeg" ,ffmpeg)
       ("gstreamer" ,gstreamer)
       ("gtk-3" ,gtk+)
       ("ilmbase" ,ilmbase)
       ("jasper" ,jasper)
       ("libgpoto2" ,libgphoto2)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("mesa" ,mesa)
       ("openexr" ,openexr)
       ("zlib" ,zlib)))
    (arguments
     ;; This thing has a strange license
     `(#:configure-flags (list "-DWITH_IPP=OFF")
       #:tests? #f; they're just too long
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'remove-3rdparties
           (lambda _
             (delete-file-recursively "3rdparty")))
         (add-after 'set-paths 'add-ilmbase-include-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; OpenEXR propagates ilmbase, but its include files do not appear
             ;; in the CPATH, so we need to add "$ilmbase/include/OpenEXR/" to
             ;; the CPATH to satisfy the dependency on "half.h".
             (setenv "CPATH"
                     (string-append (assoc-ref inputs "ilmbase")
                                    "/include/OpenEXR"
                                    ":" (or (getenv "CPATH") "")))
             #t)))))
    (home-page "http://opencv.org/")
    (synopsis "Computer vision and machine learning software library")
    (description
     "OpenCV (Open Source Computer Vision Library) is an open source computer
vision and machine learning software library.")
    (license license:bsd-3)))
