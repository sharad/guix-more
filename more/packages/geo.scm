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

(define-module (more packages geo)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public tegola
  (package
    (name "tegola")
    (version "0.6.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/go-spatial/tegola/archive/v"
                     version ".tar.gz"))
              (sha256
               (base32
                "0172ikggayprxmhhm9yfk1gc4i8m48llc8zzgqklbijkdpvp1zh0"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-spatial/tegola/cmd/tegola"
       #:unpack-path "github.com/go-spatial"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'rename-import
           (lambda _
             (rename-file (string-append "src/github.com/go-spatial/tegola-" ,version)
                          "src/github.com/go-spatial/tegola")
             #t)))))
    (home-page "http://tegola.io")
    (synopsis "Vector tile server for maps")
    (description "Tegola is a free vector tile server written in Go.  Tegola
takes geospatial data and slices it into vector tiles that can be efficiently
delivered to any client.")
    (license license:expat)))

(define-public gdal
  (package
    (name "gdal")
    (version "2.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://download.osgeo.org/gdal/" version "/gdal-"
                     version ".tar.gz"))
              (sha256
               (base32
                "1951f7b69x3d1vic0rmq92q8f4bj3hbxnxmj5jl0cc3zg0isgmdr"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; TODO: frmts contains a lot more bundled code.
                   (for-each delete-file-recursively
                     '("frmts/png/libpng"
                       "frmts/gif/giflib"
                       "frmts/jpeg/libjpeg"
                       "frmts/jpeg/libjpeg12"
                       "frmts/gtiff/libtiff"
                       "frmts/gtiff/libgeotiff"
                       ;; ?
                       "frmts/zlib"
                       "ogr/ogrsf_frmts/geojson/libjson"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:configure-flags
       (let-syntax ((with (syntax-rules ()
                            ((_ option input)
                             (string-append option "="
                                            (assoc-ref %build-inputs input))))))
         (list
           ;; TODO: --with-pcidsk, --with-pcraster
           (with "--with-freexl" "freexl")
           (with "--with-libjson-c" "json-c")
           (with "--with-png" "libpng")
           (with "--with-webp" "libwebp")
           (with "--with-gif" "giflib")
           (with "--with-jpeg" "libjpeg")
           (with "--with-libtiff" "libtiff")
           (with "--with-geotiff" "libgeotiff")
           (with "--with-libz" "zlib")
           "--with-pcre"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-path
           (lambda _
             (substitute* "frmts/mrf/mrf_band.cpp"
               (("\"../zlib/zlib.h\"") "<zlib.h>")))))))
    (inputs
     `(("freexl" ,freexl)
       ("geos" ,geos)
       ("giflib" ,giflib)
       ("json-c" ,json-c)
       ("libgeotiff" ,libgeotiff)
       ("libjpeg" ,libjpeg)
       ("libpng" ,libpng)
       ("libtiff" ,libtiff)
       ("libwebp" ,libwebp)
       ("pcre" ,pcre)
       ("zlib" ,zlib)))
    (home-page "http://www.gdal.org/")
    (synopsis "Raster and vector geospatial data format library")
    (description "GDAL is a translator library for raster and vector geospatial
data formats.  As a library, it presents a single raster abstract data model
and single vector abstract data model to the calling application for all
supported formats.  It also comes with a variety of useful command line
utilities for data translation and processing.")
    ;; TODO: CHECK
    (license (list
               ; general license
               license:expat
               ; gdal/frmts/gtiff/tif_float.c, gdal/frmts/pcraster/libcsf,
               ; gdal/ogr/ogrsf_frmts/dxf/intronurbs.cpp, gdal/frmts/pdf/pdfdataset.cpp
               ; gdal/frmts/mrf/
               license:bsd-3
               ; gdal/frmts/hdf4/hdf-eos/*
               ; similar to the expat license, but without guarantee exclusion
               (license:non-copyleft "file://LICENSE.txt")
               ; gdal/frmts/grib/degrib/
               license:public-domain ; with restriction on guarantee
               ; port/cpl_minizip*
               ; ???
               ; gdal/alg/thinplatespline.cpp
               ; very short license, permission granted to copy, use, share and modify.
               (license:non-copyleft "file://LICENSE.txt")
               ; gdal/alg/libqhull
               ; TODO: not used by default, so we may snip it away
               ;license:bsd-5
               ; gdal/frmts/mrf/libLERC
               license:asl2.0))))
    
(define-public postgis
  (package
    (name "postgis")
    (version "2.4.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://download.osgeo.org/postgis/source/postgis-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1hm8migjb53cymp4qvg1h20yqllmy9f7x0awv5450391i6syyqq6"))))
    (build-system gnu-build-system)
    (home-page "postgis.net")
    (arguments
     `(#:tests? #f
       #:make-flags
       (list (string-append "datadir=" (assoc-ref %outputs "out") "/share")
             (string-append "docdir="(assoc-ref %outputs "out") "/share/doc")
             (string-append "pkglibdir="(assoc-ref %outputs "out") "/lib")
             (string-append "bindir=" (assoc-ref %outputs "out") "/bin"))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-install-path
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* '("raster/loader/Makefile" "raster/scripts/python/Makefile")
               (("\\$\\(DESTDIR\\)\\$\\(PGSQL_BINDIR\\)")
                (string-append (assoc-ref outputs "out") "/bin"))))))))
    (inputs
     `(("gdal" ,gdal)
       ("geos" ,geos)
       ("libxml2" ,libxml2)
       ("pcre" ,pcre)
       ("postgresql" ,postgresql)
       ("proj.4" ,proj.4)))
    (native-inputs
     `(("perl" ,perl)
       ("pkg-config" ,pkg-config)))
    (synopsis "")
    (description "")
    ;; TODO: CHECK
    (license license:expat)))
