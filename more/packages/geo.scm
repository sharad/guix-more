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
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system go)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages datastructures)
  #:use-module (gnu packages documentation)
  #:use-module (gnu packages geo)
  #:use-module (gnu packages image)
  #:use-module (gnu packages lua)
  #:use-module (gnu packages pcre)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml))

(define-public tegola
  (package
    (name "tegola")
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/go-spatial/tegola/archive/v"
                     version ".tar.gz"))
              (sha256
               (base32
                "09vnzxfn0r70kmd776kcdfqxhzdj11syxa0b27z4ci1k367v7viw"))))
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

(define-public protozero
  (package
    (name "protozero")
    (version "1.6.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/mapbox/protozero/archive/v"
			    version ".tar.gz"))
	(file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1xaj4phz1r7xn0vgdfvfkz8b0bizgb6mavjky1zqcvdmbwgwgly5"))))
    (build-system cmake-build-system)
    (home-page "https://github.com/mapbox/protozero")
    (synopsis "Minimalistic protocol buffer decoder and encoder in C++")
    (description "Protozero is a minimalistic protocol buffer decored and
encoder in C++.  The developer using protozero has to manually translate the
@file{.proto} description into code.")
    (license (list
	       license:asl2.0; for folly
	       license:bsd-2))))

(define-public libosmium
  (package
    (name "libosmium")
    (version "2.14.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/osmcode/libosmium/archive/v"
			    version ".tar.gz"))
	(file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0d9b46qiw7zkw1h9lygjdwqxnbhm3c7v8kydzw9f9f778cyagc94"))))
    (build-system cmake-build-system)
    (inputs
     `(("boost" ,boost)
       ("expat" ,expat)
       ("gdal" ,gdal)
       ("geos" ,geos)
       ("proj.4" ,proj.4)
       ("protozero" ,protozero)
       ("sparsehash" ,sparsehash)
       ("zlib" ,zlib)))
    (native-inputs
     `(("doxygen" ,doxygen)))
    (home-page "https://osmcode.org/libosmium")
    (synopsis "C++ library for working with OpenStreetMap data")
    (description "Libosmium is a fast and flexible C++ library for working with
OpenStreetMap data.")
    (license license:boost1.0)))

(define-public imposm3
  (package
    (name "imposm3")
    (version "0.6.0-alpha.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/omniscale/imposm3/archive/v"
			    version ".tar.gz"))
	(file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "06f0kwmv52yd5m9jlckqxqmkf0cnqy3hamakrvg9lspplyqrds80"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/omniscale/imposm3/cmd/imposm"
       #:unpack-path "github.com/omniscale"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'rename-import
           (lambda _
             (rename-file (string-append "src/github.com/omniscale/imposm3-" ,version)
                          "src/github.com/omniscale/imposm3")
             #t)))))
    (inputs
     `(("geos" ,geos)
       ("leveldb" ,leveldb)))
    (home-page "http://imposm.org/")
    (synopsis "OpenStreetMap importer for PostGIS.")
    (description "OpenStreetMap importer for PostGIS.")
    (license license:asl2.0)))

(define-public osmconvert
  (package
    (name "osmconvert")
    (version "0")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "http://m.m.i24.cc/osmconvert.c"))
	      (sha256
	       (base32
		"19glwq8w5sl8579zxbpydj56lybs94nrf47f3i2xjwlkmzrlljfv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
	 (delete 'unpack)
	 (delete 'configure)
	 (delete 'install)
	 (replace 'build
	   (lambda* (#:key inputs outputs #:allow-other-keys)
	     (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
	     (invoke "gcc" (assoc-ref inputs "source") "-lz" "-o"
		     (string-append (assoc-ref outputs "out") "/bin/osmconvert"))
	     (chmod (string-append (assoc-ref outputs "out") "/bin/osmconvert")
		    #o755)
	     #t)))))
    (inputs
     `(("zlib" ,zlib)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:agpl3+)))

(define-public osm2pgsql
  (package
    (name "osm2pgsql")
    (version "0.96.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/openstreetmap/osm2pgsql/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08y7776r4l9v9177a4q6cfdri0lpirky96m6g699hwl7v1vhw0mn"))))
    (build-system cmake-build-system)
    (arguments
      ;; failure
     `(#:tests? #f))
    (inputs
     `(("boost" ,boost)
       ("expat" ,expat)
       ("lua" ,lua)
       ("postgresql" ,postgresql)
       ("proj.4" ,proj.4)
       ("zlib" ,zlib)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl2)))

(define-public tippecanoe
  (package
    (name "tippecanoe")
    (version "1.31.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mapbox/tippecanoe/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1057na1dkgjaryr7jr15lqkxpam111d3l5zdpdkqzzzpxmdjxqcf"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases (delete 'configure))
       #:test-target "test"
       #:make-flags
       (list "CC=gcc"
             (string-append "PREFIX=" (assoc-ref %outputs "out")))))
    (inputs
     `(("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-2)))
