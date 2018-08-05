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
