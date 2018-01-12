;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2014, 2015 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2014 Eric Bavier <bavier@member.fsf.org>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Jan Nieuwenhuizen <janneke@gnu.org>
;;; Copyright © 2017 Kei Kebreau <kei@openmailbox.org>
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

(define-module (more packages web)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:))

(define-public cat-avatar-generator
  (package
    (name "cat-avatar-generator")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://framagit.org/Deevad/cat-avatar-generator.git")
                     (commit "71c0c662742cafe8afd2d2d50ec84243113e35ad")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0s7b5whqsmfa57prbgl66ym551kg6ly0z14h5dgrlx4lqm70y2yw"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils)
                  (srfi srfi-1)
                  (srfi srfi-26))
       #:builder
       (begin
         (use-modules (guix build utils)
                      (srfi srfi-1)
                      (srfi srfi-26))
         (let ((source (assoc-ref %build-inputs "source"))
               (php-dir (string-append %output "/share/web/" ,name "/")))
           ;; The cache directory must not be in the store, but in a writable
           ;; location.  The webserver will give us this location.
           (copy-recursively source php-dir)
           (substitute* (string-append php-dir "/cat-avatar-generator.php")
             (("\\$cachepath = .*")
              "if(isset($_SERVER['CACHE_DIR']))
$cachepath = $_SERVER['CACHE_DIR'];
else
die('You need to set the CACHE_DIR variable first.');"))))))
    (home-page "")
    (synopsis "")
    (description "")
    ;; expat for the code, CC-BY 4.0 for the artwork
    (license (list license:expat
                   license:cc-by4.0))))
