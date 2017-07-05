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

(define-module (more packages lua)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix hg-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (more packages boost)
  #:use-module (gnu packages lua))

(define-public ryzom-cmake
  (package
    (name "ryzom-cmake")
    (version "0")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                     (url "http://hg.kervala.net/cmake")
                     (changeset "56f5e1fd7677")))
              (sha256
               (base32
                "1n7gs9g9igls1xw3dndr6x8ww6xza0ig7igrrn9x382vpn9dw3g7"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (output (assoc-ref %outputs "out"))
                          (vardir (string-append output "/share/cmake")))
                     (chdir source)
                     (for-each
                       (lambda (file)
                         (mkdir-p (dirname (string-append vardir "/" file)))
                         (copy-file file (string-append vardir "/" file)))
                       (find-files "." "[^/]*"))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public luabind
  (package
    (name "luabind")
    (version "0.9.2")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                     (url "http://hg.kervala.net/luabind")
                     (changeset "3333deedec4a")))
              ;(uri (git-reference
              ;       (url "https://github.com/luabind/luabind")
              ;       (commit "cc743c37a7fffc72c809aed39e75385a34502ece")))
              ;(method url-fetch)
              ;(uri (string-append "https://github.com/luabind/luabind/archive/v"
              ;                    version ".tar.gz"))
              ;(uri (string-append "mirror://sourceforge/luabind/luabind/"
              ;                    version "/luabind-" version ".tar.gz"))
              (sha256
               (base32
                ;"1bma96ba9ahzwax1kis4yx5zfirj9fij4fxcdn7dsy46j425xpl0"))))
                ;"0bgz0lpq8ml6gsfck8j6ds40r40g499nzypji2cd4s3nl18asphf"))))
                ;"0j6lm48yz2y0hxk3qznilfdv1nqfql7cybhih781kq4d6kvccz5n"))))
                "0y6qyb49gsf3dhvyy7sqihcivkhjryyq6cc9q9y3s0flg0gifsw2"))))
    ;(build-system gnu-build-system)
    (build-system cmake-build-system)
    ;(native-inputs
    ; `(("boost-build" ,boost-build)))
    (inputs
     `(("boost" ,boost-fix)
       ("lua" ,lua-5.1)))
    (native-inputs
     `(("ryzom-cmake" ,ryzom-cmake)))
    (arguments
     `(#:out-of-source? #t
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'export
           (lambda _
             (setenv "CMAKE_MODULE_PATH"
                     (string-append (assoc-ref %build-inputs "ryzom-cmake")
                                    "/share/cmake/modules"))
             (format #t (getenv "CMAKE_MODULE_PATH")))))))
    ;(arguments
    ; `(#:tests? #f
    ;   #:phases
    ;   (modify-phases %standard-phases
    ;     (delete 'configure)
    ;     (replace 'build
    ;       (lambda* (#:key inputs #:allow-other-keys)
    ;         (substitute* "luabind/object.hpp"
    ;           (("  LUABIND_OPERATOR_ADL_WKND.*") ""))
    ;         (setenv "LUA_PATH" (assoc-ref inputs "lua"))
    ;         (zero? (system* "b2"))))
    ;     (replace 'install
    ;       (lambda* (#:key outputs #:allow-other-keys)
    ;         (zero? (system* "b2" "install"
    ;                         (string-append "--prefix="
    ;                                        (assoc-ref outputs "out")))))))))
    (home-page "http://www.rasterbar.com/products/luabind.html")
    (synopsis "")
    (description "")
    (license license:expat)))
