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

(define-module (more packages package-managers)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system meson)
  #:use-module (gnu packages backup)
  #:use-module (gnu packages cmake)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages freedesktop)
  #:use-module (gnu packages gettext)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages polkit)
  #:use-module (gnu packages sqlite)
  #:use-module (gnu packages valgrind)
  #:use-module (gnu packages xml))

(define-public libsoup-2.67.3
  (package
    (inherit libsoup)
    (version "2.67.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://gnome/sources/libsoup/"
                                  (version-major+minor version) "/"
                                  "libsoup-" version ".tar.xz"))
              (sha256
               (base32
                "0ps7y2s2kb3dgx22mnfw2692h3ckr5lq74mcwax5v3db5lzwsmfi"))))
    (outputs '("out"))
    (arguments
     `(,@(substitute-keyword-arguments (package-arguments libsoup)
           ((#:phases phases)
            `(modify-phases ,phases
               (delete 'move-doc))))
       #:tests? #f))
    (propagated-inputs
      `(("google-brotli" ,google-brotli)
        ,@(package-propagated-inputs libsoup)))))

(define-public gnome-software-center
  (package
    (name "gnome-software-center")
    (version "3.32.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.gnome.org/GNOME/gnome-software")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "168s7y59q5j4sgvm1x07qyz6b88cynyhxbigsil6w5fc9j4hm30f"))))
    (build-system meson-build-system)
    (arguments
     `(#:configure-flags '("-Dfwupd=false" "-Dflatpak=false")
       #:tests? #f
       #:glib-or-gtk? #t))
    (native-inputs
     `(("cmake" ,cmake)
       ("desktop-file-utils" ,desktop-file-utils)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("gettext" ,gettext-minimal)
       ("glib" ,glib "bin")
       ("gtk-doc" ,gtk-doc)
       ("gtk-bin" ,gtk+ "bin")
       ("libxslt" ,libxslt) ; for xsltproc
       ("pkg-config" ,pkg-config)
       ("valgrind" ,valgrind)))
    (inputs
     `(("appstream-glib" ,appstream-glib)
       ("gnome-desktop" ,gnome-desktop)
       ("gnome-online-accounts" ,gnome-online-accounts "lib")
       ("gspell" ,gspell)
       ("json-glib" ,json-glib)
       ("libarchive" ,libarchive)
       ("libgudev" ,libgudev)
       ("libsoup" ,libsoup-2.67.3)
       ("libxmlb" ,libxmlb)
       ("packagekit" ,packagekit)
       ("polkit" ,polkit)))
    (home-page "https://gitlab.gnome.org/GNOME/gnome-software")
    (synopsis "")
    (description "")
    (license #f)))
