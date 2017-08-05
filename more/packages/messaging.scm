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

(define-module (more packages messaging)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix build-system cmake)
  #:use-module (guix build-system gnu)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages libupnp)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages readline)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages video)
  #:use-module (gnu packages vim)
  #:use-module (gnu packages xiph)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages java)
  #:use-module (more packages java))

;; goes to check.scm
(define-public bcunit
  (package
    (name "bcunit")
    ;; version 3.0 as released in github cannot be used
    (version "3.0-1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/BelledonneCommunications/bcunit")
                     (commit "cf1aaa36c5738c25e59c8fafbade388a0081cd53")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1ryzq704p7pfkxfy5fcp624lpa8y0s2raj4742hhr9d01j6dnghq"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (home-page "https://linphone.org")
    (synopsis "C unit testing framework")
    (description
      "fork of the defunct project CUnit (see below), with several fixes and
patches applied.")
    (license license:lgpl2.0+)))

;; Fix in tls.scm
(define-public mbedtls
  (package
    (name "mbedtls")
    (version "2.4.2")
    (source
     (origin
       (method url-fetch)
       ;; XXX: The download links on the website are script redirection links
       ;; which effectively lead to the format listed in the uri here.
       (uri (string-append "https://tls.mbed.org/download/mbedtls-"
                           version "-apache.tgz"))
       (sha256
        (base32
         "065hn5zibzflivabdh9p41dknda7wicl2zhc936dmakqfjprip8p"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'configure))
       #:make-flags
       (list "CC=gcc" (string-append "DESTDIR=\"" (assoc-ref %outputs "out") "\"")
             "SHARED=1")
       #:validate-runpath? #f))
    (native-inputs
     `(("perl" ,perl)))
    (synopsis "Small TLS library")
    (description
     "@code{mbed TLS}, formerly known as PolarSSL, makes it trivially easy
for developers to include cryptographic and SSL/TLS capabilities in their
(embedded) products, facilitating this functionality with a minimal
coding footprint.")
    (home-page "https://tls.mbed.org")
    (license license:asl2.0)))

(define-public bctoolbox
  (package
    (name "bctoolbox")
    (version "0.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BelledonneCommunications/"
                                  "bctoolbox/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qx65qva29ljpx42c530n96lc5f0gdxbkd33nkcvbpj641zkd2bi"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))
       #:configure-flags
       (list "--with-pic" "CFLAGS=-fPIC" "CXXFLAGS=-fPIC")
       #:make-flags (list "AM_V_CC=" "AM_V_CXX=")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("bcunit" ,bcunit)))
    (propagated-inputs
     `(("mbedtls-apache" ,mbedtls)))
    (home-page "https://linphone.org")
    (synopsis "OS abstraction layer")
    (description
      "Utilities library used by Belledonne Communications software like
belle-sip, mediastreamer2 and linphone.")
    (license license:gpl2+)))

(define-public ortp
  (package
    (name "ortp")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BelledonneCommunications/"
                                  "ortp/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1rkwawydvkk1bnl4y5fqm4ip472wsviys7mcd6ryhrb9p21azakv"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("bctoolbox" ,bctoolbox)))
    (home-page "https://linphone.org")
    (synopsis "RTP protocol implementation")
    (description
      "RTP protocol implementation.")
    (license license:gpl2+)))

(define-public belle-sip
  (package
    (name "belle-sip")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BelledonneCommunications/"
                                  "belle-sip/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0bn5g4a62dg60lsm7qmmnqa6wrl1vs2z7sgwg3fmjgs044nlbsk0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))
       #:configure-flags
       (list "CFLAGS=-D_temp=NULL"
             (string-append "--with-antlr=" (assoc-ref %build-inputs "antlr")))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("vim" ,vim)
       ("which" ,which)))
    (inputs
     `(("antlr" ,antlr3)
       ("bctoolbox" ,bctoolbox)
       ("libantlr3c" ,libantlr3c)))
    (home-page "https://linphone.org")
    (synopsis "")
    (description
      "")
    (license license:gpl2+)))

(define-public mediastreamer
  (package
    (name "mediastreamer")
    (version "2.15.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/BelledonneCommunications/"
                                  "mediastreamer2/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ln8h2c420y25y8zx7bfmbh28yjxl3qpkhicv88pg06aqgpkwzay"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))
       #:configure-flags (list "--enable-external-ortp")
       #:make-flags (list "ECHO=echo")))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("vim" ,vim)
       ("which" ,which)))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("ffmpeg" ,ffmpeg)
       ("glew" ,glew)
       ("libx11" ,libx11)
       ("libxext" ,libxext)
       ("libxv" ,libxv)
       ("mesa" ,mesa)
       ("opus" ,opus)
       ("ortp" ,ortp)
       ("pulseaudio" ,pulseaudio)
       ("speex" ,speex)
       ("speexdsp" ,speexdsp)
       ("v4l-utils" ,v4l-utils)))
    (home-page "https://linphone.org")
    (synopsis "")
    (description
      "")
    (license license:gpl2+)))

(define-public linphone
  (package
    (name "linphone")
    (version "3.11.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.linphone.org/releases/sources/"
                                  "linphone/linphone-" version ".tar.gz"))
              (sha256
               (base32
                "0kj0l3qa8wa07gk145lw6f8hmc7nk5xyvwj1c3dvk58l64yyz26w"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autogen
           (lambda _
             (zero? (system* "./autogen.sh")))))))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("intltool" ,intltool)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (inputs
     `(("bctoolbox" ,bctoolbox)
       ("eudev" ,eudev)
       ("gtk2" ,gtk+-2)
       ("libupnp" ,libupnp)
       ("libxml2" ,libxml2)
       ("mediastreamer" ,mediastreamer)
       ("ortp" ,ortp)
       ("readline" ,readline)
       ("speex" ,speex)
       ("sqlite" ,sqlite)
       ("zlib" ,zlib)))
    (home-page "https://linphone.org")
    (synopsis "VOIP application")
    (description
      "Linphone is an open source Voice Over IP phone (or SIP phone) that makes
possible to communicate freely with people over the internet, with voice, video,
and text instant messaging.")
    (license license:gpl2+)))
