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

(define-module (more packages ruby)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system ruby)
  #:use-module (gnu packages ncurses)
  #:use-module (gnu packages ruby))

(define-public ruby-pry-doc
  (package
    (name "ruby-pry-doc")
    (version "0.13.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "pry-doc" version))
        (sha256
          (base32
            "036z1nsmrxnslf5xskbv6w59p4nb03bb0j3x23xw7v35sky5q0p7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependencies
           (lambda _
             (substitute* "Rakefile"
               (("require 'latest_ruby'") ""))
             #t)))))
    (propagated-inputs
      `(("ruby-pry" ,ruby-pry)
        ("ruby-yard" ,ruby-yard)))
    (native-inputs
     `(("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "Pry Doc is a Pry REPL plugin. It provides extended documentation support for the
REPL by means of improving the `show-doc` and `show-source` commands. With help
of the plugin the commands are be able to display the source code and the docs
of Ruby methods and classes implemented in C.
documentation
")
    (description
      "Pry Doc is a Pry REPL plugin.  It provides extended documentation support for the
REPL by means of improving the `show-doc` and `show-source` commands.  With help
of the plugin the commands are be able to display the source code and the docs
of Ruby methods and classes implemented in C.
documentation
")
    (home-page "https://github.com/pry/pry-doc")
    (license license:expat)))

(define-public ruby-trollop
  (package
    (name "ruby-trollop")
    (version "2.9.9")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "trollop" version))
        (sha256
          (base32
            "074h7lns72kg1dl5gvz5apl3xz1i0axbnbc01pf2kbw4q0lkpnp4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (synopsis
      "Trollop is a commandline option parser for Ruby that just gets out of your way.")
    (description
      "Trollop is a commandline option parser for Ruby that just gets out of your way.")
    (home-page "")
    (license license:expat)))

(define-public ruby-wp2txt
  (package
    (name "ruby-wp2txt")
    (version "0.9.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "wp2txt" version))
        (sha256
          (base32
            "01l0r83ma3fp9zb94km4lqasvxpml2azd4dj36qzpm71c2pyhng4"))))
    (build-system ruby-build-system)
    (arguments
     `(#:tests? #f))
    (propagated-inputs
      `(("ruby-htmlentities" ,ruby-htmlentities)
        ("ruby-nokogiri" ,ruby-nokogiri)
        ("ruby-parallel" ,ruby-parallel)
        ("ruby-trollop" ,ruby-trollop)))
    (synopsis
      "WP2TXT extracts plain text data from Wikipedia dump file (encoded in XML/compressed with Bzip2) stripping all the MediaWiki markups and other metadata.")
    (description
      "WP2TXT extracts plain text data from Wikipedia dump file (encoded in XML/compressed with Bzip2) stripping all the MediaWiki markups and other metadata.")
    (home-page "http://github.com/yohasebe/wp2txt")
    (license #f)))
