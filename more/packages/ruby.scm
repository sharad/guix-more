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

(define-public ruby-bacon-colored-output
  (package
    (name "ruby-bacon-colored-output")
    (version "1.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "bacon-colored_output" version))
        (sha256
          (base32
            "1znyh3vkfdlmf19p3k4zip88ibym41dn5g4p4n5hmks2iznb7qpx"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs `(("ruby-bacon" ,ruby-bacon)))
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis
      "Colored output for Bacon test framework! http://i.imgur.com/EpTpw.png")
    (description
      "Colored output for Bacon test framework! http://i.imgur.com/EpTpw.png")
    (home-page "")
    (license #f)))

(define-public ruby-coveralls
  (package
    (name "ruby-coveralls")
    (version "0.8.22")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "coveralls" version))
        (sha256
          (base32
            "022kc16np6w4mv17hq3m9hhw9l8hjl78ld3fzqqx6337vwvwvwcg"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ruby-json" ,ruby-json)
        ("ruby-simplecov" ,ruby-simplecov)
        ("ruby-term-ansicolor" ,ruby-term-ansicolor)
        ("ruby-thor" ,ruby-thor)
        ("ruby-tins" ,ruby-tins)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "A Ruby implementation of the Coveralls API.")
    (description
      "This package provides a Ruby implementation of the Coveralls API.")
    (home-page "https://coveralls.io")
    (license license:expat)))

(define-public ruby-hashdiff
  (package
    (name "ruby-hashdiff")
    (version "0.3.7")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "hashdiff" version))
        (sha256
          (base32
            "0yj5l2rw8i8jc725hbcpc4wks0qlaaimr3dpaqamfjkjkxl0hjp9"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
      " HashDiff is a diff lib to compute the smallest difference between two hashes. ")
    (description
      " HashDiff is a diff lib to compute the smallest difference between two hashes. ")
    (home-page
      "https://github.com/liufengyun/hashdiff")
    (license license:expat)))

(define-public ruby-crack
  (package
    (name "ruby-crack")
    (version "0.4.3")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "crack" version))
        (sha256
          (base32
            "0abb0fvgw00akyik1zxnq7yv391va148151qxdghnzngv66bl62k"))))
    (build-system ruby-build-system)
    (arguments
     ;; No Rakefile
     `(#:tests? #f))
    (propagated-inputs
      `(("ruby-safe-yaml" ,ruby-safe-yaml)))
    (synopsis
      "Really simple JSON and XML parsing, ripped from Merb and Rails.")
    (description
      "Really simple JSON and XML parsing, ripped from Merb and Rails.")
    (home-page "http://github.com/jnunemaker/crack")
    (license license:expat)))

(define-public ruby-webmock
  (package
    (name "ruby-webmock")
    (version "3.4.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "webmock" version))
        (sha256
          (base32
            "03994dxs4xayvkxqp01dd1ivhg4xxx7z35f7cxw7y2mwj3xn24ib"))))
    (build-system ruby-build-system)
    (propagated-inputs
      `(("ruby-addressable" ,ruby-addressable)
        ("ruby-crack" ,ruby-crack)
        ("ruby-hashdiff" ,ruby-hashdiff)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.")
    (description
      "WebMock allows stubbing HTTP requests and setting expectations on HTTP requests.")
    (home-page "http://github.com/bblimke/webmock")
    (license license:expat)))

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

(define-public ruby-rest-client
  (package
    (name "ruby-rest-client")
    (version "2.0.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rest-client" version))
        (sha256
          (base32
            "1hzcs2r7b5bjkf2x2z3n8z6082maz0j8vqjiciwgg3hzb63f958j"))))
    (build-system ruby-build-system)
    (arguments
     ;; Cyclic dependency on rubocop
     `(#:tests? #f))
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (add-before 'build 'fix-dependency-version
       ;    (lambda _
       ;      (substitute* "rest-client.gemspec"
       ;        (("webmock>.freeze, \\[\"~> 2.0")
       ;         "webmock>.freeze, [\"~> 3.0"))
       ;      #t)))))
    (propagated-inputs
      `(("ruby-http-cookie" ,ruby-http-cookie)
        ("ruby-mime-types" ,ruby-mime-types)
        ("ruby-netrc" ,ruby-netrc)))
    ;(native-inputs
    ;  `(("bundler" ,bundler)
    ;    ("ruby-pry" ,ruby-pry)
    ;    ("ruby-pry-doc" ,ruby-pry-doc)
    ;    ("ruby-rspec" ,ruby-rspec)
    ;    ("ruby-webmock" ,ruby-webmock)))
    (synopsis
      "A simple HTTP and REST client for Ruby, inspired by the Sinatra microframework style of specifying actions: get, put, post, delete.")
    (description
      "This package provides a simple HTTP and REST client for Ruby, inspired by the Sinatra microframework style of specifying actions: get, put, post, delete.")
    (home-page
      "https://github.com/rest-client/rest-client")
    (license license:expat)))

(define-public ruby-simplecov-fix
  (package
    (inherit ruby-simplecov)
    (version "0.16.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "simplecov" version))
        (sha256
          (base32
            "1sfyfgf7zrp2n42v7rswkqgk3bbwk1bnsphm24y7laxv3f8z0947"))))))

(define-public ruby-ast
  (package
    (name "ruby-ast")
    (version "2.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ast" version))
        (sha256
          (base32
            "184ssy3w93nkajlz2c70ifm79jp3j737294kbc5fjw69v1w0n9x7"))))
    (build-system ruby-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependency-version
           (lambda _
             (substitute* "ast.gemspec"
               (("~> 10.0") ">= 10.0")
               (("~> 1.6.7") "~> 2.0") ; rest-client
               (("~> 1.25") ">= 3.0")) ; mime-types
             #t)))))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-bacon" ,ruby-bacon)
       ("ruby-bacon-colored-output" ,ruby-bacon-colored-output)
       ("ruby-coveralls" ,ruby-coveralls)
       ("ruby-json-pure" ,ruby-json-pure)
       ("ruby-kramdown" ,ruby-kramdown)
       ("ruby-mime-types" ,ruby-mime-types)
       ("ruby-rake" ,ruby-rake)
       ("ruby-rest-client" ,ruby-rest-client)
       ("ruby-simplecov" ,ruby-simplecov-fix)
       ("ruby-yard" ,ruby-yard)))
    (synopsis
      "A library for working with Abstract Syntax Trees.")
    (description
      "This package provides a library for working with Abstract Syntax Trees.")
    (home-page "https://whitequark.github.io/ast/")
    (license license:expat)))

(define-public ruby-powerpack
  (package
    (name "ruby-powerpack")
    (version "0.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "powerpack" version))
        (sha256
          (base32
            "1r51d67wd467rpdfl6x43y84vwm8f5ql9l9m85ak1s2sp3nc5hyv"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (synopsis
      "A few useful extensions to core Ruby classes.")
    (description
      "This package provides a few useful extensions to core Ruby classes.")
    (home-page
      "https://github.com/bbatsov/powerpack")
    (license license:expat)))

(define-public ruby-rainbow
  (package
    (name "ruby-rainbow")
    (version "3.0.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rainbow" version))
        (sha256
          (base32
            "0bb2fpjspydr6x0s8pn1pqkzmxszvkfapv0p4627mywl7ky4zkhk"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (synopsis
      "Colorize printed text on ANSI terminals")
    (description
      "Colorize printed text on ANSI terminals")
    (home-page "https://github.com/sickill/rainbow")
    (license license:expat)))

(define-public ruby-progressbar
  (package
    (name "ruby-progressbar")
    (version "1.10.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ruby-progressbar" version))
        (sha256
          (base32
            "1cv2ym3rl09svw8940ny67bav7b2db4ms39i4raaqzkf59jmhglk"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (synopsis
      "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby. The output can be customized with a flexible formatting system including: percentage, bars of various formats, elapsed time and estimated time remaining.")
    (description
      "Ruby/ProgressBar is an extremely flexible text progress bar library for Ruby.  The output can be customized with a flexible formatting system including: percentage, bars of various formats, elapsed time and estimated time remaining.")
    (home-page
      "https://github.com/jfelchner/ruby-progressbar")
    (license license:expat)))

(define-public ruby-unicode-display-width
  (package
    (name "ruby-unicode-display-width")
    (version "1.4.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "unicode-display_width" version))
        (sha256
          (base32
            "0040bsdpcmvp8w31lqi2s9s4p4h031zv52401qidmh25cgyh4a57"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests try to run "rspec", but we don't have a binary with that name...
     `(#:tests? #f))
    (synopsis
      "[Unicode 11.0.0] Determines the monospace display width of a string using EastAsianWidth.txt, Unicode general category, and other data.")
    (description
      "[Unicode 11.0.0] Determines the monospace display width of a string using EastAsianWidth.txt, Unicode general category, and other data.")
    (home-page
      "http://github.com/janlelis/unicode-display_width")
    (license license:expat)))

(define-public ruby-racc
  (package
    (name "ruby-racc")
    (version "1.4.14")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "racc" version))
        (sha256
          (base32
            "00yhs2ag7yy5v83mqvkbnhk9bvsh6mx3808k53n61ddzx446v1zl"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("ruby-hoe" ,ruby-hoe)
       ("ruby-rake-compiler" ,ruby-rake-compiler)))
    (synopsis
      "Racc is a LALR(1) parser generator.
  It is written in Ruby itself, and generates Ruby program.

  NOTE: Ruby 1.8.x comes with Racc runtime module.  You
  can run your parsers generated by racc 1.4.x out of the
  box.")
    (description
      "Racc is a LALR(1) parser generator.
  It is written in Ruby itself, and generates Ruby program.

  NOTE: Ruby 1.8.x comes with Racc runtime module.  You
  can run your parsers generated by racc 1.4.x out of the
  box.")
    (home-page
      "http://i.loveruby.net/en/projects/racc/")
    (license license:expat)))

(define-public ragel
  (package
    (name "ragel")
    (version "6.10")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://www.colm.net/files/ragel/ragel-"
                            version ".tar.gz"))
        (sha256
         (base32
          "0gvcsl62gh6sg73nwaxav4a5ja23zcnyxncdcdnqa2yjcpdnw5az"))))
    (build-system gnu-build-system)
    (home-page "https://www.colm.net/open-source/ragel/")
    (synopsis "")
    (description "")
    (license license:gpl2+)))

(define-public ruby-cliver
  (package
    (name "ruby-cliver")
    (version "0.3.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "cliver" version))
        (sha256
          (base32
            "096f4rj7virwvqxhkavy0v55rax10r4jqf8cymbvn4n631948xc7"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (synopsis
      "Assertions for command-line dependencies")
    (description
      "Assertions for command-line dependencies")
    (home-page
      "https://www.github.com/yaauie/cliver")
    (license license:expat)))

(define-public ruby-parser
  (package
    (name "ruby-parser")
    (version "2.5.1.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "parser" version))
        (sha256
          (base32
            "1zp89zg7iypncszxsjp8kiccrpbdf728jl449g6cnfkz990fyb5k"))))
    (build-system ruby-build-system)
    (propagated-inputs `(("ruby-ast" ,ruby-ast)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ragel" ,ragel)
       ("ruby-cliver" ,ruby-cliver)
       ("ruby-racc" ,ruby-racc)
       ("ruby-simplecov" ,ruby-simplecov)))
    (synopsis "A Ruby parser written in pure Ruby.")
    (description
      "This package provides a Ruby parser written in pure Ruby.")
    (home-page
      "https://github.com/whitequark/parser")
    (license license:expat)))

(define-public ruby-jaro-winkler
  (package
    (name "ruby-jaro-winkler")
    (version "1.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "jaro_winkler" version))
        (sha256
          (base32
            "0rr797nqz081bfk30m2apj5h24bg5d1jr1c8p3xwx4hbwsrbclah"))))
    (build-system ruby-build-system)
    (arguments
     ;; No Rakefile
     `(#:tests? #f))
    (synopsis
      "jaro_winkler is an implementation of Jaro-Winkler \\
  distance algorithm which is written in C extension and will fallback to pure \\
  Ruby version in platforms other than MRI/KRI like JRuby or Rubinius. Both of \\
  C and Ruby implementation support any kind of string encoding, such as \\
  UTF-8, EUC-JP, Big5, etc.")
    (description
      "jaro_winkler is an implementation of Jaro-Winkler \\
  distance algorithm which is written in C extension and will fallback to pure \\
  Ruby version in platforms other than MRI/KRI like JRuby or Rubinius.  Both of \\
  C and Ruby implementation support any kind of string encoding, such as \\
  UTF-8, EUC-JP, Big5, etc.")
    (home-page
      "https://github.com/tonytonyjan/jaro_winkler")
    (license license:expat)))

(define-public ruby-rubocop
  (package
    (name "ruby-rubocop")
    (version "0.58.2")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rubocop" version))
        (sha256
          (base32
            "0fc1fw9z98qd91ipsh9hdvpcb401qvkhw518s35l8a67sv4vdnj3"))))
    (build-system ruby-build-system)
    (arguments
     ;; No Rakefile
     `(#:tests? #f))
    (propagated-inputs
      `(("ruby-jaro-winkler" ,ruby-jaro-winkler)
        ("ruby-parallel" ,ruby-parallel)
        ("ruby-parser" ,ruby-parser)
        ("ruby-powerpack" ,ruby-powerpack)
        ("ruby-rainbow" ,ruby-rainbow)
        ("ruby-progressbar" ,ruby-progressbar)
        ("ruby-unicode-display-width"
         ,ruby-unicode-display-width)))
    (synopsis
      "    Automatic Ruby code style checking tool.
    Aims to enforce the community-driven Ruby Style Guide.
")
    (description
      "    Automatic Ruby code style checking tool.
    Aims to enforce the community-driven Ruby Style Guide.
")
    (home-page "http://www.rubocop.org/")
    (license license:expat)))
