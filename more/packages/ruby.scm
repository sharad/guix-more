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

(define-public ruby-concurrent
  (package
    (name "ruby-concurrent")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "concurrent-ruby" version))
              (sha256
               (base32
                "183lszf5gx84kcpb779v6a2y0mx9sssy8dgppng1z9a505nj1qcf"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "http://www.concurrent-ruby.com/")
    (synopsis "Concurrency tools for Ruby")
    (description "This gem provides concurrency tools for Ruby.  Its design goals are:
@itemize
@item Be an 'unopinionated' toolbox that provides useful utilities without
      debating which is better or why
@item Remain free of external gem dependencies
@item Stay true to the spirit of the languages providing inspiration
@item But implement in a way that makes sense for Ruby
@item Keep the semantics as idiomatic Ruby as possible
@item Support features that make sense in Ruby
@item Exclude features that don't make sense in Ruby
@item Be small, lean, and loosely coupled
@item Thread-safety
@item Backward compatibility
@end itemize")
    (license license:expat)))

(define-public ruby-i18n-fix
  (package
    (inherit ruby-i18n)
    ;(version "0.9.0")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "i18n" version))
              (sha256
               (base32
                ;"0h5wygnbpxas8kwhqkwk6n4s334dxyxvlxykc6mxfndb0m56166r"))))
                "0ppvmla21hssvrfm8g1n2fnb4lxn4yhy9qmmba0imanflgldrjmr"))))
    (propagated-inputs
     `(("concurrent-ruby" ,ruby-concurrent)))))

(define-public ruby-ttfunk-fix
  (package
    (inherit ruby-ttfunk)
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "ttfunk" version))
              (sha256
               (base32
                "1mgrnqla5n51v4ivn844albsajkck7k6lviphfqa8470r46c58cd"))))
    (arguments '(#:tests? #f)))); No rakefile

(define-public ruby-public-suffix
  (package
    (name "ruby-public-suffix")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "public_suffix" version))
              (sha256
               (base32
                "08q64b5br692dd3v0a9wq9q5dvycc6kmiqmjbdxkxbfizggsvx6l"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require network
     `(#:tests? #f))
    (home-page "https://simonecarletti.com/code/publicsuffix-ruby/")
    (synopsis "Domain name parser")
    (description "The gem @code{public_suffix} is a domain name parser,
written in Ruby, and based on the @dfn{Public Suffix List}.  A public suffix
is one under which Internet users can (or historically could) directly
register names.  Some examples of public suffixes are @code{.com},
@code{.co.uk} and @code{pvt.k12.ma.us}.  The Public Suffix List is a list of
all known public suffixes.")
    (license license:expat)))

(define-public ruby-addressable
  (package
    (name "ruby-addressable")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "addressable" version))
              (sha256
               (base32
                "0viqszpkggqi8hq87pqp0xykhvz60g99nwmkwsb0v45kc2liwxvk"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("public_suffix" ,ruby-public-suffix)))
    (arguments
     ;; No test target
     `(#:tests? #f))
    (home-page "https://github.com/sporkmonger/addressable")
    (synopsis "Alternative URI implementation")
    (description "Addressable is a replacement for the URI implementation that
is part of Ruby's standard library.  It more closely conforms to RFC 3986,
RFC 3987, and RFC 6570 (level 4), providing support for IRIs and URI templates.")
    (license license:asl2.0)))

(define-public ruby-colorator
  (package
    (name "ruby-colorator")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "colorator" version))
              (sha256
               (base32
                "0f7wvpam948cglrciyqd798gdc6z3cfijciavd0dfixgaypmvy72"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (home-page "http://octopress.org/colorator/")
    (synopsis "Terminal color library")
    (description "Colorator is a Ruby gem that helps you colorize your text
for the terminal.")
    (license license:expat)))

(define-public ruby-colored
  (package
    (name "ruby-colored")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "colored" version))
              (sha256
               (base32
                "0f7wvpam948cglrciyqd798gdc6z3cfijciavd0dfixgaypmvy72"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (home-page "https://github.com/defunkt/colored")
    (synopsis "Terminal color library")
    (description "Colored is a Ruby gem that helps you colorize your text
for the terminal.  Warning: this package is unmaintained!")
    (license license:expat)))

(define-public ruby-command-line-reporter
  (package
    (name "ruby-command-line-reporter")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "command_line_reporter" version))
              (sha256
               (base32
                "1qma35xrb772whxwy1rs9bicb9d6lvz0s2dd2dnn4fr6zcbcxc0a"))))
    (build-system ruby-build-system)
    (arguments
     ;; No Rakefile
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-dependency
           (lambda _
             (substitute* ".gemspec"
               (("colored") "colorator")
               (("= 1.2") "= 1.1"))
             #t)))))
    (propagated-inputs
     `(("ruby-colorator" ,ruby-colorator)))
    (home-page "https://github.com/wbailey/command_line_reporter")
    (synopsis "Report production while executing Ruby scripts")
    (description "This gem provides a DSL that makes it easy to write reports
of various types in ruby.  It eliminates the need to litter your source with
puts statements, instead providing a more readable, expressive interface to
your application.")
    (license license:asl2.0)))

(define-public ruby-command-line-reporter-3
  (package
    (inherit ruby-command-line-reporter)
    (version "3.3.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "command_line_reporter" version))
              (sha256
               (base32
                "1h39zqqxp3k4qk49ajpx0jps1vmvxgkh43mqkb6znk583bl0fv71"))))))

(define-public ruby-rdoc
  (package
    (name "ruby-rdoc")
    (version "6.0.4")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "rdoc" version))
        (sha256
          (base32
            "0anv42cqcdc6g4n386mrva7mgav5i0c2ry3yzvzzc6z6hymkmcr7"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (synopsis "HTML and command-line documentation utility.")
    (description "RDoc produces HTML and command-line documentation for Ruby
projects.  RDoc includes the +rdoc+ and +ri+ tools for generating and displaying
documentation from the command-line.")
    (home-page "https://ruby.github.io/rdoc")
    (license license:gpl2+)))

(define-public ruby-sass-listen
  (package
    (name "ruby-sass-listen")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sass-listen" version))
              (sha256
               (base32
                "0xw3q46cmahkgyldid5hwyiwacp590zj2vmswlll68ryvmvcp7df"))))
    (build-system ruby-build-system)
    (arguments
     ;; No test target
     `(#:tests? #f))
    (propagated-inputs
     `(("rb-fsevent" ,ruby-rb-fsevent)
       ("rb-inotify" ,ruby-rb-inotify)))
    (home-page "https://github.com/sass/listen")
    (synopsis "File modification notification library")
    (description "The Listen gem listens to file modifications and notifies you
about the changes.")
    (license license:expat)))

(define-public ruby-terminfo
  (package
    (name "ruby-terminfo")
    (version "0.1.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "ruby-terminfo" version))
        (sha256
          (base32
            "0rl4ic5pzvrpgd42z0c1s2n3j39c9znksblxxvmhkzrc0ckyg2cm"))))
    (build-system ruby-build-system)
    (arguments
     `(#:test-target "test"
       #:tests? #f))
    (inputs
     `(("ncurses" ,ncurses)))
    (native-inputs
     `(("ruby-rubygems-tasks" ,ruby-rubygems-tasks)
       ("ruby-rdoc" ,ruby-rdoc)))
    (synopsis "terminfo binding for Ruby")
    (description "ruby-terminfo provides terminfo binding for Ruby.")
    (home-page "http://ruby-terminfo.rubyforge.org")
    (license license:bsd-3)))

(define-public ruby-diffy
  (package
    (name "ruby-diffy")
    (version "3.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "diffy" version))
        (sha256
          (base32
            "119imrkn01agwhx5raxhknsi331y5i4yda7r0ws0an6905ximzjg"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     `(("rspec" ,ruby-rspec)))
    (synopsis "Convenient diffing in ruby")
    (description "Diffy provides a convenient way to generate a diff from two
strings or files.")
    (home-page "http://github.com/samg/diffy")
    (license license:expat)))

(define-public ruby-sass-spec
  (package
    (name "ruby-sass-spec")
    (version "3.5.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sass/sass-spec/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0nx8lp7c9qa58w489crgqa3c489xsyarn1a8h4np9mwwfqm1h3rr"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-command-line-reporter-3" ,ruby-command-line-reporter-3)
       ("ruby-diffy" ,ruby-diffy)
       ("ruby-terminfo" ,ruby-terminfo)))
    (arguments
     ;; No Rakefile
     `(#:tests? #f))
    (home-page "https://github.com/sass/sass-spec")
    (synopsis "Test suite for Sass")
    (description "Sass Spec is a test suite for Sass.  Test cases are all in
the @file{spec} directory.")
    (license license:expat)))

(define-public ruby-sass
  (package
    (name "ruby-sass")
    (version "3.5.7")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sass" version))
              (sha256
               (base32
                "1sy7xsbgpcy90j5ynbq967yplffp74pvph3r8ivn2sv2b44q6i61"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("sass-listen" ,ruby-sass-listen)))
    (native-inputs
     `(("ruby-sass-spec" ,ruby-sass-spec)))
    (home-page "http://sass-lang.com/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-jekyll-sass-converter
  (package
    (name "ruby-jekyll-sass-converter")
    (version "1.5.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-sass-converter" version))
              (sha256
               (base32
                "008ikh5fk0n6ri54mylcl8jn0mq8p2nfyfqif2q3pp0lwilkcxsk"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("sass" ,ruby-sass)))
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/jekyll/jekyll-sass-converter")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-listen-3.0
  (package
    (inherit ruby-listen)
    (version "3.0.8")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "listen" version))
              (sha256
               (base32
                "1l0y7hbyfiwpvk172r28hsdqsifq1ls39hsfmzi1vy4ll0smd14i"))))))

(define-public ruby-jekyll-watch
  (package
    (name "ruby-jekyll-watch")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-watch" version))
              (sha256
               (base32
                "0m7scvj3ki8bmyx5v8pzibpg6my10nycnc28lip98dskf8iakprp"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-listen" ,ruby-listen-3.0)))
    (arguments
     ;; No rakefile
     `(#:tests? #f))
    (home-page "https://github.com/jekyll/jekyll-watch")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-parallel
  (package
    (name "ruby-parallel")
    (version "1.12.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "parallel" version))
              (sha256
               (base32
                "01hj8v1qnyl5ndrs33g8ld8ibk0rbcqdpkpznr04gkbxd11pqn67"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/grosser/parallel")
    (arguments `(#:tests? #f)); No rakefile
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-cane
  (package
    (name "ruby-cane")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "cane" version))
              (sha256
               (base32
                "0yf5za3l7lhrqa3g56sah73wh33lbxy5y3cb7ij0a2bp1b4kwhih"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "http://github.com/square/cane")
    (propagated-inputs
     `(("parallel" ,ruby-parallel)))
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public ruby-morecane
  (package
    (name "ruby-morecane")
    (version "0.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "morecane" version))
              (sha256
               (base32
                "0w70vb8z5bdhvr21h660aa43m5948pv0bd27z7ngai2iwdvqd771"))))
    (build-system ruby-build-system)
    (home-page "https://github.com/yob/morecane")
    (arguments `(#:tests? #f)); No rakefile
    (propagated-inputs
     `(("parallel" ,ruby-parallel)))
    (synopsis "")
    (description "")
    (license license:expat)))

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

(define-public ruby-pdf-reader
  (package
    (name "ruby-pdf-reader")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-reader" version))
              (sha256
               (base32
                "1b3ig4wpcgdbqa7yw0ahwbmikkkywn2a22bfmrknl5ls7g066x45"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("rspec" ,ruby-rspec)
       ("cane" ,ruby-cane)
       ("morecane" ,ruby-morecane)))
    (propagated-inputs
     `(("afm" ,ruby-afm)
       ("ascii85" ,ruby-ascii85)
       ("hashery" ,ruby-hashery)
       ("ruby-rc4" ,ruby-rc4)
       ("ttfunk" ,ruby-ttfunk-fix)))
    (arguments `(#:test-target "spec"))
    (home-page "http://github.com/yob/pdf-reader")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public ruby-pdf-inspector
  (package
    (name "ruby-pdf-inspector")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-inspector" version))
              (sha256
               (base32
                "1g853az4xzgqxr5xiwhb76g4sqmjg4s79mm35mp676zjsrwpa47w"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("pdf-reader" ,ruby-pdf-reader)))
    (arguments `(#:tests? #f)); No rakefile
    (home-page "https://github.com/prawnpdf/pdf-inspector")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public ruby-pdf-core
  (package
    (name "ruby-pdf-core")
    (version "0.8.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-core" version))
              (sha256
               (base32
                "15d6m99bc8bbzlkcg13qfpjjzphfg5x905pjbfygvpcxsm8gnsvg"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-pdf-inspector" ,ruby-pdf-inspector)
       ("ruby-pdf-reader" ,ruby-pdf-reader)
       ("ruby-rspec" ,ruby-rspec)))
    (arguments
     `(#:tests? #f; Cyclic dependency on rubocop
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-pdf-reader-dep
           (lambda _
             (substitute* "pdf-core.gemspec"
               (("~> 1.2") "~> 2.0")
               (("~> 1.1.0") "~> 1.3.0"))
             #t)))))
    (home-page "http://prawn.majesticseacreature.com/")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public ruby-yard
  (package
    (name "ruby-yard")
    (version "0.9.16")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "yard" version))
              (sha256
               (base32
                "0lmmr1839qgbb3zxfa7jf5mzy17yjl1yirwlgzdhws4452gqhn67"))))
    (build-system ruby-build-system)
    (arguments `(#:test-target "spec"))
    (home-page "http://yardoc.org/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-prawn
  (package
    (name "ruby-prawn")
    (version "2.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prawn" version))
              (sha256
               (base32
                "1qdjf1v6sfl44g3rqxlg8k4jrzkwaxgvh2l4xws97a8f3xv4na4m"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("pdf-core" ,ruby-pdf-core)
       ("ttfunk" ,ruby-ttfunk-fix)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-pdf-inspector" ,ruby-pdf-inspector)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-simplecove" ,ruby-simplecov)
       ("ruby-yard" ,ruby-yard)))
    (arguments `(#:tests? #f)); no tests
    (home-page "http://prawnpdf.org/")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public ruby-prawn-table
  (package
    (name "ruby-prawn-table")
    (version "0.2.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "prawn-table" version))
              (sha256
               (base32
                "1nxd6qmxqwl850icp18wjh5k0s3amxcajdrkjyzpfgq0kvilcv9k"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("prawn" ,ruby-prawn)))
    (arguments `(#:tests? #f)); No rakefile
    (home-page "https://github.com/prawnpdf/prawn-table")
    (synopsis "")
    (description "")
    (license license:gpl3+)))

(define-public ruby-kramdown
  (package
    (name "ruby-kramdown")
    (version "1.17.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "kramdown" version))
              (sha256
               (base32
                "1n1c4jmrh5ig8iv1rw81s4mw4xsp4v97hvf8zkigv4hn5h542qjq"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("prawn" ,ruby-prawn)
       ("prawn-table" ,ruby-prawn-table)))
    (arguments `(#:tests? #f)); FIXME: failure
    (home-page "http://kramdown.gettalong.org/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-liquid
  (package
    (name "ruby-liquid")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "liquid" version))
              (sha256
               (base32
                "17fa0jgwm9a935fyvzy8bysz7j5n1vf1x2wzqkdfd5k08dbw3x2y"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "http://www.liquidmarkup.org/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-mercenary
  (package
    (name "ruby-mercenary")
    (version "0.3.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "mercenary" version))
              (sha256
               (base32
                "10la0xw82dh5mqab8bl0dk21zld63cqxb1g16fk8cb39ylc4n21a"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (arguments `(#:test-target "spec"))
    (home-page "https://github.com/jekyll/mercenary")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-forwardable-extended
  (package
    (name "ruby-forwardable-extended")
    (version "2.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "forwardable-extended" version))
              (sha256
               (base32
                "15zcqfxfvsnprwm8agia85x64vjzr2w0xn9vxfnxzgcv8s699v0v"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)
       ("ruby-rubocop" ,ruby-rubocop)))
    (arguments `(#:tests? #f)); Cyclic dependency on luna-rspec-formatters
    (home-page "http://github.com/envygeeks/forwardable-extended")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-pathutil
  (package
    (name "ruby-pathutil")
    (version "0.16.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pathutil" version))
              (sha256
               (base32
                "0wc18ms1rzi44lpjychyw2a96jcmgxqdvy2949r4vvb5f4p0lgvz"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("forwardable-extended" ,ruby-forwardable-extended)))
    (native-inputs
     `(("bundler" ,bundler)
       ("rspec" ,ruby-rspec)))
    (arguments `(#:tests? #f)); cannot load such file -- /tmp/guix-build-ruby-pathutil-0.16.0.drv-0/gem/benchmark/support/task
    (home-page "http://github.com/envygeeks/pathutil")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-rouge
  (package
    (name "ruby-rouge")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rouge" version))
              (sha256
               (base32
                "0h79gn2wmn1wix2d27lgiaimccyj8gvizrllyym500pir408x62f"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); No rakefile
    (home-page "http://rouge.jneen.net/")
    (synopsis "")
    (description "")
    (license (list
               ;; rouge is licensed under expat
               license:expat
               ;; pygments is licensed under bsd-2
               license:bsd-2))))

(define-public ruby-rouge-2
  (package
    (inherit ruby-rouge)
    (version "2.2.1")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rouge" version))
              (sha256
               (base32
                "02kpahk5nkc33yxnn75649kzxaz073wvazr2zyg491nndykgnvcs"))))))

(define-public ruby-hashie
  (package
    (name "ruby-hashie")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hashie" version))
              (sha256
               (base32
                "13bdzfp25c8k51ayzxqkbzag3wj5gc1jd8h7d985nsq6pn57g5xh"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)))
    (arguments `(#:tests? #f)); FIXME: Could not locate Gemfile or .bundle/ directory
    (home-page "https://github.com/intridea/hashie")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-heredoc-unindent
  (package
    (name "ruby-heredoc-unindent")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "heredoc_unindent" version))
              (sha256
               (base32
                "14ijr2fsjwhrkjkcaz81d5xnfa4vvgvcflrff83avqw9klm011yw"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("hoe" ,ruby-hoe)))
    (home-page "https://github.com/adrianomitre/heredoc_unindent")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-safe-yaml
  (package
    (name "ruby-safe-yaml")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "safe_yaml" version))
              (sha256
               (base32
                "1hly915584hyi9q9vgd968x2nsi5yag9jyf5kq60lwzi5scr7094"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("rspec" ,ruby-rspec)
       ("hashie" ,ruby-hashie)
       ("heredoc_unindent" ,ruby-heredoc-unindent)))
    (arguments `(#:test-target "spec"
                 #:tests? #f));; FIXME: one failure only
    (home-page "https://github.com/dtao/safe_yaml")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-http-parser.rb
  (package
    (name "ruby-http-parser.rb")
    (version "0.6.0")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "http_parser.rb" version))
        (sha256
          (base32
            "15nidriy0v5yqfjsgsra51wmknxci2n2grliz78sf9pga3n0l7gi"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (native-inputs
     `(("ruby-rake-compiler" ,ruby-rake-compiler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis
      "Ruby bindings to http://github.com/ry/http-parser and http://github.com/a2800276/http-parser.java")
    (description
      "Ruby bindings to http://github.com/ry/http-parser and http://github.com/a2800276/http-parser.java")
    (home-page
      "http://github.com/tmm1/http_parser.rb")
    (license license:expat)))

(define-public ruby-em-websocket
  (package
    (name "ruby-em-websocket")
    (version "0.5.1")
    (source
      (origin
        (method url-fetch)
        (uri (rubygems-uri "em-websocket" version))
        (sha256
          (base32
            "1bsw8vjz0z267j40nhbmrvfz7dvacq4p0pagvyp17jif6mj6v7n3"))))
    (build-system ruby-build-system)
    (arguments
     ;; No tests
     `(#:tests? #f))
    (propagated-inputs
      `(("ruby-eventmachine" ,ruby-eventmachine)
        ("ruby-http-parser.rb" ,ruby-http-parser.rb)))
    (native-inputs
     `(("bundler" ,bundler)
       ("ruby-rspec" ,ruby-rspec)))
    (synopsis "EventMachine based WebSocket server")
    (description
      "EventMachine based WebSocket server")
    (home-page
      "http://github.com/igrigorik/em-websocket")
    (license #f)))

(define-public ruby-jekyll
  (package
    (name "ruby-jekyll")
    (version "3.8.3")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll" version))
              (sha256
               (base32
                "1iw90wihk9dscgmppf5v6lysg3kjmnx50mjyl4gghkdb4spw97xk"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile, but a test subdirectory
     `(#:tests? #f))
    (propagated-inputs
     `(("ruby-addressable" ,ruby-addressable)
       ("ruby-colorator" ,ruby-colorator)
       ("ruby-em-websocket" ,ruby-em-websocket)
       ("ruby-i18n" ,ruby-i18n-fix)
       ("ruby-jekyll-sass-converter" ,ruby-jekyll-sass-converter)
       ("ruby-jekyll-watch" ,ruby-jekyll-watch)
       ("ruby-kramdown" ,ruby-kramdown)
       ("ruby-liquid" ,ruby-liquid)
       ("ruby-mercenary" ,ruby-mercenary)
       ("ruby-pathutil" ,ruby-pathutil)
       ("ruby-rouge" ,ruby-rouge-2)
       ("ruby-safe-yaml" ,ruby-safe-yaml)))
    (home-page "https://jekyllrb.com/")
    (synopsis "Static site generator")
    (description "Jekyll is a simple, blog aware, static site generator.")
    (license license:expat)))

(define-public ruby-jekyll-paginate-v2
  (package
    (name "ruby-jekyll-paginate-v2")
    (version "1.9.4")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-paginate-v2" version))
              (sha256
               (base32
                "01fgy8mg4kp5pi12vrmk3z743h005n1qyxk2ajirgyhg0qpvml1p"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("ruby-jekyll" ,ruby-jekyll)))
    ;(arguments `(#:tests? #f))
    (home-page "https://github.com/sverrirs/jekyll-paginate-v2")
    (synopsis "")
    (description "")
    (license license:expat)))
