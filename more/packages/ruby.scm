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
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-i18n-fix
  (package
    (inherit ruby-i18n)
    (version "0.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "i18n" version))
              (sha256
               (base32
                "0h5wygnbpxas8kwhqkwk6n4s334dxyxvlxykc6mxfndb0m56166r"))))
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
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "public_suffix" version))
              (sha256
               (base32
                "0snaj1gxfib4ja1mvy3dzmi7am73i0mkqr0zkz045qv6509dhj5f"))))
    (build-system ruby-build-system)
    (arguments
     ;; Tests require network
     `(#:tests? #f))
    (home-page "https://simonecarletti.com/code/publicsuffix-ruby/")
    (synopsis "")
    (description "")
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
    (synopsis "")
    (description "")
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
    (home-page "https://github.com/octopress/colorator")
    (synopsis "")
    (description "")
    (license license:expat)))

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
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-sass
  (package
    (name "ruby-sass")
    (version "3.5.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "sass" version))
              (sha256
               (base32
                "0p0192bzpimw9wc4ld29sdx56s4qyf1wg6c89p3k9jqch0ikyri6"))))
    (build-system ruby-build-system)
    (propagated-inputs
     `(("sass-listen" ,ruby-sass-listen)))
    (arguments
     ;; FIXME: tests fail
     `(#:tests? #f))
    (home-page "http://sass-lang.com/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-jekyll-sass-converter
  (package
    (name "ruby-jekyll-sass-converter")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-sass-converter" version))
              (sha256
               (base32
                "01m921763yfgx1gc33k5ixqz623f4c4azgnpqhgsc2q61fyfk3q1"))))
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
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-watch" version))
              (sha256
               (base32
                "02rg3wi95w2l0bg1igl5k6pza723vn2b2gj975gycz1cpmhdjn6z"))))
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
    (version "1.12.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "parallel" version))
              (sha256
               (base32
                "0qv2yj4sxr36ga6xdxvbq9h05hn10bwcbkqv6j6q1fiixhsdnnzd"))))
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
    ;; FIXME: There is actually no license!
    (license license:public-domain)))

(define-public ruby-pdf-reader
  (package
    (name "ruby-pdf-reader")
    (version "2.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-reader" version))
              (sha256
               (base32
                "0nlammdpjy3padmzxhsql7mw31jyqp88n6bdffiarv5kzl4s3y7p"))))
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
    (version "0.7.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pdf-core" version))
              (sha256
               (base32
                "19llwch2wfg51glb0kff0drfp3n6nb9vim4zlvzckxysksvxpby1"))))
    (build-system ruby-build-system)
    (native-inputs
     `(("bundler" ,bundler)
       ("pdf-inspector" ,ruby-pdf-inspector)
       ("pdf-reader" ,ruby-pdf-reader)
       ("rspec" ,ruby-rspec)))
    (arguments
     `(#:tests? #f; TODO: rubocop
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
    (version "0.9.9")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "yard" version))
              (sha256
               (base32
                "19x60ay7fqkmc7h7mk751wx9v3bkjpwkw8cs8bal9r0xpcs4zrxa"))))
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
       ("pdf-inspector" ,ruby-pdf-inspector)
       ("rspec" ,ruby-rspec)
       ("yard" ,ruby-yard)))
    (arguments `(#:tests? #f)); TODO: Requires rubocop and cov
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
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "kramdown" version))
              (sha256
               (base32
                "12k1dayq3dh20zlllfarw4nb6xf36vkd5pb41ddh0d0lndjaaf5f"))))
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
     `(("bundler" ,bundler)))
    (arguments `(#:tests? #f)); TODO: requires luna/rubocop
    (home-page "http://github.com/envygeeks/forwardable-extended")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-pathutil
  (package
    (name "ruby-pathutil")
    (version "0.16.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "pathutil" version))
              (sha256
               (base32
                "17ipzhp1zmb0shskgvcrkpgicv499cb3nd5g4r2qaj9j2cf12b6l"))))
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
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "rouge" version))
              (sha256
               (base32
                "1in49lnhck46amlg6sxaiv2irl9w3r032agnsdlziaxy48ddk9h1"))))
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
    (version "3.5.6")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "hashie" version))
              (sha256
               (base32
                "120mkd2hkwhcfj7avi1dphb0lm7wx364d1cjm9yr4fibqpvsgqi7"))))
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

(define-public ruby-jekyll-paginate-v2
  (package
    (name "ruby-jekyll-paginate-v2")
    (version "1.9.0")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll-paginate-v2" version))
              (sha256
               (base32
                "0hkl5vfgrz4p0j5dgji0rhnv081y4mrvsy2lnnzl0zv9rd5gqsn7"))))
    (build-system ruby-build-system)
    (arguments `(#:tests? #f)); Cyclic dependencies?
    (home-page "https://github.com/sverrirs/jekyll-paginate-v2")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public ruby-jekyll
  (package
    (name "ruby-jekyll")
    (version "3.6.2")
    (source (origin
              (method url-fetch)
              (uri (rubygems-uri "jekyll" version))
              (sha256
               (base32
                "0rgdml6ypwwxrwv4dk2r8v9vp0ch3c060f6svhxggvk31w9k5lki"))))
    (build-system ruby-build-system)
    (arguments
     ;; No rakefile, but a test subdirectory
     `(#:tests? #f))
    (propagated-inputs
     `(("addressable" ,ruby-addressable)
       ("colorator" ,ruby-colorator)
       ("jekyll-sass-converter" ,ruby-jekyll-sass-converter)
       ("jekyll-watch" ,ruby-jekyll-watch)
       ("kramdown" ,ruby-kramdown)
       ("liquid" ,ruby-liquid)
       ("mercenary" ,ruby-mercenary)
       ("pathutil" ,ruby-pathutil)
       ("rouge" ,ruby-rouge-2)
       ("safe-yaml" ,ruby-safe-yaml)))
    (home-page "https://jekyllrb.com/")
    (synopsis "Static site generator")
    (description "")
    (license license:expat)))
