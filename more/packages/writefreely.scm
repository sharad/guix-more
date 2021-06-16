(define-module (more packages writefreely)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system go)
  #:use-module (guix licenses)
  #:use-module (guix git-download)
  #:use-module (gnu packages golang)
  #:use-module (gnu packages syncthing))

(define-public go-github.com-go-sql-driver-mysql
  (package
   (name "go-github.com-go-sql-driver-mysql")
   (version "1.4.1")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/go-sql-driver/mysql")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1fvsvwc1v2i0gqn01mynvi1shp5xm0xaym6xng09fcbqb56lbjx1"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/go-sql-driver/mysql"))
   (synopsis "")
   (description "")
   (home-page "https://github.com/go-sql-driver/mysql")
   (license mpl2.0)))

(define-public go-github.com-dustin-go-humanize
  (package
   (name "go-github.com-dustin-go-humanize")
   (version "1.0.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/dustin/go-humanize")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1kqf1kavdyvjk7f8kx62pnm7fbypn9z1vbf8v2qdh3y7z7a0cbl3"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/dustin/go-humanize"))
   (synopsis "")
   (description "")
   (home-page "https://github.com/dustin/go-humanize")
   (license expat)))

(define-public go-github.com-shurcool-sanitized-anchor-name
  (package
   (name "go-github.com-shurcool-sanitized-anchor-name")
   (version "1.0.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/shurcooL/sanitized_anchor_name")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1gv9p2nr46z80dnfjsklc6zxbgk96349sdsxjz05f3z6wb6m5l8f"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/shurcooL/sanitized_anchor_name"))
   (synopsis "")
   (description "")
   (home-page "https://github.com/shurcooL/sanitized_anchor_name")
   (license expat)))

(define-public go-github.com-writeas-saturday
  (package
   (name "go-github.com-writeas-saturday")
   (version "1.7.1")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/writeas/saturday")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1hzcmf532cvnm15r9p55wkh4jlqv0wgg0fddn33ls75i0z9d70xz"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/writeas/saturday"
      ;; Let's not run tests for now as they seem to be quite expensive
      #:tests? #f))
   (propagated-inputs
    `(("go-github.com-shurcool-sanitized-anchor-name" ,go-github.com-shurcool-sanitized-anchor-name)))
   (synopsis "")
   (description "")
   (home-page "https://github.com/writeas/saturday")
   (license bsd-2)))

(define-public go-github.com-writeas-nerds
  (package
   (name "go-github.com-writeas-nerds")
   (version "1.0.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/writeas/nerds")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0y51gjc2zj3a9k77i66xx3rk59sh1df9cg88g0gigfrpvwz1mc1a"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/writeas/nerds"))
   (propagated-inputs
    `(("go-github.com-go-sql-driver-mysql" ,go-github.com-go-sql-driver-mysql)))
   (synopsis "")
   (description "")
   (home-page "https://github.com/writeas/nerds")
   (license expat)))

(define-public go-github.com-fatih-color
  (package
   (name "go-github.com-fatih-color")
   (version "1.7.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/fatih/color")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0v8msvg38r8d1iiq2i5r4xyfx0invhc941kjrsg5gzwvagv55inv"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/fatih/color"))
   (synopsis "")
   (description "")
   (home-page "https://github.com/fatih/color")
   (license expat)))

(define-public go-github.com-fatih-structs
  (package
   (name "go-github.com-fatih-structs")
   (version "1.1.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/fatih/structs")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "1wrhb8wp8zpzggl61lapb627lw8yv281abvr6vqakmf569nswa9q"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/fatih/structs"))
   (synopsis "")
   (description "")
   (home-page "https://github.com/fatih/structs")
   (license expat)))

(define-public go-github.com-nu7hatch-gouuid
  (let ((commit "179d4d0c4d8d407a32af483c2354df1d2c91e6c3")
        (revision "1"))
    (package
     (name "go-github.com-nu7hatch-gouuid")
     (version (git-version "0.0.0" revision commit))
     (source (origin
          (method git-fetch)
          (uri (git-reference
            (url "https://github.com/nu7hatch/gouuid")
            (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1isyfix5w1wm26y3a15ha3nnpsxqaxz5ngq06hnh6c6y0inl2fwj"))))
     (build-system go-build-system)
     (arguments
      `(#:import-path "github.com/nu7hatch/gouuid"
        ;; fails with go 1.10
        ;; Is it OK?
        #:tests? #f))
     (synopsis "")
     (description "")
     (home-page "https://github.com/nu7hatch/gouuid")
     (license expat))))

(define-public go-github.com-writeas-openssl-go
  (package
   (name "go-github.com-writeas-openssl-go")
   (version "1.0.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/writeas/openssl-go")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "10bncxgjg2a6x3qgg8gs1jg2pqdaca40v47zvshh8rh5zr2cmvyk"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/writeas/openssl-go"))
   (synopsis "")
   (description "")
   (home-page "https://github.com/writeas/openssl-go")
   ;; XXX: NO LICENSE!
   (license #f)))

(define-public go-github.com-microcosm-cc-bluemonday
  (package
   (name "go-github.com-microcosm-cc-bluemonday")
   (version "1.0.2")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/microcosm-cc/bluemonday")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0j0aylsxqjcj49w7ph8cmpaqjlpvg7mb5mrcrd9bg71dlb9z9ir2"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/microcosm-cc/bluemonday"
      ;; FIXME: one failure
      #:tests? #f))
   (propagated-inputs
    `(("go-golang-org-x-net-html" ,go-golang-org-x-net-html)))
   (synopsis "")
   (description "")
   (home-page "https://github.com/microcosm-cc/bluemonday")
   (license bsd-3)))

(define-public go-gopkg-in-fatih-set-v0
  (let ((commit "2c768e3c5489976167bfc42b5c7c92ca783f4389")
        (revision "0"))
    (package
      (name "go-gopkg-in-fatih-set-v0")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/fatih/set.v0.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1vif67ijhsm3p2613jl51fshlrn4d5pncrly73jvfyc8nsss8i9x"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/fatih/set.v0"))
      (home-page "https://gopkg.in/fatih/set.v0")
      (synopsis "")
      (description "")
      (license asl2.0))))

(define-public go-gopkg-in-bufio-v1
  (let ((commit "567b2bfa514e796916c4747494d6ff5132a1dfce")
        (revision "0"))
    (package
      (name "go-gopkg-in-bufio-v1")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/bufio.v1.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1z5pj778hdianlfj14p0d67g69v4gc2kvn6jg27z5jf75a88l19b"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/bufio.v1"))
      (home-page "https://gopkg.in/bufio.v1")
      (synopsis "")
      (description "")
      (license bsd-2))))

(define-public go-gopkg-in-redis-v2
  (let ((commit "e6179049628164864e6e84e973cfb56335748dea")
        (revision "0"))
    (package
      (name "go-gopkg-in-redis-v2")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/redis.v2.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "02hifpgak39y39lbn7v2ybbpk3rmb8nvmb3h3490frr8s4pfkb8h"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/redis.v2"
         ;; tests are running forever
         #:tests? #f))
      (propagated-inputs
       `(("go-gopkg-in-bufio-v1" ,go-gopkg-in-bufio-v1)))
      (native-inputs
       `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
      (home-page "https://gopkg.in/redis.v2")
      (synopsis "")
      (description "")
      (license bsd-2))))

(define-public go-gopkg-in-macaron-v1
  (let ((commit "dfcb80ca86e8534962c62812efd93209c7e600e7")
        (revision "0"))
    (package
      (name "go-gopkg-in-macaron-v1")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/macaron.v1.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1s5ba96sqdb6mcac7gmm801qy5lkx2dqg6qwd0mlxw4pgskwgky0"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/macaron.v1"
         ;; TODO: missing deps
         #:tests? #f))
      (propagated-inputs
       `(("go-github.com-unknwon-com" ,go-github.com-unknwon-com)
         ("go-github.com-go-macaron-inject" ,go-github.com-go-macaron-inject)
         ("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
         ("go-gopkg-in-ini-v1" ,go-gopkg-in-ini-v1)))
      (home-page "https://gopkg.in/macaron.v1")
      (synopsis "")
      (description "")
      (license asl2.0))))

(define-public go-gopkg-in-clog-v1
  (let ((commit "3bc2eaba5fa35df0338549cc1180dc45f6fc2a16")
        (revision "0"))
    (package
      (name "go-gopkg-in-clog-v1")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/clog.v1.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "0s7vwcjm043021zd3xa5kx3zfmcxxsiywwlk3p2byfl0v95n7msr"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/clog.v1"
         ;; TODO: missing deps
         #:tests? #f))
      (propagated-inputs
       `(("go-github.com-fatih-color" ,go-github.com-fatih-color)))
      (home-page "https://gopkg.in/clog.v1")
      (synopsis "")
      (description "")
      (license asl2.0))))

(define-public go-gopkg-in-ini-v1
  (let ((commit "c85607071cf08ca1adaf48319cd1aa322e81d8c1")
        (revision "0"))
    (package
      (name "go-gopkg-in-ini-v1")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/ini.v1.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "18ywm8zyv091j1pp5mvx8szl7928chk8lw02br6jy568d7rk4xal"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/ini.v1"
         ;; TODO: missing dependencies
         #:tests? #f))
      (home-page "https://gopkg.in/ini.v1")
      (synopsis "")
      (description "")
      (license asl2.0))))

(define-public go-gopkg-in-yaml-v1
  (let ((commit "9f9df34309c04878acc86042b16630b0f696e1de")
        (revision "0"))
    (package
      (name "go-gopkg-in-yaml-v1")
      (version (git-version "0.0.0" revision commit))
      (source
        (origin
          (method git-fetch)
          (uri (git-reference
                 (url "https://gopkg.in/yaml.v1.git")
                 (commit commit)))
          (file-name (git-file-name name version))
          (sha256
            (base32
              "1r8d346szqa9x8q03wiycik5qy3d6w8qq4hs99z1p64q5lm0g7gm"))))
      (build-system go-build-system)
      (arguments
       '(#:import-path "gopkg.in/yaml.v1"
         ;; FIXME: tests failures
         #:tests? #f))
      (native-inputs
       `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
      (home-page "https://gopkg.in/yaml.v1")
      (synopsis "YAML reader and writer for the Go language")
      (description
       "This package provides a Go library for encode and decode YAML
values.")
      (license asl2.0))))

(define-public go-github.com-kylemcc-twitter-text-go-extract
  (let ((commit "7f582f6736ec1777a4725aaae652edfd2c28470a")
        (revision "1"))
   (package
     (name "go-github.com-kylemcc-twitter-text-go-extract")
     (version (git-version "0.0.0" revision commit))
     (source (origin
          (method git-fetch)
          (uri (git-reference
            (url "https://github.com/kylemcc/twitter-text-go")
            (commit commit)))
          (file-name (git-file-name name version))
          (sha256
           (base32
            "1ri8b51in68rmhw7qz2zkimbv7gyx6ldgmz9925zs17s502l2q36"))))
     (build-system go-build-system)
     (arguments
      `(#:import-path "github.com/kylemcc/twitter-text-go/extract"
        #:unpack-path "github.com/kylemcc/twitter-text-go"))
     (native-inputs
      `(("go-gopkg-in-yaml-v1" ,go-gopkg-in-yaml-v1)))
     (synopsis "")
     (description "")
     (home-page "https://github.com/kylemcc/twitter-text-go")
     (license bsd-3))))

(define-public go-github.com-writeas-web-core-activitypub
  (package
   (name "go-github.com-writeas-web-core-activitypub")
   (version "1.1.0")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/writeas/web-core")
          (commit (string-append "v" version))))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0mqhzhwxrqwbc5r4c7gzqrcbrsvbcz4pqjj4blaamjx5mpng9h6x"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/writeas/web-core/activitypub"
      #:unpack-path "github.com/writeas/web-core"))
   (propagated-inputs
    `(("go-github.com-writeas-openssl-go" ,go-github.com-writeas-openssl-go)))
   (synopsis "")
   (description "")
   (home-page "https://github.com/writeas/web-core")
   (license mpl2.0)))

(define-public go-github.com-writeas-web-core-activitystreams
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-activitystreams")
   (arguments
    `(#:import-path "github.com/writeas/web-core/activitystreams"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-auth
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-auth")
   (arguments
    `(#:import-path "github.com/writeas/web-core/auth"
      #:unpack-path "github.com/writeas/web-core"))
   (propagated-inputs
    `(("go-golang-org-x-crypto" ,go-golang-org-x-crypto)
      ("go-github.com-nu7hatch-gouuid" ,go-github.com-nu7hatch-gouuid)))))

(define-public go-github.com-writeas-web-core-bots
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-bots")
   (arguments
    `(#:import-path "github.com/writeas/web-core/bots"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-converter
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-converter")
   (arguments
    `(#:import-path "github.com/writeas/web-core/converter"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-data
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-data")
   (arguments
    `(#:import-path "github.com/writeas/web-core/data"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-i18n
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-i18n")
   (arguments
    `(#:import-path "github.com/writeas/web-core/i18n"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-id
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-id")
   (arguments
    `(#:import-path "github.com/writeas/web-core/id"
      #:unpack-path "github.com/writeas/web-core"))
   (propagated-inputs
    `(("go-github.com-writeas-nerds" ,go-github.com-writeas-nerds)))))

(define-public go-github.com-writeas-web-core-l10n
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-l10n")
   (arguments
    `(#:import-path "github.com/writeas/web-core/l10n"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-log
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-log")
   (arguments
    `(#:import-path "github.com/writeas/web-core/log"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-memo
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-memo")
   (arguments
    `(#:import-path "github.com/writeas/web-core/memo"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-posts
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-posts")
   (arguments
    `(#:import-path "github.com/writeas/web-core/posts"
      #:unpack-path "github.com/writeas/web-core"))
   (propagated-inputs
    `(("go-github.com-microcosm-cc-bluemonday" ,go-github.com-microcosm-cc-bluemonday)
      ("go-github.com-shurcool-sanitized-anchor-name"
       ,go-github.com-shurcool-sanitized-anchor-name)
      ("go-github.com-writeas-saturday" ,go-github.com-writeas-saturday)))))

(define-public go-github.com-writeas-web-core-query
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-query")
   (arguments
    `(#:import-path "github.com/writeas/web-core/query"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-stringmanip
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-stringmanip")
   (arguments
    `(#:import-path "github.com/writeas/web-core/stringmanip"
      #:unpack-path "github.com/writeas/web-core"))))

(define-public go-github.com-writeas-web-core-tags
  (package
   (inherit go-github.com-writeas-web-core-activitypub)
   (name "go-github.com-writeas-web-core-tags")
   (arguments
    `(#:import-path "github.com/writeas/web-core/tags"
      #:unpack-path "github.com/writeas/web-core"))
   (propagated-inputs
    `(("go-github.com-kylemcc-twitter-text-go-extract"
       ,go-github.com-kylemcc-twitter-text-go-extract)))))

(define-public go-github.com-gogs-gogs-chardet
  (let ((commit "2404f777256163ea3eadb273dada5dcb037993c0")
        (revision "0"))
    (package
      (name "go-github.com-gogs-gogs-chardet")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/gogs/chardet")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "1dki2pqhnzcmzlqrq4d4jwknnjxm82xqnmizjjdblb6h98ans1cd"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gogs/chardet"
         ;; FIXME: need more deps
         #:tests? #f))
      (synopsis "")
      (description "")
      (home-page "")
      (license expat))))

(define-public go-github.com-gogs-gogs-go-libravatar
  (let ((commit "cd1abbd55d09b793672732a7a1dfdaa12a40dfd0")
        (revision "0"))
    (package
      (name "go-github.com-gogs-gogs-go-libravatar")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/gogs/go-libravatar")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "00xvnddfh1m5g17mrnvp505i4sgwpk1r0wqz6a15bp6lvadwwlnj"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/gogs/go-libravatar"
         ;; FIXME: failures
         #:tests? #f))
      (synopsis "")
      (description "")
      (home-page "")
      (license expat))))

(define-public go-github.com-gogs-gogs-pkg-tool
  (package
    (name "go-github.com-gogs-gogs-pkg-tool")
    (version "0.11.86")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gogs/gogs")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0l8mwy0cyy3cdxqinf8ydb35kf7c8pj09xrhpr7rr7lldnvczabw"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gogs/gogs/pkg/tool"
       #:unpack-path "github.com/gogs/gogs"))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-gogs-gogs-pkg-user
  (package
    (inherit go-github.com-gogs-gogs-pkg-tool)
    (name "go-github.com-gogs-gogs-pkg-user")
    (arguments
     `(#:import-path "github.com/gogs/gogs/pkg/user"
       #:unpack-path "github.com/gogs/gogs"))))

(define-public go-github.com-gogs-gogs-pkg-process
  (package
    (inherit go-github.com-gogs-gogs-pkg-tool)
    (name "go-github.com-gogs-gogs-pkg-process")
    (arguments
     `(#:import-path "github.com/gogs/gogs/pkg/process"
       #:unpack-path "github.com/gogs/gogs"))))

(define-public go-github.com-gogs-gogs-pkg-bindata
  (package
    (inherit go-github.com-gogs-gogs-pkg-tool)
    (name "go-github.com-gogs-gogs-pkg-bindata")
    (arguments
     `(#:import-path "github.com/gogs/gogs/pkg/bindata"
       #:unpack-path "github.com/gogs/gogs"))))

(define-public go-github.com-gogs-gogs-pkg-setting
  (package
    (inherit go-github.com-gogs-gogs-pkg-tool)
    (name "go-github.com-gogs-gogs-pkg-setting")
    (arguments
     `(#:import-path "github.com/gogs/gogs/pkg/setting"
       #:unpack-path "github.com/gogs/gogs"))))

(define-public go-github.com-writeas-activity-streams
  (package
    (name "go-github.com-writeas-activity-streams")
    (version "0.1.2")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writeas/activity")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0bzq290bgjsknqc6pfjq5pmbxha94a3bxdxs9c5xvr70zv4bj6gj"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writeas/activity/streams"
       #:unpack-path "github.com/writeas/activity"
       ;; TODO: need go-github.com-go-test-deep
       #:tests? #f))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-writeas-activity-vocab
  (package
    (inherit go-github.com-writeas-activity-streams)
    (name "go-github.com-writeas-activity-vocab")
    (arguments
     `(#:import-path "github.com/writeas/activity/vocab"
       #:unpack-path "github.com/writeas/activity"
       ;; TODO: need go-github.com-go-test-deep
       #:tests? #f))))

(define-public go-github.com-writeas-go-strip-markdown
  (package
    (name "go-github.com-writeas-go-strip-markdown")
    (version "2.0.1")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writeas/go-strip-markdown")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1c9xlrdpnl1wkm4z5vwqplw7sqf8n7gdah6xm58z2kh0ry5hm325"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writeas/go-strip-markdown"))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-captncraig-cors
  (let ((commit "48080ede89fe41f3e3ba556770d0f3fd6ed02e38")
        (revision "0"))
    (package
      (name "go-github.com-captncraig-cors")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/captncraig/cors")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "0ij17pw44gcr2w867mcvk3r5rivp0g1x3cmlakrrh9v1chnqjmnf"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/captncraig/cors"))
      (synopsis "")
      (description "")
      (home-page "")
      (license asl2.0))))

(define-public go-github.com-writeas-go-webfinger
  (package
    (name "go-github.com-writeas-go-webfinger")
    (version "1.1.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writeas/go-webfinger")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "01w6m1lwsjgmq3h198l07nisp6jplm8cx71yvys5jn2wcz5jn3s9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writeas/go-webfinger"
       ;; FIXME: test failures
       #:tests? #f))
    (propagated-inputs
     `(("go-github.com-captncraig-cors" ,go-github.com-captncraig-cors)))
    (native-inputs
     `(("go-github-com-pkg-errors" ,go-github-com-pkg-errors)))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-writeas-httpsig
  (package
    (name "go-github.com-writeas-httpsig")
    (version "1.0.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writeas/httpsig")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0705f8pvp3m4l7yzx11307hdzk52n600j0sk5ldhfvm5rlxc4n14"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writeas/httpsig"))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0)))

(define-public go-github.com-writeas-impart
  (package
    (name "go-github.com-writeas-impart")
    (version "1.1.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writeas/impart")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0fiqjmjs2zyxsfhay5hps1s97jlad1pwnp1xiqmi7nikfdi9zjbz"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writeas/impart"))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-writeas-monday
  (let ((commit "54a7dd5792199cfeac78b93a55f99cebb6303164")
        (revision "0"))
    (package
      (name "go-github.com-writeas-monday")
      (version (git-version "1.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/writeas/monday")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "18bcx3majshsq4idvr5iazh8s8ldglnciw8v8xhylzmilqz9wz20"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/writeas/monday"))
      (synopsis "")
      (description "")
      (home-page "")
      ;; XXX: No license? only a responsibility waiver
      (license #f))))

(define-public go-github.com-writeas-slug
  (package
    (name "go-github.com-writeas-slug")
    (version "1.2.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writeas/slug")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1njikf777ap3j8idlvywsifw5alxsi1m1nnfhdvv2hi3m7z1ms5h"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writeas/slug"))
    (propagated-inputs
     `(("go-github.com-rainycape-unidecode" ,go-github.com-rainycape-unidecode)))
    (synopsis "")
    (description "")
    (home-page "")
    (license mpl2.0)))

(define-public go-github.com-writefreely-go-nodeinfo
  (package
    (name "go-github.com-writefreely-go-nodeinfo")
    (version "1.2.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/writefreely/go-nodeinfo")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0m94p49n4snjkkyaygdyfl8a2gi8g60fkqqr07jr1j940152ldbv"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/writefreely/go-nodeinfo"))
    (propagated-inputs
     `(("go-github.com-writeas-go-webfinger" ,go-github.com-writeas-go-webfinger)))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-gorilla-feeds
  (package
    (name "go-github.com-gorilla-feeds")
    (version "1.1.1")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gorilla/feeds")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1lwqibra4hyzx0jhaz12rfhfnw73bmdf8cn9r51nqidk8k7zf7sg"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/feeds"))
    (propagated-inputs
     `(("go-github-com-kr-pretty" ,go-github-com-kr-pretty)))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-2)))

(define-public go-github.com-gorilla-mux
  (package
    (name "go-github.com-gorilla-mux")
    (version "1.7.2")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gorilla/mux")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gjcbgpdl3snc38i8sj8k6sqbv3inyrxqyggbqvr4jfx2phv2zy6"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/mux"))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-gorilla-schema
  (package
    (name "go-github.com-gorilla-schema")
    (version "1.1.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gorilla/schema")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "14d31i3h6bg83r7ncmwm2pirab66z9hza38in18l89pbazxyh2n9"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/schema"))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-gorilla-context
  (package
    (name "go-github.com-gorilla-context")
    (version "1.1.1")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gorilla/context")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "03p4hn87vcmfih0p9w663qbx9lpsf7i7j3lc7yl7n84la3yz63m4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/context"))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-gorilla-securecookie
  (package
    (name "go-github.com-gorilla-securecookie")
    (version "1.1.1")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gorilla/securecookie")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "16bqimpxs9vj5n59vm04y04v665l7jh0sddxn787pfafyxcmh410"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/securecookie"))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-gorilla-sessions
  (package
    (name "go-github.com-gorilla-sessions")
    (version "1.1.3")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/gorilla/sessions")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0a99mlw5gvqbghnc1nx76gaanpxzjqfd74klp3s3sgbss69clayi"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/gorilla/sessions"))
    (propagated-inputs
     `(("go-github.com-gorilla-context" ,go-github.com-gorilla-context)
       ("go-github.com-gorilla-securecookie" ,go-github.com-gorilla-securecookie)))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-guregu-null
  (package
    (name "go-github.com-guregu-null")
    (version "3.4.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/guregu/null")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0lhxnkcmfydj7sc8ybalvkjhpwb6zndwpmkasmv1z2qyl4dmik1l"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/guregu/null"))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-2)))

(define-public go-github.com-rainycape-unidecode
  (let ((commit "cb7f23ec59bec0d61b19c56cd88cee3d0cc1870c")
        (revision "0"))
    (package
      (name "go-github.com-rainycape-unidecode")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/rainycape/unidecode")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "1wvzdijd640blwkgmw6h09frkfa04kcpdq87n2zh2ymj1dzla5v5"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/rainycape/unidecode"))
      (synopsis "")
      (description "")
      (home-page "")
      (license asl2.0))))

(define-public go-github.com-chzyer-logex
  (package
    (name "go-github.com-chzyer-logex")
    (version "1.1.10")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/chzyer/logex")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "08pbjj3wx9acavlwyr055isa8a5hnmllgdv5k6ra60l5y1brmlq4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/chzyer/logex"))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-chzyer-test
  (let ((commit "a1ea475d72b168a29f44221e0ad031a842642302")
        (revision "0"))
    (package
      (name "go-github.com-chzyer-test")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/chzyer/test")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "0rns2aqk22i9xsgyap0pq8wi4cfaxsri4d9q6xxhhyma8jjsnj2k"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/chzyer/test"))
      (propagated-inputs
       `(("go-github.com-chzyer-logex" ,go-github.com-chzyer-logex)))
      (synopsis "")
      (description "")
      (home-page "")
      (license expat))))

(define-public go-github.com-chzyer-readline
  (let ((commit "2972be24d48e78746da79ba8e24e8b488c9880de")
        (revision "0"))
    (package
      (name "go-github.com-chzyer-readline")
      (version (git-version "1.4" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/chzyer/readline")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "104q8dazj8yf6b089jjr82fy9h1g80zyyzvp3g8b44a7d8ngjj6r"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/chzyer/readline"))
      (native-inputs
       `(("go-github.com-chzyer-test" ,go-github.com-chzyer-test)))
      (synopsis "")
      (description "")
      (home-page "")
      (license expat))))

(define-public go-github.com-lunixbochs-vtclean
  (package
    (name "go-github.com-lunixbochs-vtclean")
    (version "1.0.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/lunixbochs/vtclean")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0jqn33l1kzy4dk66zbvxz7rlgkgg34s9mhc8z0lrz0i88466zhd8"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/lunixbochs/vtclean"))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-juju-ansiterm
  (let ((commit "720a0952cc2ac777afc295d9861263e2a4cf96a1")
        (revision "0"))
    (package
      (name "go-github.com-juju-ansiterm")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/juju/ansiterm")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "0n6j0y7xhashp8gdkdl0r7vlbkdrkymrzxn9hxrx522k2isggs7h"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/juju/ansiterm"))
      (propagated-inputs
       `(("go-github.com-lunixbochs-vtclean" ,go-github.com-lunixbochs-vtclean)
         ("go-github-com-mattn-go-colorable" ,go-github-com-mattn-go-colorable)
         ("go-github-com-mattn-go-isatty" ,go-github-com-mattn-go-isatty)))
      (native-inputs
       `(("go-gopkg-in-check-v1" ,go-gopkg-in-check-v1)))
      (synopsis "")
      (description "")
      (home-page "")
      (license lgpl3+))))

(define-public go-github.com-manifoldco-promptui
  (package
    (name "go-github.com-manifoldco-promptui")
    (version "0.3.2")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/manifoldco/promptui")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "185h0lqm14l9j0yvdsn9njq7jw3j6x3l21jvvczzbcbbrj44q0pl"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/manifoldco/promptui"))
    (propagated-inputs
     `(("go-github.com-chzyer-readline" ,go-github.com-chzyer-readline)
       ("go-github.com-juju-ansiterm" ,go-github.com-juju-ansiterm)))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-mitchellh-go-wordwrap
  (package
    (name "go-github.com-mitchellh-go-wordwrap")
    (version "1.0.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/mitchellh/go-wordwrap")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1jffbwcr3nnq6c12c5856bwzv2nxjzqk3jwgvxkwi1xhpd2by0bf"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mitchellh/go-wordwrap"))
    (synopsis "")
    (description "")
    (home-page "")
    (license bsd-3)))

(define-public go-github.com-beevik-etree
  (package
    (name "go-github.com-beevik-etree")
    (version "1.1.0")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/beevik/etree")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "12dqgh8swrnk8c1bwqmq4mgd65rj4waxgb02filkm3f52vyxryxn"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/beevik/etree"))
    (synopsis "")
    (description "")
    (home-page "")
    ;; or bsd-2?
    (license expat)))

(define-public go-github.com-imdario-mergo
  (package
    (name "go-github.com-imdario-mergo")
    (version "0.3.7")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/imdario/mergo")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05ir0jj74w0yfi1lrhjd97v759in1dpsma64cgmbiqvyp6hfmmf8"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/imdario/mergo"))
    (propagated-inputs
     `(("go-gopkg-in-yaml-v2" ,go-gopkg-in-yaml-v2)))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

;; latest version is 2.0.2, but this version is required for writefreely
(define-public go-github.com-ikeikeikeike-go-sitemap-generator-stm
  (package
    (name "go-github.com-ikeikeikeike-go-sitemap-generator-stm")
    (version "1.0.1")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/ikeikeikeike/go-sitemap-generator")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0j178hwjy0b7p6199andik5ci9p6gi74bm0pz2bnibw8c8bp0bn4"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/ikeikeikeike/go-sitemap-generator/stm"
       #:unpack-path "github.com/ikeikeikeike/go-sitemap-generator"
       ;; TODO: missing deps
       #:tests? #f))
    (propagated-inputs
     `(("go-github.com-beevik-etree" ,go-github.com-beevik-etree)
       ("go-github.com-fatih-structs" ,go-github.com-fatih-structs)
       ("go-github.com-imdario-mergo" ,go-github.com-imdario-mergo)))
    (synopsis "")
    (description "")
    (home-page "")
    (license expat)))

(define-public go-github.com-unknwon-com
  (package
    (name "go-github.com-unknwon-com")
    (version "2")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/Unknwon/com")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1gs9hvfxh5x03q4z8syyzlffsz8d9ggk5r49w8ab289qfpxwp7r6"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/Unknwon/com"
       ;; TODO: missing deps
       #:tests? #f))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0)))

(define-public go-github.com-unknwon-i18n
  (let ((commit "b64d336589669d317928070e70ba0ae558f16633")
        (revision "0"))
    (package
      (name "go-github.com-unknwon-i18n")
      (version (git-version "0.0.0" revision commit))
      (source (origin
           (method git-fetch)
           (uri (git-reference
             (url "https://github.com/Unknwon/i18n")
             (commit commit)))
           (file-name (git-file-name name version))
           (sha256
            (base32
             "07pbca7qr1h5il6rvpmyyqm8msqha61fc95wbjacw8vl4f97pgq0"))))
      (build-system go-build-system)
      (arguments
       `(#:import-path "github.com/Unknwon/i18n"
         ;; TODO: missing deps
         #:tests? #f))
      (propagated-inputs
       `(("go-gopkg-in-ini-v1" ,go-gopkg-in-ini-v1)))
      (synopsis "")
      (description "")
      (home-page "")
      (license asl2.0))))

(define-public go-github.com-mcuadros-go-version
  (let ((commit "92cdf37c5b7579ebaf7a036da94b40995972088d")
        (revision "0"))
  (package
    (name "go-github.com-mcuadros-go-version")
    (version (git-version "0.0.0" revision commit))
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/mcuadros/go-version")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "05ifbzcb4qqgip86abqwar2w32x0lg3m91p4v7v0mbh4nqcxmnh2"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/mcuadros/go-version"))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0))))

(define-public go-github.com-bradfitz-gomemcache-memcache
  (let ((commit "551aad21a6682b95329c1f5bd62ee5060d64f7e8")
        (revision "0"))
  (package
    (name "go-github.com-bradfitz-gomemcache-memcache")
    (version (git-version "0.0.0" revision commit))
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/bradfitz/gomemcache")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0vd63pik9lx1172mk8l4ha5k53hlsjx4h989p688c8n9nhz00cr3"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/bradfitz/gomemcache/memcache"
       #:unpack-path "github.com/bradfitz/gomemcache"))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0))))

(define-public go-github.com-go-macaron-cache-memcache
  (let ((commit "56173531277692bc2925924d51fda1cd0a6b8178")
        (revision "0"))
  (package
    (name "go-github.com-go-macaron-cache-memcache")
    (version (git-version "0.0.0" revision commit))
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/go-macaron/cache")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1116a22wm43q2l54nnycgli90kix787j20mpgya9qb6xnglcck59"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-macaron/cache/memcache"
       #:unpack-path "github.com/go-macaron/cache"
       ;; TODO: missing deps
       #:tests? #f))
    (propagated-inputs
     `(("go-github.com-unknwon-com" ,go-github.com-unknwon-com)
       ("go-github.com-bradfitz-gomemcache-memcache"
        ,go-github.com-bradfitz-gomemcache-memcache)
       ("go-gopkg-in-macaron-v1" ,go-gopkg-in-macaron-v1)))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0))))

(define-public go-github.com-go-macaron-cache-redis
  (package
    (inherit go-github.com-go-macaron-cache-memcache)
    (name "go-github.com-go-macaron-cache-redis")
    (arguments
     `(#:import-path "github.com/go-macaron/cache/redis"
       #:unpack-path "github.com/go-macaron/cache"
       ;; TODO: missing deps
       #:tests? #f))
    (propagated-inputs
     `(("go-github.com-unknwon-com" ,go-github.com-unknwon-com)
       ("go-gopkg-in-macaron-v1" ,go-gopkg-in-macaron-v1)
       ("go-gopkg-in-redis-v2" ,go-gopkg-in-redis-v2)))))

(define-public go-github.com-go-macaron-cache
  (package
    (inherit go-github.com-go-macaron-cache-memcache)
    (name "go-github.com-go-macaron-cache")
    (arguments
     `(#:import-path "github.com/go-macaron/cache"
       ;; TODO: missing deps
       #:tests? #f))))

(define-public go-github.com-go-macaron-session
  (let ((commit "0a0a789bf1934e55fde19629869caa015a40c525")
        (revision "0"))
  (package
    (name "go-github.com-go-macaron-session")
    (version (git-version "0.0.0" revision commit))
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/go-macaron/session")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1l3m41ffxmysk07wsyvyazhhhv16d8vg1psrvjfv04ar3plpyvmm"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-macaron/session"
       ;; TODO: missing deps
       #:tests? #f))
    (propagated-inputs
     `(("go-github.com-unknwon-com" ,go-github.com-unknwon-com)
       ("go-gopkg-in-macaron-v1" ,go-gopkg-in-macaron-v1)))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0))))

(define-public go-github.com-go-macaron-inject
  (let ((commit "d8a0b8677191f4380287cfebd08e462217bac7ad")
        (revision "0"))
  (package
    (name "go-github.com-go-macaron-inject")
    (version (git-version "0.0.0" revision commit))
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/go-macaron/inject")
           (commit commit)))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "0p47pz699xhmi8yxhahvrpai9r49rqap5ckwmz1dlkrnh3zwhrhh"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/go-macaron/inject"
       ;; TODO: missing deps
       #:tests? #f))
    (synopsis "")
    (description "")
    (home-page "")
    (license asl2.0))))

(define-public go-github.com-jteeuwen-go-bindata
  (package
    (name "go-github.com-jteeuwen-go-bindata")
    (version "3.0.7")
    (source (origin
         (method git-fetch)
         (uri (git-reference
           (url "https://github.com/jteeuwen/go-bindata")
           (commit (string-append "v" version))))
         (file-name (git-file-name name version))
         (sha256
          (base32
           "1v8xwwlv6my5ixvis31m3vgz4sdc0cq82855j8gxmjp1scinv432"))))
    (build-system go-build-system)
    (arguments
     `(#:import-path "github.com/jteeuwen/go-bindata/go-bindata"
       #:unpack-path "github.com/jteeuwen/go-bindata"))
    (synopsis "")
    (description "")
    (home-page "")
    (license cc0)))

(define-public writefreely
  (package
   (name "writefreely")
   (version "0.9.1")
   (source (origin
        (method git-fetch)
        (uri (git-reference
          (url "https://github.com/writeas/writefreely/")
          (commit "fdbefa806fd05ea5f61cf1a0fc75900c2a1dd24e")))
        (file-name (git-file-name name version))
        (sha256
         (base32
          "0zp9d6n1mp0nfcpfxl3pyjy235q386h84skz97yhd348zjnmq7ki"))))
   (build-system go-build-system)
   (arguments
    `(#:import-path "github.com/writeas/writefreely/cmd/writefreely"
      #:unpack-path "github.com/writeas/writefreely"
      #:phases
      (modify-phases %standard-phases
        (add-before 'build 'fix-gogits-pkg-tool
          (lambda _
            (substitute* "src/github.com/writeas/writefreely/admin.go"
              (("gogits/gogs") "gogs/gogs"))
            #t))
        (add-before 'build 'generate-asset
          (lambda _
            (with-directory-excursion "src/github.com/writeas/writefreely"
              (substitute* "Makefile"
                ((": generate") ":"))
              (invoke "make" "assets")))))
      ;; FIXME: failure
      #:tests? #f))
   (propagated-inputs
    `(("go-github.com-dustin-go-humanize" ,go-github.com-dustin-go-humanize)
      ("go-github.com-fatih-color" ,go-github.com-fatih-color)
      ("go-github.com-go-macaron-cache-memcache"
       ,go-github.com-go-macaron-cache-memcache)
      ("go-github.com-go-macaron-cache" ,go-github.com-go-macaron-cache)
      ("go-github.com-go-macaron-cache-redis" ,go-github.com-go-macaron-cache-redis)
      ("go-github.com-go-macaron-session" ,go-github.com-go-macaron-session)
      ("go-github.com-go-sql-driver-mysql" ,go-github.com-go-sql-driver-mysql)
      ("go-github.com-gogs-gogs-chardet" ,go-github.com-gogs-gogs-chardet)
      ("go-github.com-gogs-gogs-go-libravatar"
       ,go-github.com-gogs-gogs-go-libravatar)
      ("go-github.com-gogs-gogs-pkg-bindata" ,go-github.com-gogs-gogs-pkg-bindata)
      ("go-github.com-gogs-gogs-pkg-process" ,go-github.com-gogs-gogs-pkg-process)
      ("go-github.com-gogs-gogs-pkg-setting" ,go-github.com-gogs-gogs-pkg-setting)
      ("go-github.com-gogs-gogs-pkg-tool" ,go-github.com-gogs-gogs-pkg-tool)
      ("go-github.com-gogs-gogs-pkg-user" ,go-github.com-gogs-gogs-pkg-user)
      ("go-github.com-gorilla-feeds" ,go-github.com-gorilla-feeds)
      ("go-github.com-gorilla-mux" ,go-github.com-gorilla-mux)
      ("go-github.com-gorilla-schema" ,go-github.com-gorilla-schema)
      ("go-github.com-gorilla-sessions" ,go-github.com-gorilla-sessions)
      ("go-github.com-guregu-null" ,go-github.com-guregu-null)
      ("go-github.com-ikeikeikeike-go-sitemap-generator-stm"
       ,go-github.com-ikeikeikeike-go-sitemap-generator-stm)
      ("go-github.com-manifoldco-promptui" ,go-github.com-manifoldco-promptui)
      ("go-github.com-mcuadros-go-version" ,go-github.com-mcuadros-go-version)
      ("go-github.com-mitchellh-go-wordwrap" ,go-github.com-mitchellh-go-wordwrap)
      ("go-github.com-shurcool-sanitized-anchor-name"
       ,go-github.com-shurcool-sanitized-anchor-name)
      ("go-github.com-unknwon-com" ,go-github.com-unknwon-com)
      ("go-github.com-unknwon-i18n" ,go-github.com-unknwon-i18n)
      ("go-github.com-writeas-activity-streams"
       ,go-github.com-writeas-activity-streams)
      ("go-github.com-writeas-activity-vocab"
       ,go-github.com-writeas-activity-vocab)
      ("go-github.com-writeas-go-webfinger" ,go-github.com-writeas-go-webfinger)
      ("go-github.com-writeas-httpsig" ,go-github.com-writeas-httpsig)
      ("go-github.com-writeas-impart" ,go-github.com-writeas-impart)
      ("go-github.com-writeas-go-strip-markdown"
       ,go-github.com-writeas-go-strip-markdown)
      ("go-github.com-writeas-monday" ,go-github.com-writeas-monday)
      ("go-github.com-writeas-saturday" ,go-github.com-writeas-saturday)
      ("go-github.com-writeas-slug" ,go-github.com-writeas-slug)
      ("go-github.com-writeas-web-core-activitypub"
       ,go-github.com-writeas-web-core-activitypub)
      ("go-github.com-writeas-web-core-activitystreams"
       ,go-github.com-writeas-web-core-activitystreams)
      ("go-github.com-writeas-web-core-auth" ,go-github.com-writeas-web-core-auth)
      ("go-github.com-writeas-web-core-bots" ,go-github.com-writeas-web-core-bots)
      ("go-github.com-writeas-web-core-converter" ,go-github.com-writeas-web-core-converter)
      ("go-github.com-writeas-web-core-data" ,go-github.com-writeas-web-core-data)
      ("go-github.com-writeas-web-core-i18n" ,go-github.com-writeas-web-core-i18n)
      ("go-github.com-writeas-web-core-id" ,go-github.com-writeas-web-core-id)
      ("go-github.com-writeas-web-core-l10n" ,go-github.com-writeas-web-core-l10n)
      ("go-github.com-writeas-web-core-log" ,go-github.com-writeas-web-core-log)
      ("go-github.com-writeas-web-core-memo" ,go-github.com-writeas-web-core-memo)
      ("go-github.com-writeas-web-core-posts" ,go-github.com-writeas-web-core-posts)
      ("go-github.com-writeas-web-core-query" ,go-github.com-writeas-web-core-query)
      ("go-github.com-writeas-web-core-stringmanip" ,go-github.com-writeas-web-core-stringmanip)
      ("go-github.com-writeas-web-core-tags" ,go-github.com-writeas-web-core-tags)
      ("go-github.com-writefreely-go-nodeinfo" ,go-github.com-writefreely-go-nodeinfo)
      ("go-gopkg-in-clog-v1" ,go-gopkg-in-clog-v1)
      ("go-gopkg-in-ini-v1" ,go-gopkg-in-ini-v1)
      ))
   (native-inputs
    `(("go-github.com-jteeuwen-go-bindata" ,go-github.com-jteeuwen-go-bindata)))
   (synopsis "Federated blogging platform")
   (description "WriteFreely is a blogging platform that can publish to
the 'Fediverse' via ActivityPub.")
   (home-page "https://writefreely.org/")
   (license agpl3)))
