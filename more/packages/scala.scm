;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2018 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more packages scala)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages protobuf)
  #:use-module (more packages java))

;; This package downloads the so-called official version of scala, a pre-built
;; binary by the scala developers.
;; This binary should never be made part of Guix itself, because we have
;; ways to bootstrap it properly. The bootstrap project of scala takes time,
;; so in the meantime... here you are :(
(define-public scala-official
  (package
    (name "scala-official")
    (version "2.12.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.lightbend.com/scala/"
                            version "/scala-" version ".tgz"))
        (sha256
         (base32
          "18w0vdbsp0q5rxglgalwlgkggld926bqi1fxc598rn4gh46a03j4"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (java (assoc-ref %build-inputs "icedtea-8"))
                          (bash (assoc-ref %build-inputs "bash"))
                          (tar (string-append
                                 (assoc-ref %build-inputs "tar")
                                 "/bin/tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (output (assoc-ref %outputs "out"))
                          (bindir (string-append output "/bin")))
                     (mkdir-p output)
                     (setenv "PATH" (string-append (getenv "PATH") ":" gzip "/bin"))
                     (invoke tar "xf" source)
                     (copy-recursively "scala-2.12.8" output)
                     (chdir output)
                     (for-each delete-file (find-files "bin" "bat$"))
                     (substitute* (find-files "bin" ".*")
                       (("^#!.*")
                        (string-append "#!" bash "/bin/bash\n" "JAVA_HOME=" java)))))))
    (inputs
     `(("bash" ,bash)
       ("gzip" ,gzip)
       ("icedtea-8" ,icedtea-8)
       ("tar" ,tar)))
    (home-page "https://scala-lang.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; TODO: put it in guix/build/java-utils.scm
;; TODO: remove contraband-* directories and regenerate them from contraband/
(define* (sbt-building-phase subprojects #:optional (kind-projector? #f))
  `(lambda* (#:key inputs #:allow-other-keys)
     (define (build-subproject prefix name)
       (let ((build-directory (string-append "build/" name))
             (kind-projector (assoc-ref inputs "scala-kind-projector"))
             (jar-name (string-append
                         (if (> (string-length prefix) 0)
                           (string-replace prefix "-" (- (string-length prefix) 1))
                           "")
                         name ".jar"))
             (java-files
                (append
                  (find-files (string-append prefix name "/src/main/contraband-java")
                              ".*.java$")
                  (find-files (string-append prefix name "/src/main/java")
                              ".*.java$")))
             (scala-files
                (append
                  (find-files (string-append prefix name "/src/main/contraband-scala")
                              ".*.scala$")
                  (find-files (string-append prefix name "/src/main/scala")
                              ".*.scala$"))))
         (mkdir-p build-directory)
         (format #t "Building project ~a...~%" jar-name)
         (unless (eq? scala-files '())
           (apply invoke "scalac" "-classpath"
                  (string-append
                    (getenv "CLASSPATH") ":"
                    ;; Add any top-level directory in build that may contain
                    ;; .class files, but don't actually add build/ iteself or
                    ;; any individual class file.
                    (string-join
                      (filter (lambda (s) (eq? (string-count s #\/) 1))
                              (find-files "build" "." #:directories? #t))
                      ":"))
                  "-d" build-directory "-language:experimental.macros"
                  (if ,kind-projector?
                    (string-append "-Xplugin:" kind-projector
                                   "/share/java/kind-projector.jar")
                    "")
                  (append scala-files java-files)))
         (unless (eq? java-files '())
           (apply invoke "javac" "-classpath"
                  (string-append
                    (getenv "CLASSPATH") ":"
                    (string-join
                      (filter (lambda (s) (eq? (string-count s #\/) 1))
                              (find-files "build" "." #:directories? #t))
                      ":"))
                  "-d" build-directory java-files))
         (invoke "jar" "cf" (string-append "build/jar/" jar-name)
                 "-C" build-directory ".")))
     (mkdir-p "build/jar")
     (for-each (lambda (project)
                   (build-subproject (car project) (cadr project)))
       '(,@subprojects))
     #t))

(define-public sbt-boilerplate-standalone
  (package
    (name "sbt-boilerplate-standalone")
    (version "0.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sbt/sbt-boilerplate/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1xzh7qrsl2nmnbyzlmrshzfsf8b4qgf6yaqm1hn3qnzk6761p2jy"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-standalone
           (lambda _
             (substitute* "src/main/scala/spray/boilerplate/Generator.scala"
               (("object Generator \\{")
                "object Generator {
def main(args: Array[String]): Unit = {
    val num = args(2).toInt
    val file = scala.io.Source.fromFile(args(0))
    val content = file.mkString
    val result = new java.io.PrintWriter(new java.io.File(args(1)))

    file.close
    result.write(generateFromTemplate(content, num))
    result.close()
}"))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (invoke "scalac" "-classpath" (getenv "CLASSPATH")
                     "-d" "build/classes"
                     "src/main/scala/spray/boilerplate/Generator.scala"
                     "src/main/scala/spray/boilerplate/TemplateParser.scala")
             (invoke "jar" "-cf" "build/jar/boilerplate.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "https://github.com/sbt/sbt-boilerplate")
    (synopsis "")
    (description "")
    (license license:bsd-2)))

(define-public scala-sjsonnew
  (package
    (name "scala-sjsonnew")
    (version "0.8.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/eed3si9n/sjson-new/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1rv0c50af5kn27x51g650wl2ig94z52fhs0rn8ykahpz4jhg1p7p"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-boilerplate
           (lambda _
             ; CaseClassFormats.scala.template  FlatUnionFormats.scala.template  TupleFormats.scala.template  UnionFormats.scala.template
             (invoke "java" "-cp" (getenv "CLASSPATH") "spray.boilerplate.Generator"
                     "core/src/main/boilerplate/sjsonnew/CaseClassFormats.scala.template"
                     "core/src/main/scala/sjsonnew/CaseClassFormats.scala" "22")
             (invoke "java" "-cp" (getenv "CLASSPATH") "spray.boilerplate.Generator"
                     "core/src/main/boilerplate/sjsonnew/FlatUnionFormats.scala.template"
                     "core/src/main/scala/sjsonnew/FlatUnionFormats.scala" "22")
             (invoke "java" "-cp" (getenv "CLASSPATH") "spray.boilerplate.Generator"
                     "core/src/main/boilerplate/sjsonnew/TupleFormats.scala.template"
                     "core/src/main/scala/sjsonnew/TupleFormats.scala" "22")
             (invoke "java" "-cp" (getenv "CLASSPATH") "spray.boilerplate.Generator"
                     "core/src/main/boilerplate/sjsonnew/UnionFormats.scala.template"
                     "core/src/main/scala/sjsonnew/UnionFormats.scala" "22")
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "core/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/sjsonnew.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (build-system ant-build-system)
    (native-inputs
     `(("scala" ,scala-official)
       ("sbt-boilerplate-standalone" ,sbt-boilerplate-standalone)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-scalajson
  (package
    (name "scala-scalajson")
    (version "1.0.0-M4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mdedetrich/scalajson/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0k4dj2zm7zilhshdnvqi9n17qr4szc5s9ymsm9imgqpr8r5hm2vj"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "shared/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/scalajson-shared.jar"
                     "-C" "build/classes" ".")
             (delete-file-recursively "build/classes")
             (setenv "CLASSPATH" (string-append (getenv "CLASSPATH") ":build/jar/scalajson-shared.jar"))
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "jvm/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/scalajson-jvm.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; Latest is 0.13.0, but this version is required for scala-jsonnew
(define-public scala-jawn
  (package
    (name "scala-jawn")
    (version "0.10.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/non/jawn/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1iic1rp7w7vsy0xhi40rcp339vcq5b4b46f51qrkfpv433f7hafi"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "util/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/jawn-util.jar"
                     "-C" "build/classes" ".")
             (delete-file-recursively "build/classes")
             (setenv "CLASSPATH" (string-append (getenv "CLASSPATH") ":build/jar/scalajson-shared.jar"))
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "parser/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/jawn-parser.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public scala-sjsonnew-support-scalajson
  (package
    (inherit scala-sjsonnew)
    (name "scala-sjsonnew-support-scalajson")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (substitute* (find-files "." ".*.scala")
               (("shaded.scalajson") "scalajson"))
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "support/scalajson/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/sjsonnew-support-scalajson.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("scala-sjsonnew" ,scala-sjsonnew)
       ("scala-scalajson" ,scala-scalajson)
       ("scala-jawn" ,scala-jawn)))))

(define-public scala-sjsonnew-support-murmurhash
  (package
    (inherit scala-sjsonnew)
    (name "scala-sjsonnew-support-murmurhash")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "support/murmurhash/src/main/scala" ".*.scala$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/sjsonnew-support-murmurhash.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("scala-sjsonnew" ,scala-sjsonnew)))))

(define-public scala-kind-projector
  (package
    (name "scala-kind-projector")
    (version "0.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/non/kind-projector/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "125amrhy7ri0z4bk2jzghg7341fm88a0p6gw3qg5diminlpaida3"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (copy-recursively "src/main/resources" "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "src/main/scala" ".*.scala$"))
             (invoke "jar" "-cf" "build/jar/kind-projector.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (copy-recursively "src/test/resources" "build/test-classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-Xplugin:build/jar/kind-projector.jar"
                    "-d" "build/test-classes"
                    (find-files "src/test/scala" ".*.scala$"))
             ;; TODO: actually run the tests... :D
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala-official" ,scala-official)
       ("java-junit" ,java-junit)))
    (home-page "https://github.com/non/kind-projector")
    (synopsis "Scala compiler plugin for type lambda")
    (description "Kind projector is a Scala compiler plugin for making type
lambdas (type projections) easier to write.")
    (license license:expat)))

(define-public sbt-util
  (package
    (name "sbt-util")
    (version "1.2.4")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/util/archive/v"
                            version ".tar.gz"))
        (file-name (string-append "sbt-util-" version ".tar.gz"))
        (sha256
         (base32
          "04ss1q4rl272ryxys05zmd4j2gm3aanaiqn6dmqbh6jii934pyxg"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-sjsonnew
           (lambda _
             (substitute* (find-files "." ".*.scala")
               (("sjsonnew.shaded.") ""))
             #t))
         (replace 'build
           ,(sbt-building-phase
              '(("internal/" "util-position")
                ("internal/" "util-control")
                ("internal/" "util-interface")
                ("internal/" "util-logging")
                ("internal/" "util-relation")
                ("internal/" "util-scripted")
                ("" "util-cache")
                ("" "util-tracking"))))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-log4j-api" ,java-log4j-api-for-sbt)
       ("java-log4j-core" ,java-log4j-core-for-sbt)
       ("sbt-io" ,sbt-io)
       ("scala-jawn" ,scala-jawn)
       ("scala-scalajson" ,scala-scalajson)
       ("scala-sjsonnew" ,scala-sjsonnew)
       ("scala-sjsonnew-support-murmurhash" ,scala-sjsonnew-support-murmurhash)
       ("scala-sjsonnew-support-scalajson" ,scala-sjsonnew-support-scalajson)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-log4j-api-for-sbt
  (package
    (inherit java-log4j-api)
    (version "2.8.1")
    ;(version "2.11.1")
    (arguments
     (ensure-keyword-arguments (package-arguments java-log4j-api)
       `(#:source-dir "src/main/java"
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'chdir
             (lambda _
               (chdir "log4j-api")
               #t))
           (add-before 'build 'fix-ambiguous
             (lambda _
               (substitute* "src/main/java/org/apache/logging/log4j/message/MapMessage.java"
                 (("append\\(data") "append((CharSequence)data"))
               #t))))))
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/logging/log4j/" version
                                  "/apache-log4j-" version "-src.tar.gz"))
              (sha256
               (base32
                "0x5gksgh0jkvd7k70rqrs2hy3glms0pkj6lhl26m6f83x1b6kvdm"))))))
                ;"1dhxnd0348is21w93m1rv2sbfwyx83rv63adnbd0bgjq01gzbvic"))))))

;; More dependencies needed
(define-public java-log4j-core-for-sbt
  (package
    (inherit java-log4j-api-for-sbt)
    (name "java-log4j-core")
    (inputs
     `(("java-osgi-core" ,java-osgi-core)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-log4j-api" ,java-log4j-api-for-sbt)
       ("java-mail" ,java-mail)
       ("java-jansi" ,java-jansi)
       ("java-jboss-jms-api-spec" ,java-jboss-jms-api-spec)
       ("java-jctools-core" ,java-jctools-core)
       ("java-lmax-disruptor" ,java-lmax-disruptor)
       ("java-kafka" ,java-kafka-clients)
       ("java-datanucleus-javax-persistence" ,java-datanucleus-javax-persistence)
       ("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
       ("java-fasterxml-jackson-dataformat-xml" ,java-fasterxml-jackson-dataformat-xml)
       ("java-fasterxml-jackson-dataformat-yaml" ,java-fasterxml-jackson-dataformat-yaml)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-csv" ,java-commons-csv)
       ("java-conversantmedia-disruptor" ,java-conversantmedia-disruptor)
       ("java-jcommander" ,java-jcommander)
       ("java-stax2-api" ,java-stax2-api)
       ("java-jeromq" ,java-jeromq)
       ("java-junit" ,java-junit)))
    (native-inputs
     `(("hamcrest" ,java-hamcrest-all)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("slf4j" ,java-slf4j-api)))
    (arguments
     `(#:tests? #f ; tests require more dependencies
       #:test-dir "src/test"
       #:source-dir "src/main/java"
       #:jar-name "log4j-core.jar"
       #:make-flags
       (list (string-append "-Ddist.dir=" (assoc-ref %outputs "out")
                            "/share/java"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "log4j-core") #t))
         (add-before 'build 'fix-ambiguous
           (lambda _
             (substitute* "src/main/java/org/apache/logging/log4j/core/pattern/MapPatternConverter.java"
               (("append\\(sortedMap") "append((CharSequence)sortedMap"))
             #t)))))
    (synopsis "Core component of the Log4j framework")
    (description "This package provides the core component of the Log4j
logging framework for Java.")))


;; TODO: Update to 2.0.6?
(define-public java-swoval-apple-file-events
  (package
    (name "java-swoval-apple-file-events")
    (version "1.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/swoval/swoval/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append "scala-swoval-" version ".tar.gz"))
              (sha256
               (base32
                "0ivrc4lcali84xp8frkjb2zi1l3lw8pim9xbkfah5iyj120gw6mq"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jar-name "apple-file-events.jar"
       #:source-dir "apple-file-events/jvm/src/main/java"))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public sbt-io
  (package
    (name "sbt-io")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/io/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0f9yjrrcr15kx2fn7w8f8swpx2dsx5zn95yg744l9gi2ri2qpyj5"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath"
                    (string-append (getenv "CLASSPATH") ":build/util-interface")
                    "-d" "build/classes"
                    (append
                      (find-files "io/src/main/java" ".*.java$")
                      (find-files "io/src/main/scala" ".*.scala$")
                      (find-files "io/src/main/contraband-scala" ".*.scala$")))
             (invoke "jar" "cf" "sbt-io.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-native-access" ,java-native-access)
       ("java-native-access-platform" ,java-native-access-platform)
       ("java-swoval-apple-file-events" ,java-swoval-apple-file-events)
       ("scala" ,scala-official)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public scala-ssl-config
  (package
    (name "scala-ssl-config")
    (version "0.3.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lightbend/ssl-config/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0sg477705flhcdnhjyl8v9jkhx1xfsnc73md8iva1agylpj5546f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "ssl-config-core/src/main/scala" ".*.scala$"))
             (invoke "jar" "cf" "okhttp.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-config" ,java-config)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-okhttp
  (package
    (name "scala-okhttp")
    (version "0.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eed3si9n/gigahorse/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "06ac03vr0cyr63zw0ibdwmswa03crm6i8mb00y69zpkm2jxqq2mb"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (append
                      (find-files "core/src/main/scala" ".*.scala$")
                      (find-files "okhttp/src/main/scala" ".*.scala$")
                      (find-files "core/src/main/contraband-scala" ".*.scala$")))
             (invoke "jar" "cf" "okhttp.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-config" ,java-config)
       ("java-reactive-streams" ,java-reactive-streams)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-okhttp" ,java-okhttp)
       ("java-okio" ,java-okio)
       ("scala-ssl-config" ,scala-ssl-config)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public sbt-launcher
  (package
    (name "sbt-launcher")
    (version "1.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sbt/launcher/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1a9dfqm47fn2nbqvjl723s0h16jf71cgnilg1i7gz6h4a7i0snak"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "launcher-implementation/src/main/input_sources/CrossVersionUtil.scala"
               (("\\$\\{\\{cross.package0\\}\\}") "xsbt")
               (("\\$\\{\\{cross.package1\\}\\}") "boot"))
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (append
                      (find-files "launcher-interface/src/main/java" ".*.java$")
                      (find-files "launcher-implementation/src/main/input_sources" ".*.scala$")
                      (find-files "launcher-implementation/src/main/scala" ".*.scala$")))
             (apply invoke "javac" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                    "-d" "build/classes"
                    (find-files "launcher-interface/src/main/java" ".*.java$"))
             (invoke "jar" "cf" "sbt-launcher.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-apache-ivy" ,java-apache-ivy)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; https://index.scala-lang.org/eed3si9n/gigahorse/gigahorse-okhttp/0.3.0?target=_2.12
(define-public sbt-librarymanagement
  (package
    (name "sbt-librarymanagement")
    (version "1.2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sbt/librarymanagement/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0g37agv3xkq1fjl9a25ybcdk4d5aq1m81rz5d2a8zvny135m73gl"))
              (modules '((guix build utils)))
              (snippet
                `(for-each delete-file (find-files "." ".*.jar")))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-sjsonnew
           (lambda _
             (substitute* (find-files "." ".*.scala")
               (("sjsonnew.shaded.") ""))
             #t))
         (replace 'build
           ,(sbt-building-phase
              `(("" "core")
                ("" "ivy"))))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-apache-ivy" ,java-apache-ivy)
       ("java-okhttp" ,java-okhttp)
       ("java-okhttp-urlconnection" ,java-okhttp-urlconnection)
       ("java-okio" ,java-okio)
       ("sbt-launcher" ,sbt-launcher)
       ("sbt-util" ,sbt-util)
       ("sbt-io" ,sbt-io)
       ("scala-jawn" ,scala-jawn)
       ("scala-okhttp" ,scala-okhttp)
       ("scala-scalajson" ,scala-scalajson)
       ("scala-sjsonnew" ,scala-sjsonnew)
       ("scala-sjsonnew-support-murmurhash" ,scala-sjsonnew-support-murmurhash)
       ("scala-sjsonnew-support-scalajson" ,scala-sjsonnew-support-scalajson)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    ;; From core/NOTICE
    ;; XXX: WARNING: no license in ivy/
    (license license:bsd-2)))

;; LICENSE?
(define-public scala-protoc-bridge
  (package
    (name "scala-protoc-bridge")
    (version "0.7.3")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/scalapb/protoc-bridge/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "17xjsa70h0w80rhps00kj42fmghk98c789aaa19g4bii2j1ckry8"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-script
           (lambda _
             (substitute* "src/main/scala/protocbridge/frontend/PosixPluginFrontend.scala"
               (("/usr/bin/env sh") (which "sh")))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (append
                      (find-files "src/main/scala" ".*.scala$")
                      (find-files "src/main/java" ".*.java$")))
             (apply invoke "javac" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                    "-d" "build/classes"
                    (find-files "src/main/java" ".*.java$"))
             (mkdir-p "build/jar")
             (invoke "jar" "-cf" "build/jar/protoc-bridge.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             ;(apply invoke "scalac" "-classpath" (string-append (getenv "CLASSPATH")
             ;                                                   ":build/classes")
             ;       "-d" "build/test-classes"
             ;       (find-files "src/test/scala" ".*.scala$"))
             ;; TODO: actually run the tests
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public scala-sourcecode
  (package
    (name "scala-sourcecode")
    (version "0.1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lihaoyi/sourcecode/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1cq15cxsv0j6xdrl566ywmab5il3239ja4cbgm39ihyn0yw2q1q4"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; in sourcecode/shared/src/test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-compat
           (lambda _
             (with-directory-excursion "sourcecode/shared/src/main"
               (copy-file "scala-2.11/sourcecode/Compat.scala"
                          "scala/sourcecode/Compat.scala"))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "sourcecode/shared/src/main/scala" ".*.scala$"))
             (invoke "jar" "cf" "build/jar/sourcecode.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public scala-acyclic
  (package
    (name "scala-acyclic")
    (version "0.1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lihaoyi/acyclic/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0c26qnjfhihn06sf17bxskacjcr51s03ygcmj8fp5vdzgcyfh7dl"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; in sourcecode/shared/src/test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-compat
           (lambda _
             (with-directory-excursion "src/main"
               (copy-file "scala-2.10_2.12/acyclic/plugin/Compat.scala"
                          "scala/acyclic/plugin/Compat.scala"))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "src/main/scala" ".*.scala$"))
             (copy-recursively "src/main/resources" "build/classes")
             (invoke "jar" "cf" "build/jar/acyclic.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public scala-fastparse
  (package
    (name "scala-fastparse")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lihaoyi/fastparse/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1h8s3izykv3x2g7klihk03mbnjcp23f2613827y62kdk6x9q6yjs"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; in fastpares/test/src
       #:modules
       ((guix build ant-build-system)
        (guix build java-utils)
        (guix build utils)
        (srfi srfi-1))
       #:imported-modules
       ((srfi srfi-1) ; for iota
        ,@%ant-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sources
           (lambda _
             (define (tuple num)
               (let* ((ts (map (lambda (n) (string-append "T" (number->string n)))
                               (iota num 1)))
                      (chunks (map (lambda (n)
                                     (string-append "t._" (number->string n)))
                                   (iota num 1)))
                      (chunkss (string-join chunks ", "))
                      (tsD (string-join (reverse (cons "D" (reverse ts))) ","))
                      (anys (string-join (map (const "Any") ts) ", "))
                      (tss (string-join ts ", ")))
                 (format #t "
val BaseSequencer~a: Sequencer[(~a), Any, (~a, Any)] =
  Sequencer0((t, d) => (~a, d))
implicit def Sequencer~a[~a]: Sequencer[(~a), D, (~a)] =
  BaseSequencer~a.asInstanceOf[Sequencer[(~a), D, (~a)]]
" num anys anys chunkss num tsD tss tsD num tss tsD)))
             (with-output-to-file "fastparse/src/fastparse/SequencerGen.scala"
               (lambda _
                 (format #t "package fastparse
trait SequencerGen[Sequencer[_, _, _]] extends LowestPriSequencer[Sequencer]{
  protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
")
                 (for-each tuple (iota 20 2))
                 (format #t "}
trait LowestPriSequencer[Sequencer[_, _, _]]{
  protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
  implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer0{case (t1, t2) => (t1, t2)}
}
")))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "fastparse/src" ".*.scala$"))
             (invoke "jar" "cf" "build/jar/fastparse.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("scala-sourcecode" ,scala-sourcecode)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define scala-fastparse1
  (package
    (inherit scala-fastparse)
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lihaoyi/fastparse/archive/"
                                  version ".tar.gz"))
              (file-name (string-append "scala-fastparse-" version ".tar.gz"))
              (sha256
               (base32
                "14d44f23hl6ypfwlvnzkzcvv52a0xipm5wkp2glr184aik7w5pnb"))))
    (arguments
     `(#:tests? #f; in fastpares/test/src
       #:modules
       ((guix build ant-build-system)
        (guix build java-utils)
        (guix build utils)
        (srfi srfi-1))
       #:imported-modules
       ((srfi srfi-1) ; for iota
        ,@%ant-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-compat
           (lambda _
             (with-directory-excursion "utils/shared/src/main"
               (copy-file "scala-2.11/fastparse/utils/Compat.scala"
                          "scala/fastparse/utils/Compat.scala"))
             #t))
         (add-before 'build 'generate-sources
           (lambda _
             (define (tuple num)
               (let* ((ts (map (lambda (n) (string-append "T" (number->string n)))
                               (iota num 1)))
                      (chunks (map (lambda (n)
                                     (string-append "t._" (number->string n)))
                                   (iota num 1)))
                      (chunkss (string-join chunks ", "))
                      (tsD (string-join (reverse (cons "D" (reverse ts))) ","))
                      (anys (string-join (map (const "Any") ts) ", "))
                      (tss (string-join ts ", ")))
                 (format #t "
val BaseSequencer~a: Sequencer[(~a), Any, (~a, Any)] =
  Sequencer0((t, d) => (~a, d))
implicit def Sequencer~a[~a]: Sequencer[(~a), D, (~a)] =
  BaseSequencer~a.asInstanceOf[Sequencer[(~a), D, (~a)]]
" num anys anys chunkss num tsD tss tsD num tss tsD)))
             (with-output-to-file
               "fastparse/shared/src/main/scala/fastparse/core/SequencerGen.scala"
               (lambda _
                 (format #t "package fastparse.core
trait SequencerGen[Sequencer[_, _, _]] extends LowestPriSequencer[Sequencer]{
  protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
")
                 (for-each tuple (iota 20 2))
                 (format #t "}
trait LowestPriSequencer[Sequencer[_, _, _]]{
  protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
  implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer0{case (t1, t2) => (t1, t2)}
}
")))
             #t))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "scalac" "-classpath" (getenv "CLASSPATH")
                    "-d" "build/classes"
                    (find-files "utils/shared/src/main/scala" ".*.scala$"))
             (apply invoke "scalac" "-classpath"
                    (string-append (getenv "CLASSPATH") ":build/classes")
                    "-d" "build/classes"
                    (find-files "fastparse/shared/src/main/scala" ".*.scala$"))
             (invoke "jar" "cf" "build/jar/fastparse.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("scala-acyclic" ,scala-acyclic)
       ("scala-sourcecode" ,scala-sourcecode)))))

;; LICENSE?
(define-public scala-protobuf
  (package
    (name "scala-protobuf")
    (version "0.8.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/scalapb/ScalaPB/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x6mfpyhzxh54dxh1pj3x952gyvp05m7vkj0c3p8ssx214r327kj"))
              (modules '((guix build utils)))
              (snippet
                `(delete-file-recursively "third_party"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; TODO
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sources
           (lambda _
             (with-output-to-file "compiler-plugin/src/main/scala/scalapb/compiler/Version.scala"
               (lambda _
                 (format #t "package scalapb.compiler
object Version {
  val scalapbVersion = \"~a\"
  val protobufVersion = \"~a\"
  val grpcJavaVersion = \"~a\"
}" ,version ,(package-version protobuf) "TODO")))
             (mkdir-p "compiler-plugin/src/main/scala/scalapb/compiler/internal")
             (copy-file
               "scalapb-runtime/shared/src/main/scala/scalapb/Encoding.scala"
               "compiler-plugin/src/main/scala/scalapb/compiler/internal/Encoding.scala")
             (substitute* "compiler-plugin/src/main/scala/scalapb/compiler/internal/Encoding.scala"
               (("package scalapb") "package scalapb.internal"))
             #t))
         (add-before 'build 'merge-runtime
           (lambda _
             (copy-recursively "scalapb-runtime/shared/src" "scalapb-runtime/src")
             (copy-recursively "scalapb-runtime/jvm/src" "scalapb-runtime/src")
             #t))
         (add-before 'build 'copy-compat
           (lambda _
             (with-directory-excursion "scalapb-runtime/src/main"
               (copy-file "scala-2.11/scalapb/textformat/ParserCompat.scala"
                          "scala/scalapb/textformat/ParserCompat.scala"))
             #t))
         (replace 'build
           ,(sbt-building-phase
              '(("lenses/" "shared")
                ("" "scalapb-runtime")
                ("" "compiler-plugin")
                ("" "scalapbc"))))
         (replace 'install
           (install-jars "build"))
         (add-after 'install 'install-bin
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/scalapbc")
                 (lambda _
                   (format #t "#!~a
java -cp ~a scalapb.ScalaPBC $@\n" (which "bash")
                           (string-join
                             (append
                               (find-files (assoc-ref outputs "out") ".*.jar")
                               (find-files (assoc-ref inputs "java-protobuf") ".*.jar")
                               (find-files (assoc-ref inputs "java-protoc-jar") ".*.jar")
                               (find-files (assoc-ref inputs "scala-fastparse1") ".*.jar")
                               (find-files (assoc-ref inputs "scala-protoc-bridge") ".*.jar")
                               (find-files (assoc-ref inputs "scala-sourcecode") ".*.jar")
                               (find-files (assoc-ref inputs "scala") ".*.jar"))
                             ":"))))
               (chmod (string-append bin "/scalapbc") #o755))
             #t)))))
    (inputs
     `(("java-protobuf" ,java-protobuf)
       ("java-protoc-jar" ,java-protoc-jar)
       ("scala-fastparse1" ,scala-fastparse1)
       ("scala-protoc-bridge" ,scala-protoc-bridge)
       ("scala-sourcecode" ,scala-sourcecode)))
    (native-inputs
     `(("protobuf" ,protobuf)
       ("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public sbt-sbinary
  (package
    (name "sbt-sbinary")
    (version "0.5.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/sbinary/archive/v"
                            version ".tar.gz"))
        (file-name (string-append "sbt-util-" version ".tar.gz"))
        (sha256
         (base32
          "12mcny4flfc6zbgncjwqljrsg6qw7azagkackyqcp4gmv9ssw99f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-fmpp
           (lambda _
             (invoke "fmpp" "-U" "all" "-S" "core/src/main/fmpp" "-O"
                     "core/src/main/scala")
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             (apply invoke "scalac" "-classpath"
                    (string-append (getenv "CLASSPATH") ":build/util-interface")
                    "-d" "build/classes"
                    (append
                      (find-files "core/src/main/scala-2.13-" ".*.scala$")
                      (find-files "core/src/main/scala" ".*.scala$")))
             (invoke "jar" "cf" "sbt-sbinary.jar" "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars ".")))))
    (native-inputs
     `(("java-fmpp" ,java-fmpp)
       ("scala" ,scala-official)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public sbt-zinc
  (package
    (name "sbt-zinc")
    (version "1.2.5")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/zinc/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "1b01nv3dk3g7jx59jnzp2m2va5brhmds4bpgp824fnmp2fdjzgl0"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-protobuf
           (lambda _
             (invoke "scalapbc"
                     (string-append
                       "-v"
                       (string-delete #\. ,(package-version protobuf)))
                     "--scala_out=internal/zinc-persist/src/main/scala"
                     "internal/zinc-persist/src/main/protobuf/schema.proto")
             #t))
         (add-before 'build 'use-correct-class
           (lambda _
             ;; Because of our way of compiling, some classes are present
             ;; in excess in the classpath, and sometimes are imported
             ;; instead of the correct one. e.g. is AnalysisCallback imported
             ;; as xsbti.AnalysisCallback and inc.AnalysisCallback
             (with-directory-excursion "internal/zinc-compile-core/src/main/scala"
               (substitute* "sbt/internal/inc/AnalyzingCompiler.scala"
                 ((": AnalysisCallback") ": xsbti.AnalysisCallback")))
             #t))
         (replace 'build
           ,(sbt-building-phase
              '(("internal/" "compiler-interface")
                ("internal/" "zinc-classpath")
                ("internal/" "zinc-classfile")
                ("internal/" "zinc-apiinfo")
                ("internal/" "zinc-core")
                ("internal/" "zinc-persist")
                ("internal/" "zinc-compile-core")
                ("internal/" "zinc-ivy-integration")
                ("" "zinc"))))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-log4j-api" ,java-log4j-api-for-sbt)
       ("java-log4j-core" ,java-log4j-core-for-sbt)
       ("java-protobuf" ,java-protobuf)
       ("sbt-io" ,sbt-io)
       ("sbt-launcher" ,sbt-launcher)
       ("sbt-librarymanagement" ,sbt-librarymanagement)
       ("sbt-sbinary" ,sbt-sbinary)
       ("sbt-util" ,sbt-util)
       ("scala-protoc-bridge" ,scala-protoc-bridge)
       ("scala-sjsonnew" ,scala-sjsonnew)))
    (native-inputs
     `(("scala-protobuf" ,scala-protobuf)
       ("scala" ,scala-official)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public scala-cache
  (package
    (name "scala-cache")
    (version "0.27.0")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/cb372/scalacache/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0iqs1zvwr19j9k726f4zf4jzqlx5y1br87ijras668c3wd301h1k"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'merge-core
           (lambda _
             (copy-recursively "modules/core/jvm" "modules/core")
             (copy-recursively "modules/core/shared" "modules/core")
             #t))
         (replace 'build
           ,(sbt-building-phase
             `(("modules/" "core")
               ("modules/" "caffeine"))))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-caffeine" ,java-caffeine)
       ("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("scala" ,scala-official)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public sbt
  (package
    (name "sbt")
    (version "1.2.8")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/sbt/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0fpm9jcd84xjxlfdfh2iwz7544ksgqik6591i7nrzlamygmbfadr"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-sjsonnew
           (lambda _
             (substitute* (find-files "." ".*.scala")
               (("sjsonnew.shaded.") ""))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "sbt/src/main/resources" "build/classes")
             #t))
         (add-before 'build 'generate-scalakeywords.scala
           (lambda _
             (with-output-to-file "project/WriteKeywords.scala"
               (lambda _
                 (format #t "package project
object WriteKeywords {
  def main(arg: Array[String]): Unit = {
    val g = new scala.tools.nsc.Global(new scala.tools.nsc.Settings)
    val keywords = g.nme.keywords.map(_.toString)
    val init = keywords.map(tn => '\"' + tn + '\"').mkString(\"Set(\", \", \", \")\")
    val ObjectName = \"ScalaKeywords\"
    val PackageName = \"sbt.internal.util\"
    val keywordsSrc = s\"\"\"
      |package $PackageName
      |object $ObjectName {
      |  val values = $init
      |}
    \"\"\".trim.stripMargin
    val base = \"internal/util-collection/src/main/scala\"
    val out = base + \"/\" + PackageName.replace('.', '/') + \"/\" + s\"$ObjectName.scala\"
    val result = new java.io.PrintWriter(new java.io.File(out))
    result.write(keywordsSrc)
    result.close()
  }
}")))
             (invoke "scalac" "-classpath" (getenv "CLASSPATH") "project/WriteKeywords.scala")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":.")
                     "project.WriteKeywords")
             #t))
         (replace 'build
           ,(sbt-building-phase
             `(("internal/" "util-collection")
               ("internal/" "util-complete")
               ("testing/" "agent")
               ("" "testing")
               ("" "core-macros")
               ("" "tasks")
               ("" "tasks-standard")
               ("" "protocol")
               ("" "run")
               ("" "main-command")
               ("" "main-settings")
               ("" "main-actions")
               ("" "main")
               ("" "sbt"))
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-apache-ivy" ,java-apache-ivy)
       ("java-sbt-ipcsocket" ,java-sbt-ipcsocket)
       ("java-sbt-test-interface" ,java-sbt-test-interface)
       ("java-log4j-api" ,java-log4j-api-for-sbt)
       ("java-log4j-core" ,java-log4j-core-for-sbt)
       ("java-native-access" ,java-native-access)
       ("scala" ,scala-official)
       ("scala-jawn" ,scala-jawn)
       ("scala-scalajson" ,scala-scalajson)
       ("scala-sjsonnew" ,scala-sjsonnew)
       ("scala-sjsonnew-support-murmurhash" ,scala-sjsonnew-support-murmurhash)
       ("scala-sjsonnew-support-scalajson" ,scala-sjsonnew-support-scalajson)
       ("sbt-io" ,sbt-io)
       ("sbt-launcher" ,sbt-launcher)
       ("sbt-librarymanagement" ,sbt-librarymanagement)
       ("sbt-util" ,sbt-util)
       ("sbt-zinc" ,sbt-zinc)))
    (native-inputs
     `(("scala-kind-projector" ,scala-kind-projector)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))
