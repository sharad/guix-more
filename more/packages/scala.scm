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
  #:use-module (gnu packages java))

;; This package downloads the so-called official version of scala, a pre-built
;; binary by the scala developers.
;; This binary should never be made part of Guix itself, because we have
;; ways to bootstrap it properly. The bootstrap project of scala takes time,
;; so in the meantime... here you are :(
(define-public scala-official
  (package
    (name "scala-official")
    (version "2.12.6")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://downloads.lightbend.com/scala/"
                            version "/scala-" version ".tgz"))
        (sha256
         (base32
          "05ili2959yrshqi44wpmwy0dyfm4kvp6i8mlbnj1xvc5b9649iqs"))))
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
                     (copy-recursively "scala-2.12.6" output)
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
       #:jdk ,icedtea-8
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
       #:jdk ,icedtea-8
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

(define-public scala-kind-projector
  (package
    (name "scala-kind-projector")
    (version "0.9.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/non/kind-projector/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "176g2d7ix2asp94ch39mza84k6z8by308hzglxs8933j8byramff"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:phases
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

(define-public sbt-util-position
  (package
    (name "sbt-util-position")
    (version "1.2.2")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/util/archive/v"
                            version ".tar.gz"))
        (file-name (string-append "sbt-util-" version ".tar.gz"))
        (sha256
         (base32
          "1mj6ny62crq1d8850lkj00g3wsjhflaxrqiiv72b02fb8hn671dh"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (define (build-subproject prefix name)
               (let ((build-directory (string-append "build/" name))
                     (jar-name (string-append name ".jar")))
                 (mkdir-p build-directory)
                 (format #t "Building project ~a...~%" name)
                 (apply invoke "scalac" "-classpath"
                        (string-append (getenv "CLASSPATH") ":build/util-interface")
                        "-d" build-directory "-language:experimental.macros"
                        (find-files (string-append prefix name "/src/main/scala")
                                    ".*.scala$"))
                 (invoke "jar" "cf" jar-name "-C" build-directory ".")))
             (build-subproject "internal/" "util-position")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-log4j-core" ,java-log4j-core)
       ("scala" ,scala-official)
       ("scala-sjsonnew" ,scala-sjsonnew)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public sbt-util-control
  (package
    (inherit sbt-util-position)
    (name "sbt-util-control")
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (define (build-subproject prefix name)
               (let ((build-directory (string-append "build/" name))
                     (jar-name (string-append name ".jar")))
                 (mkdir-p build-directory)
                 (format #t "Building project ~a...~%" name)
                 (apply invoke "scalac" "-classpath"
                        (string-append (getenv "CLASSPATH") ":build/util-interface")
                        "-d" build-directory "-language:experimental.macros"
                        (find-files (string-append prefix name "/src/main/scala")
                                    ".*.scala$"))
                 (invoke "jar" "cf" jar-name "-C" build-directory ".")))
             (build-subproject "internal/" "util-control")
             #t))
         (replace 'install
           (install-jars ".")))))))

(define-public sbt-util-interface
  (package
    (inherit sbt-util-position)
    (name "sbt-util-interface")
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:source-dir "internal/util-interface/src/main/java"
       #:jar-name "util-interface.jar"))))

(define-public java-log4j-api-for-sbt
  (package
    (inherit java-log4j-api)
    (version "2.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/logging/log4j/" version
                                  "/apache-log4j-" version "-src.tar.gz"))
              (sha256
               (base32
                "0x5gksgh0jkvd7k70rqrs2hy3glms0pkj6lhl26m6f83x1b6kvdm"))))))

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
       ("java-jboss-jms-api-spec" ,java-jboss-jms-api-spec)
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
       #:jdk ,icedtea-8
       #:make-flags
       (list (string-append "-Ddist.dir=" (assoc-ref %outputs "out")
                            "/share/java"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'enter-dir
           (lambda _ (chdir "log4j-core") #t)))))
    (synopsis "Core component of the Log4j framework")
    (description "This package provides the core component of the Log4j
logging framework for Java.")))


(define-public sbt-util-logging
  (package
    (inherit sbt-util-position)
    (name "sbt-util-logging")
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (define (build-subproject prefix name)
               (let ((build-directory (string-append "build/" name))
                     (jar-name (string-append name ".jar")))
                 (mkdir-p build-directory)
                 (format #t "Building project ~a...~%" name)
                 (apply invoke "scalac" "-classpath"
                        (string-append (getenv "CLASSPATH") ":build/util-interface")
                        "-d" build-directory "-language:experimental.macros"
                        (append
                          (find-files (string-append prefix name "/src/main/contraband-scala")
                                      ".*.scala$")
                          (find-files (string-append prefix name "/src/main/scala")
                                      ".*.scala$")))
                 (invoke "jar" "cf" jar-name "-C" build-directory ".")))
             (build-subproject "internal/" "util-logging")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-log4j-api" ,java-log4j-api-for-sbt)
       ;("java-log4j-core" ,java-log4j-core)
       ("sbt-util-interface" ,sbt-util-interface)))
    (native-inputs
     `(("scala-official" ,scala-official)))))

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
     `(#:jdk ,icedtea-8
       #:tests? #f; no tests
       #:jar-name "apple-file-events.jar"
       #:source-dir "apple-file-events/jvm/src/main/java"))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public sbt-io
  (package
    (name "sbt-io")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/io/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "0cgk3y3w8yjpivi910px529bz8bil49lrnib6wbwmvq8lw8mgrwq"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
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

(define-public sbt
  (package
    (name "sbt")
    (version "1.2.1")
    (source
      (origin
        (method url-fetch)
        (uri (string-append "https://github.com/sbt/sbt/archive/v"
                            version ".tar.gz"))
        (file-name (string-append name "-" version ".tar.gz"))
        (sha256
         (base32
          "15i8fd7zgairaaikscrva8d1klz0w9nh7fc0896x1n8nrs578vmy"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
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
           (lambda* (#:key inputs #:allow-other-keys)
             (define (build-subproject prefix name)
               (let ((build-directory (string-append "build/" name))
                     (jar-name (string-append name ".jar"))
                     (kind-projector (assoc-ref inputs "scala-kind-projector")))
                 (mkdir-p build-directory)
                 (format #t "Building project ~a...~%" name)
                 (apply invoke "scalac" "-classpath"
                        ;; 13:36 < snape> roptat: I think you could use 'readdir', as in 'files-in-directory' from guix/build/union.scm
                        (apply string-append (getenv "CLASSPATH")
                               (map (lambda (file) (string-append ":" file))
                                    (find-files "build" "." #:directories? #t)))
                        "-d" build-directory
                        (string-append "-Xplugin:" kind-projector
                                       "/share/java/kind-projector.jar")
                        (find-files (string-append prefix name "/src/main/scala")
                                    ".*.scala$"))
                 (invoke "jar" "cf" jar-name "-C" build-directory ".")))
             (build-subproject "internal/" "util-collection")
             (build-subproject "internal/" "util-complete")
             (build-subproject "" "core-macros")
             (build-subproject "" "tasks")
             (build-subproject "" "tasks-standard")
             (build-subproject "" "main-settings")
             (build-subproject "" "sbt")
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("scala" ,scala-official)
       ("scala-sjsonnew" ,scala-sjsonnew)
       ("sbt-io" ,sbt-io)
       ("sbt-util-control" ,sbt-util-control)
       ("sbt-util-interface" ,sbt-util-interface)
       ("sbt-util-logging" ,sbt-util-logging)
       ("sbt-util-position" ,sbt-util-position)))
    (native-inputs
     `(("scala-kind-projector" ,scala-kind-projector)))
    (home-page "https://www.scala-sbt.org/")
    (synopsis "")
    (description "")
    (license license:bsd-3)))
