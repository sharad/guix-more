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
  #:use-module (guix build-system copy)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages base)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages web)
  #:use-module (more packages java))

;; This package downloads the so-called official version of scala, a pre-built
;; binary by the scala developers.
;; This binary should never be made part of Guix itself, because we have
;; ways to bootstrap it properly. The bootstrap project of scala takes time,
;; so in the meantime... here you are :(

(define %binary-scala
  (package
    (name "scala")
    (version "2.13.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://downloads.lightbend.com/scala/"
                                  version  "/scala-" version ".tgz"))
              (sha256
               (base32
                "0hzd6pljc8z5fwins5a05rwpx2w7wmlb6gb8973c676i7i895ps9"))))
    (build-system copy-build-system)
    (arguments
     `(#:install-plan
       '(("." ""))
       #:phases
       (modify-phases %standard-phases
         (add-before 'install 'set-java-home
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* (find-files "bin" ".")
               (("^#!.*" shebang)
                (string-append shebang "\nJAVA_HOME="
                               (assoc-ref inputs "openjdk"))))))
         (add-before 'set-java-home 'remove-unneeded
           (lambda _
             (for-each delete-file (find-files "bin" "bat$")))))))
    (inputs
     `(("openjdk" ,openjdk14)))
    (home-page "https://scala-lang.org/")
    (synopsis "Scala programming language")
    (description "Scala combines object-oriented and functional programming in
one concise, high-level language.  Scala's static types help avoid bugs in
complex applications, and its JVM and JavaScript runtimes let you build
high-performance systems with easy access to huge ecosystems of libraries.")
    (license license:bsd-3)))

(define scala-asm
  (package
    (inherit java-asm)
    (version "9.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/scala/scala-asm")
                     (commit "s-9.1")))
              (file-name (git-file-name "scala-asm" version))
              (sha256
               (base32
                "1wsrlb6kb0fwxjdqanxqgmq4qcyq9gqn129w3l4bj7gvlspll33l"))))
    (arguments
     `(#:jar-name "java-asm.jar"
       #:source-dir "src/main/java"
       ;; no tests
       #:tests? #f))))

(define-public scala
  (package
    (inherit %binary-scala)
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/scala/scala")
                     (commit (string-append "v" (package-version %binary-scala)))))
              (file-name (git-file-name "scala" (package-version %binary-scala)))
              (sha256
               (base32
                "1gl156n6nd4xnq3cb6f1bbfbb9s4cp6bd9xczl99plpx6jwnpmhl"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (define* (build-project directory jar-name name #:optional (jar? #t))
               (let* ((build-dir (string-append "build/class/" name))
                      (java-files (find-files directory ".*.java$"))
                      (scala-files (find-files directory ".*.scala$")))
                 (mkdir-p build-dir)
                 (format #t "Building project ~a...~%" name)
                 (with-output-to-file (string-append build-dir "/" name ".properties")
                   (lambda _
                     (format #t "version.number=~a~%" ,(package-version this-package))
                     (format #t "maven.version.number=~a~%" ,(package-version this-package))))
                 (unless (eq? scala-files '())
                   (apply invoke "scalac" "-classpath"
                          (string-append
                            ;; Add any top-level directory in build that may contain
                            ;; .class files, but don't actually add build/ iteself or
                            ;; any individual class file.
                            (string-join
                              (filter (lambda (s) (eq? (string-count s #\/) 2))
                                      (find-files "build/class" "." #:directories? #t))
                              ":"))
                          "-d" build-dir "-nobootcp"
                          (append scala-files java-files)))
                 (unless (eq? java-files '())
                   (apply invoke "javac" "-classpath"
                          (string-append
                            (getenv "CLASSPATH") ":"
                            (string-join
                              (filter (lambda (s) (eq? (string-count s #\/) 2))
                                      (find-files "build/class" "." #:directories? #t))
                              ":"))
                          "-g" "-source" "1.8" "-target" "1.8"
                          "-d" build-dir java-files))
                 (mkdir-p "build/jar")
                 (when jar?
                   (invoke "jar" "cf" (string-append "build/jar/" jar-name)
                           "-C" build-dir "."))))

             (let ((scala-asm (assoc-ref inputs "scala-asm")))
               (setenv "CLASSPATH" (string-join (find-files scala-asm ".*.jar$") ":")))
             (setenv "JAVA_OPTS" "-Xmx1G")
             (build-project "src/library" "scala-library.jar" "library")
             (build-project "src/reflect" "scala-reflect.jar" "reflect")
             (build-project "src/compiler" "scala-compiler.jar" "compiler" #f)
             (build-project "src/interactive" "scala-compiler-interactive.jar" "interactive" #f)
             (build-project "src/scaladoc" "scala-compiler-doc.jar" "scaladoc" #f)
             (build-project "src/repl" "scala-repl.jar" "repl" #f)
             (build-project "src/repl-frontend" "scala-repl-frontend.jar" "replFrontend" #f)
             (build-project "src/scalap" "scalap.jar" "scalap")

             ;; create scala-compiler.jar as a union of some of those above
             (mkdir-p "build/class/scala-compiler")
             (with-directory-excursion "build/class/scala-compiler"
               (let ((scala-asm (assoc-ref inputs "scala-asm")))
                 (invoke "jar" "xf" (car (find-files scala-asm ".*.jar$"))))
               (copy-recursively "../compiler" ".")
               (copy-recursively "../interactive" ".")
               (copy-recursively "../scaladoc" ".")
               (copy-recursively "../repl" ".")
               (copy-recursively "../replFrontend" "."))
             (invoke "jar" "cf" "build/jar/scala-compiler.jar"
                     "-C" "build/class/scala-compiler" ".")))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (jna (assoc-ref inputs "java-native-access"))
                    (jline-terminal (assoc-ref inputs "java-jline3-terminal"))
                    (jline-reader (assoc-ref inputs "java-jline3-reader")))
               (mkdir-p lib)
               (for-each
                 (lambda (jar)
                   (copy-file jar (string-append lib "/" (basename jar))))
                 (find-files "build/jar" ".*.jar$"))
               (symlink (car (find-files jna "linux-.*.jar$"))
                        (string-append lib "/jna-lib.jar"))
               (symlink (car (find-files jna "jna.jar$"))
                        (string-append lib "/jna.jar"))
               (symlink (car (find-files jline-reader ".*.jar$"))
                        (string-append lib "/jline-reader.jar"))
               (symlink (car (find-files jline-terminal ".*.jar$"))
                        (string-append lib "/jline-terminal.jar")))))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (define (install-script script class)
                 (let ((script (string-append bin "/" script)))
                   (copy-file "src/compiler/templates/tool-unix.tmpl" script)
                   ;; See project/ScalaTool and build.sbt
                   (substitute* script
                     (("@@") "@")
                     (("@class@") class)
                     (("@properties@") "-Dscala.home=\"$SCALA_HOME\"")
                     (("@javaflags@") "-Xmx256M -Xms32M")
                     (("@toolflags@") "")
                     (("@classpath@") ""))
                   (chmod script #o755)))
               (mkdir-p bin)
               (install-script "scala" "scala.tools.nsc.MainGenericRunner")
               (install-script "scalac" "scala.tools.nsc.Main")
               (install-script "fsc" "scala.tools.nsc.fsc.CompileClient")
               (install-script "scaladoc" "scala.tools.nsc.ScalaDoc")
               (install-script "scalap" "scala.tools.scalap.Main")))))))
    (inputs
     `(("scala" ,%binary-scala)
       ("scala-asm" ,scala-asm)
       ("java-jline3-terminal" ,java-jline3-terminal)
       ("java-jline3-reader" ,java-jline3-reader)
       ("java-native-access" ,java-native-access)))))

(define-public sbt-ivy
  (package
    (inherit java-apache-ivy)
    (name "sbt-ivy")
    (version "2.4.0-rc1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sbt/ivy")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i13iqq8r4lqkxhg9gc192874hs3wyxykyg64m98vydf5zddb5zd"))
              (modules '((guix build utils)))
              (snippet
               ; these jars are not build products, but they interfere with the
               ; ant-build-system, and we don't run the tests anyway.
               `(delete-file-recursively "test"))
              (patches
                (search-patches "sbt-ivy-fix-bouncycastle-api.patch"))))
    (arguments
     (substitute-keyword-arguments (package-arguments java-apache-ivy)
       ((#:phases phases)
        `(modify-phases ,phases
           (add-after 'unpack 'make-writable
             (lambda _
               (for-each make-file-writable (find-files "."))
               #t))))))))

(define %default-java-locations
  '("shared/src/main/java" "jvm/src/main/java" "src/main/java"))

(define %default-scala-locations
  '("shared/src/main/scala" "jvm/src/main/scala" "src/main/scala"))

(define %default-resources-locations
  '("shared/src/main/resources" "jvm/src/main/resources" "src/main/resources"))

;; TODO: put it in guix/build/java-utils.scm
(define* (build-scala-phase spec
                            #:key (scala-arguments '())
                                  (java-locations %default-java-locations)
                                  (scala-locations %default-scala-locations)
                                  (resources-locations %default-resources-locations))
  `(lambda* (#:key inputs #:allow-other-keys)
     (define (get-files directory expr)
       (if (file-exists? directory)
           (find-files directory expr)
           '()))

     (define (build-scala directory name)
       (let* ((build-directory (string-append (getcwd) "/build"))
              (current-build-directory (string-append build-directory "/" name))
              (jar-name (string-append name ".jar"))
              (manifest (string-append current-build-directory "/META-INF/MANIFEST.MF"))
              (classpath
                ;; Add any top-level directory in build that may contain
                ;; .class files, but don't actually add build/ iteself or
                ;; any individual class file.
                (string-join
                  (map
                    (lambda (s) (string-append (getcwd) "/" s))
                    (filter
                      (lambda (s) (eq? (string-count s #\/) 1))
                      (find-files "build" "." #:directories? #t)))
                  ":")))
           (mkdir-p current-build-directory)
           (with-directory-excursion directory
             (let ((java-files
                     (apply append
                            (map (lambda (loc) (get-files loc "\\.java$"))
                                 ',java-locations)))
                   (scala-files
                     (apply append
                            (map (lambda (loc) (get-files loc "\\.scala$"))
                                 ',scala-locations))))
               (format #t "Building project ~a...~%" jar-name)
               (unless (eq? scala-files '())
                 (apply invoke "scalac" "-classpath"
                        (string-append
                          (getenv "CLASSPATH") ":"
                          classpath)
                        "-d" current-build-directory ,@scala-arguments
                        (append scala-files java-files)))
               (unless (eq? java-files '())
                 (apply invoke "javac" "-classpath"
                        (string-append
                          (getenv "CLASSPATH") ":"
                          classpath)
                        "-d" current-build-directory java-files)))
             (for-each
               (lambda (dir)
                 (when (file-exists? dir)
                   (copy-recursively dir current-build-directory)))
               ',resources-locations))
           (if (file-exists? manifest)
             (invoke "jar" "cfm" (string-append "build/jar/" jar-name)
                     manifest "-C" current-build-directory ".")
             (invoke "jar" "cf" (string-append "build/jar/" jar-name)
                     "-C" current-build-directory "."))))

     (mkdir-p "build/jar")
     (for-each
       (lambda (s)
         (apply build-scala s))
       ',spec)))

(define-public scala-data-class
  (package
    (name "scala-data-class")
    (version "0.2.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexarchambault/data-class")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "10csh0gc0snd8xpxmx8pad1rgci1i8xjhbfcx02njiy2vh4x5lh3"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase '(("." "data-class"))
              #:scala-arguments '("-Ymacro-annotations")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-xml
  (package
    (name "scala-xml")
    (version "2.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/scala/scala-xml")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16nw209dwj82mn0jc3ax77jmg0jqvvbc8wdwc7kaq3102k1wdv73"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'move-version-specific
           (lambda _
             (copy-file "shared/src/main/scala-2.13+/scala/xml/ScalaVersionSpecific.scala"
                        "shared/src/main/scala/scala/xml/ScalaVersionSpecific.scala")
             #t))
         (replace 'build
           ,(build-scala-phase '(("." "scala-xml"))))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-simulacrum
  (package
    (name "scala-simulacrum")
    (version "1.0.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/typelevel/simulacrum")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1a4yrv9x6vb989385m0rmb7y6lcd8b7lcq5ksp37k70dl3piam4z"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase
              '(("core" "simulacrum"))
               #:scala-arguments
               '("-Ymacro-annotations" "-deprecation" "-feature"
                 "-language:higherKinds" "-language:implicitConversions")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-geny
  (package
    (name "scala-geny")
    (version "0.6.10")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/com-lihaoyi/geny")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1km1xpvyy14ipgv019axg31vd9csnsi54cp8sw9mzdjikh6i211f"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase '(("geny" "scala-geny"))
              #:scala-locations '("src")
              #:java-locations '("src")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-sourcecode
  (package
    (name "scala-sourcecode")
    (version "0.2.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/com-lihaoyi/sourcecode")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "11xd0a0svm15hcknpsfzsyl9sy7dc692b93i39j3z8mgrnh5x4di"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase '(("sourcecode" "scala-sourcecode"))
              #:scala-locations '("src" "src-2")
              #:java-locations '("src")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-fastparse
  (package
    (name "scala-fastparse")
    (version "2.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/com-lihaoyi/fastparse")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "04wqggkywfy3q733x18mi873wh23w1ix5kypradpz0njdyfznikh"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'generate-source
           (lambda _
             ;; this file would be generated by build.sc, but it requires
             ;; more dependencies
             (with-output-to-file "fastparse/src/fastparse/SequencerGen.scala"
               (lambda _
                 (display "package fastparse
trait SequencerGen[Sequencer[_, _, _]] extends LowestPriSequencer[Sequencer]{
  protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
")
                 (display
                   (string-join
                     (map
                       (lambda (i)
                         (let* ((ts (map
                                      (lambda (n) (string-append "T" (number->string n)))
                                      (iota i 1)))
                                (chunks
                                  (map
                                    (lambda (n) (string-append "t._" (number->string n)))
                                    (iota i 1)))
                                (tsD (string-join
                                       (append ts (list "D"))
                                       ","))
                                (anys (string-join
                                        (map (lambda (t) "Any")
                                             ts)
                                        ", ")))
                           (string-append
                             "val BaseSequencer" (number->string i) ": Sequencer[(" anys "), Any, (" anys ", Any)] =
  Sequencer0((t, d) => (" (string-join chunks ", ") ", d))
implicit def Sequencer" (number->string i) "[" tsD "]: Sequencer[(" (string-join ts ", ") "), D, (" tsD ")] =
  BaseSequencer" (number->string i) ".asInstanceOf[Sequencer[(" (string-join ts ", ") "), D, (" tsD ")]]
")))
                       (iota 20 2))
                     "\n"))
                 (display "}
trait LowestPriSequencer[Sequencer[_, _, _]]{
  protected[this] def Sequencer0[A, B, C](f: (A, B) => C): Sequencer[A, B, C]
  implicit def Sequencer1[T1, T2]: Sequencer[T1, T2, (T1, T2)] = Sequencer0{case (t1, t2) => (t1, t2)}
}
")))
             #t))
         (replace 'build
           ,(build-scala-phase '(("fastparse" "scala-fastparse")
                                 ("scalaparse" "scala-scalaparse"))
              #:scala-locations '("src" "src-jvm")
              #:java-locations '("src" "src-jvm")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (propagated-inputs
     `(("scala-geny" ,scala-geny)
       ("scala-sourcecode" ,scala-sourcecode)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-dirs-dev-directories
  (package
    (name "scala-dirs-dev-directories")
    (version "23")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/dirs-dev/directories-jvm")
                     ;; exact version used by coursier
                     (commit "7acadf2ab9a4ce306d840d652cdb77fade11b94b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "09h2dxzbkhki65kkkm29w7id0m0hgswsbwrbb2ix50yj297n6nbm"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase '(("." "scala-directories"))))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-windowsansi
  (package
    (name "scala-windowsansi")
    (version "0.0.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexarchambault/windows-ansi")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0r21yhxmwi71qx7qqrpk0z7ykcd08xs7fl6yhzwlqjl0kar7wq0m"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase '(("jni" "scala-windowsansi-jni")
                                 ("ps" "scala-windowsansi-ps"))))
         (add-after 'unpack 'remove-proprietary-interfaces
           (lambda _
             (delete-file
               "jni/src/main/java/io/github/alexarchambault/windowsansi/NativeImageFeature.java")
             (delete-file
               "jni/src/main/java/io/github/alexarchambault/windowsansi/WindowsAnsiSubstitutions.java")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (propagated-inputs
     `(("java-jansi" ,java-jansi)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-argonaut
  (package
    (name "scala-argonaut")
    (version "6.3.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/argonaut-io/argonaut")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1pvc5jklaqqx957brkrwywvh5gsq08xd2k4c8bm4gnkd9b5n8wnl"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sources
           (lambda _
             ;; Converted from project/Boilerplate.scala
             (define header "package argonaut\n\n")
             (define arities (iota 22 1))
             (define aritiesExceptOne (iota 21 2))
             (define (arityChars n)
               (string (integer->char (+ (char->integer #\A) (- n 1)))))
             (define (functionTypeParameters arity)
               (string-join (map arityChars (iota arity 1)) ", "))
             (define (tupleFields arity)
               (string-join
                 (map
                   (lambda (n)
                     (string-append "x._" (number->string n)))
                   (iota arity 1))
                 ", "))
             (define (listPatternMatch arity)
               (string-append
                 (string-join
                   (map
                     (lambda (n)
                       (string-append "c" (string-downcase (arityChars n))))
                     (iota arity 1))
                   " :: ")
                 " :: Nil"))
             (define (jsonStringParams arity)
               (string-join
                 (map
                   (lambda (n)
                     (format #f "~an: JsonString" (string-downcase (arityChars n))))
                   (iota arity 1))
                 ", "))
             (define (jsonStringParamNames arity)
               (string-join
                 (map
                   (lambda (n)
                     (format #f "~an" (string-downcase (arityChars n))))
                   (iota arity 1))
                 ", "))

             (with-output-to-file "argonaut/shared/src/main/scala/argonaut/GeneratedDecodeJsons.scala"
               (lambda _
                 (define (decodeJsonContextArities n)
                   (string-join
                     (map (lambda (n) (format #f "~a: DecodeJson" (arityChars n)))
                          (iota n 1))
                     ","))
                 (define (decodeJsonParams n)
                   (string-join
                     (map
                       (lambda (n)
                         (define char (arityChars n))
                         (format #f "decode~a: DecodeJson[~a]"
                                 (string-downcase char) char))
                       (iota n 1))
                     ", "))
                 (define tupleDecodes
                   (map
                     (lambda (arity)
                       (define forComprehensionLines
                         (string-join
                           (map
                             (lambda (n)
                               (define char (arityChars n))
                               (format #f "          x~a <- decode~a(c~a)"
                                       (string-downcase char) (string-downcase char)
                                       (string-downcase char)))
                             (iota arity 1))
                           "\n"))
                       (define yieldResult
                         (string-join
                           (map
                             (lambda (n)
                               (string-append "x" (string-downcase (arityChars n))))
                             (iota arity 1))
                           ", "))
                       (format #f "
  implicit def Tuple~aDecodeJson[~a](implicit ~a): DecodeJson[(~a)] =
    DecodeJson(c =>
      c.jdecode[List[HCursor]] flatMap {
        case ~a => for {
~a
        } yield (~a)
        case _ => DecodeResult.fail(\"[~a]Tuple~a[~a]\", c.history)
      })
"
                               arity
                               (functionTypeParameters arity)
                               (decodeJsonParams arity)
                               (functionTypeParameters arity)
                               (listPatternMatch arity)
                               forComprehensionLines
                               yieldResult
                               (functionTypeParameters arity)
                               arity
                               (functionTypeParameters arity)))
                     aritiesExceptOne))
                 (define jdecode1 "
  def jdecode1[A, X](f: A => X)(implicit decodea: DecodeJson[A]): DecodeJson[X] =
    decodea.map(f)
")
                 (define jdecodes
                   (map
                     (lambda (arity)
                       (format #f "
  def jdecode~a[~a, X](f: (~a) => X)(implicit dx: DecodeJson[(~a)]): DecodeJson[X] =
    dx.map(x => f(~a))"
                               arity
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)
                               (tupleFields arity)))
                     aritiesExceptOne))

                 (define jdecode1L "
  def jdecode1L[A: DecodeJson, X](f: A => X)(an: JsonString): DecodeJson[X] =
    DecodeJson(x => for {
      aa <- x.get[A](an)
    } yield f(aa))")

                 (define jdecodeLs
                   (map
                     (lambda (arity)
                       (define forComprehensionLines
                         (string-join
                           (map
                             (lambda (n)
                               (define upperChar (arityChars n))
                               (define lowerChar (string-downcase upperChar))
                               (format #f "       ~a~a <- x.get[~a](~an)"
                                       lowerChar lowerChar upperChar lowerChar))
                             (iota arity 1))
                           "\n"))
                       (define yieldExpression
                         (string-join
                           (map
                             (lambda (n)
                               (define lowerChar (string-downcase (arityChars n)))
                               (format #f "~a~a" lowerChar lowerChar))
                             (iota arity 1))
                           ", "))

                       (format #f "
  def jdecode~aL[~a, X](f: (~a) => X)(~a): DecodeJson[X] =
    DecodeJson(x => for {
~a
    } yield f(~a))"
                               arity
                               (decodeJsonContextArities arity)
                               (functionTypeParameters arity)
                               (jsonStringParams arity)
                               forComprehensionLines
                               yieldExpression))
                     aritiesExceptOne))
                 (display header)
                 (display "trait GeneratedDecodeJsons {
  this: DecodeJsons =>
  import Json._
")
                 (display
                   (string-join
                     (append tupleDecodes (cons jdecode1 jdecodes)
                             (cons jdecode1L jdecodeLs))
                     ""))
                 (display "\n}\n")))

             (with-output-to-file "argonaut/shared/src/main/scala/argonaut/GeneratedEncodeJsons.scala"
               (lambda _
                 (define (encodeJsonContextArities n)
                   (string-join
                     (map
                       (lambda (n)
                         (format #f "~a: EncodeJson" (arityChars n)))
                       (iota n 1))
                     ", "))
                 (define (encodeJsonParams n)
                   (string-join
                     (map
                       (lambda (n)
                         (define char (arityChars n))
                         (format #f "encode~a: EncodeJson[~a]"
                                 (string-downcase char) char))
                       (iota n 1))
                     ", "))
                 (define (invokeEncodeJsonParams n)
                   (string-join
                     (map
                       (lambda (n)
                         (define char (string-downcase (arityChars n)))
                         (format #f "encode~a(~a)" char char))
                       (iota n 1))
                     ", "))

                 (define tupleEncodes
                   (map
                     (lambda (arity)
                       (format #f "
  implicit def Tuple~aEncodeJson[~a](implicit ~a): EncodeJson[(~a)] =
    EncodeJson({
     case (~a) => jArray(List(~a))
    })
"
                               arity
                               (functionTypeParameters arity)
                               (encodeJsonParams arity)
                               (functionTypeParameters arity)
                               (string-downcase (functionTypeParameters arity))
                               (invokeEncodeJsonParams arity)))
                     aritiesExceptOne))

                 (define jencode1 "
  def jencode1[X, A](f: X => A)(implicit encodea: EncodeJson[A]): EncodeJson[X] =
    encodea.contramap(f)
")

                 (define jencodes
                   (map
                     (lambda (arity)
                       (format #f "
  def jencode~a[X, ~a](f: X => (~a))(implicit encodex: EncodeJson[(~a)]): EncodeJson[X] =
    encodex.contramap(f)
"
                               arity
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)))
                     aritiesExceptOne))

                 (define jencode1L "
  def jencode1L[X, A](f: X => A)(an: JsonString)(implicit encodea: EncodeJson[A]): EncodeJson[X] =
    EncodeJson(x => jSingleObject(an, encodea.apply(f(x))))
")
                 (define jencodeLs
                   (map
                     (lambda (arity)
                       (define encodePairs
                         (string-join
                           (map
                             (lambda (n)
                               (define upperChar (arityChars n))
                               (define lowerChar (string-downcase upperChar))
                               (format #f "(~an, encode~a.apply(~a))"
                                       lowerChar lowerChar lowerChar))
                             (iota arity 1))
                           ", "))

                       (format #f "
  def jencode~aL[X, ~a](fxn: X => (~a))(~a)(implicit ~a): EncodeJson[X] =
    EncodeJson(x => jObjectAssocList({
      val (~a) = fxn(x)
      List(~a)
    }))
"
                               arity
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)
                               (jsonStringParams arity)
                               (encodeJsonParams arity)
                               (string-downcase (functionTypeParameters arity))
                               encodePairs)
                       )
                     aritiesExceptOne))

                 (define content
                   (string-join
                     (append
                       tupleEncodes (cons jencode1 jencodes) (cons jencode1L jencodeLs))
                     ""))

                 (display header)
                 (format #t "
trait GeneratedEncodeJsons {
  this: EncodeJsons =>
  import Json._
~a
}
"
                         content)))

             (with-output-to-file "argonaut/shared/src/main/scala/argonaut/GeneratedCodecJsons.scala"
               (lambda _
                 (define (codecJsonContextArities n)
                   (string-join
                     (map
                       (lambda (n)
                         (format #f "~a: EncodeJson: DecodeJson" (arityChars n)))
                       (iota n 1))
                     ", "))

                 (define codec1 "
  def codec1[A: EncodeJson: DecodeJson, X](f: A => X, g: X => A)(an: JsonString): CodecJson[X] =
    CodecJson(jencode1L(g)(an).encode, jdecode1L(f)(an).decode)
")
                 (define codecs
                   (map
                     (lambda (arity)
                       (format #f "
  def codec~a[~a, X](f: (~a) => X, g: X => (~a))(~a): CodecJson[X] =
    CodecJson(jencode~aL(g)(~a).encode, jdecode~aL(f)(~a).decode)
"
                               arity
                               (codecJsonContextArities arity)
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)
                               (jsonStringParams arity)
                               arity
                               (jsonStringParamNames arity)
                               arity
                               (jsonStringParamNames arity)))
                     aritiesExceptOne))

                 (define casecodec1 "
  def casecodec1[A: EncodeJson: DecodeJson, X](f: A => X, g: X => Option[A])(an: JsonString): CodecJson[X] =
    CodecJson(jencode1L(g)(an).encode, jdecode1L(f)(an).decode)
")
                 (define casecodecs
                   (map
                     (lambda (arity)
                       (format #f "
  def casecodec~a[~a, X](f: (~a) => X, g: X => Option[(~a)])(~a): CodecJson[X] =
    CodecJson(jencode~aL(g andThen (_.get))(~a).encode, jdecode~aL(f)(~a).decode)
"
                               arity
                               (codecJsonContextArities arity)
                               (functionTypeParameters arity)
                               (functionTypeParameters arity)
                               (jsonStringParams arity)
                               arity
                               (jsonStringParamNames arity)
                               arity
                               (jsonStringParamNames arity)))
                     aritiesExceptOne))
                 (define content
                   (string-join
                     (append
                       (cons codec1 codecs) (cons casecodec1 casecodecs))
                   ""))

                 (display header)
                 (format #t "
trait GeneratedCodecJsons {
  import Json._
  import DecodeJson._
  import EncodeJson._
~a
}
"
                         content)))
             #t))
         (replace 'build
           ,(build-scala-phase '(("argonaut" "scala-argonaut"))
              #:scala-locations '("shared/src/main/scala"
                                  "shared/src/main/scala2")
              #:java-locations '("shared/src/main/java")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public scala-shapeless
  (package
    (name "scala-shapeless")
    (version "2.3.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/milessabin/shapeless")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1ajni5f5gi76bghv5ajxlnfpz7163za5zv5w02fyfc0rq9p0lxai"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sources
           (lambda _
             ;; Converted from project/Boilerplate.scala
             (define header "package shapeless\n\n")
             (define scalaBinaryVersion ,(version-major+minor (package-version scala)))
             (define (templateVals arity)
               (let* ((synTypes (map
                                  (lambda (n)
                                    (string (integer->char (+ (char->integer #\A) n))))
                                  (iota arity)))
                      (synVals (map
                                  (lambda (n)
                                    (string (integer->char (+ (char->integer #\a) n))))
                                  (iota arity)))
                      (synTypedVals (map
                                      (lambda (v t)
                                        (string-append v ":" t))
                                      synVals synTypes)))
                `((arity . ,(number->string arity))
                  (synTypes . ,synTypes)
                  (synVals . ,synVals)
                  (synTypedVals . ,synTypedVals)
                  (A--N . ,(string-join synTypes ", "))
                  (A--N,Res . ,(string-join (append synTypes (list "Res")) ", "))
                  (a--n . ,(string-join synVals ", "))
                  (A::N . ,(string-join (append synTypes (list "HNil")) "::"))
                  (a::n . ,(string-join (append synVals (list "HNil")) "::"))
                  (_--_ . ,(string-join (map (const "_") synVals) ", "))
                  (tupleA--N . ,(if (equal? arity 1)
                                    "Tuple1[A]"
                                    (string-append
                                      "("
                                      (string-join synTypes ", ")
                                      ")")))
                  (tuple_--_ . ,(if (equal? arity 1)
                                    "Tuple1[_]"
                                    (string-append
                                      "("
                                      (string-join (map (const "_") synTypes) ", ")
                                      ")")))
                  (tuplea--n . ,(if (equal? arity 1)
                                    "Tuple1(a)"
                                    (string-append
                                      "("
                                      (string-join synVals ", ")
                                      ")")))
                  (a:A--n:N . ,(string-join synTypedVals ", ")))))

             (define* (generate-file filename content #:key
                                     (range (iota 22 1)))
               (with-output-to-file (string-append "core/src/main/scala/shapeless/"
                                                   filename)
                 (lambda _
                   (define rawContents
                     (map
                       (lambda (n)
                         (filter
                           (lambda (s) (not (equal? s "")))
                           (string-split (content n (templateVals n)) #\newline)))
                       range))
                   (define preBody
                     (let loop ((strs (car rawContents)) (ans '()))
                       (cond
                         ((null? strs) (reverse ans))
                         ((string-prefix? "|" (car strs))
                          (loop (cdr strs) (cons (car strs) ans)))
                         (else (reverse ans)))))
                   (define instances
                     (apply append
                            (map
                              (lambda (content)
                                (filter
                                  (lambda (s)
                                    (string-prefix? "-" s))
                                  content))
                              rawContents)))
                   (define postBody
                     (let loop ((strs (reverse (car rawContents))) (ans '()))
                       (cond
                         ((null? strs) (reverse ans))
                         ((string-prefix? "|" (car strs))
                          (loop (cdr strs) (cons (car strs) ans)))
                         (else (reverse ans)))))
                   (display
                     (string-append
                       header
                       (string-join
                         (map
                           (lambda (s)
                             (substring s 1))
                           (append preBody instances postBody))
                         "\n"))))))
             (generate-file
               "tupler.scala"
               (lambda (arity template)
                 (string-append "
|package ops
|
|import hlist.Tupler
|
|trait TuplerInstances {
|  type Aux[L <: HList, T] = Tupler[L] { type Out = T }
-
-  implicit def hlistTupler" (assoc-ref template 'arity) "[
-    " (assoc-ref template 'A--N) "
-  ]: Aux[
-    " (assoc-ref template 'A::N) ",
-    " (assoc-ref template 'tupleA--N) "
-  ] = Tupler.instance { case " (assoc-ref template 'a::n) " =>
-    " (assoc-ref template 'tuplea--n) "
-  }
|}
")))
             (generate-file
               "fntoproduct.scala"
               (lambda (arity template)
                 (define fnType (string-append "(" (assoc-ref template 'A--N) ") => Res"))
                 (define hlistFnType
                   (string-append "(" (assoc-ref template 'A::N) ") => Res"))
                 (define fnBody
                   (if (equal? arity 0)
                       "_ => fn()"
                       (string-append "{ case " (assoc-ref template 'a::n)
                                      " => fn(" (assoc-ref template 'a--n) ") }")))
                 (string-append "
|package ops
|
|import function.FnToProduct
|
|trait FnToProductInstances {
|  type Aux[F, P] = FnToProduct[F] { type Out = P }
-
-  implicit def fnToProduct" (assoc-ref template 'arity) "[
-    " (assoc-ref template 'A--N,Res) "
-  ]: Aux[
-    (" fnType "),
-    "hlistFnType"
-  ] = FnToProduct.instance(fn => " fnBody ")
|}
")))
             (generate-file
               "fnfromproduct.scala"
               (lambda (arity template)
                 (define fnType (string-append "(" (assoc-ref template 'A--N) ") => Res"))
                 (define hlistFnType
                   (string-append "(" (assoc-ref template 'A::N) ") => Res"))
                 (string-append "
|package ops
|
|import function.FnFromProduct
|
|trait FnFromProductInstances {
|  type Aux[F, O] = FnFromProduct[F] { type Out = O }
-
-  implicit def fnFromProduct" (assoc-ref template 'arity) "[
-    "(assoc-ref template 'A--N,Res)"
-  ]: Aux[
-    " hlistFnType ",
-    " fnType "
-  ] = FnFromProduct.instance { hf =>
-    (" (assoc-ref template 'a:A--n:N) ") =>
-      hf(" (assoc-ref template 'a::n) ")
-  }
|}
"))
               #:range (iota 23))
             (generate-file
               "caseinst.scala"
               (lambda (arity template)
                 (string-append
                   "
|
|trait CaseInst {
|  import poly._
|
-  implicit def inst" (assoc-ref template 'arity) "
-    [Fn <: Poly, " (assoc-ref template 'A--N) ", Res]
-    (cse : Case[Fn, " (assoc-ref template 'A::N) "] { type Result = Res }) 
-  : (" (assoc-ref template 'A--N) ") => Res =
-    (" (assoc-ref template 'a:A--n:N) ")
-      => cse.value(" (assoc-ref template 'a::n) ")
-
|}
")))
             (generate-file
               "polyapply.scala"
               (lambda (arity template)
                 (string-append "
|
|trait PolyApply {
|  import poly._
-  def apply
-    [" (assoc-ref template 'A--N) "]
-    (" (assoc-ref template 'a:A--n:N) ")
-    (implicit cse : Case[this.type, " (assoc-ref template 'A::N) "])
-  : cse.Result =
-    cse(" (assoc-ref template 'a::n) ")
-
|}
")))
             (generate-file
               "polyinst.scala"
               (lambda (arity template)
                 (string-append "
|
|trait PolyInst {
|
-  implicit def inst" (assoc-ref template 'arity) "
-    [" (assoc-ref template 'A--N) "]
-    (fn : Poly)(implicit cse : fn.ProductCase[" (assoc-ref template 'A::N) "])
-  : (" (assoc-ref template 'A--N) ") => cse.Result =
-    (" (assoc-ref template 'a:A--n:N) ")
-      => cse(" (assoc-ref template 'a::n) ")
-
|}
")))
             (generate-file
               "cases.scala"
               (lambda (arity template)
                 (string-append "
|
|trait Cases {
|  import poly._
|
-  type Case" (assoc-ref template 'arity) "[Fn, " (assoc-ref template 'A--N) "] =
-    Case[Fn, " (assoc-ref template 'A::N) "]
-
-  object Case" (assoc-ref template 'arity) " {
-    type Aux[Fn, " (assoc-ref template 'A--N) ", Result0] =
-      Case[Fn, " (assoc-ref template 'A::N) "] { type Result = Result0 }
-
-    def apply[
-      Fn, " (assoc-ref template 'A--N) ", Result0
-    ](
-      fn: (" (assoc-ref template 'A--N) ") => Result0
-    ): Aux[Fn, " (assoc-ref template 'A--N) ", Result0] =
-      Case { case " (assoc-ref template 'a::n) " =>
-        fn(" (assoc-ref template 'a--n) ")
-      }
-  }
-
|}
")))
             (generate-file
               "polyntraits.scala"
               (lambda (arity template)
                 (define fnBody
                   (if (equal? arity 0)
                       "_ => fn()"
                       (string-append
                         "{ case " (assoc-ref template 'a::n) " => fn("
                         (assoc-ref template 'a--n) ") }")))
                 (string-append "
|
-
-trait Poly" (assoc-ref template 'arity) " extends Poly { outer =>
-  type Case[" (assoc-ref template 'A--N) "] =
-    poly.Case[this.type, " (assoc-ref template 'A::N) "]
-
-  object Case {
-    type Aux[" (assoc-ref template 'A--N) ", Result0] =
-      poly.Case[outer.type, " (assoc-ref template 'A::N) "] { type Result = Result0 }
-  }
-
-  class CaseBuilder[" (assoc-ref template 'A--N) "] {
-    def apply[Res](
-      fn: (" (assoc-ref template 'A--N) ") => Res
-    ): Case.Aux[" (assoc-ref template 'A--N) ", Res] =
-      poly.Case(" fnBody ")
-  }
-  
-  def at[" (assoc-ref template 'A--N) "] =
-    new CaseBuilder[" (assoc-ref template 'A--N) "]
-}
-
-object Poly" (assoc-ref template 'arity) " extends PolyNBuilders.Poly" (assoc-ref template 'arity) "Builder[HNil] {
-  val functions = HNil
-}
|
")))
             (generate-file
               "polynbuilders.scala"
               (lambda (arity template)
                 (string-append "
|
|
|/**
|  * Provides elegant syntax for creating polys from functions
|  *
|  * @author Aristotelis Dossas
|  */
|object PolyNBuilders {
-
- trait Poly" (assoc-ref template 'arity) "Builder[HL <: HList] { self =>
-
-   val functions: HL
-   class AtAux[" (assoc-ref template 'A--N) "] {
-     def apply[Out](λ: (" (assoc-ref template 'A--N) ") => Out) = {
-       new Poly" (assoc-ref template 'arity) "Builder[((" (assoc-ref template 'A--N) ") => Out) :: HL] {
-         val functions = λ :: self.functions
-       }
-     }
-   }
-   def at[" (assoc-ref template 'A--N) "] = new AtAux[" (assoc-ref template 'A--N) "]
-
-   def build = new Poly" (assoc-ref template 'arity) " {
-     val functions = self.functions
-
-     implicit def allCases[" (assoc-ref template 'A--N) ", Out](implicit tL: Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, HL]) = {
-       val func: (" (assoc-ref template 'A--N) ") => Out = tL(functions)
-       at(func)
-     }
-   }
- }
-
- /* For internal use of Poly" (assoc-ref template 'arity) "Builder */
- trait Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, HL <: HList] {
-   def apply(l: HL): (" (assoc-ref template 'A--N) ") => Out
- }
-
- object Function" (assoc-ref template 'arity) "TypeAt {
-   private def instance[" (assoc-ref template 'A--N) ", Out, HL <: HList](
-     f: HL => (" (assoc-ref template 'A--N) ") => Out
-   ): Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, HL] =
-     new Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, HL] {
-       def apply(l: HL) = f(l)
-     }
-
-   implicit def at0[
-     " (assoc-ref template 'A--N) ", Out, Tail <: HList
-   ]: Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, ((" (assoc-ref template 'A--N) ") => Out) :: Tail] =
-     instance(_.head)
-
-   implicit def atOther[
-     " (assoc-ref template 'A--N) ", Out, Tail <: HList, Head
-   ](
-     implicit tprev: Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, Tail]
-   ): Function" (assoc-ref template 'arity) "TypeAt[" (assoc-ref template 'A--N) ", Out, Head :: Tail] =
-     instance(l => tprev(l.tail))
- }
|}
")))
             (generate-file
               "nats.scala"
               (lambda (arity template)
                 (define n (number->string arity))
                 (string-append "
|
|trait Nats {
-
-  type _" n " = Succ[_" (number->string (- arity 1)) "]
-  val _" n ": _" n " = new _" n "
|}
")))
             (generate-file
               "tupletypeables.scala"
               (lambda (arity template)
                 (define implicitArgs
                   (string-join
                     (map
                       (lambda (a)
                         (string-append "cast" a ":Typeable[" a "]"))
                       (assoc-ref template 'synTypes))
                     ", "))
                 (define enumerators
                   (let ((synTypes (assoc-ref template 'synTypes)))
                     (string-join
                       (map
                         (lambda (a i)
                           (string-append "_ <- p._" (number->string i) ".cast[" a "]"))
                         synTypes
                         (iota (length synTypes) 1))
                       "; ")))
                 (define castVals
                   (string-join
                     (map
                       (lambda (a)
                         (string-append "${cast" a ".describe}"))
                       (assoc-ref template 'synTypes))
                     ", "))
                 (string-append "
|
|trait TupleTypeableInstances {
|  import syntax.typeable._
-
-  implicit def tuple" (assoc-ref template 'arity) "Typeable[
-    " (assoc-ref template 'A--N) "
-  ](
-    implicit " implicitArgs "
-  ): Typeable[" (assoc-ref template 'tupleA--N) "] =
-    Typeable.instance(s\"(" castVals ")\") {
-      case p: " (assoc-ref template 'tuple_--_) " =>
-        for (" enumerators ")
-          yield p.asInstanceOf[" (assoc-ref template 'tupleA--N) "]
-      case _ =>
-        None
-    }
|}
")))
             (generate-file
               "sizedbuilder.scala"
               (lambda (arity template)
                 (define synVals (assoc-ref template 'synVals))
                 (define a:T--n:T
                   (string-join
                     (map
                       (lambda (v) (string-append v ":T"))
                       synVals)
                     ", "))
                 (define commonImplicits
                   "factory: Factory[T, CC[T]], ev: AdditiveCollection[CC[T]]")
                 (define implicits
                   (case scalaBinaryVersion
                     (("2.11" "2.12") commonImplicits)
                     (else (string-append "dis: DefaultToIndexedSeq[CC], " commonImplicits))))
                 (string-append "
|
|class SizedBuilder[CC[_]] {
|  import scala.collection._
|  import nat._
|  import Sized.wrap
|
-  def apply[T](" a:T--n:T ")
-    (implicit " implicits ") =
-    wrap[CC[T], _" (assoc-ref template 'arity) "]((factory.newBuilder ++= Seq(" (assoc-ref template 'a--n) ")).result())
-
|}
")))
             (generate-file
               "hmapbuilder.scala"
               (lambda (arity template)
                 (define typeArgs
                   (string-join
                     (map
                       (lambda (n)
                         (let ((n (number->string n)))
                           (string-append "K" n ", V" n)))
                       (iota arity))
                     ", "))
                 (define args
                   (string-join
                     (map
                       (lambda (n)
                         (let ((n (number->string n)))
                           (string-append "e" n ": (K" n ", V" n ")")))
                       (iota arity))
                     ", "))
                 (define witnesses
                   (string-join
                     (map
                       (lambda (n)
                         (let ((n (number->string n)))
                           (string-append "ev" n ": R[K" n ", V" n "]")))
                       (iota arity))
                     ", "))
                 (define mapArgs
                   (string-join
                     (map
                       (lambda (n)
                         (let ((n (number->string n)))
                           (string-append "e" n)))
                       (iota arity))
                     ", "))
                 (string-append "
|
|class HMapBuilder[R[_, _]] {
-
-  def apply
-    [" typeArgs "]
-    (" args ")
-    (implicit " witnesses ")
-    = new HMap[R](Map(" mapArgs "))
|}
")))
             (generate-file
               "unpack.scala"
               (lambda (arity template)
                 (define typeblock
                   (string-append "FF[" (assoc-ref template 'A--N) "], FF, " 
                                  (assoc-ref template 'A--N)))
                 (define hktypeblock
                   (string-append "FF[" (assoc-ref template '_--_) "], "
                                  (assoc-ref template 'A--N)))
                 (define traitname (string-append "Unpack" (number->string arity)))
                 (string-append "
|
-
-/**
- * Type class witnessing that type `PP` is equal to `FF[" (assoc-ref template 'A--N) "]` for some higher kinded type `FF[" (assoc-ref template '_--_) "]` and type(s) `" (assoc-ref template 'A--N) "`.
- * 
- * @author Miles Sabin
- */
-trait " traitname "[-PP, " hktypeblock "]
-
-object " traitname " {
-  implicit def unpack[" hktypeblock "]: " traitname "[" typeblock "] = new " traitname "[" typeblock "] {}
-}
|
")))
             #t))
         (replace 'build
           ,(build-scala-phase '(("core" "scala-shapeless"))
              #:scala-locations '("src/main/scala"
                                  "src/main/scala_2.13+")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-argonaut-shapeless
  (package
    (name "scala-argonaut-shapeless")
    (version "1.3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexarchambault/argonaut-shapeless")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0afa81fjhrjhb97iapkhv4jax9ply6mk9chmr0kwps12drjmwn03"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           ,(build-scala-phase '(("core" "scala-argonaut-shapeless"))
              #:scala-locations '("shared/src/main/scala")))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    (propagated-inputs
     `(("scala-argonaut" ,scala-argonaut)
       ("scala-shapeless" ,scala-shapeless)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public scala-coursier
  (package
    (name "scala-coursier")
    (version "2.0.16")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/coursier/coursier")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0cvy7bnqg8s2s4xljnw0p9r970yjqzg56f5nwgg0z2g18pafa3im"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-proprietary-interfaces
           (lambda _
             (delete-file
               "modules/cache/jvm/src/main/java/coursier/cache/internal/SigWinchNativeWindows.java")))
         (replace 'build
           ,(build-scala-phase
              '(("modules/util" "scala-coursier-util")
                ("modules/core" "scala-coursier-core")
                ("modules/paths" "scala-coursier-paths")
                ("modules/cache" "scala-coursier-cache")
                ("modules/coursier" "scala-coursier"))
              #:scala-arguments '("-Ymacro-annotations")))
         (add-after 'unpack 'remove-compat
           (lambda _
             (substitute* (find-files "." "\\.scala$")
               (("import scala.collection.compat.*") ""))
             #t))
         (replace 'install
           (install-jars "build")))))
    (native-inputs
     `(("scala" ,scala)))
    ;; XXX: dirs-dev is normally shaded, so should we really propagate it?
    (propagated-inputs
     `(("java-jsoup" ,java-jsoup)
       ("java-concurrent-reference-hash-map" ,java-concurrent-reference-hash-map)
       ("scala-argonaut-shapeless" ,scala-argonaut-shapeless)
       ("scala-data-class" ,scala-data-class)
       ("scala-dirs-dev-directories" ,scala-dirs-dev-directories)
       ("scala-fastparse" ,scala-fastparse)
       ("scala-simulacrum" ,scala-simulacrum)
       ("scala-windowsansi" ,scala-windowsansi)
       ("scala-xml" ,scala-xml)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public sbt-launcher
  (package
    (name "sbt-launcher")
    (version "1.3.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sbt/launcher")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0xff9w9arj91vgh5mbzb52r2l1ij42cby3kbbmkly1nf3j4xhrx2"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-manifest
           (lambda* (#:key outputs #:allow-other-keys)
             (define (split n str)
               (if (<= (string-length str) n)
                 (list str)
                 (cons (substring str 0 n) (split n (substring str n)))))
             (let ((dir "launcher-implementation/src/main/resources/META-INF"))
               (mkdir-p dir)
               (with-output-to-file (string-append dir "/MANIFEST.MF")
                 (lambda _
                   (format #t "Manifest-Version: 1.0\n")
                   ;(format #t "Class-Path: ~a\n"
                   ;        (string-join
                   ;          (split
                   ;            58
                   ;            (string-join
                   ;              (map
                   ;                (lambda (jar)
                   ;                  ;; We can't use absolute paths with java 8 :/
                   ;                  (string-append "../../../../../../../../.." jar))
                   ;                (cons
                   ;                  (string-append
                   ;                    (assoc-ref outputs "out")
                   ;                    "/share/java/sbt-launcher-interface.jar")
                   ;                  (string-split (getenv "CLASSPATH") #\:)))
                   ;              " "))
                   ;           "\n "))
                   (format #t "Main-Class: xsbt.boot.Boot\n\n"))))))
         (add-after 'unpack 'fill-template
           (lambda _
             (substitute* "launcher-implementation/src/main/input_sources/CrossVersionUtil.scala"
               (("\\$\\{\\{cross.package0\\}\\}") "xsbt")
               (("\\$\\{\\{cross.package1\\}\\}") "boot"))
             (copy-file "launcher-implementation/src/main/input_sources/CrossVersionUtil.scala"
                        "launcher-implementation/src/main/scala/CrossVersionUtil.scala")
             #t))
         (replace 'build
           ,(build-scala-phase
              '(("launcher-interface" "sbt-launcher-interface")
                ("launcher-implementation" "sbt-launcher-implementation"))))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("sbt-ivy" ,sbt-ivy)
       ("scala-coursier" ,scala-coursier)))
    (native-inputs
     `(("scala" ,scala)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; This subproject is the one that is provides the binary "sbt". Its role is
;; is to download and execute the full sbt, so it is not very usable for package
;; building right now.
(define-public sbt-launch
  (package
    (name "sbt")
    (version "1.5.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sbt/sbt")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k3ywjmahlhnr9rg29pac6n64x8pgaz5q7p4gn548mc3nm1fwzdj"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((launcher (assoc-ref inputs "sbt-launcher"))
                   (scala (assoc-ref inputs "scala")))
               (mkdir-p "build/sbt")
               (with-directory-excursion "build"
                 (copy-file "../launch/src/main/input_resources/sbt/sbt.boot.properties"
                            "sbt/sbt.boot.properties")
                 (substitute* "sbt/sbt.boot.properties"
                   (("\\$\\{\\{sbt.version\\}\\}") ,version)
                   (("\\$\\{\\{org\\}\\}") "org.scala-sbt"))
                 (copy-recursively "../launch/src/main/resources" ".")
                 ;; Create a standalon jar file, with all its dependencies
                 (for-each
                   (lambda (package)
                     (for-each
                       (lambda (jar)
                         (invoke "jar" "xf" jar))
                       (find-files (assoc-ref inputs package) ".*.jar$")))
                   '("java-concurrent-reference-hash-map" "java-jansi"
                     "sbt-ivy" "scala-coursier" "scala-dirs-dev-directories"
                     "scala-fastparse" "scala-xml"))
                 (invoke
                   "jar" "xf"
                   (string-append scala "/lib/scala-library.jar"))
                 (invoke
                   "jar" "xf"
                   (string-append launcher "/share/java/sbt-launcher-interface.jar"))
                 ;; Make it last so it can override META-INF/MANIFEST.MF
                 (invoke
                   "jar" "xf"
                   (string-append launcher "/share/java/sbt-launcher-implementation.jar"))
                 (invoke "jar" "cfm" "../sbt-launch.jar" "META-INF/MANIFEST.MF"
                         ".")
                 ;; We need to shade some of the dependencies, or they will conflict
                 ;; with what SBT downloads later, leading to errors such as
                 ;; NoSuchMethodError, etc.
                 ;; We use jarjar for that task.
                 (let ((jarjar
                        (car (find-files (assoc-ref inputs "java-jarjar") ".*.jar$")))
                       (injar "../sbt-launch.jar")
                       (outjar "../sbt-launch-shaded.jar"))
                   (with-output-to-file "rules"
                     (lambda _
                       (format #t (string-append
                                    "rule "
                                    "coursier.** "
                                    "xsbt.boot.internal.shaded."
                                    "coursier.@1~%"))
                       (format #t (string-append
                                    "rule "
                                    "fastparse.** "
                                    "coursier.core.shaded."
                                    "fastparse.@1~%"))
                       (format #t (string-append
                                    "rule "
                                    "dev.dirs.** "
                                    "coursier.chache.shaded.dirs."
                                    "dev.dirs.@1~%"))
                       (format #t (string-append
                                    "rule "
                                    "concurrentrefhashmap.** "
                                    "xsbt.boot.internal.shaded.concurrentrefhashmap."
                                    "concurrentrefhashmap.@1~%"))))
                   (invoke "java" "-jar" jarjar "process" "rules" injar outjar)
                   (delete-file injar)
                   (rename-file outjar injar))
                 ))))
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (substitute* "sbt"
               (("_to_be_replaced") ,(package-version this-package)))
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (chmod "sbt" #o755)
               (install-file "sbt" bin)
               (install-file "sbt-launch.jar" bin)))))))
    (inputs
     `(("java-concurrent-reference-hash-map" ,java-concurrent-reference-hash-map)
       ("java-jansi" ,java-jansi)
       ("java-jarjar" ,java-jarjar)
       ("sbt-launcher" ,sbt-launcher)
       ("sbt-ivy" ,sbt-ivy)
       ("scala" ,scala)
       ("scala-coursier" ,scala-coursier)
       ("scala-dirs-dev-directories" ,scala-dirs-dev-directories)
       ("scala-fastparse" ,scala-fastparse)
       ("scala-xml" ,scala-xml)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
