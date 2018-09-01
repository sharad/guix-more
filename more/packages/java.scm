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

(define-module (more packages java)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (guix svn-download)
  #:use-module (guix cvs-download)
  #:use-module (guix hg-download)
  #:use-module (guix utils)
  #:use-module (guix gexp)
  #:use-module (guix build syscalls)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (more packages maven)
  #:use-module (more packages python))

(define-public java-fastutil
  (package
    (name "java-fastutil")
    (version "8.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://fastutil.di.unimi.it/fastutil-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "039gh3b9a72dvc9jmijfhshd2i7bnj18p2yi67200qfrnw17wbkj"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sources
           (lambda _
             (invoke "make" "CC=gcc" "sources")
             #t))
         (add-after 'build 'generate-javadoc
           (lambda _
             (invoke "ant" "javadoc")
             #t))
         (replace 'install
           (install-jars ".")))))
    (home-page "http://fastutil.di.unimi.it/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; Maintained version of http-builder (groovyx.net.http)
(define-public java-http-builder-ng
  (package
    (name "java-http-builder-ng")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/http-builder-ng/"
                                  "http-builder-ng/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1acgdf5jnnsw32nbanwba5ax6m7y48c5zqdwrmpk3i2p9l5m30fd"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "http-builder-ng.jar"
       #:jdk ,icedtea-8
       #:source-dir "http-builder-ng-core/src/main/java"
       #:test-dir "http-builder-ng-core/src/test"
       #:tests? #f)); TODO: com.stehno.ersatz
    (inputs
     `(("groovy" ,groovy)
       ("java-apache-xml-commons-resolver" ,java-apache-xml-commons-resolver)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
       ("java-javax-mail" ,java-javax-mail)
       ("java-jsoup" ,java-jsoup)
       ("java-nekohtml" ,java-nekohtml)
       ("java-opencsv" ,java-opencsv)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-xerces" ,java-xerces)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://http-builder-ng.github.io/http-builder-ng")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-text
  (package
    (name "java-commons-text")
    ;; latest version is advertized to be 1.1.8
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/text/source/"
                                  "commons-text-" version "-src.tar.gz"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0rzffm5al0nkxcbrbbjnnwvpky4y6mv8j1cfnhbkfvxq1zlr3aim"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-text.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"))
    (inputs
     `(("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("java-assertj" ,java-assertj)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-text")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-opencsv
  (package
    (name "java-opencsv")
    ;; latest version is advertized to be 1.1.8
    (version "4.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://git.code.sf.net/p/opencsv/source")
                     (commit "Release_4_1")))
              ;(uri (string-append "https://sourceforge.net/code-snapshots/git/"
              ;                    "o/op/opencsv/source.git/opencsv-source-"
              ;                    "1a8d01d569f81390a88299a5344f2685dc690127.zip"))
              ;(file-name (string-append name "-" version ".zip"))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1dmhssidx81n0mc6ifzw7j1q0q7882iz2cjmwk8p3gissndjf5li"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "opencsv.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (add-before 'build 'remove-failing-test
           (lambda _
             ;; This file fails to build
             (delete-file "src/test/java/com/opencsv/bean/StatefulBeanToCsvTest.java")
             #t)))))
    (inputs
     `(("java-commons-beanutils" ,java-commons-beanutils)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-commons-text" ,java-commons-text)))
    (native-inputs
     `(("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (home-page "https://github.com/sirthias/parboiled")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-parboiled
  (package
    (name "java-parboiled")
    ;; latest version is advertized to be 1.1.8
    (version "1.1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sirthias/parboiled/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "02akw95mqwnx5w1gag5ymilavgc0kr5dsiachpzbps282qmlssr2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "parboiled.jar"
       #:source-dir "parboiled-core/src/main/java:parboiled-java/src/main/java"
       #:test-dir "parboiled-core/src/test"
       #:tests? #f)); requires scala
    (inputs
     `(("java-asm" ,java-asm)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-testng" ,java-testng)))
    (home-page "https://github.com/sirthias/parboiled")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-pegdown
  (package
    (name "java-pegdown")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sirthias/pegdown/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "18y97gvsvpqc9i7wvrq5zs2ir8ycd7f1igz6qgibrhw14i118xmx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "pegdown.jar"
       #:source-dir "src/main/java"))
    (inputs
     `(("java-parboiled" ,java-parboiled)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/sirthias/pegdown")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-nekohtml
  (package
    (name "java-nekohtml")
    (version "1.9.21")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://downloads.sourceforge.net/nekohtml/"
                                  "nekohtml-" version ".tar.gz"))
              (sha256
               (base32
                "1dzdvm3wyl32ljbk3f5kvpg7h9fzps86q4hvfscr232d6xz6f18j"))
              (modules '((guix build utils)))
              (snippet
                `(for-each delete-file (find-files "." ".*.jar")))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-classpath
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "build.xml"
               (("\\$\\{lib.dir\\}/xerces-@\\{xercesVersion\\}")
                (string-append (assoc-ref inputs "java-xerces") "/share/java"))
               ;; We don't have older versions of xerces, so we don't need to
               ;; get the compatibility with those.
               (("<compileWith xercesVersion=\".*\" bridge=\"2_0\"/>")
                "")
               (("<compileWith xercesVersion=\".*\" bridge=\"2_1\"/>")
                "")
               (("<compileWith xercesVersion=\".*\" bridge=\"2_2\"/>")
                ""))
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-xerces" ,java-xerces)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jcifs
  (package
    (name "java-jcifs")
    (version "1.3.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://jcifs.samba.org/src/jcifs-"
                                  version ".tgz"))
              (sha256
               (base32
                "19kzac3c19j0fyssibcj47868k8079wlj9azgsd7i6yqmdgqyk3y"))
              (modules '((guix build utils)))
              (snippet
                `(delete-file (string-append "jcifs-" ,version ".jar")))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-compiler
           (lambda _
             ;; Expects to find the compiler in /usr/local
             (substitute* "build.xml"
               (("executable=.*") ""))
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-classpathx-servletapi" ,java-classpathx-servletapi)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-airline
  (package
    (name "java-airline")
    (version "0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/airlift/airline/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "047ckxslpaw6j54zyfdf4b7sw71vcx7rqbwqa3lzz3pprhdfnm4x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "airline.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (system* "ant" "compile-tests")
             ;; This fails though
             (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":build/classes"
                                                         ":build/test-classes")
                             "org.testng.TestNG" "-verbose" "5" "-testclass"
                             "build/test-classes/io/airlift/airline/command/CommandTest.class")
             #t)))))
    (inputs
     `(("java-guava-23.5" ,java-guava-23.5)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)))
    (native-inputs
     `(("java-testng" ,java-testng)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "")
    (synopsis "Command line parser")
    (description "Airline is a Java annotation-based framework for parsing Git
like command line structures.  Airline contains a fully automated help system,
which generates man-page-like documentation driven by the Java annotations.")
    (license license:asl2.0)))

(define-public java-japicmp
  (package
    (name "java-japicmp")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/siom79/japicmp/archive/"
                                  "japicmp-base-" version ".tar.gz"))
              (sha256
               (base32
                "0kb3ja3j6kliakbqp1ypva1y42qllwvyin38ky3kxqzlkzhzpy9j"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "japicmp.jar"
       #:source-dir "japicmp/src/main/java"
       #:test-dir "japicmp/src/test"
       #:tests? #f; require org.junit.contrib
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "japicmp/src/main/resources" "build/classes"))))))
    (inputs
     `(("java-airline" ,java-airline)
       ("java-jboss-javassist" ,java-jboss-javassist)
       ("java-guava" ,java-guava)
       ("java-javax-inject" ,java-javax-inject)))
    (home-page "")
    (synopsis "Japicmp is a tool to compare two versions of a jar archive")
    (description "Japicmp is a tool to compare two versions of a jar archive.
It can also be used as a library.")
    (license license:asl2.0)))

(define-public java-jatl
  (package
    (name "java-jatl")
    (version "0.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/agentgt/jatl/archive/jatl-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0z3s4zpq97rwwn2gcfkhf28sib60d1sczg682fskyq1nfdxnlvgb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jatl.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-javaparser
  (package
    (name "java-javaparser")
    (version "3.5.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaparser/javaparser/"
                                  "archive/javaparser-parent-" version ".tar.gz"))
              (sha256
               (base32
                "04b1frvz65sg5jchw32ah32n2wj5agmil1fcq6kf1vxg78dj5d3d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javaparser.jar"
       #:source-dir "javaparser-core/src/main/java:javaparser-core/src/main/javacc-support"
       #:tests? #f; require org.junit.contrib
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-parser
           (lambda* _
             (mkdir-p "")
             (zero? (system* "javacc" "-DEBUG_PARSER=false"
                             "-DEBUG_TOKEN_MANAGER=false" "-JDK_VERSION=1.8"
                             "-GRAMMAR_ENCODING=UTF-8"
                             "-OUTPUT_DIRECTORY=javaparser-core/src/main/java"
                             "javaparser-core/src/main/javacc/java.jj")))))))
    (native-inputs
     `(("java-javacc" ,java-javacc)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl3+)))

(define-public java-jul-to-slf4j
  (package
    (inherit java-slf4j-api)
    (name "java-jul-to-slf4j")
    (arguments
     `(#:jar-name "jul-to-slf4j.jar"
       #:source-dir "jul-to-slf4j/src/main/java"
       #:test-dir "jul-to-slf4j/src/test"
       #:tests? #f)); Depend on log4j-1.2 (not log4j-1.2-api)
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)))))

;; Actually we need to build the native library too.
(define-public java-native-platform
  (package
    (name "java-native-platform")
    (version "0.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/adammurdoch/"
                                  "native-platform/archive/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1if8h1lz8rh6gv6rrych63j2a03cfcqshb5c2973xsfs7jfrvbrr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "native-platform.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); TODO: fix build.xml to run Groovy tests
    (home-page "https://github.com/EsotericSoftware/minlog")
    (synopsis "Java bindings for various native APIs")
    (description "Native-platform is a collection of cross-platform Java APIs
for various native APIs.  It currently supports OS X, Linux, Windows and
FreeBSD on Intel architectures")
    (license license:asl2.0)))

(define-public java-minlog
  (package
    (name "java-minlog")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/EsotericSoftware/minlog/"
                                  "archive/minlog-" version ".tar.gz"))
              (sha256
               (base32
                "1vciwr6zw6bky70fi13sa85jc27r5nk5bzii8kdkblfaakmy8ifb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "minlog.jar"
       #:tests? #f)); No tests
    (home-page "https://github.com/EsotericSoftware/minlog")
    (synopsis "Logging library")
    (description "MinLog is a tiny Java logging library which features:
@itemize
@item Zero overhead: Logging statements below a given level can be
      automatically removed by javac at compile time.  This means applications
      can have detailed trace and debug logging without having any impact on
      the finished product.
@item Extremely lightweight: The entire project consists of a single Java file
      with ~100 non-comment lines of code.
@item Simple and efficient: The API is concise and the code is very efficient
      at runtime.
@end itemize")
    (license license:bsd-3)))

(define-public java-reflectasm
  (package
    (name "java-reflectasm")
    (version "1.11.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/EsotericSoftware/"
                                  "reflectasm/archive/reflectasm-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1rcgr5rm2g0jl2z0qk1bddlq72mh16ywznyy8pra0ns8xk6hwa8g"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "reflectasm.jar"
       #:tests? #f; Tests are not in a java subdirectory
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-asm
           (lambda _
             ;; asm has been renamed
             (substitute* '("src/com/esotericsoftware/reflectasm/ConstructorAccess.java"
                            "src/com/esotericsoftware/reflectasm/FieldAccess.java"
                            "src/com/esotericsoftware/reflectasm/MethodAccess.java")
               (("com.esotericsoftware.asm") "org.objectweb.asm"))
             #t)))))
    (inputs
     `(("java-asm" ,java-asm)))
    (home-page "https://github.com/EsotericSoftware/reflectasm")
    (synopsis "Reflection library for Java")
    (description "ReflectASM is a very small Java library that provides high
performance reflection by using code generation.  An access class is generated
to set/get fields, call methods, or create a new instance.  The access class
uses bytecode rather than Java's reflection, so it is much faster.  It can
also access primitive fields via bytecode to avoid boxing.")
    (license license:bsd-3)))

(define-public java-kryo
  (package
    (name "java-kryo")
    (version "4.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/EsotericSoftware/kryo/"
                                  "archive/kryo-parent-" version ".tar.gz"))
              (sha256
               (base32
                "0l0mwxfym29ssvxxwawg8h6psnzbb1dbhqfzhhhxxbm7p79xzga5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "kryo.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); No tests
    (inputs
     `(("java-minlog" ,java-minlog)
       ("java-objenesis" ,java-objenesis)
       ("java-reflectasm" ,java-reflectasm)))
    (home-page "https://github.com/EsotericSoftware/kryo")
    (synopsis "Object graph serialization framework")
    (description "Kryo is a fast and efficient object graph serialization
framework for Java.  The project is useful any time objects need to be
persisted, whether to a file, database, or over the network.")
    (license license:bsd-3)))

(define-public java-jformatstring
  (package
    (name "java-jformatstring")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://framagit.org/tyreunom/j-format-string/-/archive/"
                                  version "/j-format-string-" version ".tar.gz"))
              (sha256
               (base32
                "0ypqnchxif7rkq3n9vshj3j0d24kw3wr6k00yb5i14jm59m21r90"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (for-each delete-file (find-files "." ".*.jar"))))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jformatstring.jar"
       ; tests are not in a java directory
       #:tests? #f))
    (inputs
     `(("java-jsr305" ,java-jsr305)
       ("java-junit" ,java-junit)
       ("java-spotbugs-annotations" ,java-spotbugs-annotations)))
    (home-page "http://findbugs.sourceforge.net/")
    (synopsis "")
    (description "")
    ;; license: gpl2 only, with classpath exception
    (license license:gpl2)))

(define-public java-commons-bcel-5
  (package
    (inherit java-commons-bcel)
    (version "5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/commons/"
                                  "bcel/source/bcel-" version "-src.tar.gz"))
              (sha256
               (base32
                "15djmay72mzk38v183j6fr3j7fj3dhkp5z53bsvdfyc3ldcrs0v8"))))
    (arguments
     `(#:jar-name "commons-bcel.jar"
       #:source-dir "src/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-iso8859
           (lambda _
             (substitute* "build.xml"
               (("<javac ") "<javac encoding=\"iso-8859-1\" "))
             #t)))))))

(define-public java-findbugs
  (package
    (name "java-findbugs")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://prdownloads.sourceforge.net/findbugs/"
                                  "findbugs-" version "-source.zip"))
              (sha256
               (base32
                "1zrkpmd87lcz62lk5dr0mpf5gbzrd1i8mmrv510fs6fla1jwd3mx"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (for-each delete-file (find-files "." ".*.jar"))))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:test-target "test"
       #:make-flags (list "-Dgitrnum=0")
       #:jdk ,icedtea-7
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-formatstring
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xf" (assoc-ref inputs "java-jformatstring"))
             (copy-recursively "j-format-string-3.0.0/src/java" "src/java")
             (copy-recursively "j-format-string-3.0.0/src/junit" "src/junit")
             #t))
         (add-before 'build 'fix-bcel-version
           (lambda _
             ;; Findbugs requires an older version of bcel. Fix it to support
             ;; newer bcel versions.
             (with-directory-excursion "src/java/edu/umd/cs/findbugs"
               (with-fluids ((%default-port-encoding "ISO-8859-1"))
                 (substitute* '("visitclass/PreorderVisitor.java"
                                "StackMapAnalyzer.java"
				"classfile/engine/ClassParserUsingASM.java")
                   ;; The two classes were merged in the latter
                   (("StackMapTable") "StackMap")
		   (("Constants") "Const")
		   (("Const2") "Constants2")
		   (("getByteCodeOffsetDelta") "getByteCodeOffset"))
		 (substitute* "detect/DumbMethods.java"
		   (("import org.apache.bcel.classfile.Attribute;")
		    "import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.Const;")
		   (("MAJOR_1") "Const.MAJOR_1"))
                 (substitute* "ba/AbstractFrameModelingVisitor.java"
                   (("VisitorSupportsInvokeDynamic") "Visitor"))
		 (substitute* "xml/XMLUtil.java"
		   (("<T> List<T>") "List<Node>"))
                 (substitute* "visitclass/PreorderVisitor.java"
                   (("import org.apache.bcel.classfile.Attribute;")
                    "import org.apache.bcel.classfile.Attribute;
import org.apache.bcel.Const;
import org.apache.bcel.classfile.BootstrapMethods;
import org.apache.bcel.classfile.ConstantInvokeDynamic;
import org.apache.bcel.classfile.ConstantMethodHandle;
import org.apache.bcel.classfile.ConstantMethodType;
import org.apache.bcel.classfile.MethodParameters;
import org.apache.bcel.classfile.ParameterAnnotationEntry;")
                   (("^}")
                    "    @Override
    public void visitBootstrapMethods(BootstrapMethods arg0) {
        // TODO Auto-generated method stub
    }
    @Override
    public void visitConstantInvokeDynamic(ConstantInvokeDynamic arg0) {
        // TODO Auto-generated method stub
    }
    @Override
    public void visitConstantMethodHandle(ConstantMethodHandle arg0) {
        // TODO Auto-generated method stub
    }
    @Override
    public void visitConstantMethodType(ConstantMethodType arg0) {
        // TODO Auto-generated method stub
    }
    @Override
    public void visitMethodParameters(MethodParameters arg0) {
        // TODO Auto-generated method stub
    }
    @Override
    public void visitParameterAnnotationEntry(ParameterAnnotationEntry arg0) {
        // TODO Auto-generated method stub
    }
}"))))
             #t))
         (add-before 'build 'remove-osx
           (lambda _
             ;; Requires AppleJavaExtensions.jar (com.apple.eawt.*)
             (delete-file "src/gui/edu/umd/cs/findbugs/gui2/OSXAdapter.java")
             #t))
         (add-before 'build 'find-dependencies
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each
               (lambda (input)
                 (for-each
                   (lambda (file)
                     (display file) (display " -> ")
                     (display (string-append "lib/" (basename file))) (newline)
                     (newline)
                     (copy-file file (string-append "lib/" (basename file))))
                   (find-files (assoc-ref inputs input) ".*.jar")))
               '("java-jsr305" "java-commons-bcel-5" "java-dom4j" "java-asm"
                 "java-jcip-annotations" "java-commons-lang"))
             #t))
         (add-before 'build 'no-git
           (lambda _
             ;; We are not building a git revision
             (substitute* "build.xml"
               ((",-get-git-revision") ""))
             #t))
         (replace 'install
           (install-jars "build")))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-commons-bcel-5" ,java-commons-bcel-6.0)
       ;("java-commons-bcel-5" ,java-commons-bcel-5)
       ("java-commons-lang" ,java-commons-lang)
       ("java-dom4j" ,java-dom4j)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-jformatstring"
        ,(origin
           (method url-fetch)
           (uri (string-append "https://framagit.org/tyreunom/j-format-string/-/archive/"
                               "3.0.0/j-format-string-3.0.0.tar.gz"))
           (sha256
            (base32
             "0ypqnchxif7rkq3n9vshj3j0d24kw3wr6k00yb5i14jm59m21r90"))
           (modules '((guix build utils)))
           (snippet
             '(begin
                (for-each delete-file (find-files "." ".*.jar"))))))))
    (home-page "http://findbugs.sourceforge.net/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-spotbugs-annotations
  (package
    (name "java-spotbugs-annotations")
    (version "3.1.6")
    (source (origin
              (method url-fetch)
	      (uri (string-append "https://github.com/spotbugs/spotbugs/archive/"
				  version ".tar.gz"))
              (sha256
               (base32
                "198gzk2vs4id90fxgpida51ygwpb31xwkv6lf91kgmvqcsknf6y4"))
              (modules '((guix build utils)))
              (snippet
                '(begin
                   (for-each delete-file (find-files "." ".*.jar"))))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "spotbugs.jar"
       #:source-dir "spotbugs-annotations/src/main/java"
       #:tests? #f
       #:jdk ,icedtea-8))
    (inputs
     `(("java-jsr305" ,java-jsr305)))
    (home-page "https://spotbugs.github.io/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-spotbugs
  (package
    (inherit java-spotbugs-annotations)
    (name "java-spotbugs")
    (arguments
     `(#:jar-name "spotbugs.jar"
       #:source-dir "spotbugs/src/main/java:spotbugs-annotations/src/main/java:spotbugs/src/gui"
       #:test-dir "spotbugs/src/test"
       #:tests? #f; depend on jdepend
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
	 (add-before 'build 'remove-osx
	   (lambda _
	     ;; Requires AppleJavaExtensions.jar (com.apple.eawt.*)
	     (delete-file "spotbugs/src/gui/edu/umd/cs/findbugs/gui2/OSXAdapter.java")
	     #t)))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-commons-bcel" ,java-commons-bcel)
       ("java-commons-lang" ,java-commons-lang)
       ("java-dom4j" ,java-dom4j)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jformatstring" ,java-jformatstring)
       ("java-jsr305" ,java-jsr305)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (synopsis "")
    (description "")))

(define-public java-mangosdk-spi
  (package
    (name "java-mangosdk-spi")
    (version "1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rspilker/spi")
                     (commit "ca933626bdd8084f9170fa478388bf1eabe10d8c")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1fbfn46923hd9lmby18zckannbw0fql9lrdf2wpbq3s9pm4zg0sc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "spi.jar"
       #:source-dir "org.mangosdk.spi/src/main/java"
       #:test-dir "org.mangosdk.spi/src/test"
       #:tests? #f;; FIXME: tests don't build
       #:jdk ,icedtea-8))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/rspilker/spi")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-lombok-core
  (package
    (name "java-lombok-core")
    (version "1.16.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/rzwitserloot/lombok/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "132p2aasip2n0633pas5xprmlzpwxc9i52na4hy7k3hzvd6bwd2j"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "lombok-core.jar"
       #:source-dir "src/core"
       #:tests? #f)); No specific test
    (inputs
     `(("java-asm" ,java-asm)
       ("java-eclipse-jdt-core" ,java-eclipse-jdt-core)
       ("java-lombok-utils" ,java-lombok-utils)
       ("java-mangosdk-spi" ,java-mangosdk-spi)
       ("java-osgi-framework" ,java-osgi-framework)))
    (home-page "https://projectlombok.org/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-lombok-utils
  (package
    (inherit java-lombok-core)
    (name "java-lombok-utils")
    (arguments
     `(#:jar-name "lombok-utils.jar"
       #:source-dir "src/utils"
       #:jdk ,icedtea-8
       #:tests? #f; No specific test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           (lambda _
             (delete-file-recursively "src/utils/lombok/javac")
             ;(delete-file-recursively "src/utils/lombok/javac/java6")
             ;(delete-file-recursively "src/utils/lombok/javac/java7")
             ;; Troubles with extending com.sun.tools.javac.code.Type
             ;(delete-file-recursively "src/utils/lombok/javac/java")
             #t)))))
    (inputs
     `(("java-eclipse-jdt-core" ,java-eclipse-jdt-core)))))

(define-public java-asm-6
  (package
    (inherit java-asm)
    (version "6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.forge.ow2.org/asm/"
                                  "asm-" version ".tar.gz"))
              (sha256
               (base32
                "115l5pqblirdkmzi32dxx7gbcm4jy0s14y5wircr6h8jdr9aix00"))))
    (arguments
      (substitute-keyword-arguments (package-arguments java-asm)
        ((#:make-flags flags)
         `(list "-Dobjectweb.ant.tasks.path=foo"
                (string-append "-Dbiz.aQute.bnd.path="
                               (assoc-ref %build-inputs "java-aqute-bndlib")
                               "/share/java/java-bndlib.jar")))))
    (inputs
     `(("java-aqute-bndlib" ,java-aqute-bndlib)))))

(define-public java-byte-buddy-dep
  (package
    (name "java-byte-buddy-dep")
    (version "1.7.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/raphw/byte-buddy/archive/"
                                  "byte-buddy-" version ".tar.gz"))
              (sha256
               (base32
                "1dyx3hp1fnw30ndk341bscr8x9sy75f8sfy4hrrwcwg4hrdg4i36"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "byte-buddy-dep.jar"
       #:source-dir "byte-buddy-dep/src/main/java"
       #:test-dir "byte-buddy-dep/src/test"
       #:tests? #f; FIXME: can't build tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-annotations
           (lambda _
             (with-directory-excursion "byte-buddy-dep/src/main/java/net/bytebuddy"
               (substitute* (find-files "." ".*.java")
                 (("@EqualsAndHashCode.*") "")
                 (("import lombok.EqualsAndHashCode;") ""))
               (substitute* (find-files "." ".*.java")
                 (("@SuppressFBWarnings.*") "")
                 (("import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;") "")))
             #t)))))
    (inputs
     `(("java-asm-6" ,java-asm-6)))
    (home-page "http://bytebuddy.net/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-intellij-annotation
  (package
    (name "java-intellij-annotation")
    (version "181-1945")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JetBrains/intellij-community/"
                                  "archive/idea/" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1mvixizk75pvhaibdy8lriy5prasg0pap80vak6vfj67kw8lmksk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-intellij-annotation.jar"
       #:source-dir "platform/annotations/src"
       #:tests? #f)); no tests
    (home-page "http://jetbrains.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-spockframework-core
  (package
    (name "java-spockframework-core")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/spockframework/spock/"
                                  "archive/spock-" version ".tar.gz"))
              (sha256
               (base32
                "1b2bybldlnid41irkavd09bkkzfjyvc2d33grpn2vgaiyw9gzz8z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "spock-core.jar"
       #:source-dir "spock-core/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f)); No tests
    (inputs
     `(("groovy" ,groovy)
       ("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-intellij-annotation" ,java-intellij-annotation)
       ("java-junit" ,java-junit)
       ("java-objenesis" ,java-objenesis)
       ("java-byte-buddy-dep" ,java-byte-buddy-dep)
       ,@(package-inputs groovy)))
    (home-page "http://spockframework.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jcip-annotations
  (package
    (name "java-jcip-annotations")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri "http://jcip.net/jcip-annotations-src.jar")
              (sha256
               (base32
                "1z4y6ga2yc01z4qwcdi6mawky8kk6pg3j1l7r3rwb9001fz5q7r2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jcip-annotations.jar"
       #:source-dir "."
       #:tests? #f)); No tests
    (home-page "http://jcip.net")
    (synopsis "Annotations for concurrency")
    (description "JCIP annotations implement the annotations described in the
\"Java Concurrency In Practice\" (JCIP) book.")
    (license license:cc-by2.0))); cc-by2.5

(define-public java-openjfx
  (package
    (name "java-openjfx")
    ;; This is the last version that can be built for java8
    (version "8u141-b14")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://hg.openjdk.java.net/openjfx/8u-dev/rt"
                                  "/archive/d6db71e77bb1.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qjmwrrkk5z3nzz08ihy0qww8y0922wiil95pz2na0d4ql3cx625"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-openjfx.jar"
       #:source-dir "buildSrc/src/main/java"
       #:test-dir "buildSrc/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-jsl-parser
           (lambda _
             (zero? (system* "antlr3" "-o" "buildSrc/src/main/java/com/sun/scenario/effect/compiler"
                             "buildSrc/src/main/antlr/JSL.g")))))))
    (inputs
     `(("antlr3" ,antlr3)
       ("java-stringtemplate" ,java-stringtemplate)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://openjdk.java.net/projects/openjfx/")
    (synopsis "")
    (description "")
    (license license:gpl2)));with classpath exception

(define-public java-openjfx-base
  (package (inherit java-openjfx)
    (name "java-openjfx-base")
    (arguments
     `(#:jar-name "java-openjfx-base.jar"
       #:source-dir "modules/base/src/main/java:modules/base/src/main/java8:modules/base/src/main/version-info"
       #:test-dir "modules/base/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-empty-file
           (lambda _
             ;; These files are completely commented, but junit expects them to
             ;; contain a class, so tests fail.
             (delete-file "modules/base/src/test/java/com/sun/javafx/property/adapter/PropertyDescriptorTest.java")
             (delete-file "modules/base/src/test/java/com/sun/javafx/property/adapter/ReadOnlyPropertyDescriptorTest.java")
             (delete-file "modules/base/src/test/java/javafx/beans/property/PropertiesTest.java")
             (delete-file "modules/base/src/test/java/javafx/beans/property/adapter/ReadOnlyJavaBeanPropertyBuilder_General_Test.java")
             ;; This one fails
             (delete-file "modules/base/src/test/java/com/sun/javafx/runtime/VersionInfoTest.java"))))))
    (inputs
     `(("java-openjfx" ,java-openjfx)))))

(define-public java-openjfx-graphics
  (package (inherit java-openjfx)
    (name "java-openjfx-graphics")
    (arguments
     `(#:jar-name "java-openjfx-graphics.jar"
       #:source-dir "modules/graphics/src/main/java"
       #:tests? #f; require X?
       #:test-dir "modules/graphics/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("java-openjfx" ,java-openjfx)
       ("java-openjfx-base" ,java-openjfx-base)
       ("java-swt" ,java-swt)))))

(define-public java-openjfx-media
  (package (inherit java-openjfx)
    (name "java-openjfx-media")
    (inputs
     `(("java-openjxf-graphics" ,java-openjfx-graphics)
       ("java-openjxf-base" ,java-openjfx-base)
       ("java-openjfx" ,java-openjfx)))
    (arguments
     `(#:jar-name "java-openjfx-media.jar"
       #:source-dir "modules/media/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))))

(define-public java-brotli-dec
  (package
    (name "java-brotli-dec")
    (version "0.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/brotli/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "03zfr6lw6ry643l6pbg0myg52clypxd0y0igd84dslchf3svvkb9"))))
    (build-system ant-build-system)
    (native-inputs
     `(("java-junit" ,java-junit)))
    (arguments
     `(#:jar-name "brotli-dec.jar"
       #:tests? #f; no test target
       #:source-dir "java"))
    (home-page "https://brotli.org")
    (synopsis "Lossless compression algorithm")
    (description "Brotli is a generic-purpose lossless compression algorithm
that compresses data using a combination of a modern variant of the LZ77
algorithm, Huffman coding and 2nd order context modeling, with a compression
ratio comparable to the best currently available general-purpose compression
methods.  It is similar in speed with deflate but offers more dense compression.")
    (license license:expat)))

(define-public java-commons-compress-latest
  (package
    (inherit java-commons-compress)
    (version "1.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/compress/source/"
                                  "commons-compress-" version "-src.tar.gz"))
              (sha256
               (base32
                "1msfjbknfgx78j96fsiqk44r45plz10x9sw88flrpf3yaf4d3br1"))))
    (inputs
     `(("java-brotli-dec" ,java-brotli-dec)
       ,@(package-inputs java-commons-compress)))))

(define-public java-jmapviewer
  (package
    (name "java-jmapviewer")
    (version "2.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://svn.openstreetmap.org/applications/viewer/jmapviewer/releases/"
                                  version "/JMapViewer-" version "-Source.zip"))
              (sha256
               (base32
                "1s8agib0pncbmavgzb372qdwvgwc9jr77p13hqzak6xjlbnxldar"))))
    (build-system ant-build-system)
    (native-inputs
     `(("unzip" ,unzip)))
    (arguments
     `(#:build-target "pack"
       #:jdk ,icedtea-8
       #:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'clean
           (lambda* _
             (zero? (system* "ant" "clean"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java/")))
               (mkdir-p dir)
               (copy-file "JMapViewer.jar" (string-append dir "JMapViewer.jar"))))))))
    (home-page "https://wiki.openstreetmap.org/wiki/JMapViewer")
    (synopsis "")
    (description "")
    (license license:gpl2)))

(define-public java-zstd
  (package
    (name "java-zstd")
    (version "1.3.0-1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/luben/zstd-jni/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1an6gqps3n1ddxdz8vyzdrq7xz2p6nvwdwpmwf52mfia0w71xl9a"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-zstd.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); Require scala
    (inputs
     `(("zstd" ,zstd)))
    (home-page "https://github.com/luben/zstd-jni")
    (synopsis "")
    (description "")
    (license license:bsd-2)))

(define-public java-josm
  (package
    (name "java-josm")
    (version "14066")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                    (url "https://svn.openstreetmap.org/applications/editors/josm")
                    (revision (string->number version))))
              (sha256
               (base32
                "1kf7cr80n2y9dwjabn8l9r5n64kjf8vk0ial1fzc870gg785s7cw"))
              (file-name (string-append name "-" version))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar"))
                   (with-directory-excursion "core"
                     (delete-file-recursively "test/lib")
                     (delete-file-recursively "windows"))
                   #t))))
    (build-system ant-build-system)
    (native-inputs
     `(("java-javacc" ,java-javacc)))
    (propagated-inputs
     `(("java-jmapviewer" ,java-jmapviewer)
       ("java-tomcat" ,java-tomcat)
       ("java-brotli-dec" ,java-brotli-dec)
       ("java-xz" ,java-xz)
       ("java-velocity" ,java-velocity)
       ("java-openjfx" ,java-openjfx)
       ("java-openjfx-base" ,java-openjfx-base)
       ("java-openjfx-media" ,java-openjfx-media)
       ("java-openjfx-graphics" ,java-openjfx-graphics)
       ("java-avalon-framework-api" ,java-avalon-framework-api)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-commons-jcs" ,java-commons-jcs)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-compress" ,java-commons-compress-latest)
       ("java-zstd" ,java-zstd)))
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:jar-name "josm.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "core")
             #t))
         (add-after 'chdir 'rm-build.xml
           (lambda* _
             (delete-file "build.xml")
             #t))
         (add-before 'build 'fix-revision
           (lambda* _
             (with-output-to-file "REVISION.XML"
               (lambda _
                 (display
                   (string-append "<info><entry><commit revision=\"" ,version "\">"
                                  "<date>1970-01-01 00:00:00 +0000</date>"
                                  "</commit></entry></info>"))))
             #t))
         (add-before 'build 'generate-parser
           (lambda* _
             (let* ((dir "src/org/openstreetmap/josm/gui/mappaint/mapcss")
                    (out (string-append dir "/parsergen"))
                    (file (string-append dir "/MapCSSParser.jj")))
               (mkdir-p "src/org/openstreetmap/josm/gui/mappaint/mapcss/parsergen")
               (invoke "javacc" "-DEBUG_PARSER=false"
                       "-DEBUG_TOKEN_MANAGER=false" "-JDK_VERSION=1.8"
                       "-GRAMMAR_ENCODING=UTF-8"
                       (string-append "-OUTPUT_DIRECTORY=" out)
                       file))
             #t))
         (add-after 'build 'generate-epsg
           (lambda _
             (system* "javac" "scripts/BuildProjectionDefinitions.java"
                      "-cp" "build/classes")
             (mkdir-p "data/projection")
             (with-output-to-file "data/projection/custom-epsg"
               (lambda _ (display "")))
             (invoke "java" "-cp" "build/classes:scripts:."
                     "BuildProjectionDefinitions" ".")
             #t))
         (add-after 'generate-epsg 'copy-data
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "data" "build/classes/data")
             #t))
         (add-before 'install 'regenerate-jar
           (lambda _
             ;; We need to regenerate the jar file to add data.
             (delete-file "build/jar/josm.jar")
             (invoke "jar" "-cf" "build/jar/josm.jar" "-C"
                     "build/classes" ".")
             #t))
         (add-before 'build 'copy-styles
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "styles" "build/classes/styles")
             #t))
         (add-before 'build 'copy-images
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "images" "build/classes/images")
             #t))
         (add-before 'build 'copy-revision
           (lambda _
             (mkdir-p "build/classes")
             (with-output-to-file "build/classes/REVISION"
               (lambda _
                 (display
                   (string-append "Revision: " ,version "\n"
                                  "Is-Local-Build: true\n"
                                  "Build-Date: 1970-01-01 00:00:00 +0000\n"))))
             #t))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/josm")
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    (assoc-ref inputs "jdk") "/bin/java"
                                    " -cp " out "/share/java/josm.jar:"
                                    (getenv "CLASSPATH")
                                    " org.openstreetmap.josm.gui.MainApplication"))))
               (chmod (string-append bin "/josm") #o755))
             #t)))))
    (home-page "https://josm.openstreetmap.de")
    (synopsis "OSM editor")
    (description "OSM editor.")
    (license license:gpl2+)))

;; As of 2010-09-01, the ORO project is retired
(define-public java-jakarta-regexp
  (package
    (name "java-jakarta-regexp")
    (version "1.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/jakarta/regexp/"
                                  "jakarta-regexp-" version ".tar.gz"))
              (sha256
               (base32
                "0zg9rmyif48dck0cv6ynpxv23mmcsx265am1fnnxss7brgw0ms3r"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar"
       #:tests? #f; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-bin
           (lambda _
             (delete-file (string-append "jakarta-regexp-" ,version ".jar"))))
         (replace 'install
           (install-jars "build")))))
    (home-page "https://jakarta.apache.org/oro/")
    (synopsis "Text-processing for Java")
    (description "The Jakarta-ORO Java classes are a set of text-processing
Java classes that provide Perl5 compatible regular expressions, AWK-like
regular expressions, glob expressions, and utility classes for performing
substitutions, splits, filtering filenames, etc.  This library is the successor
of the OROMatcher, AwkTools, PerlTools, and TextTools libraries originally
from ORO, Inc.")
    (license license:asl2.0)))

(define-public java-jboss-annotations-api-spec
  (package
    (name "java-jboss-annotations-api-spec")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-annotations-api_spec/"
                                  "archive/jboss-annotations-api_" version
                                  "_spec-1.0.1.Final.tar.gz"))
              (sha256
               (base32
                "0qmczwma8wiyqvs95yy6dibjhkzqyhfvik1qk01bxp9kzah9k882"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-annotations-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "."
       #:tests? #f)); no tests
    (home-page "https://github.com/jboss/jboss-annotations-api_spec")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-jboss-transaction-api-spec
  (package
    (name "java-jboss-transaction-api-spec")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-transaction-api_spec/"
                                  "archive/jboss-transaction-api_" version
                                  "_spec-1.0.1.Final.tar.gz"))
              (sha256
               (base32
                "0yhyjf9p21cjs84nz66bxnmzdxdr98kfpbyp5gr3js0hwl6zz7xb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-transaction-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("java-cdi-api" ,java-cdi-api)
       ("java-jboss-interceptors-api-spec" ,java-jboss-interceptors-api-spec)))
    (home-page "https://github.com/jboss/jboss-transaction-api_spec")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-aspectj-weaver
  (package
    (name "java-aspectj-weaver")
    (version "1.8.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://eclipsemirror.itemis.de/eclipse/tools"
                                  "/aspectj/aspectj-" version "-src.jar"))
              (sha256
               (base32
                "0r16lgzindqf4xhdmdyk9j6p15nak2fwhqlp42yg3axvn8fx6r23"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-aspectj-weaver.jar"
       #:source-dir "."
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-jar
           (lambda _
             (mkdir-p "weaver-src")
             (chdir "weaver-src")
             (zero? (system* "jar" "xf" "../src/aspectjweaver1.8.10-src.jar"))))
         (add-after 'unpack-jar 'remove-propriatory
           (lambda _
             ;; this file depends on JRockit, for which I can't find a free implementation
             (delete-file "org/aspectj/weaver/loadtime/JRockitAgent.java")))
         (add-after 'unpack-jar 'rename-lib-back
           (lambda _
             ;; aj.org.objectweb.asm is actually java-asm, renamed
             (substitute* "org/aspectj/weaver/bcel/asm/StackMapAdder.java"
               (("aj.org.objectweb.asm") "org.objectweb.asm"))))
         (add-before 'build 'copy-ressource
           (lambda _
             (mkdir-p "build/classes")
             (copy-file "aspectj_1_5_0.dtd" "build/classes/aspectj_1_5_0.dtd"))))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-asm" ,java-asm)))
    (home-page "https://www.eclipse.org/aspectj")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-aspectj-rt
  (package
    (inherit java-aspectj-weaver)
    (name "java-aspectj-rt")
    (arguments
     `(#:jar-name "java-aspectj-rt.jar"
       #:source-dir "."
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-jar
           (lambda _
             (mkdir-p "rt-src")
             (chdir "rt-src")
             (zero? (system* "jar" "xf" "../src/aspectjrt1.8.10-src.jar")))))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-asm" ,java-asm)))
    (description "")))

(define-public java-jsr107
  (package
    (name "java-jsr107")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jsr107/jsr107spec/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "19gxsanr9l3cbvpxzvvgs2cgxbpbl6llmfg7nbmdzxpv7mhmprxs"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jsr107.jar"
       #:source-dir "src/main/java"
       ; no tests
       #:tests? #f))
    (inputs
     `(("java-cdi-api" ,java-cdi-api)))
    (home-page "https://github.com/jsr107/jsr107spec")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-minimal-json
  (package
    (name "java-minimal-json")
    (version "0.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ralfstx/minimal-json/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y51icz6bdzd3x9nzf9npwwc7inag3hn1b685izys7qy0kgw9nih"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-minimal-json.jar"
       #:source-dir "com.eclipsesource.json/src/main/java"
       #:test-dir "com.eclipsesource.json/src/test"
       #:test-exclude
       ;; Unable to set MockitoNamingPolicy on cglib generator which creates FastClasses
       (list "**/JsonValue_Test.java")))
    (native-inputs
     `(("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (home-page "https://github.com/ralfstx/minimal-json")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public javacc-6
  (package
    (inherit javacc)
    (version "6.1.3")
    (source (origin
	      (method url-fetch)
	      (uri (string-append "https://github.com/javacc/javacc/archive/release_"
                                  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
				  ".tar.gz"))
              (sha256
               (base32
                "03xpipk365szfzrab7divlr1i1r58j1hh47mhj5cpj1kv9zc2p6c"))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
	 (add-after 'unpack 'delete-bundled-libs
	   (lambda _
	     (delete-file-recursively "lib")
	      #t))
	 (replace 'install (install-jars "target")))))
    (native-inputs
     `(("java-junit" ,java-junit)))))

(define-public java-slf4j-jdk14
  (package
    (inherit java-slf4j-api)
    (name "java-slf4j-jdk14")
    (arguments
     `(#:jar-name "slf4j-jdk14.jar"
       #:source-dir "slf4j-jdk14/src/main"
       #:test-dir "slf4j-jdk14/src/test"
       ;; Require test files from slf4j-api
       #:tests? #f))
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (synopsis "")
    (description "")))

(define-public java-log4j-over-slf4j
  (package
    (inherit java-slf4j-api)
    (name "java-log4j-over-slf4j")
    (arguments
     `(#:jar-name "log4j-over-slf4j.jar"
       #:source-dir "log4j-over-slf4j/src/main"
       #:test-dir "log4j-over-slf4j/src/test"))
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-slf4j-jdk14" ,java-slf4j-jdk14)))
    (synopsis "")
    (description "")))

;; NOTE FOR JDOM:
;;
;; - remove lib/
;; - add dependencies (cobertura, ...)
;; - remove java 1.2 version
;; - break dependency cycle with jaxen
(define-public java-jdom-for-freemarker
  (package
    (inherit java-jdom)
    (version "1.0b8")
    (source (origin
              (method url-fetch)
              (uri "http://jdom.org/dist/binary/archive/jdom-b8.tar.gz")
              (sha256
               (base32
                "1y26baiamx67zl2lkqrjd19my3vz4xbjhv9l1iylirq0fcr9v7a1"))))
    (arguments
     `(#:build-target "package"
       #:tests? #f; tests are part of the package target
       #:phases
       (modify-phases %standard-phases
         ;(add-before 'build 'remove-collections
         ;  (lambda _
         ;    (delete-file "lib/collections.jar")
         ;    #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((jar-dir (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p jar-dir)
               (install-file "build/jdom.jar" jar-dir)
               #t))))))))

(define-public java-apache-freemarker
  (package
    (name "java-apache-freemarker")
    (version "2.3.28")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/freemarker/engine/" version
                                  "/source/apache-freemarker-" version "-src.tar.gz"))
              (sha256
               (base32
                "0zar7lrjliklldihhpn0v5j3n4jlc022rj299yzmwc1yqzj7nzmv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-apache-freemarker.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; TODO: require spring-framework-test.
       #:jdk ,icedtea-8
       ;#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-unpackaged-dependencies
	   ;; TODO: package these dependencies
	   (lambda _
	     (delete-file-recursively "src/main/java/freemarker/ext/jython")
         (delete-file "src/test/java/freemarker/core/ObjectBuilderSettingsTest.java")
	     (delete-file-recursively "src/main/java/freemarker/ext/rhino")
	     ;; This class depends on javareble, a non-free package
	     (delete-file "src/main/java/freemarker/ext/beans/JRebelClassChangeNotifier.java")
	     (delete-file "src/main/java/freemarker/ext/ant/UnlinkedJythonOperationsImpl.java")
	     (delete-file "src/main/java/freemarker/template/utility/JythonRuntime.java")
	     #t))
     (add-before 'build 'update-jsp
       (lambda _
         (substitute* "src/main/java/freemarker/ext/jsp/FreeMarkerJspFactory.java"
           (("^}$")
            "@Override
public JspApplicationContext getJspApplicationContext(ServletContext c) {
  throw new UnsupportedOperationException();
}
}")
           (("package freemarker.ext.jsp;")
            "package freemarker.ext.jsp;

import javax.servlet.ServletContext;
import javax.servlet.jsp.JspApplicationContext;"))
         (substitute* "src/main/java/freemarker/ext/jsp/_FreeMarkerPageContext2.java"
           (("^}")
            "@Override
public ELContext getELContext() {
  throw new UnsupportedOperationException();
}
}")
           (("package freemarker.ext.jsp;")
            "package freemarker.ext.jsp;

import javax.el.ELContext;"))
         #t))
	 (add-before 'build 'run-javacc
	   (lambda _
	     (invoke "java" "-cp" (getenv "CLASSPATH") "javacc"
		     "-OUTPUT_DIRECTORY=src/main/java/freemarker/core"
		     "src/main/javacc/FTL.jj")
	     #t)))))
    (inputs
     `(("java-avalon-logkit" ,java-avalon-logkit)
       ("java-commons-jxpath" ,java-commons-jxpath)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-dom4j" ,java-dom4j)
       ("java-jaxen" ,java-jaxen)
       ("java-jdom" ,java-jdom-for-freemarker)
       ("java-log4j-over-slf4j" ,java-log4j-over-slf4j)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-spotbugs-annotations" ,java-spotbugs-annotations)
       ("java-tomcat" ,java-tomcat)
       ("java-xalan" ,java-xalan)))
    (native-inputs
     `(("javacc" ,javacc-6)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-eclipse-jetty-server" ,java-eclipse-jetty-server)
       ("java-eclipse-jetty-webapp" ,java-eclipse-jetty-webapp)
       ("java-junit" ,java-junit)))
    (home-page "https://github.com/ralfstx/minimal-json")
    (synopsis "")
    (description "")
    (license license:expat)))

(define java-hazelcast-client-protocol-version "1.7.0-3")
(define java-hazelcast-version "3.10.4")

(define java-hazelcast-client-protocol-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hazelcast/"
                        "hazelcast-client-protocol/archive/v"
                        java-hazelcast-client-protocol-version ".tar.gz"))
    (file-name (string-append "hazelcast-client-protocol-" java-hazelcast-client-protocol-version ".tar.gz"))
    (sha256
     (base32
      "091j9as0zsm4rldj4x605hh0rmiwly39y0kg8zi6c2rkabzp2qpb"))))

(define java-hazelcast-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hazelcast/hazelcast/archive/v"
                        java-hazelcast-version ".tar.gz"))
    (file-name (string-append "java-hazelcast-" java-hazelcast-version ".tar.gz"))
    (sha256
     (base32
      "0bmhjh15xcqc4k77ncfw60b0gfnh6ndc3rr8am09ys8yga4w59hf"))))

(define-public java-hazelcast-code-generator
  (package
    (name "java-hazelcast-code-generator")
    (version java-hazelcast-client-protocol-version)
    (source java-hazelcast-client-protocol-source)
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-hazelcast-code-generator.jar"
       #:source-dir "hazelcast-code-generator/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "hazelcast-core-generator/src/main/resources"
                               "build/classes")
             #t)))))
    (inputs
     `(("java-apache-freemarker" ,java-apache-freemarker)))
    (home-page "https://hazelcast.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-hazelcast-client-protocol
  (package
    (name "java-hazelcast-client-protocol")
    (version java-hazelcast-client-protocol-version)
    (source java-hazelcast-client-protocol-source)
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-hazelcast-client-protocol.jar"
       #:source-dir "hazelcast/src/main/java"
       #:test-dir "hazelcast/src/test"))
    (inputs
     `(("java-hazelcast-code-generator" ,java-hazelcast-code-generator)))
    (home-page "https://hazelcast.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-hazelcast-bootstrap
  (package
    (name "java-hazelcast-bootstrap")
    (version java-hazelcast-version)
    (source java-hazelcast-source)
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-hazelcast-bootstrap.jar"
       #:source-dir
       (string-append "hazelcast-client/src/main/java:hazelcast/src/main/java:"
                      "hazelcast-client-protocol-" ,java-hazelcast-client-protocol-version
                      "/hazelcast/src/main/java")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'unpack-client-protocol
           (lambda* (#:key inputs #:allow-other-keys)
             (display (assoc-ref inputs "java-hazelcast-client-protocol-source"))
             (newline)
             (invoke "tar" "xzf" (assoc-ref inputs "java-hazelcast-client-protocol-source"))
             #t))
         (add-before 'build 'remove-package-info
           (lambda _
             (for-each delete-file (find-files "." "package-info.java"))
             #t)))))
    (inputs
     `(("java-findbugs" ,java-findbugs)
       ("java-jsr107" ,java-jsr107)
       ("java-jsr305" ,java-jsr305)
       ("java-minimal-json" ,java-minimal-json)
       ("java-hazelcast-client-protocol-source" ,java-hazelcast-client-protocol-source)))
    (home-page "https://hazelcast.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-hazelcast
  (package
    (inherit java-hazelcast-bootstrap)
    (name "java-hazelcast")
    (arguments
     `(#:jar-name "java-hazelcast.jar"
       #:source-dir "hazelcast/src/main/java"
       #:test-dir "hazelcast/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "hazelcast/src/main/resources" "build/classes")
             #t))
         (add-before 'build 'copy-template
           (lambda _
             (with-directory-excursion "hazelcast/src/main"
               (copy-file "template/com/hazelcast/instance/GeneratedBuildProperties.java"
                            "java/com/hazelcast/instance/GeneratedBuildProperties.java")
               (substitute* "java/com/hazelcast/instance/GeneratedBuildProperties.java"
                 (("\\$\\{project.version\\}") ,version)
                 (("\\$\\{timestamp\\}") "0")
                 (("\\$\\{git.commit.id.abbrev\\}") "0f51fcf")
                 (("\\$\\{hazelcast.distribution\\}") "Hazelcast")
                 (("\\$\\{hazelcast.serialization.version\\}") "1")))
             #t))
         (add-before 'build 'remove-fb
           (lambda _
             (substitute* (find-files "hazelcast/src/main/java" ".*.java")
              ; (("@SuppressFBWarnings.*") "")
              ; (("justification = \".*") "")
               (("import edu.umd.cs.findbugs.*") ""))
             #t)))))
    (description "")))

(define-public java-jamonapi-jamon-bootstrap
  (package
    (name "java-jamonapi-jamon")
    (version "2.81")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/stevensouza/jamonapi/archive/v"
                                  (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                                  ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0kh0p1h546k6myd268jlr681bx15q6ip15an56rmqdw4q87xk23v"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jamonapi-jamon.jar"
       #:source-dir "jamon/src/main/java"
       #:test-dir "jamon/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-cyclic-dependency
           (lambda _
             ;; Classes in this directory depend on spring-framework-context,
             ;; which depends on spring-framework-aop which depends on jamonapi.
             (delete-file-recursively "jamon/src/main/java/com/jamonapi/aop")
             #t))
         (add-before 'build 'port-to-jetty9
           (lambda _
             (substitute* "jamon/src/main/java/com/jamonapi/http/JettyHttpMonItem.java"
               (("org.mortbay.jetty.Request")
                "org.eclipse.jetty.server.Request"))
             (substitute* "jamon/src/main/java/com/jamonapi/http/JAMonJettyHandler.java"
               (("org.mortbay.jetty.Request")
                "org.eclipse.jetty.server.Request")
               (("org.mortbay.jetty.Response")
                "org.eclipse.jetty.server.Response")
               (("org.mortbay.jetty.HttpConnection")
                "org.eclipse.jetty.server.HttpConnection")
               (("org.mortbay.jetty.handler.HandlerWrapper")
                "org.eclipse.jetty.server.handler.HandlerWrapper"))
             #t)))))
    (inputs
     `(("java-aspectj-rt" ,java-aspectj-rt)
       ("java-eclipse-jetty-server" ,java-eclipse-jetty-server)
       ;("java-javaee-servletapi" ,java-javaee-servletapi)
       ("java-tomcat" ,java-tomcat) ; for catalina and servletapi
       ("java-log4j-api" ,java-log4j-api)))
    (home-page "")
    (synopsis "")
    (description "")
    ;; A link to the license is present in pom.xml
    (license license:bsd-3)))

(define-public java-spring-framework-core
  (package
    (name "java-spring-framework-core")
    (version "4.3.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/spring-projects/"
                                  "spring-framework/archive/v" version
                                  ".RELEASE.tar.gz"))
              (sha256
               (base32
                "036jcwh2g3qlv14lalhkpkjnwc1hjn4zdqf251231vywxyd838zm"))))
    (arguments
     `(#:jar-name "java-spring-framework-core.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-include (list "**/*Tests.java")
       #:test-exclude
       (list
         "**/Abstract*.java"
         ;; Test failures
         "**/LocalVariableTableParameterNameDiscovererTests.java"
         "**/StandardReflectionParameterNameDiscoverTests.java"
         "**/SpringFactoriesLoaderTests.java"
         "**/PropertySourceTests.java"
         "**/StaxEventXMLReaderTests.java"
         "**/StaxStreamHandlerTests.java"
         ;; Unable to set MockitoNamingPolicy on cglib generator which creates FastClasses
         "**/util/StreamUtilsTests.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-core")
             #t))
         (add-before 'configure 'rename-dep
           (lambda _
             (substitute* "src/main/java/org/springframework/objenesis/SpringObjenesis.java"
               (("org.springframework.objenesis") "org.objenesis"))
             #t))
         (add-before 'configure 'add-import
           (lambda _
             (substitute* "src/main/java/org/springframework/cglib/core/SpringNamingPolicy.java"
               (("public class")
                "import net.sf.cglib.core.DefaultNamingPolicy;\npublic class"))
             #t))
         (add-before 'check 'remove-log4j-1-dep
           (lambda _
             ;; These tests require log4j-1 (log4j-1.2-api doesn't work)
             (delete-file "src/test/java/org/springframework/util/MockLog4jAppender.java")
             (delete-file "src/test/java/org/springframework/util/Log4jConfigurerTests.java")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dir (string-append (getcwd) "/build/test-classes/")))
               (with-directory-excursion "src/test/resources"
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*"))))
             #t)))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-jopt-simple" ,java-jopt-simple)
       ("java-commons-codec" ,java-commons-codec)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("java-objenesis" ,java-objenesis)
       ("java-cglib" ,java-cglib)
       ("java-aspectj-weaver" ,java-aspectj-weaver)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-asm" ,java-asm)
       ("java-jboss-annotations-api-spec" ,java-jboss-annotations-api-spec)
       ("java-xmlunit-legacy" ,java-xmlunit-legacy)
       ("java-xmlunit" ,java-xmlunit)
       ("java-mockito-1" ,java-mockito-1)))
    (build-system ant-build-system)
    (home-page "https://projects.spring.io/spring-framework/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-spring-framework-beans
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-beans")
    (arguments
     `(#:jar-name "java-spring-framework-core.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-beans")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'configure 'rename-dep
           (lambda _
             (substitute* "src/main/java/org/springframework/beans/factory/support/CglibSubclassingInstantiationStrategy.java"
               (("org.springframework.cglib") "net.sf.cglib")
               (("net.sf.cglib.core.SpringNamingPolicy") "org.springframework.cglib.core.SpringNamingPolicy"))
             #t))
         (add-before 'check 'copy-test-classes
           (lambda _
             (copy-file "../spring-core/src/test/java/org/springframework/tests/Assume.java"
                        "src/test/java/org/springframework/tests/Assume.java")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/TestGroup.java"
                        "src/test/java/org/springframework/tests/TestGroup.java")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/TestResourceUtils.java"
                        "src/test/java/org/springframework/tests/TestResourceUtils.java")
             (mkdir-p "src/test/java/org/springframework/stereotype")
             (mkdir-p "src/test/java/org/springframework/util")
             (copy-file "../spring-core/src/test/java/org/springframework/stereotype/Component.java"
                        "src/test/java/org/springframework/stereotype/Component.java")
             (copy-file "../spring-core/src/test/java/org/springframework/util/SerializationTestUtils.java"
                        "src/test/java/org/springframework/util/SerializationTestUtils.java")
             (substitute* "src/test/java/org/springframework/beans/factory/BeanFactoryUtilsTests.java"
               (("org.springframework.cglib") "net.sf.cglib"))
             #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively "src/test/resources"
                               "build/test-classes")
             #t)))))
    (inputs
     `(("java-cglib" ,java-cglib)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-javax-inject" ,java-javax-inject)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ;; Note: for javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    (description "")))

(define-public java-spring-framework-aop
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-aop")
    (arguments
     `(#:jar-name "java-spring-framework-aop.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-aop")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'configure 'rename-dep
           (lambda _
             (substitute*
               '("src/main/java/org/springframework/aop/framework/CglibAopProxy.java"
                 "src/main/java/org/springframework/aop/framework/ObjenesisCglibAopProxy.java")
               (("org.springframework.cglib") "net.sf.cglib")
               (("net.sf.cglib.core.SpringNamingPolicy") "org.springframework.cglib.core.SpringNamingPolicy"))
             (substitute* "src/main/java/org/springframework/aop/framework/ObjenesisCglibAopProxy.java"
               (("org.springframework.objenesis") "org.objenesis"))
             #t))
         ;(add-before 'check 'copy-test-classes
         ;  (lambda _
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/Assume.java"
         ;               "src/test/java/org/springframework/tests/Assume.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/TestGroup.java"
         ;               "src/test/java/org/springframework/tests/TestGroup.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/TestResourceUtils.java"
         ;               "src/test/java/org/springframework/tests/TestResourceUtils.java")
         ;    (mkdir-p "src/test/java/org/springframework/stereotype")
         ;    (mkdir-p "src/test/java/org/springframework/util")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/stereotype/Component.java"
         ;               "src/test/java/org/springframework/stereotype/Component.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/util/SerializationTestUtils.java"
         ;               "src/test/java/org/springframework/util/SerializationTestUtils.java")
         ;    (substitute* "src/test/java/org/springframework/beans/factory/BeanFactoryUtilsTests.java"
         ;      (("org.springframework.cglib") "net.sf.cglib"))
         ;    #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively "src/test/resources"
                               "build/test-classes")
             #t)))))
    (inputs
     `(("java-aspectj-rt" ,java-aspectj-rt)
       ("java-aspectj-weaver" ,java-aspectj-weaver)
       ("java-cglib" ,java-cglib)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-pool" ,java-commons-pool)
       ("java-commons-pool2" ,java-commons-pool2)
       ("java-javax-inject" ,java-javax-inject)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ;; Note: for javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    (description "")))

(define-public java-spring-framework-context
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-context")
    (arguments
     `(#:jar-name "java-spring-framework-context.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-context")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         ;(add-before 'configure 'rename-dep
         ;  (lambda _
         ;    (substitute* "src/main/java/org/springframework/beans/factory/support/CglibSubclassingInstantiationStrategy.java"
         ;      (("org.springframework.cglib") "net.sf.cglib")
         ;      (("net.sf.cglib.core.SpringNamingPolicy") "org.springframework.cglib.core.SpringNamingPolicy"))
         ;    #t))
         ;(add-before 'check 'copy-test-classes
         ;  (lambda _
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/Assume.java"
         ;               "src/test/java/org/springframework/tests/Assume.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/TestGroup.java"
         ;               "src/test/java/org/springframework/tests/TestGroup.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/TestResourceUtils.java"
         ;               "src/test/java/org/springframework/tests/TestResourceUtils.java")
         ;    (mkdir-p "src/test/java/org/springframework/stereotype")
         ;    (mkdir-p "src/test/java/org/springframework/util")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/stereotype/Component.java"
         ;               "src/test/java/org/springframework/stereotype/Component.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/util/SerializationTestUtils.java"
         ;               "src/test/java/org/springframework/util/SerializationTestUtils.java")
         ;    (substitute* "src/test/java/org/springframework/beans/factory/BeanFactoryUtilsTests.java"
         ;      (("org.springframework.cglib") "net.sf.cglib"))
         ;    #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively "src/test/resources"
                               "build/test-classes")
             #t)))))
    (inputs
     `(("java-cglib" ,java-cglib)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-javax-inject" ,java-javax-inject)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ;; Note: for javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    (description "")))

(define-public java-spring-framework-test
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-test")
    (arguments
     `(#:jar-name "java-spring-framework-test.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-test")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively "src/test/resources"
                               "build/test-classes")
             #t)))))
    (inputs
     `(("java-spring-framework-core" ,java-spring-framework-core)
       ("java-spring-framework-web" ,java-spring-framework-web)))
    (description "")))

(define-public java-lucene-core
  (package
    (name "java-lucene-core")
    (version "6.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/lucene/java/" version
                                  "/lucene-" version "-src.tgz"))
              (sha256
               (base32
                "105z3y931hxygczl602d8vqypbs28h1jfzihdq7zlvcfw0a5b5if"))))
    (arguments
     `(#:jar-name "lucene-core.jar"
       #:jdk ,icedtea-8
       #:source-dir "core/src/java"
       #:test-dir "core/src/test"
       #:tests? #f)); FIXME: circular dependencies
    (build-system ant-build-system)
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://lucene.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-lucene-queries
  (package
    (inherit java-lucene-core)
    (name "java-lucene-queries")
    (arguments
     `(#:jar-name "lucene-queries.jar"
       #:jdk ,icedtea-8
       #:source-dir "queries/src/java"
       #:test-dir "queries/src/test"
       #:tests? #f));; FIXME: not in java subdirectory
    (inputs
     `(("java-lucene-core" ,java-lucene-core)))))

(define-public java-lucene-sandbox
  (package
    (inherit java-lucene-core)
    (name "java-lucene-sandbox")
    (arguments
     `(#:jar-name "lucene-sandbox.jar"
       #:jdk ,icedtea-8
       #:source-dir "sandbox/src/java"
       #:test-dir "sandbox/src/test"
       #:tests? #f));; FIXME: not in java subdirectory
    (inputs
     `(("java-lucene-core" ,java-lucene-core)
       ("java-lucene-queries" ,java-lucene-queries)))))

(define-public java-lucene-queryparser
  (package
    (inherit java-lucene-core)
    (name "java-lucene-queryparser")
    (arguments
     `(#:jar-name "lucene-queryparser.jar"
       #:jdk ,icedtea-8
       #:source-dir "queryparser/src/java"
       #:test-dir "queryparser/src/test"
       #:tests? #f));; FIXME: not in java subdirectory
    (inputs
     `(("java-lucene-core" ,java-lucene-core)
       ("java-lucene-sandbox" ,java-lucene-sandbox)
       ("java-lucene-queries" ,java-lucene-queries)))))

(define-public java-jts
  (package
    (name "java-jts")
    (version "1.15.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/locationtech/jts/archive"
                                  "/jts-" version "-M1.tar.gz"))
              (sha256
               (base32
                "00r9slwbzk2ngysmbnw9m4yazrdqzyi6gh62kxsvh52g30rs71jc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jts.jar"
       #:source-dir "modules/core/src/main/java"
       #:test-dir "modules/core/src/test"
       #:tests? #f)); requires swingui from junit3
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license (list license:epl1.0 license:edl1.0))))

(define-public java-h2
  (package
    (name "java-h2")
    (version "1.4.196")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/h2database/h2database/"
                                  "archive/version-" version ".tar.gz"))
              (sha256
               (base32
                "06djd2wimqwaj1vmcvvzlgy2jczn3bzjlw23az9alzxbqvd7w34v"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "h2.jar"
       #:source-dir "h2/src/main"
       #:jdk ,icedtea-8
       #:tests? #f; no tess
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-jts
           (lambda _
             (for-each (lambda (file)
                         (substitute* file
                           (("com.vividsolutions") "org.locationtech")))
               '("h2/src/main/org/h2/index/SpatialTreeIndex.java"
                 "h2/src/main/org/h2/mvstore/db/MVSpatialIndex.java"
                 "h2/src/main/org/h2/value/ValueGeometry.java"))
             (substitute* "h2/src/main/org/h2/fulltext/FullTextLucene.java"
               (("queryParser.QueryParser") "queryparser.classic.QueryParser")))))))
    (inputs
     `(("java-osgi-framework" ,java-osgi-framework)
       ("java-tomcat" ,java-tomcat)
       ("java-jts" ,java-jts)
       ("java-lucene-core" ,java-lucene-core)
       ("java-lucene-queryparser" ,java-lucene-queryparser)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-osgi-service-jdbc" ,java-osgi-service-jdbc)))
    (home-page "http://h2database.com")
    (synopsis "")
    (description "")
    (license '(license:mpl2.0 license:epl1.0))))

(define-public java-apache-felix-utils
  (package
    (name "java-apache-felix-utils")
    (version "1.10.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://apache.mindstudios.com/felix/"
                                  "org.apache.felix.utils-" version
                                  "-source-release.tar.gz"))
              (sha256
               (base32
                "0b2cvw7pfkslvlg0hfgqp3kl0qbzj5hq62mmx1m4iqwbi2h8103s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "felix-utils.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-version
           (lambda _
             (substitute* "src/main/java/org/apache/felix/utils/filter/FilterImpl.java"
               (("compareTo\\(converted\\)")
                "compareTo((Version) converted)"))
             (substitute* "src/main/java/org/apache/felix/utils/filter/FilterImpl.java"
               (("\\* Filter using a service's properties.")
                "*/\n@Override\npublic boolean matches(Map m) { return match0(m); }\n/**"))
             #t))
         (add-before 'check 'fix-mock
           (lambda _
             (substitute* "src/test/java/org/apache/felix/utils/properties/MockBundleContext.java"
               (("import org.osgi.framework.ServiceRegistration;")
                (string-append "import org.osgi.framework.ServiceRegistration;\n"
                               "import org.osgi.framework.ServiceFactory;\n"
                               "import java.util.Collection;\n"
                               "import org.osgi.framework.ServiceObjects;"))
               (("public Bundle getBundle\\(\\)")
                (string-append "@Override\n"
                               "public Bundle getBundle(String s) {\n"
                               " throw new UnsupportedOperationException();\n"
                               " }\n"
                               "@Override\n"
                               "public <S> ServiceObjects<S> getServiceObjects(ServiceReference<S> reference) {\n"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "@Override\n"
                               "public <S> Collection<ServiceReference<S>> getServiceReferences(Class<S> clazz, String filter) throws InvalidSyntaxException {"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "@Override\n"
                               "public <S> ServiceReference<S> getServiceReference(Class<S> clazz) {"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "@Override\n"
                               "public <S> ServiceRegistration<S> registerService(Class<S> clazz, ServiceFactory<S> factory, Dictionary<String, ?> properties) {"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "@Override\n"
                               "public <S> ServiceRegistration<S> registerService(Class<S> clazz, S service, Dictionary<String, ?> properties) {"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "public Bundle getBundle()"))))))))
    (inputs
     `(("java-osgi-framework" ,java-osgi-framework)
       ("java-osgi-service-log" ,java-osgi-service-log)
       ("java-osgi-service-cm" ,java-osgi-service-cm)
       ("java-osgi-util-tracker" ,java-osgi-util-tracker)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-apache-felix-resolver
  (package
    (name "java-apache-felix-resolver")
    (version "1.14.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://apache.mindstudios.com/felix/"
                                  "org.apache.felix.resolver-" version
                                  "-source-release.tar.gz"))
              (sha256
               (base32
                "0qz8gjafqyrd76v824i98601za289l0fmqm8rk68fxxl2cfai14z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "felix-resolver.jar"
       #:source-dir "src/main/java"))
    (inputs
     `(("java-osgi-resource" ,java-osgi-resource)
       ("java-osgi-framework" ,java-osgi-framework)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-apache-felix-utils" ,java-apache-felix-utils)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-objenesis" ,java-objenesis)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-apache-felix
  (package
    (name "java-apache-felix")
    (version "5.6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://apache.mindstudios.com//felix/"
                                  "org.apache.felix.framework-" version
                                  "-source-release.tar.gz"))
              (sha256
               (base32
                "1slbyqsnnzzfc48k9ab01yi8qalh5nrhy9gv5h1bi4y3d9mpx758"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "felix.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); tests require easymock 2, but we have easymock 3
    (inputs
     `(("java-osgi-annotation" ,java-osgi-annotation)
       ("java-apache-felix-resolver" ,java-apache-felix-resolver)
       ("java-osgi-service-resolver" ,java-osgi-service-resolver)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-asm" ,java-asm)
       ("java-easymock" ,java-easymock)
       ("java-mockito-1" ,java-mockito-1)))
    (home-page "https://felix.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; This package is outdated, but it still required by java-velocity
(define-public java-log4j-1.2
  (package
    (inherit java-log4j-core)
    (version "1.2.17")
    (name "java-log4j-1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/logging/log4j/" version
                                  "/log4j-" version ".tar.gz"))
              (sha256
               (base32
                "0qw618mdyg8nih499piqxkgqkvps2hpa03zbnmhlc8z63rvy6a55"))))
    (arguments
     `(#:tests? #f ; tests require unpackaged and outdated software
       #:test-dir "src/test"
       #:source-dir "src/main/java"
       #:jar-name "log4j-1.2.jar"
       #:jdk ,icedtea-8))))

(define-public java-avalon-logkit
  (package
    (name "java-avalon-logkit")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/excalibur/"
                                  "avalon-logkit/source/avalon-logkit-2.1-src.zip"))
              (sha256
               (base32
                "1gx5xc35w9yzc44brw73ghm17h4dnlai4kgz9sy808mhlfc6x4pz"))
              (patches
                (search-patches "java-avalon-logkit-default-datasource.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p dir)
               (copy-file "target/avalon-logkit-2.1.jar"
                          (string-append dir "/avalon-logkit.jar"))))))))
    (inputs
     `(("java-mail" ,java-mail)
       ("java-tomcat" ,java-tomcat)
       ("java-log4j-1.2" ,java-log4j-1.2)
       ("java-jboss-jms-api-spec" ,java-jboss-jms-api-spec)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-junit" ,java-junit)))
    (home-page "https://excalibur.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-avalon-framework-api
  (package
    (name "java-avalon-framework-api")
    (version "4.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/excalibur/"
                                  "avalon-framework/source/avalon-framework-api-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "0iqx6g3lqzmq805cdzr9xghda20pl4akyb54yrvzrp896q2nmmd4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "avalon-framework.jar"
       #:source-dir "src/java"
       #:tests? #f; FIXME: not in the java subdirectory
       #:test-dir "src/test"))
    (inputs
     `(("java-avalon-logkit" ,java-avalon-logkit)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-digester
  (package
    (name "java-commons-digester")
    (version "3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/digester/source/"
                                  "commons-digester3-" version "-src.tar.gz"))
              (sha256
               (base32
                "03kc18dfl5ma50cn02ji7rbhm33qpxyd9js6mvzznf8f7y6pmykk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-digester.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resource
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (add-before 'test 'copy-test-resource
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes"))))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-cglib" ,java-cglib)
       ("java-commons-beanutils", java-commons-beanutils)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "https://commons.apache.org/proper/commons-digester/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-validator
  (package
    (name "java-commons-validator")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache//commons/validator/source/"
                                  "commons-validator-" version "-src.tar.gz"))
              (sha256
               (base32
                "1v2iqhjz4iqwmv38gzf953php770mmhglibixzvxjc2yca3sizkb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-validator.jar"
       #:source-dir "src/main/java"
       #:tests? #f; Require network access (jakarta.apache.org)
       #:test-exclude (list "**/Abstract*.java"
                            ;; Require network access (jakarta.apache.org)
                            "**/ByteTest.java"
                            "**/DateTest.java"
                            "**/DoubleTest.java"
                            "**/EmailTest.java"
                            "**/EntityImportTest.java"
                            "**/ExceptionTest.java"
                            "**/ExtensionTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resource
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (add-before 'test 'copy-test-resource
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes")))
         (add-before 'build 'fix-digester
           (lambda _
             ;; Port from digester 1 to digester 3.
             (substitute* (find-files "src/main/java" ".*\\.java")
               (("commons.digester") "commons.digester3")
               (("org.apache.commons.digester3.xmlrules.DigesterLoader")
                "org.apache.commons.digester3.binder.DigesterLoader"))
             ;; digester is private in this class, so we use the getter
             (substitute* "src/main/java/org/apache/commons/validator/FormSetFactory.java"
               (("digester.peek") "getDigester().peek"))
             (substitute* "src/main/java/org/apache/commons/validator/ValidatorResources.java"
               (("// DEPRECATED")
                "// DEPRECATED\nimport org.apache.commons.digester3.xmlrules.FromXmlRulesModule;")
               (("private Digester initDigester")
                (string-append
                  "private FromXmlRulesModule rulesModule(final URL url) {\n"
                  "  return new FromXmlRulesModule() {\n"
                  "    @Override\n"
                  "    protected void loadRules() {\n"
                  "      loadXMLRules(url);"
                  "    }\n"
                  "  };\n"
                  "}\n"
                  "private Digester initDigester"))
               ;; Copied from digester tests
               (("createDigester\\(rulesUrl\\)")
                "newLoader(rulesModule(rulesUrl)).newDigester()")))))))
    (inputs
     `(("java-commons-digester" ,java-commons-digester)
       ("java-commons-beanutils" ,java-commons-beanutils)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-validator")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-ognl
  (package
    (name "java-ognl")
    (version "3.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jkuhnert/ognl/archive/OGNL_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                  ".tar.gz"))
              (sha256
               (base32
                "1p4yni36ln69cdl7icylpg87yzgnx9i08k4a5yhcvgmbr49p273w"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; Tests are run as a dependency of "dist"
       #:jdk ,icedtea-8
       #:make-flags (list "-Dcompile.version=7")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-binaries
           (lambda _
             (for-each delete-file (find-files "." ".*\\.jar"))
             #t))
         (add-before 'build 'remove-clover
           (lambda _
             (substitute* "osbuild.xml"
               (("clover-check,") "")
               ((", clover.report"), "")
               ((".*clover-setup.*") "")
               ((".*src/test/\\*\\*/\\*.java.*") "")
               (("<files>") "")
               (("</files>") ""))
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-jboss-javassist" ,java-jboss-javassist)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("docbook-xml" ,docbook-xml)
       ("docbook-xsl" ,docbook-xsl)
       ("libxml2" ,libxml2)))
    (home-page "http://www.opensymphony.com/ognl/"); down ? and ognl.org is not owned by the project
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-apache-struts
  (package
    (name "java-apache-struts")
    (version "2.5.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/struts/" version "/struts-"
                                  version "-src.zip"))
              (sha256
               (base32
                "14by1nsz7ky7zdw7ikmki1w9xznnvbyrjj9lbplk7wxsyhqak270"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "apache-struts.jar"
       #:source-dir "src/core/src/main/java"
       #:test-dir "src/core/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("java-log4j-api" ,java-log4j-api)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-ognl" ,java-ognl)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-context" ,java-spring-framework-context)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ("java-velocity" ,java-velocity)
       ("java-testng" ,java-testng)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("unzip" ,unzip)))
    (home-page "https://struts.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-velocity
  (package
    (name "java-velocity")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/velocity/engine/"
                                  version "/velocity-" version ".tar.gz"))
              (sha256
               (base32
                "0rk7s04hkrr2k3glccx0yrglzqzj4qbipcrxhglk46yhx92vravc"))
              (patches
            (search-patches "java-velocity-dont-use-werken-xpath.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:test-target "test-main"
       #:tests? #f; FIXME: need a fix to build.xml and hsqldb
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file-recursively "lib")
             (mkdir-p "bin/lib")
             ;; Don't download anything
             (substitute* "build/build.xml"
               ((".*download.xml.*") ""))
             (chdir "build")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/java")))
               (mkdir-p dir)
               (copy-file "../bin/velocity-1.7.jar"
                          (string-append dir "/velocity-1.7.jar")))
             #t)))))
    (native-inputs
     `(("javacc" ,java-javacc)
       ("antlr" ,antlr2)))
    (propagated-inputs
     `(("java-commons-collections" ,java-commons-collections)
       ("java-jakarta-oro" ,java-jakarta-oro)
       ("java-jdom" ,java-jdom)
       ("java-tomcat" ,java-tomcat)
       ("java-avalon-logkit" ,java-avalon-logkit)
       ("java-log4j-1.2" ,java-log4j-1.2)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-lang" ,java-commons-lang)))
    (home-page "https://velocity.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-velocity-2
  (package
    (inherit java-velocity)
    (name "java-velocity")
    (version "2.0")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                     (url "http://svn.apache.org/repos/asf/velocity/engine/tags/2.0")
                     (revision 1804253)))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "02s1dl9walwb965gryg15qy48477knb2rnxg5vmk33r9phrwvan8"))))
    (arguments
     `(#:jar-name "velocity.jar"
       #:tests? #f; FIXME: need a fix to build.xml and hsqldb
       #:source-dir "velocity-engine-core/src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "velocity-engine-core/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'build 'generate-parser
           (lambda _
             (invoke "jjtree" "-STATIC=false" "-MULTI=true"
                     "-NODE_PACKAGE=org.apache.velocity.runtime.parser.node"
                     "-BUILD_NODE_FILES=false" "-NODE_USES_PARSER=true"
                     "velocity-engine-core/src/main/parser/Parser.jjt")
             (rename-file "Parser.jj"
                          "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/Parser.jj")
             (rename-file "ParserTreeConstants.java"
                          "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/node/ParserTreeConstants.java")
             (rename-file "JJTParserState.java"
                          "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/node/JJTParserState.java")
             (invoke "javacc" "-STATIC=false" "-JDK_VERSION=1.8"
                     (string-append "-OUTPUT_DIRECTORY=velocity-engine-core/src"
                                    "/main/java/org/apache/velocity/runtime/parser")
                     "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/Parser.jj")
             #t)))))
    (native-inputs
     `(("java-javacc-5" ,java-javacc-5)))
    (propagated-inputs '())
    (inputs
     `(("java-commons-collections" ,java-commons-collections)
       ("java-jdom" ,java-jdom)
       ("java-log4j-api" ,java-log4j-api)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang3)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public java-velocity-tools
  (package
    (name "java-velocity-tools")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/velocity/tools/" version
                                  "/velocity-tools-" version "-src.tar.gz"))
              (sha256
               (base32
                "0d93v8nj95jfdgx7n72axaavdq2h800vxyi4vx35rdphndy1xg51"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:test-target "test-main"
       #:tests? #f; FIXME: need a fix to build.xml and hsqldb
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Don't download anything
             (substitute* "build.xml"
               ((".*download.xml.*") ""))
             ;; Replace digester with digester3
             (substitute* (find-files "src/main/java" ".*\\.java")
               (("commons.digester") "commons.digester3"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/java")))
               (mkdir-p dir)
               (copy-file "../bin/velocity-tools-2.0.jar"
                          (string-append dir "/velocity-tools-2.0.jar")))
             #t)))))
    (inputs
     `(("java-dom4j" ,java-dom4j)
       ("java-velocity" ,java-velocity)
       ("java-commons-digester" ,java-commons-digester)
       ("java-commons-validator" ,java-commons-validator)
       ("java-commons-beanutils", java-commons-beanutils)
       ("java-apache-struts" ,java-apache-struts)))
    ;; apache struts
    (home-page "https://velocity.apache.org/tools/devel")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-velocity-component
  (package
    (name "java-plexus-velocity-component")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/codehaus-plexus/"
                                  "plexus-velocity/archive/plexus-velocity-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04d34iny6364zcr1xy1xmg4grp6av8pcw3gsb1abrpxz4qhm84a6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-plexus-velocity-component.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8))
    (inputs
     `(("java-commons-collections" ,java-commons-collections)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-guava" ,java-guava)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-velocity" ,java-velocity)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://codehaus-plexus.github.io/plexus-velocity/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-pool
  (package
    (name "java-commons-pool")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/pool/source/"
                                  "commons-pool-" version "-src.tar.gz"))
              (sha256
               (base32
                "0nhrv5lf4a7ixzn7s4sgbw3pkijqj59gkjj0lvdncxl5vkjq5l9i"))))
    (arguments
     `(#:build-target "build-jar"
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/java")))
               (install-file (string-append "dist/commons-pool-" ,version "-SNAPSHOT.jar")
                             (string-append target "/commons-pool-" ,version ".jar"))))))))
    (build-system ant-build-system)
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-pool")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-pool2
  (package
    (name "java-commons-pool2")
    (version "2.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/pool/source/"
                                  "commons-pool2-" version "-src.tar.gz"))
              (sha256
               (base32
                "11hbkh9djzm2v486ypykmawf92li7z20xazpk5dp9zsl937s024f"))))
    (arguments
     `(#:build-target "build-jar"
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((target (string-append (assoc-ref outputs "out")
                                          "/share/java")))
               (install-file (string-append "dist/commons-pool2-" ,version ".jar")
                             target)))))))
    (build-system ant-build-system)
    (inputs
     `(("java-cglib" ,java-cglib)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-asm" ,java-asm)
       ("java-objenesis" ,java-objenesis)))
    (home-page "https://commons.apache.org/proper/commons-pool")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-dbcp2
  (package
    (name "java-commons-dbcp2")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/dbcp/source/"
                                  "commons-dbcp2-" version "-src.tar.gz"))
              (sha256
               (base32
                "023p6qlwyj8i75gcycxqi6i9b3rxpzq5pim0l37i8lrsvhhm19z1"))))
    (arguments
     `(#:source-dir "src/main/java"
       #:jar-name "java-commons-dbcp.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); requires apache-geronimo
    (inputs
     `(("java-commons-pool2" ,java-commons-pool2)
       ("java-commons-logging" ,java-commons-logging-minimal)
       ("java-jboss-transaction-api-spec" ,java-jboss-transaction-api-spec)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (build-system ant-build-system)
    (home-page "https://commons.apache.org/proper/commons-dbcp")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-dbcp
  (package
    (inherit java-commons-dbcp2)
    (version "1.4")
    (name "java-commons-dbcp")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/dbcp/source/"
                                  "commons-dbcp-" version "-src.tar.gz"))
              (sha256
               (base32
                "10zjdngdki7bfklikrsr3fq0cmapf4fwc0klzqhi3iwzwx30iwgm"))
              (patches
                (search-patches "java-commons-dbcp-fix-abstract.patch"))))
    (arguments
     `(#:source-dir "src/java"
       #:jar-name "java-commons-dbcp.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); FIXME: error in a test class
    (inputs
     `(("java-commons-pool" ,java-commons-pool)
       ("java-commons-logging" ,java-commons-logging-minimal)
       ("java-jboss-transaction-api-spec" ,java-jboss-transaction-api-spec)))))


(define-public java-commons-jcs
  (package
    (name "java-commons-jcs")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/jcs/source/"
                                  "commons-jcs-dist-" version "-src.tar.gz"))
              (sha256
               (base32
                "17l78mpxx1qkgp213b91sl69wawv6xzgllr479mygbg76ygwpffv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-jcs.jar"
       #:source-dir "commons-jcs-core/src/main/java"
       #:test-dir "commons-jcs-core/src/test"
       #:tests? #f; requires hsqldb
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda _
             (substitute* "commons-jcs-core/src/main/java/org/apache/commons/jcs/auxiliary/disk/jdbc/dsfactory/SharedPoolDataSourceFactory.java"
                (("commons.dbcp") "commons.dbcp2")
                ((".*\\.setMaxActive.*") ""))
             ;; Remove dependency on velocity-tools
             (delete-file "commons-jcs-core/src/main/java/org/apache/commons/jcs/admin/servlet/JCSAdminServlet.java"))))))
    (propagated-inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-httpclient" ,java-commons-httpclient)
       ("java-commons-dbcp" ,java-commons-dbcp2)
       ("java-velocity" ,java-velocity)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-jcs/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-javacc
  (package
    (name "java-javacc")
    (version "7.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javacc/javacc/archive/release_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                  ".tar.gz"))
              (sha256
               (base32
                "0yf93993nlsk5kijazddi5621x4y2bwq3vl46j8h8f7di2z9jv2h"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "unittest"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-binaries
           (lambda* _
             ;; Note: we cannot remove bootstrap/javacc.jar because no version of javacc comes with no bootstrap
             (delete-file-recursively "lib")))
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/java"))
                    (bin (string-append out "/bin")))
               (mkdir-p dir)
               (mkdir-p bin)
               (copy-file "target/javacc.jar" (string-append dir "/javacc.jar"))
               (with-output-to-file (string-append bin "/javacc")
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    (assoc-ref inputs "jdk") "/bin/java"
                                    " -cp " dir "/javacc.jar" " `basename $0`" " $*"))))
               (chmod (string-append bin "/javacc") #o755)
               (symlink (string-append bin "/javacc")
                        (string-append bin "/jjdoc"))
               (symlink (string-append bin "/javacc")
                        (string-append bin "/jjtree"))))))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://javacc.org")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; This version is required by velocity 2.0
(define-public java-javacc-5
  (package
    (inherit java-javacc)
    (version "5.0")
    (source (origin
              (method url-fetch)
              (uri "https://javacc.org/downloads/javacc-5.0src.tar.gz")
              (sha256
               (base32
                "0w3kl5zal9g0gwpcnlii6spgvb2yi3dpj1vz592ly18h6yfswv3n"))))
    (arguments
      (substitute-keyword-arguments (package-arguments java-javacc)
        ((#:phases phases)
         `(modify-phases ,phases
            ;; This phase renames the generated jar so it can be handled by
            ;; our already written 'install phase.
            (add-before 'install 'rename-jar
              (lambda _
                (mkdir-p "target")
                (rename-file "bin/lib/javacc.jar" "target/javacc.jar")))))))))

(define-public java-icu4j
  (package
    (name "java-icu4j")
    (version "58.2")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "http://download.icu-project.org/files/icu4j/" version
                     "/icu4j-"
                     (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
                     ".tgz"))
              (sha256
               (base32
                "1mvqjlc3cbaraa0bv0vyl44xf0x6n81inqsh69bl7f88iycfpns9"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; Requires java-ivy that we don't have yet.
       #:phases
       (modify-phases %standard-phases
         ;; icu4j archive contains its sources directly at the top, not in
         ;; a subdirectory as usual.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "..")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p share)
               (copy-file "icu4j-charset.jar" (string-append share "/icu4j-charset.jar"))
               (copy-file "icu4j.jar" (string-append share "/icu4j.jar"))))))))
    (home-page "http://site.icu-project.org/")
    (synopsis "")
    (description "")
    (license license:x11)))

(define-public java-treelayout
  (package
    (name "java-treelayout")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                     "https://github.com/abego/treelayout/archive/v" version
                     ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04dp7hx84c955w5g4ry7kbjaz74appczia8fz5r8pydwhwzl8fgw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "org.abego.treelayout/src/main/java"
       #:test-dir "org.abego.treelayout/src/test"))
    (inputs
     `(("java-junit" ,java-junit)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "http://treelayout.sourceforge.net")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

; propose update
;(define-public java-jsr305
;  (package
;    (name "java-jsr305")
;    (version "3.0.2")
;    (source (origin
;              (method git-fetch)
;              (uri (git-reference
;                     (url "https://github.com/amaembo/jsr-305.git")
;                     (commit "d7734b13c61492982784560ed5b4f4bd6cf9bb2c")))
;              (file-name (string-append name "-" version))
;              (sha256
;               (base32
;                "1wk159136pgc6i54drbq2whazfmdilvfqlxj3k19s9dfwbayf621"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name (string-append ,name "-" ,version ".jar")
;       #:source-dir "ri/src/main/java"
;       #:tests? #f))
;    (home-page "https://github.com/amaembo/jsr-305")
;    (synopsis "")
;    (description "")
;    (license license:bsd-3)))

(define-public java-jsr250
  (package
    (name "java-jsr250")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/"
                                  "javax/annotation/javax.annotation-api/"
                                  version "/javax.annotation-api-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "08clh8n4n9wfglf75qsqfjs6yf79f7x6hqx38cn856pksszv50kz"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f ; no tests included
       #:jdk ,icedtea-8
       #:jar-name "jsr250.jar"))
    (home-page "https://jcp.org/en/jsr/detail?id=250")
    (synopsis "Security-related annotations")
    (description "This package provides annotations for security.  It provides
packages in the @code{javax.annotation} and @code{javax.annotation.security}
namespaces.")
    (license (list license:cddl1.0 license:gpl2)))); gpl2 only, with classpath exception

(define-public java-checker-framework
  (package
    (name "java-checker-framework")
    (version "2.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/typetools/checker-framework/"
                                  "archive/checker-framework-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1infq1hr53zi9bd81v90rn3iripbinb3w145m3xblq8yvnfrxy20"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "dataflow/src:javacutil/src"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (home-page "https://checkerframework.org")
    (synopsis "")
    (description "")
    (license license:gpl2))); with classpath exception

(define-public java-javapoet
  (package
    (name "java-javapoet")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/square/javapoet/archive/javapoet-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0xpjbh8wcyj9yd9hb936ia5g6l2q1jlyqjvwcc290cwjrz7crb93"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8))
    (native-inputs
     `(("guice" ,java-guice)
       ("java-junit" ,java-junit)))
    (home-page "https://github.com/square/javapoet")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-auto-value
  (package
    (name "java-auto-value")
    (version "1.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/auto/archive/auto-value-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qd59bwa56bynsdxfbgm40i7ndrj599wflza214kzigk16nprc1m"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "value/src/main/java:common/src/main/java:service/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f))
    (inputs
     `(("java-guava" ,java-guava)
       ("java-javapoet" ,java-javapoet)))
    (home-page "https://github.com/google/auto/tree/master/value")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-diff-utils
  (package
    (name "java-diff-utils")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/KengoTODA/java-diff-utils/archive/"
                                  "diffutils-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "107bkk542cgpk8sqgc41j0ljarb6zs9p59m3phvvv9rln6rwnmjc"))))
    (arguments
     `(#:build-target "all"
       #:tests? #f; I don't know how to run src/test
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-build.xml
           (lambda _
             (substitute* "build.xml"
               (("1.5") "1.7")
               (("1.3.0-SNAPSHOT") ,version))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/share/java"))
             (with-directory-excursion "dist"
               (for-each (lambda (file)
                           (copy-file file
                                      (string-append (assoc-ref outputs "out")
                                                     "/share/java/" file)))
                 (find-files "." ".*.jar"))))))))
    (propagated-inputs
     `(("guava" ,java-guava)
       ("java-jsr305" ,java-jsr305)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (build-system ant-build-system)
    (home-page "https://github.com/KengoTODA/java-diff-utils")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; com.sun.tools.javac.code.Scope.LookupKind.NON_RECURSIVE
;; com.sun.source.tree.PackageTree
;; com.sun.tools.javac.tree.JCTree.JCPackageDecl

;; TODO: error-prone depends on java9 at least from version 2.0.13 which is the
;; earliest version that guava can use.
;; Fortunately, java7 can be used for -annotations.
(define-public java-error-prone
  (package
    (name "java-error-prone")
    (version "2.0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/error-prone/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qm7zpf0m75ps623h90xwb0rfyj4pywybvp005s9ykaqcvp50kzf"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jar-name "error-prone.jar"
       #:source-dir "check_api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-internal
           (lambda _
             (mkdir-p "ant/src/main/java/com/google/errorprone/internal")
             (copy-file
               "core/src/main/java/com/google/errorprone/internal/NonDelegatingClassLoader.java"
               "ant/src/main/java/com/google/errorprone/internal/NonDelegatingClassLoader.java"))))))
    (propagated-inputs '())
    (home-page "https://github.com/google/guava")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;(define-public java-error-prone-check-api
;  (package
;    (inherit java-error-prone)
;    (name "java-error-prone-check-api")
;    (version (package-version java-error-prone))
;    (arguments
;     `(#:tests? #f
;       #:jdk ,icedtea-8
;       #:jar-name (string-append ,name "-" ,version ".jar")
;       #:source-dir "check_api/src/main/java"))
;    (propagated-inputs
;     `(("java-error-prone-annotations" ,java-error-prone-annotations)
;       ("java-error-prone-annotation" ,java-error-prone-annotation)
;       ("java-jsr305" ,java-jsr305)
;       ("java-diff-utils" ,java-diff-utils)
;       ("java-auto-value" ,java-auto-value)
;       ("java-checker-framework" ,java-checker-framework)
;       ("java-guava" ,java-guava)))))

;(define-public java-error-prone-core
;  (package
;    (inherit java-error-prone)
;    (name "java-error-prone-core")
;    (version (package-version java-error-prone))
;    (arguments
;     `(#:tests? #f
;       #:jdk ,icedtea-8
;       #:jar-name (string-append ,name "-" ,version ".jar")
;       #:source-dir "core/src/main/java"))
;    (propagated-inputs
;     `(("java-error-prone-annotations" ,java-error-prone-annotations)
;       ("java-error-prone-annotation" ,java-error-prone-annotation)
;       ("java-jsr305" ,java-jsr305)
;       ("java-auto-value" ,java-auto-value)
;       ("java-checker-framework" ,java-checker-framework)
;       ("java-guava" ,java-guava)))))

(define-public java-error-prone-annotation
  (package
    (inherit java-error-prone)
    (name "java-error-prone-annotation")
    (version (package-version java-error-prone))
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "annotation/src/main/java"))
    (propagated-inputs
     `(("java-jsr305" ,java-jsr305)
       ("java-guava" ,java-guava)))))

(define-public java-error-prone-annotations
  (package
    (inherit java-error-prone)
    (name "java-error-prone-annotations")
    (version (package-version java-error-prone))
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "annotations/src/main/java"))
    (propagated-inputs
     `(("java-jsr305" ,java-jsr305)))))

;; Java-j2objc is for OS X, but the annotations sub-project is used by other
;; packages here, such as guava.
(define-public java-j2objc-annotations
  (package
    (name "java-j2objc-annotations")
    (version "1.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/j2objc/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0d5spbr1whw2afg6mknyr7ifm6xivn3bbvnzjxva2zzkyq944hv0"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "annotations/src/main/java"))
    (synopsis "")
    (description "")
    (home-page "https://github.com/google/j2objc")
    (license license:asl2.0)))

;; TODO: animal-sniffer-enforcer-rule and animal-sniffer-maven-plugin depend
;; on maven.
(define-public java-animal-sniffer
  (package
    (name "java-animal-sniffer")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mojohaus/animal-sniffer/"
                                  "archive/animal-sniffer-parent-"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1350yl003y1fjzdwis0dg5jhi5kggk2sxnkv9821z5janw4p986m"))))
    (build-system ant-build-system)
    (propagated-inputs
     `(("java-asm" ,java-asm)))
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "animal-sniffer/src/main/java"))
    (home-page "http://www.mojohaus.org/animal-sniffer")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-animal-sniffer-annotations
  (package
    (inherit java-animal-sniffer)
    (name "java-animal-sniffer-annotations")
    (version (package-version java-animal-sniffer))
    (propagated-inputs '())
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "animal-sniffer-annotations/src/main/java"))))

(define-public java-animal-sniffer-ant-tasks
  (package
    (inherit java-animal-sniffer)
    (name "java-animal-sniffer-ant-tasks")
    (version (package-version java-animal-sniffer))
    (propagated-inputs
     `(("animal-sniffer" ,java-animal-sniffer)))
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "animal-sniffer-ant-tasks/src/main/java"))))

(define-public java-boot-classpath-detector
  (package
    (inherit java-animal-sniffer)
    (name "java-boot-classpath-detector")
    (version (package-version java-animal-sniffer))
    (propagated-inputs '())
    (arguments
     `(#:tests? #f
       #:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "java-boot-classpath-detector/src/main/java"))))

(define-public java-guava-23.5
  (package
    (inherit java-guava)
    (version "23.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/guava/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append "guava-" version ".tar.gz"))
              (sha256
               (base32
                "1a4yl9l8b3w967l3ahhkic2n7aiygykw49nvmvp525l6qxwdl6a9"))))
    (arguments
     `(#:tests? #f                      ; no tests included
       #:jar-name "guava.jar"
       #:source-dir "guava/src"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'trim-sources
           (lambda _
             (with-directory-excursion "guava/src/com/google/common"
               ;; Remove annotations to avoid extra dependencies:
               ;; * "j2objc" annotations are used when converting Java to
               ;;   Objective C;
               ;; * "errorprone" annotations catch common Java mistakes at
               ;;   compile time;
               ;; * "IgnoreJRERequirement" is used for Android.
               (substitute* (find-files "." "\\.java$")
                 (("import com.google.j2objc.*") "")
                 (("import com.google.errorprone.annotation.*") "")
                 (("import org.codehaus.mojo.animal_sniffer.*") "")
                 (("@CanIgnoreReturnValue") "")
                 (("@LazyInit") "")
                 (("@WeakOuter") "")
                 (("@RetainedWith") "")
                 (("@Weak") "")
                 (("@ForOverride") "")
                 (("@J2ObjCIncompatible") "")
                 (("@CompatibleWith\\(\"[A-Z]\"\\)") "")
                 (("@Immutable\\([^\\)]*\\)") "")
                 (("@Immutable") "")
                 (("@ReflectionSupport\\([^\\)]*\\)") "")
                 (("@DoNotMock.*") "")
                 (("@MustBeClosed") "")
                 (("@IgnoreJRERequirement") "")))
             #t)))))))

(define-public java-guava-25
  (package
    (inherit java-guava)
    (version "25.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/guava/"
                                  "archive/v" version ".tar.gz"))
              (file-name (string-append "java-guava-" version ".tar.gz"))
              (sha256
               (base32
                "0jxwp8kfjcj4hyjwvnakk4d0yszp9np2l8c3hwz3ipxmwxk4dx7k"))))
    (arguments
     `(#:tests? #f                      ; no tests included
       #:jar-name "guava.jar"
       #:source-dir "guava/src"
       #:jdk ,openjdk9
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'trim-sources
           (lambda _
             (with-directory-excursion "guava/src/com/google/common"
               ;; Remove annotations to avoid extra dependencies:
               ;; * "j2objc" annotations are used when converting Java to
               ;;   Objective C;
               ;; * "errorprone" annotations catch common Java mistakes at
               ;;   compile time;
               ;; * "IgnoreJRERequirement" is used for Android.
               (substitute* (find-files "." "\\.java$")
                 (("import com.google.j2objc.*") "")
                 (("import com.google.errorprone.annotation.*") "")
                 (("import org.codehaus.mojo.animal_sniffer.*") "")
                 (("@CanIgnoreReturnValue") "")
                 (("@LazyInit") "")
                 (("@WeakOuter") "")
                 (("@RetainedWith") "")
                 (("@Weak") "")
                 (("@ForOverride") "")
                 (("@J2ObjCIncompatible") "")
                 (("@CompatibleWith\\(\"[A-Z]\"\\)") "")
                 (("@Immutable\\([^\\)]*\\)") "")
                 (("@Immutable") "")
                 (("@ReflectionSupport\\([^\\)]*\\)") "")
                 (("@DoNotMock.*") "")
                 (("@MustBeClosed") "")
                 (("@IgnoreJRERequirement") "")))
             #t)))))))

;(define-public java-xml-commons
;  (package
;    (name "java-xml-commons")
;    (version "1.4.01")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "mirror://apache/xerces/xml-commons/source/"
;                                  "xml-commons-external-" version "-src.tar.gz"))
;              (sha256
;               (base32
;                "0rhq32a7dl9yik7zx9h0naz2iz068qgcdiayak91wp4wr26xhjyk"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name "xml-commons.jar"
;       #:source-dir "."
;       #:tests? #f)); no tests
;    (home-page "")
;    (synopsis "")
;    (description "")
;    (license (list license:asl2.0; apache/...
;                   license:public-domain; sax/...
;                   license:bsd-3; w3c/...
;                   ))))
;;; actually http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231

;; This is very old (2002)!
(define-public java-xmlpull
  (package
    (name "java-xmlpull")
    (version "1.0.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.xmlpull.org/v1/download/xmlpull_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                  "_src.tgz"))
              (sha256
               (base32
                "1m2gymkvvpclz6x6f3vz3xkh00p94d28iy6k1j54nklf5crm979p"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p out)
               (copy-file "build/lib/xmlpull_1_0_5.jar"
                          (string-append out "/xmlpull.jar"))))))))
    (home-page "http://www.xmlpull.org/")
    (synopsis "")
    (description "")
    (license license:public-domain)))

(define-public java-commons-bcel-6.0
  (package
    (name "java-commons-bcel")
    (version "6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/bcel/source/bcel-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "0n39601zcj7ymjihfv53r260mf3n8kj6bqhxv90dw5sgc7qbjqxr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-bcel.jar"
       ;#:jdk ,icedtea-8
       #:source-dir "src/main/java"
       ;; FIXME: requires org.openjdk.jmh.* and com.sun.jna.platform.win32 for tests
       #:tests? #f))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("collections" ,java-commons-collections4)
       ("lang3" ,java-commons-lang3)
       ("io" ,java-commons-io)))
    (home-page "https://commons.apache.org/proper/commons-bcel/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jlex
  (package
    (name "java-jlex")
    (version "1.2.6")
    (source (origin
              (method url-fetch)
	      (uri (string-append "https://www.cs.princeton.edu/~appel/modern/"
				  "java/JLex/Archive/" version "/Main.java"))
              (sha256
               (base32
                "1msblmsgzij3z9pwm7gff1q2cv1q802q23xsn0mrflrs7g7axsxf"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
	 (delete 'unpack)
	 (delete 'configure)
	 (replace 'build
	   (lambda* (#:key inputs #:allow-other-keys)
	     (mkdir "JLex")
	     (copy-file (assoc-ref inputs "source") "Main.java")
	     (invoke "javac" "Main.java" "-d" ".")
	     (apply invoke "jar" "cf" "jlex.jar" (find-files "." ".*.class"));"JLex/Main.class")
	     #t))
	 (replace 'install
	   (install-jars ".")))))
    (home-page "https://jflex.de")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define %java-jflex-bootstrap
  (package
    (name "java-jflex")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.jflex.de/release/jflex-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1h7q2vhb4s42g4pqz5xxxliagprray7i9krr6hyaz1mjlx7gnycq"))))
    (build-system trivial-build-system)
    ;(arguments
    ; `(#:tests? #f
    ;   #:phases
    ;   (modify-phases %standard-phases
    ;     (delete 'build)
    ;     (replace 'install
    ;       (lambda* (#:key outputs #:allow-other-keys)
    ;         (let ((java-dir (string-append (assoc-ref outputs "out") "/share/java")))
    ;           (install-file (string-append "lib/jflex-" ,version ".jar")
    ;                         java-dir)
    ;           (install-file (string-append "lib/java-cup-11a.jar")
    ;                         java-dir))
    ;         #t)))))
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (string-append
                                 (assoc-ref %build-inputs "tar")
                                 "/bin/tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (output (assoc-ref %outputs "out"))
                          (java-dir (string-append output "/share/java")))
                     (mkdir-p java-dir)
                     (setenv "PATH" (string-append (getenv "PATH") ":" gzip "/bin"))
                     (invoke tar "xf" source)
                     (install-file "jflex-1.6.1/lib/jflex-1.6.1.jar" java-dir)
                     (install-file "jflex-1.6.1/lib/java-cup-11a.jar" java-dir)
                     #t))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "http://www.jflex.de")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define %java-cup-bootstrap
  (package
    (name "java-cup")
    (version "11b-20160615")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www2.cs.tum.edu/projects/cup/"
                                  "releases/java-cup-bin-" version ".tar.gz"))
              (sha256
               (base32
                "1k6ycm5bpg7r2z2jprdp54s8bvaxggdxk4qmvkjw3013i1bxc09z"))))
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((source (assoc-ref %build-inputs "source"))
                          (tar (string-append
                                 (assoc-ref %build-inputs "tar")
                                 "/bin/tar"))
                          (gzip (assoc-ref %build-inputs "gzip"))
                          (output (assoc-ref %outputs "out"))
                          (java-dir (string-append output "/share/java")))
                     (mkdir-p java-dir)
                     (chdir java-dir)
                     (setenv "PATH" (string-append (getenv "PATH") ":" gzip "/bin"))
                     (invoke tar "xf" source)))))
    (native-inputs
     `(("tar" ,tar)
       ("gzip" ,gzip)))
    (home-page "http://www2.cs.tum.edu/projects/cup/")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-jflex
  (package
    (name "java-jflex")
    (version "1.6.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.jflex.de/release/jflex-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1h7q2vhb4s42g4pqz5xxxliagprray7i9krr6hyaz1mjlx7gnycq"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; The first entry is a symlink to jflex-version
                   (delete-file "../jflex")
                   ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   (chdir "..")
                   (rename-file "jflex-1.6.1" "jflex")
                   (chdir "jflex")
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars ".")))))
    (native-inputs
     `(("%java-jflex-bootstrap" ,%java-jflex-bootstrap)
       ("java-junit" ,java-junit)))
    (home-page "https://jflex.de")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-cup-runtime
  (package
    (name "java-cup-runtime")
    (version "11b")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://versioncontrolseidl.in.tum.de/parsergenerators/cup.git")
                     (commit "fe729fe8c27441f046dab19135a38b9dde4c4e5e")))
              (sha256
               (base32
                "09xigxm7b44hz79xhqpfykvjrk4q90p33j2l07w69izx9sn0y42b"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   (for-each delete-file (find-files "." ".*\\.tar.gz"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "cup-runtime.jar"
       #:source-dir "src/java/java_cup/runtime"
       #:tests? #f; no tests for runtime
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'remove-build-xml
           (lambda _
             (delete-file "build.xml"))))))
    (home-page "http://www2.cs.tum.edu/projects/cup")
    (synopsis "")
    (description "")
    (license license:expat))); http://www2.cs.tum.edu/projects/cup/licence.html

(define-public java-cup
  (package
    (inherit java-cup-runtime)
    (name "java-cup")
    (arguments
     `(#:jdk ,icedtea-8
       #:build-target "dist"
       #:tests? #f; no test target
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-jflex
           (lambda _
             (substitute* "build.xml"
               (("JFlex.ant") "jflex.ant"))
             #t))
         (add-before 'build 'add-lib
           (lambda _
             (mkdir-p "lib")))
         (replace 'install
           (install-jars ".")))))
    (native-inputs
     `(("%java-jflex-bootstrap" ,%java-jflex-bootstrap)
       ("git" ,git)))
    (home-page "http://www2.cs.tum.edu/projects/cup")
    (synopsis "")
    (description "")
    (license license:expat))); http://www2.cs.tum.edu/projects/cup/licence.html

(define-public java-xalan
  (package
    (name "java-xalan")
    (version "2.7.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xalan/xalan-j/source/xalan-j_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                  "-src.tar.gz"))
              (sha256
               (base32
                "166vg9i11qzi0vbv09abfb50q8caq8wr6zrwg0cwqws9k394l62w"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   (for-each delete-file (find-files "." ".*\\.tar.gz"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xalan.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); no tests
    (inputs
     `(("java-commons-bcel" ,java-commons-bcel)
       ("java-xerces" ,java-xerces)))
    (native-inputs
     `(("java-cup" ,java-cup)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-2)))

;; vanished from the face of the earth :/
(define-public java-jsonp
  (package
    (name "java-jsonp")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "git://java.net/jsonp~git")
                     (commit "a586e706aea82dc80fb05bdf59f2a25150ee1801")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "01r4syivcywpvxkr1hn0az9316pr7qpnx154zzzw0nijfmdlbw7n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:tests? #f
       #:source-dir "api/src"))
    (home-page "https://jsonp.java.net")
    (synopsis "")
    (description "")
    (license (list license:gpl2
                   license:cddl1.1))))

(define-public ant-junit
  (package
    (inherit ant)
    (name "ant-junit")
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:tests? #f; disabled for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file) (copy-file file "lib/optional/junit.jar"))
                       (find-files (string-append (assoc-ref inputs "java-junit") "/share") ".*.jar"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "build/lib/ant-junit.jar"
                           (string-append (assoc-ref outputs "out") "/share/java")))))))
    (inputs
     `(("java-junit" ,java-junit)))))

(define-public ant-commons-net
  (package
    (inherit ant)
    (name "ant-commons-net")
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:tests? #f; disabled for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file) (copy-file file "lib/optional/commons-net.jar"))
                       (find-files (string-append (assoc-ref inputs "java-commons-net") "/share") ".*.jar"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "build/lib/ant-commons-net.jar"
                           (string-append (assoc-ref outputs "out") "/share/java")))))))
    (inputs
     `(("java-commons-net" ,java-commons-net)))))

(define-public ant-apache-oro
  (package
    (inherit ant)
    (name "ant-apache-oro")
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:tests? #f; disabled for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file) (copy-file file "lib/optional/apache-oro.jar"))
                       (find-files (string-append (assoc-ref inputs "java-jakarta-oro") "/share") ".*.jar"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "build/lib/ant-apache-oro.jar"
                           (string-append (assoc-ref outputs "out") "/share/java")))))))
    (inputs
     `(("java-jakarta-oro" ,java-jakarta-oro)))))

(define-public ant-apache-regexp
  (package
    (inherit ant)
    (name "ant-apache-regexp")
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:tests? #f; disabled for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file) (copy-file file "lib/optional/apache-regexp.jar"))
                       (find-files (string-append (assoc-ref inputs "java-jakarta-regexp") "/share") ".*.jar"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "build/lib/ant-apache-regexp.jar"
                           (string-append (assoc-ref outputs "out") "/share/java")))))))
    (inputs
     `(("java-jakarta-regexp" ,java-jakarta-regexp)))))

(define-public ant-jsch
  (package
    (inherit ant)
    (name "ant-jsch")
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:tests? #f; disabled for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file) (copy-file file "lib/optional/jsch.jar"))
                       (find-files (string-append (assoc-ref inputs "java-jsch") "/share") ".*.jar"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "build/lib/ant-jsch.jar"
                           (string-append (assoc-ref outputs "out") "/share/java")))))))
    (inputs
     `(("java-jsch" ,java-jsch)))))

(define-public ant-junit-tests
  (package
    (inherit ant)
    (name "ant-junit-tests")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "ant-junit-tests.jar"
       #:tests? #f; disabled for now
       #:source-dir "src/tests/junit"))
    (inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("ant-commons-net" ,ant-commons-net)
       ("java-jsch" ,java-jsch)
       ("ant-jsch" ,ant-jsch)
       ("java-jakarta-oro" ,java-jakarta-oro)
       ("ant-apache-oro" ,ant-apache-oro)
       ("java-jakarta-regexp" ,java-jakarta-regexp)
       ("ant-apache-regexp" ,ant-apache-regexp)
       ("java-commons-net" ,java-commons-net)))))

(define-public ant-antlr
  (package
    (inherit ant)
    (name "ant-antlr")
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jars"
       #:tests? #f; disabled for now
       ;#:make-flags
       ;(list (string-append "-Dant.install=" (assoc-ref %outputs "out")))
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy
           (lambda* (#:key inputs #:allow-other-keys)
             (for-each (lambda (file) (begin (display ">") (display file) (newline) (copy-file file "lib/optional/antlr.jar")))
                       (find-files (string-append (assoc-ref inputs "antlr2") "/share") ".*.jar"))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "build/lib/ant-antlr.jar"
                           (string-append (assoc-ref outputs "out") "/share/java")))))))
    (inputs
     `(("antlr2" ,antlr2)))))

(define-public libantlr3c
  (package
    (inherit antlr3)
    (name "libantlr3c")
    (build-system gnu-build-system)
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)))
    (propagated-inputs
     `(("antlr3" ,antlr3)))
    (arguments
     `(#:configure-flags (list "--enable-64bit" "--disable-static")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'autoreconf
           (lambda _
             (chdir "runtime/C")
             (system* "libtoolize")
             (system* "autoreconf" "-fiv"))))))))

(define-public java-json
  (package
    (name "java-json")
    (version "1.1.0-M2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/javax/json/"
                                  "javax.json-api/" version "/javax.json-api-"
                                  version "-sources.jar"))
              (file-name (string-append name "-" version ".jar"))
              (sha256
               (base32
                "0gam8w52xjbmfc1inviyajk36jnj3lg4bzwhw05iq52kadycy6v0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:source-dir "src"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-module-info
           (lambda _
             (format #t "~a\n" (getcwd))
             (delete-file "src/module-info.java"))))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; We still need one file to be generated with ST4.
;; tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java
;; See https://github.com/kevinbirch/string-template-maven-plugin
;; We should take this and adapt to get a standalone tool.
(define-public java-antlr4
  (package
    (name "java-antlr4")
    (version "4.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/antlr4/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0x355893x029d50lcg853v9g6y0ci7jfij9i03jn6fik87s181sd"))
              (patches
                (search-patches "java-antlr4-Add-standalone-generator.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "runtime/Java/src:tool/src"
       #:jdk ,icedtea-8
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jar (string-append (assoc-ref outputs "out") "/share/java"))
                   (bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/antlr4")
                 (lambda _
                   (display
                     (string-append "#!" (which "sh") "\n"
                                    "java -cp " jar "/" ,name "-" ,version ".jar:"
                                    (string-join
                                      (apply
                                        append
                                        (map
                                          (lambda (input)
                                            (find-files (assoc-ref inputs input)
                                                  ".*\\.jar"))
                                          '("antlr3" "java-stringtemplate"
                                            "java-antlr4-runtime" "java-treelayout"
                                            "java-json" "java-icu4j")))
                                      ":")
                                    " org.antlr.v4.Tool $*"))))
               (chmod (string-append bin "/antlr4") #o755))))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "tool/resources/" "build/classes")))
         (add-before 'build 'generate-unicode
           (lambda _
             (and
               ;; First: build the generator
               (zero? (system*
                        "javac" "-cp" (getenv "CLASSPATH")
                        "tool/src/org/antlr/v4/unicode/UnicodeRenderer.java"
                        "tool/src/org/antlr/v4/unicode/UnicodeDataTemplateController.java"))
               ;; Then use it
               (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                           ":tool/src:runtime/Java")
                               "org.antlr.v4.unicode.UnicodeRenderer"
                               "tool/resources/org/antlr/v4/tool/templates"
                               "unicodedata"
                               "tool/src/org/antlr/v4/unicode/UnicodeData.java"))
               (begin
                 ;; It seems there is a bug with our ST4
                 (substitute* "tool/src/org/antlr/v4/unicode/UnicodeData.java"
                   (("\\\\>") ">"))
                 ;; Remove the additional file
                 (delete-file "tool/src/org/antlr/v4/unicode/UnicodeRenderer.java")
                 #t))))
         (add-before 'build 'generate-grammar
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "tool/src/org/antlr/v4/parse"
               (for-each (lambda (file)
                           (format #t "~a\n" file)
                           (system* "antlr3" file))
                         '("ANTLRLexer.g" "ANTLRParser.g" "BlockSetTransformer.g"
                           "GrammarTreeVisitor.g" "ATNBuilder.g"
                           "ActionSplitter.g" "LeftRecursiveRuleWalker.g")))
             (with-directory-excursion "tool/src/org/antlr/v4/codegen"
               (copy-file "../parse/ANTLRParser.tokens" "ANTLRParser.tokens")
               (format #t "SourceGenTriggers.g\n")
               (system* "antlr3" "SourceGenTriggers.g")))))))
    (inputs
     `(("antlr3" ,antlr3)
       ("java-icu4j" ,java-icu4j)
       ("java-json" ,java-json)
       ("java-treelayout" ,java-treelayout)
       ("java-stringtemplate" ,java-stringtemplate)))
    (native-inputs
     `(("java-antlr4-runtime" ,java-antlr4-runtime)))
    (home-page "https://antlr.org")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-antlr4-runtime
  (package
    (inherit java-antlr4)
    (name "java-antlr4-runtime")
    (arguments
     `(#:jar-name "java-antlr4-runtime.jar"
       #:source-dir "runtime/Java/src/org"
       #:tests? #f
       #:jdk ,icedtea-8))
    (native-inputs '())))
;
;;; Requires gradle.
;(define-public android-anysoft-keyboard
;  (package
;    (name "android-anysoft-keyboard")
;    (version "1.8-r9")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/AnySoftKeyboard/"
;                                  "AnySoftKeyboard/archive/" version ".tar.gz"))
;              (file-name (string-append name "-" version ".tar.gz"))
;              (sha256
;               (base32
;                "1mrin9mw1rs23d25v8yx4jprx7j05zir6756sqvk4myxbkcp8mag"))))
;    (build-system ant-build-system)
;    (home-page "https://anysoftkeyboard.github.io/")
;    (synopsis "Alternative on-screen keyboard for multiple languages")
;    (description "Alternative on-screen keyboard for multiple languages.")
;    (license license:asl2.0)))

(define-public java-batik
  (package
    (name "java-batik")
    (version "1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xmlgraphics/batik/source/"
                                  "batik-src-" version ".tar.gz"))
              (sha256
               (base32
                "18y60rfzbd0ljndaq7a5adjxqbgld4krmpx8fj94k6mcnk03dx5y"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "regard"; FIXME: no test is actually run
       #:build-target "all-jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java/")))
               (mkdir-p dir)
               (copy-file (string-append "batik-" ,version "/lib/batik-all-" ,version ".jar")
                          (string-append dir "batik-all.jar"))))))))
    (inputs
     `(("java-xmlgraphics-commons" ,java-xmlgraphics-commons)))
    (home-page "https://xmlgraphics.apache.org/batik")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-xmlgraphics-commons
  (package
    (name "java-xmlgraphics-commons")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xmlgraphics/commons/source/"
                                  "xmlgraphics-commons-" version "-src.tar.gz"))
              (sha256
               (base32
                "0i128sj8g29hqc66kqckjr2n1n2amfgijadp5xq4y9fy45q5mrrb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xmlgraphics-commons.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; FIXME: need commons-xml-resolver
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-commons-io" ,java-commons-io)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)))
    (home-page "https://xmlgraphics.apache.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-fop
  (package
    (name "java-fop")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xmlgraphics/fop/source/"
                                  "fop-" version "-src.tar.gz"))
              (sha256
               (base32
                "0lk59ba2388yq69i7wi8nr1k97aw4lkgd6yj96yqif64gzwgwljh"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar-main"
       #:test-target "junit"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; FIXME: need dependencies
             ;(for-each delete-file
             ;          (find-files "." ".*.jar"))
             (chdir "fop")
             (substitute* "build.xml"
               (("<path id=\"libs-build-classpath\">")
                "<path id=\"libs-build-classpath\"><pathelement location=\"${env.CLASSPATH}\" />")
               (("TestCase.class\"/>")
                "TestCase.class\" excludes=\"**/IFTestCase.class **/*HyphenationLayoutTestCase.class\"/>")
               (("<fail><condition><or>"); don't fail after all tests actually passed
                "<fail><condition><and>")
               (("</not></or></condition>")
                "</not></and></condition>"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/share/java/"))
                    (bin (string-append out "/bin/"))
                    (etc (string-append out "/etc")))
               (mkdir-p lib)
               (mkdir-p bin)
               (mkdir-p etc)
               (copy-file "build/fop.jar"
                          (string-append lib "fop.jar"))
               (copy-file "build/fop-hyph.jar"
                          (string-append lib "fop-hyph.jar"))
               (copy-file "build/fop-sandbox.jar"
                          (string-append lib "fop-sandbox.jar"))
               (copy-file "fop"
                          (string-append bin "fop"))
               (chmod (string-append bin "fop") #o755)
               (substitute* (string-append bin "fop")
                 (("/etc/fop.conf")
                  (string-append etc "fop.conf")))
               (with-output-to-file (string-append etc "fop.conf")
                 (lambda _
                   (display
                     (string-append
                       "FOP_HOME=\"" lib "\"\n"
                       "CLASSPATH=\"$CLASSPATH:$FOP_HOME/fop.jar\"\n"
                       "CLASSPATH=\"$CLASSPATH:" (getenv "CLASSPATH") "\""))))))))))
    (home-page "https://xmlgraphics.apache.org/fop")
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-io" ,java-commons-io)
       ("java-xmlgraphics-commons" ,java-xmlgraphics-commons)
       ("java-tomcat" ,java-tomcat)
       ("java-batik" ,java-batik)
       ("java-avalon-framework-api" ,java-avalon-framework-api)
       ("java-avalon-logkit" ,java-avalon-logkit)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-fop-util
  (package
    (inherit java-fop)
    (name "java-fop-util")
    (arguments
     `(#:jar-name "fop-util.jar"
       #:source-dir "fop-util/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("java-commons-io" ,java-commons-io)
       ("java-xmlgraphics-commons" ,java-xmlgraphics-commons)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)))))

(define-public java-fop-events
  (package
    (inherit java-fop)
    (name "java-fop-events")
    (arguments
     `(#:jar-name "fop-events.jar"
       #:source-dir "fop-events/src/main/java"
       #:test-dir "fop-events/src/test"))
    (inputs
     `(("java-commons-io" ,java-commons-io)
       ("java-xmlgraphics-commons" ,java-xmlgraphics-commons)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-fop-util" ,java-fop-util)))))

;;
;;
;;
;; let's try to build maven :)
;; This was mostly adapted from https://gitlab.com/htgoebel/guix/blob/WIP-maven/gnu/packages/java.scm
;;
;;
;;

(define* (codehaus-plexus-origin projname version hash
                                 #:optional (prefix "plexus-"))
  (let* ((projname (string-append prefix projname))
         (url (string-append "https://github.com/codehaus-plexus/" projname
                             "/archive/" projname "-" version ".tar.gz")))
    (origin
      (method url-fetch)
      (uri url)
      (sha256 (base32 hash)))))

(define-public java-jackrabbit-core
  (package
    (name "java-jackrabbit-core")
    (version "2.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://apache.crihan.fr/dist/jackrabbit/"
                                  version "/jackrabbit-" version "-src.zip"))
              (sha256
               (base32
                "1vcryb0p4937nlaiicyqfia6n46jqd71nwch3d9a0wd1ma14h0vg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackrabbit.jar"
       #:source-dir "jackrabbit-core/src/main/java"
       #:test-dir "jackrabbit-core/src/test"))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://jackrabbit.apache.org/jcr")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jackrabbit-webdav
  (package
    (inherit java-jackrabbit-core)
    (name "java-jackrabbit-webdav")
    (arguments
     `(#:jar-name "jackrabbit-webdav.jar"
       #:source-dir "jackrabbit-webdav/src/main/java"
       #:test-dir "jackrabbit-webdav/src/test"
       #:tests? #f; FIXME: either use servlet 2.2 or fix tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-servlet
           (lambda _
             ;; jackrabbit is built for servlet 2.2, but we have 3.0
             (substitute* "jackrabbit-webdav/src/main/java/org/apache/jackrabbit/webdav/WebdavRequestImpl.java"
               (("import javax.servlet.RequestDispatcher;")
                "import javax.servlet.RequestDispatcher;
import javax.servlet.http.Part;
import javax.servlet.http.HttpServletResponse;
import javax.servlet.AsyncContext;
import javax.servlet.DispatcherType;
import javax.servlet.ServletRequest;
import javax.servlet.ServletResponse;
import javax.servlet.ServletContext;
import javax.servlet.ServletException;
import java.util.Collection;") 
               (("^}") "public Part getPart(String name) throws IOException,
                       ServletException {
    return httpRequest.getPart(name);
  }
  public Collection<Part> getParts() throws IOException, ServletException {
    return httpRequest.getParts();
  }
  public void logout() throws ServletException {
    httpRequest.logout();
  }
  public void login(String username, String password) throws ServletException {
    httpRequest.login(username, password);
  }
  public boolean authenticate(HttpServletResponse response) throws IOException,
      ServletException {
    return httpRequest.authenticate(response);
  }
  public DispatcherType getDispatcherType() {
    return httpRequest.getDispatcherType();
  }
  public AsyncContext getAsyncContext() {
    return httpRequest.getAsyncContext();
  }
  public boolean isAsyncSupported() {
    return httpRequest.isAsyncSupported();
  }
  public boolean isAsyncStarted() {
    return httpRequest.isAsyncStarted();
  }
  public AsyncContext startAsync(ServletRequest request, ServletResponse response) {
    return httpRequest.startAsync(request, response);
  }
  public AsyncContext startAsync() {
    return httpRequest.startAsync();
  }
  public ServletContext getServletContext() {
    return httpRequest.getServletContext();
  }
  public int getLocalPort() throws IOException {
    return httpRequest.getLocalPort();
  }
  public String getLocalAddr() throws IOException {
    return httpRequest.getLocalAddr();
  }
  public String getLocalName() throws IOException {
    return httpRequest.getLocalName();
  }
  public int getRemotePort() {
    return httpRequest.getRemotePort();
  }
}"))
             (substitute* "jackrabbit-webdav/src/main/java/org/apache/jackrabbit/webdav/WebdavResponseImpl.java"
               (("import java.util.Locale;")
                "import java.util.Collection;\nimport java.util.Locale;")
               (("^}")
                "public Collection<String> getHeaderNames() {
    return httpResponse.getHeaderNames();
  }
  public Collection<String> getHeaders(String name) {
    return httpResponse.getHeaders(name);
  }
  public String getHeader(String name) {
    return httpResponse.getHeader(name);
  }
  public int getStatus() {
    return httpResponse.getStatus();
  }
  public void setCharacterEncoding(String encoding) {
    httpResponse.setCharacterEncoding(encoding);
  }
  public String getContentType() {
    return httpResponse.getContentType();
  }
}"))
             #t)))))
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-osgi-annotation" ,java-osgi-annotation)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-junit" ,java-junit)))))

(define-public bazel
  (package
    (name "bazel")
    (version "0.10.0")
    (source (origin
              (method url-fetch)
              ;(uri (string-append "https://github.com/bazelbuild/bazel/archive/"
              ;                    version ".tar.gz"))
              ;; FIXME: This contains build artifacts generated by running bazel.
              (uri (string-append "https://github.com/bazelbuild/bazel/releases/"
                                  "download/" version "/bazel-" version "-dist.zip"))
              (file-name (string-append name "-" version ".zip"))
              ;; TODO: remove third_party/
              (sha256
               (base32
                ;"0a1n20n4r2z852i3ik3c7bmaiiikny5i7r82p0rkcvy6wdsqzclp"))))
                "0j4mgmrivpkk8id0h9sshm58igaaj555bzp5pjcz8kdcma67kq27"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'unpack 'subdir
           (lambda _
             (mkdir "src")
             (chdir "src")))
         (add-before 'patch-source-shebangs 'chdir
           (lambda _
             (chdir "..")))
         (replace 'build
           (lambda _
             (invoke "./compile.sh"))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("which" ,which)))
    (home-page "https://bazel.build")
    (synopsis "Build system")
    (description "")
    (license license:asl2.0)))

(define-public openjdk9
  (package
    (name "openjdk")
    (version "9+181")
    (source (origin
              (method url-fetch)
              (uri "http://hg.openjdk.java.net/jdk/jdk/archive/3cc80be736f2.tar.bz2")
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "01ihmyf7k5z17wbr7xig7y40l9f01d5zjgkcmawn1102hw5kchpq"))))
    (build-system gnu-build-system)
    (outputs '("out" "jdk" "doc"))
    (arguments
     `(#:tests? #f; require jtreg
       #:imported-modules
       ((guix build syscalls)
        ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'patch-source-shebangs)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "bash" "./configure"
                     (string-append "--with-freetype=" (assoc-ref inputs "freetype"))
                     "--disable-freetype-bundling"
                     "--disable-warnings-as-errors"
                     "--disable-hotspot-gtest"
                     (string-append "--prefix=" (assoc-ref outputs "out")))
             #t))
         (replace 'build
           (lambda _
             (with-output-to-file ".src-rev"
               (lambda _
                 (display ,version)))
             (setenv "GUIX_LD_WRAPPER_ALLOW_IMPURITIES" "yes")
             (invoke "make" "all")
             #t))
         ;; Some of the libraries in the lib/ folder link to libjvm.so.
         ;; But that shared object is located in the server/ folder, so it
         ;; cannot be found.  This phase creates a symbolic link in the
         ;; lib/ folder so that the other libraries can find it.
         ;;
         ;; See:
         ;; https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00169.html
         ;;
         ;; FIXME: Find the bug in the build system, so that this symlink is
         ;; not needed.
         (add-after 'install 'install-libjvm
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((lib-out (string-append (assoc-ref outputs "out")
                                             "/lib"))
                    (lib-jdk (string-append (assoc-ref outputs "jdk")
                                             "/lib")))
               (symlink (string-append lib-jdk "/server/libjvm.so")
                        (string-append lib-jdk "/libjvm.so"))
               (symlink (string-append lib-out "/server/libjvm.so")
                        (string-append lib-out "/libjvm.so")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (jdk (assoc-ref outputs "jdk"))
                   (doc (assoc-ref outputs "doc"))
                   (images (car (find-files "build" ".*-server-release"
                                            #:directories? #t))))
               (copy-recursively (string-append images "/images/jdk") jdk)
               (copy-recursively (string-append images "/images/jre") out)
               (copy-recursively (string-append images "/images/docs") doc))
             #t))
         (add-after 'install 'strip-zip-timestamps
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (guix build syscalls))
             (for-each (lambda (zip)
                         (let ((dir (mkdtemp! "zip-contents.XXXXXX")))
                           (with-directory-excursion dir
                             (invoke "unzip" zip))
                           (delete-file zip)
                           (for-each (lambda (file)
                                       (let ((s (lstat file)))
                                         (unless (eq? (stat:type s) 'symlink)
                                           (format #t "reset ~a~%" file)
                                           (utime file 0 0 0 0))))
                             (find-files dir #:directories? #t))
                           (with-directory-excursion dir
                             (let ((files (find-files "." ".*" #:directories? #t)))
                               (apply invoke "zip" "-0" "-X" zip files)))))
               (find-files (assoc-ref outputs "doc") ".*.zip$"))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libelf" ,libelf)
       ("libice" ,libice)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxrender" ,libxrender)
       ("libxt" ,libxt)
       ("libxtst" ,libxtst)))
    (native-inputs
     `(("icedtea-8" ,icedtea-8)
       ("icedtea-8:jdk" ,icedtea-8 "jdk")
       ("gcc" ,gcc)
       ("unzip" ,unzip)
       ("which" ,which)
       ("zip" ,zip)))
    (home-page "http://openjdk.java.net/projects/jdk9/")
    (synopsis "")
    (description "")
    (license license:gpl2)))

(define-public openjdk10
  (package
    (name "openjdk")
    (version "10+46")
    (source (origin
              (method url-fetch)
              (uri "http://hg.openjdk.java.net/jdk/jdk/archive/6fa770f9f8ab.tar.bz2")
              (file-name (string-append name "-" version ".tar.bz2"))
              (sha256
               (base32
                "0zywq2203b4hx4jms9vbwvjcj1d3k2v3qpx4s33729fkpmid97r4"))))
    (build-system gnu-build-system)
    (outputs '("out" "jdk" "doc"))
    (arguments
     `(#:tests? #f; require jtreg
       #:imported-modules
       ((guix build syscalls)
        ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (delete 'patch-source-shebangs)
         (replace 'configure
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (invoke "bash" "./configure"
                     (string-append "--with-freetype=" (assoc-ref inputs "freetype"))
                     "--disable-freetype-bundling"
                     "--disable-warnings-as-errors"
                     "--disable-hotspot-gtest"
                     (string-append "--prefix=" (assoc-ref outputs "out")))
             #t))
         (replace 'build
           (lambda _
             (with-output-to-file ".src-rev"
               (lambda _
                 (display ,version)))
             (setenv "GUIX_LD_WRAPPER_ALLOW_IMPURITIES" "yes")
             (invoke "make" "all")
             #t))
         ;; Some of the libraries in the lib/ folder link to libjvm.so.
         ;; But that shared object is located in the server/ folder, so it
         ;; cannot be found.  This phase creates a symbolic link in the
         ;; lib/ folder so that the other libraries can find it.
         ;;
         ;; See:
         ;; https://lists.gnu.org/archive/html/guix-devel/2017-10/msg00169.html
         ;;
         ;; FIXME: Find the bug in the build system, so that this symlink is
         ;; not needed.
         (add-after 'install 'install-libjvm
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((lib-out (string-append (assoc-ref outputs "out")
                                             "/lib"))
                    (lib-jdk (string-append (assoc-ref outputs "jdk")
                                             "/lib")))
               (symlink (string-append lib-jdk "/server/libjvm.so")
                        (string-append lib-jdk "/libjvm.so"))
               (symlink (string-append lib-out "/server/libjvm.so")
                        (string-append lib-out "/libjvm.so")))
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out"))
                   (jdk (assoc-ref outputs "jdk"))
                   (doc (assoc-ref outputs "doc"))
                   (images (car (find-files "build" ".*-server-release"
                                            #:directories? #t))))
               (copy-recursively (string-append images "/images/jdk") jdk)
               (copy-recursively (string-append images "/images/jre") out)
               (copy-recursively (string-append images "/images/docs") doc))
             #t))
         (add-after 'install 'strip-zip-timestamps
           (lambda* (#:key outputs #:allow-other-keys)
             (use-modules (guix build syscalls))
             (for-each (lambda (zip)
                         (let ((dir (mkdtemp! "zip-contents.XXXXXX")))
                           (with-directory-excursion dir
                             (invoke "unzip" zip))
                           (delete-file zip)
                           (for-each (lambda (file)
                                       (let ((s (lstat file)))
                                         (unless (eq? (stat:type s) 'symlink)
                                           (format #t "reset ~a~%" file)
                                           (utime file 0 0 0 0))))
                             (find-files dir #:directories? #t))
                           (with-directory-excursion dir
                             (let ((files (find-files "." ".*" #:directories? #t)))
                               (apply invoke "zip" "-0" "-X" zip files)))))
               (find-files (assoc-ref outputs "doc") ".*.zip$"))
             #t)))))
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("cups" ,cups)
       ("fontconfig" ,fontconfig)
       ("freetype" ,freetype)
       ("libelf" ,libelf)
       ("libice" ,libice)
       ("libx11" ,libx11)
       ("libxcomposite" ,libxcomposite)
       ("libxi" ,libxi)
       ("libxinerama" ,libxinerama)
       ("libxrender" ,libxrender)
       ("libxt" ,libxt)
       ("libxtst" ,libxtst)))
    (native-inputs
     `(("openjdk9" ,openjdk9)
       ("openjdk9:jdk" ,openjdk9 "jdk")
       ("unzip" ,unzip)
       ("which" ,which)
       ("zip" ,zip)))
    (home-page "http://openjdk.java.net/projects/jdk9/")
    (synopsis "")
    (description "")
    (license license:gpl2)))

(define-public java-procyon
  (package
    (name "java-procyon")
    (version "0.5.30+1")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                     (url "https://bitbucket.org/mstrobel/procyon")
                     (changeset "1aa0bd29339b8782b7e9ffafe367b94ee15a7039")))
              (sha256
               (base32
                "0khihmkmv4lrx67mp2s6h5ifd5sk7hf0adfp64ybiq4c3w7786b0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "procyon.jar"
       #:jdk ,icedtea-8
       #:source-dir
       (string-append "Procyon.CompilerTools/src/main/java:"
                      "Procyon.Core/src/main/java:"
                      "Procyon.Decompiler/src/main/java:"
                      "Procyon.Expressions/src/main/java:"
                      "Procyon.Reflection/src/main/java")
       ;; Tests in CompilerTools depend on .class files, no tests in Core or Decompiler.
       ;; Tests in Expressions and Reflections, but build.xml doesn't support tests
       ;; in more than one dir.
       #:tests? #f))
    (inputs
     `(("java-jcommander" ,java-jcommander)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-modello-plugins-xsd
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-xsd")
    (arguments
     `(#:jar-name "modello-plugins-xsd.jar"
       #:source-dir "modello-plugins/modello-plugin-xsd/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-xsd/src/test"
       #:tests? #f; Require some test classes from java-modello
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively
               "modello-plugins/modello-plugin-xsd/src/main/resources"
               "build/classes")
             #t)))))
    (inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ,@(package-inputs java-modello-core)))
    (native-inputs
     `(("java-modello-test" ,java-modello-test)
       ,@(package-native-inputs java-modello-core)))
    (synopsis "Modello XSD Plugin")
    (description "Modello XSD Plugin generates an XML Schema from the model to
be able to validate XML content.")))

(define-public java-xmlunit-matchers
  (package
    (inherit java-xmlunit)
    (name "java-xmlunit-matchers")
    (arguments
     `(#:jar-name "java-xmlunit-matchers.jar"
       #:source-dir "xmlunit-matchers/src/main/java"
       #:test-dir "xmlunit-matchers/src/test"
       #:test-exclude
       ;; Cannot open xsd for http://www.xmlunit.org/test-support/Book.xsd
       (list "**/ValidationMatcherTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-test-class
           (lambda _
             (copy-file "xmlunit-core/src/test/java/org/xmlunit/TestResources.java"
                        "xmlunit-matchers/src/test/java/org/xmlunit/TestResources.java")
             #t))
         (add-before 'build 'fix-test-resources-path
           (lambda _
             (substitute* (find-files "xmlunit-matchers/src/test" ".*.java")
               (("../test-resources") "test-resources"))
             #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively (assoc-ref inputs "resources") "test-resources")
             #t)))))
    (inputs
     `(("java-xmlunit" ,java-xmlunit)
       ("java-junit" ,java-junit)))))

(define-public java-jtidy
  (package
    (name "java-jtidy")
    (version "r938")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/jtidy/JTidy/r938/jtidy-r938-sources.zip"))
              (sha256
               (base32
                "19kszpqjihdfacxwk0bzv8ajwbs86k1qb9j67vzg8lwvxcxdkmsh"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'chdir
           (lambda _
             (chdir "..")
             #t))
         (replace 'install
           (install-jars ".")))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
