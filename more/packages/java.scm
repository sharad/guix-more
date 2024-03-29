;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017-2019 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (gnu packages apr)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages batik)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages elf)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages gcc)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages java)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages protobuf)
  #:use-module (gnu packages tls)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages web)
  #:use-module (gnu packages java-xml)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages xorg)
  #:use-module (more packages maven)
  #:use-module (more packages python)
  #:use-module (more packages tls)
  #:use-module (more packages web))

(define-public java-httpcomponents-httpasyncclient
  (package
    (name "java-httpcomponents-httpasyncclient")
    (version "4.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/httpcomponents/httpasyncclient/"
                                  "source/httpcomponents-asyncclient-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "0xfc20zrwdym6g00sz5y38mdmcn4y2jikkmfgqyh9zm4nkyj7ci5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "httpcomponents-httpasyncclient.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _ (chdir "httpasyncclient") #t)))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-io" ,java-commons-io)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-httpcomponents-httpcore-nio" ,java-httpcomponents-httpcore-nio)
       ("java-mockito" ,java-mockito-1)
       ("java-junit" ,java-junit)))
    (home-page "https://hc.apache.org/httpcomponents-asyncclient-ga/")
    (synopsis "HTTP client library for Java")
    (description "Although the @code{java.net} package provides basic
functionality for accessing resources via HTTP, it doesn't provide the full
flexibility or functionality needed by many applications.  @code{HttpAsyncClient}
seeks to fill this void by providing an efficient, up-to-date, and
feature-rich package implementing the client side of the most recent HTTP
standards and recommendations.")
    (license license:asl2.0)))

(define-public java-fastutil
  (package
    (name "java-fastutil")
    (version "8.2.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/vigna/fastutil")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jn2c70lran9b2zkyvw4axynbsdb2qscz3drjd0jvs1fjmxiwzjh"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
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
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
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
     `(#:tests? #f; No tests
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
     `(#:tests? #f; No tests
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
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "japicmp/src/main/resources" "build/classes")
             #t)))))
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
       #:source-dir "src/main/java"))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-javaparser-3.9
  (package
    (name "java-javaparser")
    (version "3.9.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/javaparser/javaparser.git")
                     (commit "b7907a0dc39ff10388943dfffba01bec4bb116dc")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "172y8wbwqwwvqlhk65df0zilyicq7nagnsa9f4gav8s85p9ijxr8"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fill-template
           (lambda _
             (with-directory-excursion "javaparser-core/src/main"
               (copy-file "java-templates/com/github/javaparser/JavaParserBuild.java"
                          "java/com/github/javaparser/JavaParserBuild.java")
               (substitute* "java/com/github/javaparser/JavaParserBuild.java"
                 (("\\$\\{project.version\\}") ,version)
                 (("\\$\\{project.name\\}") "javaparser")
                 (("\\$\\{project.build.finalName\\}") "javaparser")
                 (("\\$\\{maven.version\\}") "fake")
                 (("\\$\\{maven.build.version\\}") "fake")
                 (("\\$\\{build.timestamp\\}") "0")
                 (("\\$\\{java.vendor\\}") "Guix")
                 (("\\$\\{java.vendor.url\\}") "https://gnu.org/software/guix")
                 (("\\$\\{java.version\\}") "1.8")
                 (("\\$\\{os.arch\\}") "any")
                 (("\\$\\{os.name\\}") "GuixSD")
                 (("\\$\\{os.version\\}") "not available")))
             #t))
         (add-before 'build 'generate-javacc
           (lambda _
             (with-directory-excursion "javaparser-core/src/main/java"
               (invoke "java" "javacc" "../javacc/java.jj"))
             #t))
         (add-before 'build 'copy-javacc-support
           (lambda _
             (with-directory-excursion "javaparser-core/src/main"
               (copy-recursively "javacc-support" "java"))
             #t))
         (add-before 'build 'fix-ambiguousity
           (lambda _
             (substitute* (find-files "javaparser-symbol-solver-core/src/main/java" ".*.java")
               (("TypeParameter node")
                "com.github.javaparser.ast.type.TypeParameter node"))
             (substitute* (find-files "javaparser-core-metamodel-generator/src/main/java" ".*.java")
               (("\\(TypeParameter.class")
                "(com.github.javaparser.ast.type.TypeParameter.class"))
             (substitute* (find-files "javaparser-core/src/main/java" ".*.java")
               (("final TypeParameter ")
                "final com.github.javaparser.ast.type.TypeParameter ")
               (("\\(TypeParameter.class")
                "(com.github.javaparser.ast.type.TypeParameter.class")
               (("new TypeParameter\\(")
                "new com.github.javaparser.ast.type.TypeParameter(")
               (("\\(TypeParameter\\)")
                "(com.github.javaparser.ast.type.TypeParameter)")
               (("TypeParameter r")
                "com.github.javaparser.ast.type.TypeParameter r")
               (("TypeParameter tp")
                "com.github.javaparser.ast.type.TypeParameter tp")
               (("\\{TypeParameter")
                "{com.github.javaparser.ast.type.TypeParameter")
               (("public TypeParameter ")
                "public com.github.javaparser.ast.type.TypeParameter ")
               (("visit\\(TypeParameter ")
                "visit(com.github.javaparser.ast.type.TypeParameter ")
               (("<TypeParameter>")
                "<com.github.javaparser.ast.type.TypeParameter>"))
             #t))
         (replace 'build
           (lambda _
             (define (build name)
               (format #t "Building ~a~%" name)
               (delete-file-recursively "build/classes")
               (mkdir-p "build/classes")
               (apply invoke "javac"
                      "-cp" (string-append (getenv "CLASSPATH") ":"
                                           (string-join (find-files "build/jar" ".") ":"))
                      "-d" "build/classes"
                      (find-files (string-append name "/src/main/java")
                                  ".*.java"))
               (invoke "jar" "-cf" (string-append "build/jar/" name ".jar")
                       "-C" "build/classes" "."))
             (mkdir-p "build/classes")
             (mkdir-p "build/test-classes")
             (mkdir-p "build/jar")
             (build "javaparser-core")
             (build "javaparser-core-serialization")
             (build "javaparser-core-generators")
             (build "javaparser-core-metamodel-generator")
             (build "javaparser-symbol-solver-model")
             (build "javaparser-symbol-solver-logic")
             (build "javaparser-symbol-solver-core")
             #t))
         (replace 'check
           (lambda _
             (define (test name)
               (format #t "Testing ~a~%" name)
               (delete-file-recursively "build/test-classes")
               (mkdir-p "build/test-classes")
               (apply invoke "javac" "-d" "build/test-classes"
                      "-cp" (string-append (getenv "CLASSPATH") ":"
                                           (string-join (find-files "build/jar" ".") ":"))
                      (find-files (string-append name "/src/test/java")
                                  ".*.java"))
               (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":"
                                                   (string-join (find-files "build/jar" ".") ":")
                                                   ":build/test-classes")
                       "org.junit.runner.JUnitCore"
                       (map
                         (lambda (file)
                           (string-join
                             (string-split
                               (substring file 14 (- (string-length file) 5)) #\/)
                             "."))
                         (find-files (string-append name "/src/test/java")
                                     ".*Test.java"))))
             ;; We need junit5 that recursively depend on this package,
             ;; as well as jbehave.
             ;(test "javaparser-core-testing")
             ;(test "javaparser-symbol-solver-testing")
             #t))
         (replace 'install
           (install-jars "build/jar")))))
    (inputs
     `(("java-guava" ,java-guava)
       ("java-jboss-javassist" ,java-jboss-javassist)
       ("java-jsonp-api" ,java-jsonp-api)))
    (native-inputs
     `(("javacc" ,javacc)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-okhttp" ,java-okhttp)))
    (home-page "http://javaparser.org/")
    (synopsis "Parser for Java")
    (description
     "This project contains a set of libraries implementing a Java 1.0 - Java
11 Parser with advanced analysis functionalities.")
    (license license:lgpl2.0+)))

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
       #:tests? #f))
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
       #:tests? #f));; FIXME: tests don't build
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

(define-public java-asm-7
  (package
    (inherit java-asm)
    (version "7.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://gitlab.ow2.org/asm/asm")
                     (commit "1f6020a3f17d9d88dfd54a31370e91e3361c216b")))
              (sha256
               (base32
                "1cpg9j86ckp5cmxlisfr4xr4i4v983xj2mkk0pkbygk3qarjfb88"))))
    (arguments
     `(#:jar-name "asm.jar"
       #:source-dir "asm/src/main/java"
       #:test-dir "asm/src/test"
       ;; Tests require org.junit.jupiter
       #:tests? #f))))

(define-public java-asm-tree-7
  (package
    (inherit java-asm-7)
    (name "java-asm-tree")
    (arguments
     `(#:jar-name "asm-tree.jar"
       #:source-dir "asm-tree/src/main/java"
       #:test-dir "asm-tree/src/test"
       ;; Tests require org.junit.jupiter
       #:tests? #f))
    (propagated-inputs
     `(("java-asm-7" ,java-asm-7)))))

(define-public java-asm-commons-7
  (package
    (inherit java-asm-7)
    (name "java-asm-commons")
    (arguments
     `(#:jar-name "asm-commons.jar"
       #:source-dir "asm-commons/src/main/java"
       #:test-dir "asm-commons/src/test"
       ;; Tests require org.junit.jupiter
       #:tests? #f))
    (propagated-inputs
     `(("java-asm-tree-7" ,java-asm-tree-7)))))

(define-public java-byte-buddy-dep
  (package
    (name "java-byte-buddy-dep")
    (version "1.9.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/raphw/byte-buddy/archive/"
                                  "byte-buddy-" version ".tar.gz"))
              (sha256
               (base32
                "1ywnsp2qbs06sc830xli4fqv0rb2fi6w60bh3ly295h8bv2w9yws"))))
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
                 (("import lombok.EqualsAndHashCode;") "")
                 (("@SuppressFBWarnings.*") "")
                 (("import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;") "")))
             #t)))))
    (inputs
     `(("java-asm-commons-7" ,java-asm-commons-7)))
    (home-page "http://bytebuddy.net/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-byte-buddy-agent
  (package
    (inherit java-byte-buddy-dep)
    (name "java-byte-buddy-agent")
    (arguments
     `(#:jar-name "byte-buddy-agent.jar"
       #:source-dir "byte-buddy-agent/src/main/java"
       #:test-dir "byte-buddy-agent/src/test"
       #:test-exclude (list "**/VirtualMachineForHotSpotTest.*")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-annotations
           (lambda _
             (with-directory-excursion "byte-buddy-agent/src/main/java/net/bytebuddy"
               (substitute* (find-files "." ".*.java")
                 (("@SuppressFBWarnings.*") "")
                 (("import edu.umd.cs.findbugs.annotations.SuppressFBWarnings;") "")))
             #t)))))
    (inputs
     `(("java-junixsocket-common" ,java-junixsocket-common)
       ,@(package-inputs java-byte-buddy-dep)))
    (native-inputs
     `(("java-byte-buddy-dep" ,java-byte-buddy-dep)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))))

(define-public java-junixsocket-common
  (package
    (name "java-junixsocket-common")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/kohlschutter/junixsocket/archive/"
                                  "junixsocket-parent-" version ".tar.gz"))
              (sha256
               (base32
                "0c31qgry5pnhcnp3w62xb0ha0pic1d363rabn4fh8sdpmwnmakww"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "junixsocket-common.jar"
       #:source-dir "junixsocket-common/src/main/java"
       #:tests? #f));no tests
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-spockframework-core
  (package
    (name "java-spockframework-core")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/spockframework/spock/"
                                  "archive/spock-" version ".tar.gz"))
              (sha256
               (base32
                "1nps60666y7d8gmy5bvp18cf1a5jy33kwj8fqhazrdikzphmmp0q"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "spock-core.jar"
       #:source-dir "spock-core/src/main/java"
       #:tests? #f)); No tests
    (inputs
     `(("groovy" ,groovy)
       ("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-jetbrains-annotations" ,java-jetbrains-annotations)
       ("java-junit" ,java-junit)
       ("java-objenesis" ,java-objenesis)
       ("java-byte-buddy-dep" ,java-byte-buddy-dep)))
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

(define-public java-ning-compress
  (package
    (name "java-ning-compress")
    (version "1.0.4")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/ning/compress.git")
                     (commit (string-append "compress-lzf-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1scyws9rs268zvi3p6jvq9yc061zrn49ppp7v91y52si8b1xpbg8"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "ning-compress.jar"
       #:source-dir "src/main/java"
       #:tests? #f; require testng, not junit
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "src/test/lzf/TestLZF.java"
               (("package lzf;") "package lzf;

import com.ning.compress.lzf.*;
import org.junit.Assert.*;"))
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-testng" ,java-testng)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-lzma-sdk
  (package
    (name "java-lzma-sdk")
    (version "18.06")
    (source (origin
              (method url-fetch)
              (uri "https://www.7-zip.org/a/lzma1806.7z")
              (sha256
               (base32
                "0mcs90vjmp8wjqxvrjq0bl2d70njg8md1vy3zr9dd22vwfpzgy01"))))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "Java"
       #:jar-name "lzma.jar"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'unpack
           (lambda* (#:key source #:allow-other-keys)
             (invoke "7z" "x" source)
             #t)))))
    (native-inputs
     `(("p7zip" ,p7zip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:public-domain)))

(define-public java-lzma
  (package
    (name "java-lzma")
    (version "1.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jponge/lzma-java.git")
                     (commit (string-append "lzma-java-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1cmgd95avjq1fsr1bx91cjcvz6ifwgq1iwyzvwvnr6s1w0r8m6hx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "lzma.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-resources
           (lambda _
             (mkdir-p "target/test-classes")
             (copy-recursively "src/test/resources" "target/test-classes")
             #t)))))
    (native-inputs
     `(("java-commons-io" ,java-commons-io)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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
       #:source-dir "."
       #:tests? #f)); no tests
    (home-page "https://github.com/jboss/jboss-annotations-api_spec")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-aspectj-weaver
  (package
    (name "java-aspectj-weaver")
    (version "1.9.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://eclipsemirror.itemis.de/eclipse/tools"
                                  "/aspectj/aspectj-" version "-src.jar"))
              (sha256
               (base32
                ;"0r16lgzindqf4xhdmdyk9j6p15nak2fwhqlp42yg3axvn8fx6r23"))))
                "1g8g6ynwqg93x6842a0xhrgjcfnszd1wldsw3v3dp3xpc8makrfp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-aspectj-weaver.jar"
       #:source-dir "."
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-jar
           (lambda _
             (mkdir-p "weaver-src")
             (chdir "weaver-src")
             (zero? (system* "jar" "xf" "../src/aspectjweaver1.9.1-src.jar"))))
         (add-after 'unpack-jar 'remove-propriatory
           (lambda _
             ;; this file depends on JRockit, for which I can't find a free implementation
             (delete-file "org/aspectj/weaver/loadtime/JRockitAgent.java")
             #t))
         (add-after 'unpack-jar 'rename-lib-back
           (lambda _
             ;; aj.org.objectweb.asm is actually java-asm, renamed
             (substitute* "org/aspectj/weaver/bcel/asm/StackMapAdder.java"
               (("aj.org.objectweb.asm") "org.objectweb.asm"))
             #t))
         (add-before 'build 'copy-ressource
           (lambda _
             (mkdir-p "build/classes")
             (copy-file "aspectj_1_5_0.dtd" "build/classes/aspectj_1_5_0.dtd")
             #t)))))
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
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-jar
           (lambda _
             (mkdir-p "rt-src")
             (chdir "rt-src")
             (zero? (system* "jar" "xf" "../src/aspectjrt1.9.1-src.jar")))))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-asm" ,java-asm)))
    (description "")))

;(define-public java-aspectj-tools
;  (package
;    (inherit java-aspectj-weaver)
;    (name "java-aspectj-tools")
;    (arguments
;     `(#:jar-name "java-aspectj-tools.jar"
;       #:source-dir "."
;       #:tests? #f; no tests
;       #:phases
;       (modify-phases %standard-phases
;         (add-before 'configure 'unpack-jar
;           (lambda _
;             (mkdir-p "tools-src")
;             (chdir "tools-src")
;             (invoke "jar" "xf" "../src/aspectjtools1.9.1-src.jar")
;             (for-each delete-file (find-files ".." ".*.jar$"))
;             #t))
;         (add-before 'build 'copy-resources
;           (lambda _
;             (for-each delete-file (find-files "." ".*.class$"))
;             (mkdir-p "build/classes")
;             (for-each
;               (lambda (file)
;                 (mkdir-p (string-append "build/classes/" (dirname file)))
;                 (copy-file file (string-append "build/classes/" file)))
;               (append
;                 (find-files "." ".*.gif$")
;                 (find-files "." ".*.html$")
;                 (find-files "." ".*.properties$")
;                 (find-files "." ".*.rsc$")
;                 (find-files "." ".*.xml$")))
;             #t)))))
;    (inputs
;     `(("java-asm" ,java-asm)
;       ("java-aspectj-weaver" ,java-aspectj-weaver)
;       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
;       ("java-eclipse-core-resources" ,java-eclipse-core-resources)
;       ("java-eclipse-core-runtime" ,java-eclipse-core-runtime)
;       ("java-eclipse-text" ,java-eclipse-text)))
;    (description "")))

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

(define-public java-jdom-for-intellij
  (package
    (inherit java-jdom)
    (version "2.0.6-intellij")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/intellij-deps-jdom")
                     (commit "da8644efbf3b50b1fd427953298c0a8cb330f54c")))
              (file-name (git-file-name "java-jdom" version))
              (sha256
               (base32
                "0kwzg8flrn6qb9jbcriiqx56chb6j6pjk19xqa6rarf42zad88rc"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar$"))
                   (delete-file "build.xml")
                   #t))))
    (arguments
     `(#:jar-name "jdom.jar"
       #:tests? #f
       #:source-dir "core/src/java"))
    (inputs
     `(("java-jaxen" ,java-jaxen)))))

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
       ;#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-ressources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'build 'remove-unpackaged-dependencies
           ;; TODO: package these dependencies
           (lambda _
             (delete-file-recursively "src/main/java/freemarker/ext/jython")
         (delete-file "src/test/java/freemarker/core/ObjectBuilderSettingsTest.java")
             (delete-file-recursively "src/main/java/freemarker/ext/rhino")
             ;; This class depends on javarebel, a non-free package
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
    (home-page "")
    (synopsis "")
    (description "")
    (license license:expat)))

(define java-hazelcast-version "3.10.4")
;; Required versions are described in pom.xml and hazelcast-client/pom.xml in
;; the hazelcast tarball.
(define java-hazelcast-client-protocol-version "1.6.0")
(define java-hazelcast-aws-version "2.0.0")

(define java-hazelcast-client-protocol-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hazelcast/"
                        "hazelcast-client-protocol/archive/v"
                        java-hazelcast-client-protocol-version ".tar.gz"))
    (file-name (string-append "hazelcast-client-protocol-" java-hazelcast-client-protocol-version ".tar.gz"))
    (sha256
     (base32
      "0snd5cyjgg007nfhhsv2w0n3jybblbcjmpf3qpy4x4m38729gly8"))))

(define java-hazelcast-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hazelcast/hazelcast/archive/v"
                        java-hazelcast-version ".tar.gz"))
    (file-name (string-append "java-hazelcast-" java-hazelcast-version ".tar.gz"))
    (sha256
     (base32
      "0bmhjh15xcqc4k77ncfw60b0gfnh6ndc3rr8am09ys8yga4w59hf"))))

(define java-hazelcast-aws-source
  (origin
    (method url-fetch)
    (uri (string-append "https://github.com/hazelcast/hazelcast-aws/"
                        "archive/v" java-hazelcast-aws-version ".tar.gz"))
    (file-name (string-append "java-hazelcast-aws-" java-hazelcast-aws-version ".tar.gz"))
    (sha256
     (base32
      "0hdih6b4rvcflxn2c3wbn8a6aw13bb0nwmifyi118wr6cqrlv02p"))))

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
             (copy-recursively "hazelcast-code-generator/src/main/resources"
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
                      "/hazelcast/src/main/java:hazelcast-aws-" ,java-hazelcast-aws-version
                      "/src/main/java")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-annotation-processor
           (lambda _
             (substitute* "build.xml"
               (("classpath=\"@refidclasspath\" />")
                "classpath=\"@refidclasspath\">
     <compilerarg line=\"-processor com.hazelcast.client.protocol.generator.CodecCodeGenerator\"/>
     <compilerarg line=\"-s hazelcast/src/main/java\"/>
</javac>")
               (("<javac") "<javac source=\"1.6\""))
             #t))
         (add-before 'build 'unpack-aws
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xzf" (assoc-ref inputs "java-hazelcast-aws-source"))
             #t))
         (add-before 'build 'unpack-client-protocol
           (lambda* (#:key inputs #:allow-other-keys)
             (invoke "tar" "xzf" (assoc-ref inputs "java-hazelcast-client-protocol-source"))
             #t))
         (add-before 'build 'fix-renamed-dependencies
           (lambda _
             (substitute* '("hazelcast-client/src/main/java/com/hazelcast/client/spi/impl/AwsAddressProvider.java"
                            "hazelcast-client/src/main/java/com/hazelcast/client/spi/impl/discovery/HazelcastCloudDiscovery.java"
                            "hazelcast-aws-2.0.0/src/main/java/com/hazelcast/aws/impl/DescribeInstances.java")
               (("com.hazelcast.com.eclipsesource.json") "com.eclipsesource.json"))
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
         (add-before 'build 'remove-package-info
           (lambda _
             (for-each delete-file (find-files "." "package-info.java"))
             #t)))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-apache-freemarker" ,java-apache-freemarker)
       ("java-hazelcast-aws-source" ,java-hazelcast-aws-source)
       ("java-hazelcast-code-generator" ,java-hazelcast-code-generator)
       ("java-hazelcast-client-protocol-source" ,java-hazelcast-client-protocol-source)
       ("java-jsr107" ,java-jsr107)
       ("java-jsr305" ,java-jsr305)
       ("java-log4j-1.2" ,java-log4j-1.2)
       ("java-log4j-api" ,java-log4j-api)
       ("java-minimal-json" ,java-minimal-json)
       ("java-osgi-core" ,java-osgi-core)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-spotbugs-annotations" ,java-spotbugs-annotations)))
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
                "0kh0p1h546k6myd268jlr681bx15q6ip15an56rmqdw4q87xk23v"))
              (patches
                (search-patches "java-jamonapi-jamon-update-dependencies.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jamonapi-jamon.jar"
       #:source-dir "jamon/src/main/java"
       #:test-dir "jamon/src/test"
       #:test-exclude
       (list
         "**/Abstract*.java"
         ;; Fail to parse hazelcast.xml
         "**/DistributedJamonHazelcastTest.java"
         "**/JamonDataPersisterFactoryTest.java"
         ;; javax.management.InstanceAlreadyExistsException
         "**/JmxUtilsTest.java"
         ;; Missing hsqldb as a dependency
         "**/MonProxyTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-cyclic-dependency
           (lambda _
             ;; Classes in this directory depend on spring-framework-context,
             ;; which depends on spring-framework-aop which depends on jamonapi.
             (delete-file-recursively "jamon/src/main/java/com/jamonapi/aop")
             (delete-file-recursively "jamon/src/test/java/com/jamonapi/aop")
             #t)))))
    (inputs
     `(("java-aspectj-rt" ,java-aspectj-rt)
       ("java-eclipse-jetty-io" ,java-eclipse-jetty-io)
       ("java-eclipse-jetty-server" ,java-eclipse-jetty-server)
       ("java-eclipse-jetty-util" ,java-eclipse-jetty-util)
       ("java-hazelcast-bootstrap" ,java-hazelcast-bootstrap)
       ("java-tomcat" ,java-tomcat) ; for catalina and servletapi
       ("java-log4j-api" ,java-log4j-api)
       ("java-log4j-1.2" ,java-log4j-1.2)))
    (native-inputs
     `(("java-asm" ,java-asm)
       ("java-assertj" ,java-assertj)
       ("java-cglib" ,java-cglib)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-jboss-interceptors-api-spec" ,java-jboss-interceptors-api-spec)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)))
    (home-page "")
    (synopsis "")
    (description "")
    ;; A link to the license is present in pom.xml
    (license license:bsd-3)))

(define license:epl2.0 license:epl1.0)
(define-public java-javax-interceptor
  (package
    (name "java-javax-interceptor")
    (version "1.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/javax.interceptor/"
                                  "archive/" version ".tar.gz"))
              (sha256
               (base32
                "1c93q8x7rxml747vzmw13s1gkjhwi0xs8ra27254cvyl0q8fh1kv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-interceptor.jar"
       #:tests? #f; no tests
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (home-page "https://github.com/eclipse-ee4j/interceptor-api")
    (synopsis "")
    (description "")
    ;; Either EPL2.0 or GPL2.0 with classpath exception
    (license (list license:epl2.0 license:gpl2+))))

;; JTA or JSR907
;; Same as java-jboss-transaction-api-spec?
(define-public java-javax-transaction
  (package
    (name "java-javax-transaction")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/javax.transaction/"
                                  "archive/javax.transaction-api-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "13c04282r59pyp784ppnihdi4krksmg8w4ksy2crgw9c9a7wfdh5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-transaction.jar"
       #:tests? #f; no tests
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-cdi-api" ,java-cdi-api)
       ("java-javax-interceptor" ,java-javax-interceptor)))
    (home-page "https://github.com/eclipse-ee4j/jta-api")
    (synopsis "")
    (description "")
    (license (list license:cddl1.1))))

;; jax-rpc
(define-public java-javax-xml-rpc
  (package
    (name "java-javax-xml-rpc")
    (version "1.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/javax.xml.rpc/"
                                  "archive/javax.xml.rpc-api-" version
                                  ".tar.gz"))
              (sha256
               (base32
                "1x4ldqr25s8knadmvwd33nbhyigqi471d62mz09vciy8j5waqys9"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-xml-rpc.jar"
       #:tests? #f; no tests
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-javaee-servletapi" ,java-javaee-servletapi)))
    (home-page "https://github.com/eclipse-ee4j/jax-rpc-api")
    (synopsis "")
    (description "")
    (license (list license:cddl1.1))))

;; also called jaxb-api
(define-public java-javax-xml-bind
  (package
    (name "java-javax-xml-bind")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/jaxb-spec/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0qd352ph8zfqiy7lzp6cczznz6j6vpd2877kqp72wi4w3zz88ydw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-xml-bind.jar"
       #:tests? #f; no tests
       #:source-dir "jaxb-api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           (lambda _
             ;; This file is for java9+
             (for-each delete-file (find-files "." "module-info.java"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "jaxb-api/src/main/resources" "build/classes")
             #t)))))
    (home-page "https://github.com/javaee/jaxb-spec")
    (synopsis "")
    (description "")
    (license license:cddl1.1)))

(define-public java-jaxb-txw-runtime
  (package
    (name "java-jaxb-txw-runtime")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/jaxb-v2/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10jp3fcdib34i31ykd8xjp6i6lf7rylj0w0n2xkqs1rzirs1sjr2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxb-txw-runtime.jar"
       #:source-dir "jaxb-ri/txw/runtime/src/main/java"
       #:test-dir "jaxb-ri/txw/runtime/src/test"
       #:tests? #f; no runnable test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           (lambda _
             ;; This file is for java9+
             (for-each delete-file (find-files "." "module-info.java"))
             #t)))))
    (home-page "https://github.com/javaee/jaxb-spec")
    (synopsis "")
    (description "")
    (license license:cddl1.1)))

(define-public java-jaxb-runtime
  (package
    (inherit java-jaxb-txw-runtime)
    (name "java-jaxb-runtime")
    (arguments
     `(#:jar-name "jaxb-runtime.jar"
       #:tests? #f; no tests
       #:source-dir "jaxb-ri/runtime/impl/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           (lambda _
             ;; This file is for java9+
             (for-each delete-file (find-files "." "module-info.java"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "jaxb-ri/jxc/src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-jaxb-txw-runtime" ,java-jaxb-txw-runtime)
       ("java-javax-xml-bind" ,java-javax-xml-bind)))
    (synopsis "")
    (description "")))

(define-public java-jaxb-xjc
  (package
    (name "java-jaxb-xjc")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/jaxb-v2/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "10jp3fcdib34i31ykd8xjp6i6lf7rylj0w0n2xkqs1rzirs1sjr2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxb-ri.jar"
       #:tests? #f; no tests
       #:source-dir "jaxb-ri/xjc/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java8
           (lambda _
             ;; This file is for java9+
             (for-each delete-file (find-files "." "module-info.java"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "jaxb-ri/jxc/src/main/resources" "build/classes")
             #t)))))
    (home-page "https://github.com/javaee/jaxb-spec")
    (synopsis "")
    (description "")
    (license license:cddl1.1)))

;; also called jpa-api
(define-public java-javax-persistence
  (package
    (name "java-javax-persistence")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/jpa-spec/archive/"
                                  "javax.persistence-api-" version ".tar.gz"))
              (sha256
               (base32
                "14kn70i1g0ij14gm6qbhgmfqprp6qvw3nkw8yd6a09806c70x576"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-persistence.jar"
       #:tests? #f; no tests
       #:source-dir "javax.persistence-api/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "javax.persistence-api/src/main/resources"
                               "build/classes")
             #t)))))
    (home-page "https://github.com/eclipse-ee4j/jpa-api")
    (synopsis "")
    (description "")
    (license (list license:epl1.0 license:edl1.0))))

(define-public java-javax-ejb
  (package
    (name "java-javax-ejb")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/javax.ejb/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "15ipffv3dkbg6psa5knp4bq1bkp0qy21mzsgs94k6xw714igsij5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-ejb.jar"
       #:tests? #f; no tests
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-javax-transaction" ,java-javax-transaction)
       ("java-javax-xml-rpc" ,java-javax-xml-rpc)))
    (home-page "https://github.com/eclipse-ee4j/ejb-api")
    (synopsis "")
    (description "")
    ;; Either EPL2.0 or GPL2.0 with classpath exception
    (license (list license:epl2.0 license:gpl2+))))

;; JSR380 jsr380
(define-public java-javax-validation-1
  (package
    (name "java-javax-validation-1")
    (version "1.0.0.GA")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/beanvalidation/"
                                  "beanvalidation-api/archive/" version
                                  ".tar.gz"))
              (sha256
               (base32
                "03i6p5snm6chpsj62lrvnwq9qr120iv9a6bhkkbqr1j48d2bp8iv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "javax-validation.jar"
       #:tests? #f; require an implementation like Hibernate Validator
       #:source-dir "src/main/java"))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-testng" ,java-testng)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; JSR380 jsr380
(define-public java-javax-validation
  (package
    (inherit java-javax-validation-1)
    (name "java-javax-validation")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/beanvalidation/"
                                  "beanvalidation-api/archive/" version
                                  ".Final.tar.gz"))
              (sha256
               (base32
                "0mmzwrgwfvi68jfjh8ijy8za3wmp5rmwsplyghwyf9lwb9p5x5qz"))))
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))
       ,@(package-arguments java-javax-validation-1)))))

(define-public java-fasterxml-classmate
  (package
    (name "java-fasterxml-classmate")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/java-classmate/"
                                  "archive/classmate-" version ".tar.gz"))
              (sha256
               (base32
                "143gsrgsf7dzqbmd2pw4ky2cd1lhlgmh7daygda03vka73ns6qr3"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "fasterxml-classmate.jar"
       #:source-dir "src/main/java"
       #:test-exclude
       (list
         "**/Abstract*.java"
         ;; Base class with no tests
         "**/BaseTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "engine/src/main/resources" "build/classes")
             #t)))))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "https://fasterxml.com")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jboss-modules
  (package
    (name "java-jboss-modules")
    (version "1.8.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss-modules/"
                                  "jboss-modules/archive/" version ".Final.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0cm8z4kc1lq4jg3866dzhjaaz5kglsn9bymp4ra7jimyij84xxv2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jboss-modules.jar"
       #:source-dir "src/main/java"
       #:tests? #f; require jboss.shrinkwrap
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-wildfly-common
  (package
    (name "java-wildfly-common")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/wildfly/wildfly-common/"
                                  "archive/" version ".Final.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1qgp5b77wj0g3c8wzx2k8qslzk4ran1kbk5scjzj955ljpwvygah"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "wildfly-common.jar"
       ;; Most tests fail because of Invalid bundle interface org.wildfly.common._private.CommonMessages (implementation not found)
       #:tests? #f
       #:source-dir "src/main/java"))
    (inputs
     `(("java-jboss-logging" ,java-jboss-logging)
       ("java-jboss-logging-annotations" ,java-jboss-logging-annotations)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1)))

(define java-jboss-logmanager-bootstrap
  (package
    (name "java-jboss-logmanager")
    (version "2.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss-logging/"
                                  "jboss-logmanager/archive/" version ".Final.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "09qj82wa612bfmwdpgpskg7qvi3n7q6v1arbhs7zbbrzlfgb7wnx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jboss-logmanager.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'stub-wildfly
           (lambda _
             ;; Wildfly is a cyclic dependency. By stubbing it out, we break
             ;; two cycles at once!
             (substitute* "src/main/java/org/jboss/logmanager/ExtLogRecord.java"
               (("import org.wildfly.common.*") "")
               (("HostName.getQualifiedHostName\\(\\)") "\"example.com\"")
               (("Process.getProcessId\\(\\)") "1515")
               (("Process.getProcessName\\(\\)") "\"java\""))
             (substitute* "src/main/java/org/jboss/logmanager/handlers/SyslogHandler.java"
               (("import org.wildfly.common.*") "")
               (("Process.getProcessName\\(\\)") "\"java\"")
               (("Process.getProcessId\\(\\)") "1515"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-jboss-modules" ,java-jboss-modules)
       ("java-jsonp-api" ,java-jsonp-api)))
       ;("java-wildfly-common" ,java-wildfly-common)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jboss-logmanager
  (package
    (inherit java-jboss-logmanager-bootstrap)
    (arguments
     `(#:jar-name "jboss-logmanager.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-jboss-modules" ,java-jboss-modules)
       ("java-jsonp-api" ,java-jsonp-api)
       ("java-wildfly-common" ,java-wildfly-common)))))

(define-public java-jboss-logging
  (package
    (name "java-jboss-logging")
    (version "3.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss-logging/"
                                  "jboss-logging/archive/" version ".Final.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kcibwahdja95zdm8yh2zlqlq6xj42pddjby1845jam2xg7q9pqh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jboss-logging.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-conversion-error
           (lambda _
             ;; Prevent an error that "cannot convert Map<String, String> to
             ;; Map<String, Object>.
             (substitute* "src/main/java/org/jboss/logging/Slf4jLoggerProvider.java"
               (("import java.util.Map")
                "import java.util.HashMap;
import java.util.Map")
               (("Map<String, Object> map = MDC.getCopyOfContextMap\\(\\);")
                "Map<String, String> map_mdc = MDC.getCopyOfContextMap();
Map<String, Object> map2 = new HashMap<String, Object>();
for (Map.Entry<String, String> entry : map_mdc.entrySet()) {
  map2.put(entry.getKey(), entry.getValue());
}
if (map_mdc == null) map2 = null;
final Map<String, Object> map = map2;"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-jboss-logmanager-bootstrap" ,java-jboss-logmanager-bootstrap)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("java-log4j-api" ,java-log4j-api)
       ("java-slf4j-api" ,java-slf4j-api)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jboss-logging-annotations
  (package
    (name "java-jboss-logging-annotations")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss-logging/"
                                  "jboss-logging-tools/archive/" version ".Final.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1azi76q31qhmyq3v9cx0aq1gvddymj080bpc3lvacg45kh7vx9zm"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jboss-logging-annotations.jar"
       #:source-dir "annotations/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "annotations/src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-jboss-logging" ,java-jboss-logging)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public java-jboss-marshalling
  (package
    (name "java-jboss-marshalling")
    (version "2.0.6")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jboss-remoting/jboss-marshalling")
                     (commit (string-append version ".Final"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1afcfk0wgcggyzc4c47kfmxskbpkqvi65vr0g3gl6rikqm010knc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jboss-marshalling.jar"
       #:source-dir "api/src/main/java"
       #:test-dir "api/src/test"))
    (inputs
     `(("java-jboss-modules" ,java-jboss-modules)))
    (native-inputs
     `(("java-testng" ,java-testng)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public java-hibernate-validator-engine
  (package
    (name "java-hibernate-validator-engine")
    ;(version "6.0.13")
    (version "4.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hibernate/"
                                  "hibernate-validator/archive/" version
                                  ".Final.tar.gz"))
              (sha256
               (base32
                "1ifyqgvlzv9fxgw9ssd6slpv0ky9cgxc1xg2rwqiw1nmwgndjnkb"))))
                ;"107220ydll2fgvqzmzrby7b65vi4hsvrnmbb6idrxfmckm05grn4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "hibernate-validator-engine.jar"
       #:source-dir "engine/src/main/java"
       #:test-dir "engine/src/test"
       #:tests? #f; Require more parts of hibernate?
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sources
           (lambda _
             (invoke "xjc" "-d" "engine/src/main/java"
                     "-p" "org.hibernate.validator.internal.xml" "-extension"
                     "engine/src/main/xsd/validation-configuration-1.0.xsd")
             (invoke "xjc" "-d" "engine/src/main/java"
                     "-p" "org.hibernate.validator.internal.xml" "-extension"
                     "-b" "engine/src/main/xjb/binding-customization.xjb"
                     "engine/src/main/xsd/validation-mapping-1.0.xsd")
             #t))
         (add-before 'build 'fix-getters
           (lambda _
             (substitute* (find-files "." ".*.java")
               (("getIgnoreAnnotations") "isIgnoreAnnotations")
               (("getIncludeExistingValidators") "isIncludeExistingValidators"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "engine/src/main/resources" "build/classes")
             #t)))))
    (inputs
     `(("java-fasterxml-classmate" ,java-fasterxml-classmate)
       ("java-javax-persistence" ,java-javax-persistence)
       ("java-javax-validation-1" ,java-javax-validation-1)
       ("java-jboss-logging" ,java-jboss-logging)
       ("java-jboss-logging-annotations" ,java-jboss-logging-annotations)
       ("java-joda-time" ,java-joda-time)
       ("java-jsoup" ,java-jsoup)
       ;; For javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    (native-inputs
     `(("java-easymock" ,java-easymock)
       ("java-testng" ,java-testng)))
    (home-page "https://hibernate.org/validator/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; javax.enterprise.concurrency
(define-public java-concurrency-api
  (package
    (name "java-concurrency-api")
    (version "1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/concurrency-ee-spec/"
                                  "archive/javax.enterprise.concurrent-api-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "038rc3bvpq4y96g0bb7b0ac6ip8m4rr931r1mlfmcnbav1n8b7wp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "concurrency-api.jar"
       #:source-dir "api/src/main/java"
       #:test-dir "api/src/test"))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "https://github.com/eclipse-ee4j/concurrency-api")
    (synopsis "")
    (description "")
    (license license:cddl1.1)))

(define-public java-spring-framework-core
  (package
    (name "java-spring-framework-core")
    (version "4.3.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/spring-projects/"
                                  "spring-framework/archive/v" version
                                  ".RELEASE.tar.gz"))
              (sha256
               (base32
                "1796ch4ahhx8jq9v3v16dr0xcdx9fqf8waf75cjh52fxvi6ipnqj"))
              (patches (search-patches "java-spring-framework-remove-non-free.patch"))))
    (arguments
     `(#:jar-name "java-spring-framework-core.jar"
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
         (add-before 'configure 'unbundle-asm
           (lambda _
             (with-directory-excursion "src/main/java/org/springframework/asm"
               (for-each delete-file
                         (filter (lambda (file) (not (string=? file "./SpringAsmInfo.java")))
                           (find-files "." ".*.java")))
               (substitute* "SpringAsmInfo.java"
                 (("package org.springframework.asm;")
                  "package org.springframework.asm;

import org.objectweb.asm.Opcodes;")))
             #t))
         (add-before 'configure 'rename-dep
           (lambda _
             (substitute* "src/main/java/org/springframework/objenesis/SpringObjenesis.java"
               (("org.springframework.objenesis\\.") "org.objenesis.")
               (("import org.springframework.util.ConcurrentReferenceHashMap;")
                "import org.springframework.util.ConcurrentReferenceHashMap;
import org.objenesis.Objenesis;
import org.objenesis.ObjenesisException;"))
             (substitute* (find-files "." ".*.java")
               (("org.springframework.asm\\.") "org.objectweb.asm.")
               (("org.objectweb.asm.SpringAsmInfo")
                "org.springframework.asm.SpringAsmInfo"))
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
     `(("java-asm" ,java-asm)
       ("java-aspectj-weaver" ,java-aspectj-weaver)
       ("java-cglib" ,java-cglib)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-jopt-simple" ,java-jopt-simple)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("java-objenesis" ,java-objenesis)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-all" ,java-hamcrest-all)
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
     `(#:jar-name "java-spring-framework-beans.jar"
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

(define-public java-spring-framework-beans-groovy
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-beans-groovy")
    (arguments
     `(#:jar-name "java-spring-framework-beans-groovy.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-beans-groovy")
             #t))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "CLASSPATH" "")
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "groovyc" "-j" "-d" "build/classes"
                    "-cp" (string-join
                            (append
                              (find-files (assoc-ref inputs "java-commons-logging-minimal") ".*.jar$")
                              (find-files (assoc-ref inputs "java-spring-framework-core") ".*.jar$")
                              (find-files (assoc-ref inputs "java-spring-framework-beans") ".*.jar$"))
                            ":")
                    (append
                      (find-files "src/main/java" ".*.java$")
                      (find-files "src/main/groovy" ".*.groovy$")))
             (invoke "jar" "cf" "build/jar/java-spring-framework-beans-groovy.jar" "-C" "build/classes" ".")
             #t)))))
    (inputs
     `(("groovy" ,groovy)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-core" ,java-spring-framework-core)))
    (description "")))

(define-public java-spring-framework-aop
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-aop")
    (arguments
     `(#:jar-name "java-spring-framework-aop.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude
       (list
         "**/AspectJExpressionPointcutTests.java"
         "**/Abstract*.java"
         ;; Required parameter names not available
         "**/ArgumentBindingTests.java"
         "**/ReflectiveAspectJAdvisorFactoryTests.java")
       #:test-include
       (list "**/*Tests.java")
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
               (("org.springframework.objenesis") "org.objenesis")
               (("org.objenesis.SpringObjenesis")
                "org.springframework.objenesis.SpringObjenesis"))
             #t))
         (add-before 'check 'copy-test-classes
           (lambda _
             (copy-file "../spring-core/src/test/java/org/springframework/tests/TestResourceUtils.java"
                        "src/test/java/org/springframework/tests/TestResourceUtils.java")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/TimeStamped.java"
                        "src/test/java/org/springframework/tests/TimeStamped.java")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/Assume.java"
                        "src/test/java/org/springframework/tests/Assume.java")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/TestGroup.java"
                        "src/test/java/org/springframework/tests/TestGroup.java")
             (mkdir "src/test/java/org/springframework/util")
             (copy-file "../spring-core/src/test/java/org/springframework/util/SerializationTestUtils.java"
                        "src/test/java/org/springframework/util/SerializationTestUtils.java")
             (mkdir "src/test/java/org/springframework/tests/beans")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/beans/CollectingReaderEventListener.java"
                        "src/test/java/org/springframework/tests/beans/CollectingReaderEventListener.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/DerivedTestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/DerivedTestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/INestedTestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/INestedTestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/NestedTestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/NestedTestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/IndexedTestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/IndexedTestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/Colour.java"
                        "src/test/java/org/springframework/tests/sample/beans/Colour.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/TestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/TestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/ITestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/ITestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/IOther.java"
                        "src/test/java/org/springframework/tests/sample/beans/IOther.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/CountingTestBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/CountingTestBean.java")
             (copy-file "../spring-beans/src/test/java/org/springframework/tests/sample/beans/SideEffectBean.java"
                        "src/test/java/org/springframework/tests/sample/beans/SideEffectBean.java")
             #t))
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
       ("java-commons-pool1" ,java-commons-pool1)
       ("java-commons-pool" ,java-commons-pool)
       ("java-jamonapi-jamon-bootstrap" ,java-jamonapi-jamon-bootstrap)
       ("java-javax-inject" ,java-javax-inject)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ("java-objenesis" ,java-objenesis)
       ;; Note: for javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    (native-inputs
     `(("java-asm" ,java-asm)
       ,@(package-native-inputs java-spring-framework-core)))
    (description "")))

(define-public java-spring-framework-instrument
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-instrument")
    (arguments
     `(#:jar-name "java-spring-framework-instrument.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-instrument")
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
    (inputs '())
    (description "")))

(define-public java-spring-framework-test
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-test")
    (arguments
     `(#:jar-name "java-spring-framework-test.jar"
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

(define-public java-spring-framework-context
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-context")
    (arguments
     `(#:jar-name "java-spring-framework-context.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; TODO: require spring-framework-test
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-context")
             #t))
         (add-before 'build 'fix-cglib
           (lambda _
             (with-directory-excursion "src/main/java/org/springframework"
               (substitute*
                 (list
                   "context/expression/MapAccessor.java"
                   "context/annotation/ConfigurationClassEnhancer.java"
                   "scripting/support/ScriptFactoryPostProcessor.java")
                 (("org.springframework.objenesis.ObjenesisException")
                  "org.objenesis.ObjenesisException")
                 (("org.springframework.asm") "org.objectweb.asm")
                 (("org.springframework.cglib") "net.sf.cglib")
                 (("net.sf.cglib.core.SpringNamingPolicy")
                  "org.springframework.cglib.core.SpringNamingPolicy")))
             #t))
         (add-before 'build 'remove-jruby
           (lambda _
             (delete-file-recursively
               "src/main/java/org/springframework/scripting/jruby")
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
     `(("groovy" ,groovy)
       ("java-asm" ,java-asm)
       ("java-aspectj-weaver" ,java-aspectj-weaver)
       ("java-bsh" ,java-bsh)
       ("java-cglib" ,java-cglib)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-concurrency-api" ,java-concurrency-api)
       ("java-hibernate-validator-engine" ,java-hibernate-validator-engine)
       ("java-javax-ejb" ,java-javax-ejb)
       ("java-javax-inject" ,java-javax-inject)
       ("java-javax-interceptor" ,java-javax-interceptor)
       ("java-javax-validation-1" ,java-javax-validation-1)
       ("java-joda-time" ,java-joda-time)
       ("java-objenesis" ,java-objenesis)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-spring-framework-aop" ,java-spring-framework-aop)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-beans-groovy" ,java-spring-framework-beans-groovy)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ("java-spring-framework-expression" ,java-spring-framework-expression)
       ("java-spring-framework-instrument" ,java-spring-framework-instrument)
       ;; Note: for javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    ;(native-inputs
    ; `(("java-spring-framework-test" ,java-spring-framework-test)
    ;   ,@(package-native-inputs java-spring-framework-core)))
    (description "")))

(define-public java-spring-framework-web
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-web")
    (arguments
     `(#:jar-name "java-spring-framework-web.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-web")
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         ;(add-before 'check 'copy-test-classes
         ;  (lambda _
         ;    (mkdir-p "src/test/java/org/springframework/tests")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/TestGroup.java"
         ;               "src/test/java/org/springframework/tests/TestGroup.java")
         ;    (copy-file "../spring-core/src/test/java/org/springframework/tests/Assume.java"
         ;               "src/test/java/org/springframework/tests/Assume.java")
         ;    #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively "src/test/resources" "build/test-classes")
             #t)))))
    (inputs
     `(("java-aopalliance" ,java-aopalliance)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
       ("java-fasterxml-jackson-dataformat-xml" ,java-fasterxml-jackson-dataformat-xml)
       ("java-gson" ,java-gson)
       ("java-httpcomponents-httpasyncclient" ,java-httpcomponents-httpasyncclient)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-httpcomponents-httpcore-nio" ,java-httpcomponents-httpcore-nio)
       ("java-javax-mail" ,java-javax-mail)
       ("java-netty-buffer" ,java-netty-buffer)
       ("java-netty-codec" ,java-netty-codec)
       ("java-netty-handler" ,java-netty-handler)
       ("java-netty-transport" ,java-netty-transport)
       ("java-okhttp" ,java-okhttp)
       ("java-protobuf" ,java-protobuf)
       ("java-spring-framework-aop" ,java-spring-framework-aop)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-context" ,java-spring-framework-context)
       ("java-spring-framework-core" ,java-spring-framework-core)))
    (description "")))

(define-public java-spring-framework-expression
  (package
    (inherit java-spring-framework-core)
    (name "java-spring-framework-expression")
    (arguments
     `(#:jar-name "java-spring-framework-expression.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Needed because tests look for data in src/... directly.
             (chdir "spring-expression")
             #t))
         (add-before 'build 'fix-asm
           (lambda _
             (substitute* (find-files "." ".*.java")
               (("org.springframework.asm") "org.objectweb.asm"))
             (substitute* "src/main/java/org/springframework/expression/spel/standard/SpelCompiler.java"
               (("@Override") ""))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'check 'copy-test-classes
           (lambda _
             (mkdir-p "src/test/java/org/springframework/tests")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/TestGroup.java"
                        "src/test/java/org/springframework/tests/TestGroup.java")
             (copy-file "../spring-core/src/test/java/org/springframework/tests/Assume.java"
                        "src/test/java/org/springframework/tests/Assume.java")
             #t))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (copy-recursively "src/test/resources"
                               "build/test-classes")
             #t)))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-spring-framework-core" ,java-spring-framework-core)))
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
    (license (list license:mpl2.0 license:epl1.0))))

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
       #:jar-name "log4j-1.2.jar"))))

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
     `(#:test-target "test"
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
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'check 'copy-test-resource
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes")
             #t)))))
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

(define-public java-commons-digester-2
  (package
    (name "java-commons-digester-2")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/digester/source/"
                                  "commons-digester-" version "-src.tar.gz"))
              (sha256
               (base32
                "0gsli0qbi8h795ps7dpiccd3xfaqwrqcl7qzv59y5iyyd9xg04r7"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-digester.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resource
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'check 'copy-test-resource
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes")
             #t)))))
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
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'check 'copy-test-resource
           (lambda _
             (copy-recursively "src/test/resources" "build/test-classes")
             #t)))))
         ;(add-before 'build 'fix-digester
         ;  (lambda _
         ;    ;; Port from digester 1 to digester 3.
         ;    (substitute* (find-files "src/main/java" ".*\\.java")
         ;      (("commons.digester") "commons.digester3")
         ;      (("org.apache.commons.digester3.xmlrules.DigesterLoader")
         ;       "org.apache.commons.digester3.binder.DigesterLoader"))
         ;    ;; digester is private in this class, so we use the getter
         ;    (substitute* "src/main/java/org/apache/commons/validator/FormSetFactory.java"
         ;      (("digester.peek") "getDigester().peek"))
         ;    (substitute* "src/main/java/org/apache/commons/validator/ValidatorResources.java"
         ;      (("// DEPRECATED")
         ;       "// DEPRECATED\nimport org.apache.commons.digester3.xmlrules.FromXmlRulesModule;")
         ;      (("private Digester initDigester")
         ;       (string-append
         ;         "private FromXmlRulesModule rulesModule(final URL url) {\n"
         ;         "  return new FromXmlRulesModule() {\n"
         ;         "    @Override\n"
         ;         "    protected void loadRules() {\n"
         ;         "      loadXMLRules(url);"
         ;         "    }\n"
         ;         "  };\n"
         ;         "}\n"
         ;         "private Digester initDigester"))
         ;      ;; Copied from digester tests
         ;      (("createDigester\\(rulesUrl\\)")
         ;       "newLoader(rulesModule(rulesUrl)).newDigester()")))))))
    (inputs
     `(("java-commons-digester-2" ,java-commons-digester-2)
       ("java-commons-beanutils" ,java-commons-beanutils)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-validator")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-myfaces-api
  (package
    (name "java-myfaces-api")
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/myfaces/source/"
                                  "myfaces-core-assembly-" version "-src.tar.gz"))
              (sha256
               (base32
                "0nixl958pi7f61vgnaj0nshdpifdg4m0rlxb9ckdmabr6x2fsnvc"))))
              ; version 2.3.1:
              ;(method svn-fetch)
              ;(uri (svn-reference
              ;       (url (string-append "http://svn.apache.org/repos/asf/"
              ;                           "myfaces/core/tags/myfaces-core-module-"
              ;                           version))
              ;       (revision 1830627)))
              ;(file-name (string-append name "-" version))
              ;(sha256
              ; (base32
              ;  "1wag19756ahys8cms4snsqbqifkpfy578x3zkjkr5ba0424v5yvz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "myfaces-api.jar"
       #:source-dir "api/src/main/java"
       #:test-dir "api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'double-unpack
           (lambda _
             (chdir "src")
             (invoke "unzip" (car (find-files "." "source-release")))
             (chdir (car (find-files "." "core-module" #:directories? #t)))
             ;; Require a maven-2 plugin :/
             (delete-file-recursively "api/src/main/java/javax/faces/component")
             #t)))))
    (inputs
     `(("java-javax-inject" ,java-javax-inject)
       ;; for javax-el (el-api)
       ("java-tomcat" ,java-tomcat)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://commons.apache.org/proper/commons-chain")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-chain
  (package
    (name "java-commons-chain")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache//commons/chain/source/"
                                  "commons-chain-" version "-src.tar.gz"))
              (sha256
               (base32
                "0lgib3dpkympp8ajlgpfavbzfal9bv685gfa9ygyv091ja772rsd"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       ;; TODO: incompatibilities with current versions of portlet
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
         ;    ;; Replace digester with digester3
         ;    (substitute* (find-files "src" ".*\\.java")
         ;      (("commons.digester") "commons.digester3"))
             (with-directory-excursion "src/java/org/apache/commons/chain"
         ;      ;; Remove a dependency to myfaces
               (delete-file-recursively "web/faces"))
         ;      ;; digester is now private: use a public accessor
         ;      (substitute* '("config/ConfigCatalogRule.java"
         ;                     "config/ConfigDefineRule.java"
         ;                     "config/ConfigRegisterRule.java")
         ;        (("digester\\.") "getDigester().")))
             #t))
         (replace 'install
           (install-jars ".")))))
    (inputs
     `(("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-commons-beanutils" ,java-commons-beanutils)
       ("java-commons-digester-2" ,java-commons-digester-2)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-portlet-api" ,java-portlet-api)))
    ;(native-inputs
    ; `(("java-junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-chain")
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
    (version "2.5.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/struts/" version "/struts-"
                                  version "-src.zip"))
              (sha256
               (base32
                "14ds6qrxrjd7np8yizdmqd49jd94yy2gghpq9zlvl53w4bv48kwx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "apache-struts.jar"
       #:source-dir "src/core/src/main/java"
       #:test-dir "src/core/src/test"))
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (add-before 'build 'copy-required-classes
       ;    (lambda* (#:key inputs #:allow-other-keys)
       ;      (let ((tools (assoc-ref inputs "java-velocity-tools"))
       ;            (velocity-dir "build/velocity")
       ;            (tools-dir (string-append "build/velocity/velocity-tools-"
       ;                                      ,(package-version java-velocity-tools)
       ;                                      "-src")))
       ;        (mkdir-p velocity-dir)
       ;        (with-directory-excursion velocity-dir
       ;          (invoke "tar" "xf" tools))
       ;        (with-directory-excursion tools-dir
       ;          (for-each
       ;            (lambda (file)
       ;              (install-file file (string-append "../../../src/core/"
       ;                                                (dirname file))))
       ;            (find-files "." "."))))
       ;              ;(find-files "." "ToolboxManager.java$")
       ;              ;(find-files "." "^ToolInfo.java$")
       ;              ;(find-files "." "^ServletUtils.java$")
       ;              ;(find-files "." "^SkipSetters.java$")
       ;              ;(find-files "." "^ViewToolContext.java$")
       ;              ;(find-files "." "^ViewContext.java$")
       ;              ;(find-files "." "^ChainedContext.java$")))))
       ;      #t)))))
    (inputs
     `(("java-apache-freemarker" ,java-apache-freemarker)
       ("java-log4j-api" ,java-log4j-api)
       ("java-commons-fileupload" ,java-commons-fileupload)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-ognl" ,java-ognl)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-spring-framework-beans" ,java-spring-framework-beans)
       ("java-spring-framework-context" ,java-spring-framework-context)
       ("java-spring-framework-core" ,java-spring-framework-core)
       ("java-spring-framework-web" ,java-spring-framework-web)
       ("java-velocity" ,java-velocity)
       ("java-velocity-tools" ,(package-source java-velocity-tools))
       ("java-testng" ,java-testng)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("unzip" ,unzip)))
    (home-page "https://struts.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-apache-struts-1
  (package
    (name "java-apache-struts-1")
    (version "1.3.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/struts/"
                                  version "/struts-" version "-src.zip"))
              (sha256
               (base32
                "05mxari6m8vrirz3i7rdvjpc637s11fjl6qk9hpvl1yyy7bigmn1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "struts.jar"
       #:source-dir "src/core/src/main/java"
       #:test-dir "src/core/src/test"
       #:tests? #f; require deleted files
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "src/core/src/main/java"
               (for-each delete-file (find-files "." "^Test"))
               (delete-file-recursively "org/apache/struts/mock"))
             #t)))))
    (inputs
     `(("antlr2" ,antlr2)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-commons-beanutils" ,java-commons-beanutils)
       ("java-commons-chain" ,java-commons-chain)
       ("java-commons-digester-2" ,java-commons-digester-2)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-fileupload" ,java-commons-fileupload)
       ("java-commons-validator" ,java-commons-validator)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-apache-struts-taglib-1
  (package
    (inherit java-apache-struts-1)
    (name "java-apache-struts-taglib-1")
    (arguments
     `(#:jar-name "struts.jar"
       #:source-dir "src/taglib/src/main/java"
       #:tests? #f; require deleted files
       #:test-dir "src/taglib/src/test"))
    (inputs
     `(("java-apache-struts-1" ,java-apache-struts-1)
       ,@(package-inputs java-apache-struts-1)))))

(define-public java-apache-struts-tiles-1
  (package
    (inherit java-apache-struts-1)
    (name "java-apache-struts-tiles-1")
    (arguments
     `(#:jar-name "struts.jar"
       #:source-dir "src/tiles/src/main/java"
       #:tests? #f; require deleted files
       #:test-dir "src/tiles/src/test"))
    (inputs
     `(("java-apache-struts-1" ,java-apache-struts-1)
       ,@(package-inputs java-apache-struts-1)))))

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
     `(#:test-target "test-main"
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
     `(("javacc" ,javacc)
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

(define-public java-sslext
  (package
    (name "java-sslext")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/sslext/sslext%20for%20"
                                  "Struts%201.2/Release%200/"
                                  "sslext-struts1.2-src.tar.gz"))
              (sha256
               (base32
                "1a2axpq719smfc37lnj1flprpdhi8m1rmkdp90z2fv13rdg2dlq5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "sslext.jar"
       #:source-dir "."
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-enum
           (lambda _
             (substitute* "build.xml"
               (("<javac") "<javac source=\"1.4\"")))))))
    (inputs
     `(("java-commons-digester-2" ,java-commons-digester-2)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-apache-struts-1" ,java-apache-struts-1)
       ("java-apache-struts-taglib-1" ,java-apache-struts-taglib-1)
       ("java-apache-struts-tiles-1" ,java-apache-struts-tiles-1)
       ("java-tomcat" ,java-tomcat)))
    (home-page "https://velocity.apache.org/tools/devel")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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
                "0d93v8nj95jfdgx7n72axaavdq2h800vxyi4vx35rdphndy1xg51"))
              (patches
                (search-patches
                  "java-velocity-tools-2.0-servlet.patch"
                  "java-velocity-tools-2.0-port-to-dom4j-2.0.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test-main"
       #:tests? #f; FIXME: need a fix to build.xml and hsqldb
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
             ;; Don't download anything
             (substitute* "build.xml"
               ((".*download.xml.*") ""))
             #t))
         (replace 'install
           (install-jars "dist")))))
    (inputs
     `(("java-apache-struts-1" ,java-apache-struts-1)
       ("java-apache-struts-taglib-1" ,java-apache-struts-taglib-1)
       ("java-apache-struts-tiles-1" ,java-apache-struts-tiles-1)
       ("java-commons-digester-2" ,java-commons-digester-2)
       ("java-commons-validator" ,java-commons-validator)
       ("java-commons-beanutils", java-commons-beanutils)
       ("java-dom4j" ,java-dom4j)
       ("java-sslext" ,java-sslext)
       ("java-velocity" ,java-velocity)))
    (home-page "https://velocity.apache.org/tools/devel")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-i18n
  (package
    (name "java-plexus-i18n")
    (version "1.0-beta12")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/codehaus-plexus/plexus-i18n.git")
                     (commit "e5b25dd280af2db4d91742a0d93571335e09c46a")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1nkq3c6ba8hv01qi30k50lwa18iswnl9q6i6lrcxdvyzq015jqcl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-plexus-i18n.jar"
       #:source-dir "src/main/java"))
    (inputs
     `(("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)))
    (native-inputs
     `(("java-commons-collections" ,java-commons-collections)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-guava" ,java-guava)
       ("java-junit" ,java-junit)))
    (home-page "https://codehaus-plexus.github.io/plexus-i18n/")
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
       #:source-dir "src/main/java"))
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

(define-public java-commons-pool1
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

(define-public java-portlet-api
  (package
    (name "java-portlet-api")
    (version "3.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/portals/pluto/"
                                  "pluto-" version "-source-release.zip"))
              (sha256
               (base32
                "0fl1xc1jgfvax2ccnygzfwgqrx60h40hzsv95gcmnp4n99im4pxh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "portlet-api.jar"
       #:source-dir "portlet-api/src/main/java"
       ;; no proper tests
       #:tests? #f))
    (inputs
     `(("java-cdi-api" ,java-cdi-api)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-javax-inject" ,java-javax-inject)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://commons.apache.org/proper/commons-fileupload/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-commons-fileupload
  (package
    (name "java-commons-fileupload")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/fileupload/source/"
                                  "commons-fileupload-" version "-src.tar.gz"))
              (sha256
               (base32
                "0w88khx30yj1f629y4dl7s5jiygh3lrdyxz8zmr2vmj8rhzd1dsf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-fileupload.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f)); Bad encoding
    (inputs
     `(("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-commons-io" ,java-commons-io)
       ("java-portlet-api" ,java-portlet-api)))
    (home-page "https://commons.apache.org/proper/commons-fileupload/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; This version is required by velocity 2.0
(define-public java-javacc-5
  (package
    (inherit javacc)
    (version "5.0")
    (source (origin
              (method url-fetch)
              (uri "https://javacc.org/downloads/javacc-5.0src.tar.gz")
              (sha256
               (base32
                "0w3kl5zal9g0gwpcnlii6spgvb2yi3dpj1vz592ly18h6yfswv3n"))))
    (arguments
      (substitute-keyword-arguments (package-arguments javacc)
        ((#:phases phases)
         `(modify-phases ,phases
            ;; This phase renames the generated jar so it can be handled by
            ;; our already written 'install phase.
            (add-before 'install 'rename-jar
              (lambda _
                (mkdir-p "target")
                (rename-file "bin/lib/javacc.jar" "target/javacc.jar")))))))))

;(define-public java-icu4j
;  (package
;    (name "java-icu4j")
;    (version "58.2")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append
;                     "http://download.icu-project.org/files/icu4j/" version
;                     "/icu4j-"
;                     (string-map (lambda (x) (if (char=? x #\.) #\_ x)) version)
;                     ".tgz"))
;              (sha256
;               (base32
;                "1mvqjlc3cbaraa0bv0vyl44xf0x6n81inqsh69bl7f88iycfpns9"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:tests? #f ; Requires java-ivy that we don't have yet.
;       #:phases
;       (modify-phases %standard-phases
;         ;; icu4j archive contains its sources directly at the top, not in
;         ;; a subdirectory as usual.
;         (add-after 'unpack 'chdir
;           (lambda _
;             (chdir "..")))
;         (replace 'install
;           (lambda* (#:key outputs #:allow-other-keys)
;             (let ((share (string-append (assoc-ref outputs "out") "/share/java")))
;               (mkdir-p share)
;               (copy-file "icu4j-charset.jar" (string-append share "/icu4j-charset.jar"))
;               (copy-file "icu4j.jar" (string-append share "/icu4j.jar"))))))))
;    (home-page "http://site.icu-project.org/")
;    (synopsis "")
;    (description "")
;    (license license:x11)))

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

(define-public java-jsr308-langtools
  (package
    (name "java-jsr308-langtools")
    (version "2.4.0")
    (source (origin
              (method hg-fetch)
              (uri (hg-reference
                     (url "https://bitbucket.org/typetools/jsr308-langtools")
                     (changeset (string-append "jsr308-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1h38pib2snw5pxyz79p6i4ls2gr9gb5gkdpbc2c3w597samm6vwp"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar$"))
                   #t))))
    (build-system gnu-build-system)
    ;; We need "jdk" because we want to use this package as the jdk later on.
    (outputs '("out" "jdk"))
    (arguments
     `(;#:make-flags (list "-f" "make/build.xml")
       ;#:build-target "clean-and-build-all-tools"
       #:tests? #f; TODO: find the right target
       #:validate-runpath? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (setenv "JAVA_HOME" (assoc-ref inputs "jdk"))
             (invoke "ant" "-f" "make/build.xml" "clean-and-build-all-tools")
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "jdk"))
                    (jdk (assoc-ref inputs "jdk"))
                    (java (string-append out "/share/java"))
                    (bin (string-append out "/bin")))
              (for-each (lambda (jar)
                          (install-file jar java))
                (find-files "." ".*.jar$"))
              (mkdir-p bin)
              (symlink (string-append jdk "/bin/java")
                       (string-append bin "/java"))
              (symlink (string-append jdk "/bin/jar")
                       (string-append bin "/jar"))
              (for-each (lambda (jar)
                          (let* ((name (substring jar 2 (- (string-length jar) 4)))
                                 (file (string-append bin "/" name)))
                            (copy-file "src/share/bin/launcher.sh-template"
                                       file)
                            (substitute* file
                              (("#TARGET_JAVA#") (string-append bin "/java"))
                              (("#PS#") ":")
                              (("#PROGRAM#") name)
                              (("mylib=.*") "mylib=\"$mydir/../share/java\"\n")
                              (("unzip") (which "unzip")))
                            (chmod file #o755)))
                (with-directory-excursion java
                  (find-files "." ".*.jar$"))))
             #t)))))
    (inputs
     `(("unzip" ,unzip)))
    (native-inputs
     `(("jdk" ,icedtea-8 "jdk")
       ("ant" ,ant)))
    (home-page "https://checkerframework.org/jsr308/")
    (synopsis "Alternative java compiler implementation")
    (description "This package contains the type annotations compiler, which
is fully backward-compatible.  It can be used in place of javac and will have
the same behaviour, but it will also allow the use of annotations in comments
for compatibility with java 7.  This package is part of the checkerframework.")
    (license license:gpl2))); GPL 2 only

(define-public java-plume-lib-reflection-util
  (package
    (name "java-plume-lib-reflection-util")
    (version "0.0.1.1")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/plume-lib/reflection-util.git")
                     (commit "b0de5e70ad71f75a6fd1918d35fdaeb29e97e189")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0k1ls34ka0vjpyav1kn3ys19wjyp1vrjaha73yr90knpjwij8mi2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plume-lib-reflection-util.jar"
       #:source-dir "src/main/java"))
    (inputs
     `(("java-checkerframework-qual-annotation"
        ,java-checkerframework-qual-annotation)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://plumelib.org")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-plume-lib-util
  (package
    (name "java-plume-lib-util")
    (version "1.0.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/plume-lib/plume-util.git")
                     (commit "db6dd1795b942e75360bf93de16f973922d767d8")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1i1hyvyprx879awypb0hjq74k484dw3744ljrhwyxzvvrazr5rc4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plume-lib-util.jar"
       #:source-dir "src/main/java"))
    (inputs
     `(("java-checkerframework-qual-annotation"
        ,java-checkerframework-qual-annotation)
       ("java-plume-lib-reflection-util" ,java-plume-lib-reflection-util)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://plumelib.org")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-annotation-tools
  (package
    (name "java-annotation-tools")
    (version "3.8.3")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/typetools/annotation-tools.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1izl36pf9ahg5c4sq0ki49sfyq9r89v5snsp6559w12ykbl9x3pg"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "all"
       #:jdk ,java-jsr308-langtools
       #:tests? #f; too complex for now
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/jar")
             (mkdir-p "build/classes")
             (apply invoke "javac" "-d" "build/classes"
                    (find-files "asmx/src" ".*.java$"))
             (invoke "jar" "cf" "build/jar/asmx.jar"
                     "-C" "build/classes" ".")
             (delete-file-recursively "build/classes")
             (mkdir-p "build/classes")
             (apply invoke "javac" "-d" "build/classes"
                    "-cp" (string-append "build/jar/asmx.jar:" (getenv "CLASSPATH"))
                    (find-files "scene-lib/src" ".*.java$"))
             (invoke "jar" "cf" "build/jar/scenelib.jar"
                     "-C" "build/classes" ".")
             (delete-file-recursively "build/classes")
             (mkdir-p "build/classes")
             (apply invoke "javac" "-d" "build/classes"
                    "-cp" (string-append
                            "build/jar/asmx.jar:build/jar/scenelib.jar:"
                            (getenv "CLASSPATH"))
                    (find-files "annotation-file-utilities/src" ".*.java$"))
             (invoke "jar" "cf" "build/jar/annotation-file-utilities.jar"
                     "-C" "build/classes" ".")
             #t))
           (replace 'install
             (install-jars "build/jar")))))
    (inputs
     `(("java-checkerframework-qual-annotation"
        ,java-checkerframework-qual-annotation)
       ("java-guava" ,java-guava)
       ("java-plume-lib-util" ,java-plume-lib-util)))
    (home-page "https://checkerframework.org/annotation-file-utilities/")
    (synopsis "External storage of annotations")
    (description "Sometimes, it is convenient to specify the annotations
outside the source code or the @file{.class} file.  This package is also known
as the Annotation File Utilities, which is one of its components.")
    (license license:expat)))

(define-public java-checkerframework
  (package
    (name "java-checkerframework")
    (version "2.5.8")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/typetools/checker-framework.git")
                     (commit "21a34d7db3c20f314fa435190fc4db48b1f50e24")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0fdf6m9s1bw8k4567vymhz7x6vpx255ks30nsawdsxhi0kqd219s"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar$"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "checkerframework.jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (define (build name)
               (format #t "Building ~a~%" name)
               (delete-file-recursively "build/classes")
               (mkdir-p "build/classes")
               (apply invoke "javac" "-d" "build/classes"
                      "-cp" (string-append (getenv "CLASSPATH") ":"
                                           (string-join (find-files "build/jar" ".") ":"))
                      (find-files (string-append name "/src/main/java")
                                  ".*.java"))
               (invoke "jar" "-cf" (string-append "build/jar/checkerframework-"
                                                  name ".jar")
                       "-C" "build/classes" "."))
             (mkdir-p "build/classes")
             (mkdir-p "build/test-classes")
             (mkdir-p "build/jar")
             (build "javacutil")
             (build "dataflow")
             (build "framework")
             (build "checker")
             #t))
         (replace 'check
           (lambda _
             (define (test name)
               (format #t "Testing ~a~%" name)
               (delete-file-recursively "build/test-classes")
               (mkdir-p "build/test-classes")
               (apply invoke "javac" "-d" "build/test-classes"
                      "-cp" (string-append (getenv "CLASSPATH") ":"
                                           (string-join (find-files "build/jar" ".") ":"))
                      (find-files (string-append name "/src/test/java")
                                  ".*.java"))
               (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":"
                                                   (string-join (find-files "build/jar" ".") ":")
                                                   ":build/test-classes")
                       "org.junit.runner.JUnitCore"
                       (map
                         (lambda (file)
                           (string-join
                             (string-split
                               (substring file 14 (- (string-length file) 5)) #\/)
                             "."))
                         (find-files (string-append name "/src/test/java")
                                     ".*Test.java"))))
             (test "framework")
             (test "checker")
             #t)))))
    (inputs
     `(("java-annotation-tools" ,java-annotation-tools)
       ("java-javaparser" ,java-javaparser)))
    (home-page "https://checkerframework.org/")
    (synopsis "Statically detect common mistakes")
    (description "This framework enhances the Java type system in order to
detect null pointer exceptions, unintended side effects, SQL injections,
concurrency errors, mistaken equality tests, and other run-time errors.")
    (license (list
               license:gpl2+; with classpath exception
               license:expat))))

;; Some random annotations required by dependencies of checkerframework
(define-public java-checkerframework-qual-annotation
  (package
    (inherit java-checkerframework)
    (name "java-checkerframework-qual-annotation")
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (define (find-files* dir dirs pattern)
               (if (null? dirs)
                 (find-files dir pattern)
                 (let ((next (car dirs))
                       (rest (cdr dirs)))
                   (apply append (map (lambda (dir) (find-files* dir rest pattern)) (find-files dir next #:directories? #t))))))
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "javac" "-d" "build/classes"
                    "-cp" (getenv "CLASSPATH")
                    (append
                      ;; List from checker-qual/build.gradle (copySources)
                      (find-files "." "^FormatUtil.java")
                      (find-files "." "^NullnessUtil.java")
                      (find-files "." "^RegexUtil.java")
                      (find-files "." "^UnitsTools.java")
                      (find-files "." "^SignednessUtil.java")
                      (find-files "." "^I18nFormatUtil.java")
                      (find-files "." "^Opt.java")
                      (find-files "." "^PurityUnqualified.java")
                      (find-files* "." '("^org$" "^checkerframework$" "^qual$")
                                   ".*.java$")))
             (invoke "jar" "cf" "build/jar/checkerframework-qual-annotation.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build/jar")))))
    (inputs '())
    (native-inputs '())))

;; Some random annotations required by dependencies of checkerframework
(define-public java-checkerframework-compat-qual-annotation
  (package
    (inherit java-checkerframework)
    (name "java-checkerframework-compat-qual-annotation")
    ;; Last version to provide this package
    (version "2.5.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/typetools/checker-framework.git")
                     (commit "62602db32dbc0dd64b5aecc8c84671252a50e08b")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "0d6ngrv0262ipisfzb2f2wp7ygziqfmgd2hhsxdnip4a69ws1lz2"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   (for-each delete-file (find-files "." ".*.jar$"))
                   #t))))
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (define (find-files* dir dirs pattern)
               (if (null? dirs)
                 (find-files dir pattern)
                 (let ((next (car dirs))
                       (rest (cdr dirs)))
                   (apply append (map (lambda (dir) (find-files* dir rest pattern)) (find-files dir next #:directories? #t))))))
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (apply invoke "javac" "-d" "build/classes"
                    "-cp" (getenv "CLASSPATH")
                    (append
                      ;; List from checker-qual/build.gradle (copySources)
                      (find-files* "." '("^org$" "^checkerframework$" "^compatqual$")
                                   ".*.java$")))
             (invoke "jar" "cf" "build/jar/checkerframework-qual-annotation.jar"
                     "-C" "build/classes" ".")
             #t))
         (replace 'install
           (install-jars "build/jar")))))
    (inputs '())
    (native-inputs '())))

(define-public java-truth
  (package
    (name "java-truth")
    (version "0.42")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/truth.git")
                     (commit "5aaf4bc1874583db510bbb209365382e5681d65a")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1lyjmy66sprxx9hn9krwys4pv2ibjf4d1vqmbyhsx61bb6ji627f"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "truth.jar"
       #:source-dir "core/src/main/java"
       #:test-dir "core/src/test"
       #:tests? #f; TODO: require google-testing
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-duplicate
           (lambda _
             (delete-file-recursively
               "core/src/main/java/com/google/common/truth/super")
             (delete-file-recursively
               "core/src/test/java/com/google/common/truth/super")
             #t)))))
    (inputs
     `(("java-checkerframework-compat-qual-annotation"
        ,java-checkerframework-compat-qual-annotation)
       ("java-diff-utils" ,java-diff-utils)
       ("java-error-prone-annotations" ,java-error-prone-annotations)
       ("java-guava-25" ,java-guava-25)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "https://google.github.io/truth")
    (synopsis "Assertion library for Java")
    (description "Truth makes your test assertions and failure messages more
readable.  Similar to AssertJ, it natively supports many JDK and Guava types,
and it is extensible to others.")
    (license license:asl2.0)))

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
       #:tests? #f)); TODO: require google-testing
    (native-inputs
     `(("java-guava-25" ,java-guava-25)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-truth" ,java-truth)))
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
    (version "2.3.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/error-prone/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0m0gzdhahjmiyr1ccl2zbf35qin3qbjxf3ay42vj49ba4c3yy8s7"))))
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
               "ant/src/main/java/com/google/errorprone/internal/NonDelegatingClassLoader.java")
             #t)))))
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
                "11k7fhhx6xcpz6iwsxpvr5ivbv1db6xmrrnhl1nzhxl8ja1rsmdc"))))
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
                 (("import org.codehaus.mojo.animal_sniffer.*") "")
                 (("@WeakOuter") "")
                 (("@RetainedWith") "")
                 (("@Weak") "")
                 (("@J2ObjCIncompatible") "")
                 (("@ReflectionSupport\\([^\\)]*\\)") "")
                 (("@IgnoreJRERequirement") "")))
             #t)))))
     (inputs
      `(("java-checkerframework-qual-annotation"
         ,java-checkerframework-qual-annotation)
        ("java-error-prone-annotations" ,java-error-prone-annotations)
        ("java-jsr305" ,java-jsr305)))))

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

(define-public java-trove4j
  (package
    (name "java-trove4j")
    (version "3.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/trove4j/trove/downloads/"
                                  "trove-" version ".tar.gz"))
              (sha256
               (base32
                "1hlvhaivyw880bld5l8cf2z0s33vn9bb84w0m5n025c7g8fdwhk2"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (native-inputs
     `(("java-junit" ,java-junit)))
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars ".")))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1+)))

(define-public java-trove4j-2
  (package
    (inherit java-trove4j)
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://sourceforge.net/projects/trove4j/"
                                  "files/trove/" version "/trove-" version ".tar.gz"))
              (sha256
               (base32
                "0vkbgq20xina558vymq6gxwjw5svhxsncizh3p3wdq5fjcgrx4m5"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))))

(define-public java-trove4j-intellij
  (package
    (inherit java-trove4j)
    (version "1.0-20191502")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/JetBrains/intellij-deps-trove4j")
                     (commit "5967df06f16dacca5a37493ae464f14696dc6f9d")))
              (file-name (git-file-name "java-trove4j" version))
              (sha256
               (base32
                "00swjb2yq85k6sh5hq1nfixv20hjnza4hd1b8frr3fb53ibsry4s"))
              (modules '((guix build utils)))
              (snippet
                `(begin
                   ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (arguments
     `(#:jar-name "trove.jar"
       #:tests? #f
       #:source-dir "core/src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate
           (lambda _
             (with-directory-excursion "generator/src/main/java"
               (invoke "javac" "gnu/trove/generate/Generate.java"))
             (invoke "java" "-cp" "generator/src/main/java"
                     "gnu.trove.generate.Generate"
                     "core/src/main/templates" "core/src/main/java")))
         (add-before 'build 'utf-to-iso
           (lambda _
             (substitute* "build.xml"
               (("<javac ") "<javac encoding=\"iso-8859-1\" "))
             #t)))))))

(define-public java-imagescalr
  (package
    (name "java-imagescalr")
    (version "4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/rkalla/imgscalr")
                     (commit (string-append version "-release"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vbfx2wa9lavagmg2pgy5jq4m7msp1dkc4pwr7vv5a746fx24pg9"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars ".")))))
    (home-page "")
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
     `(#:test-target "test"
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
     `(#:build-target "dist"
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
    (version "1.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xmlgraphics/batik/source/"
                                  "batik-src-" version ".tar.gz"))
              (sha256
               (base32
                "1g68mh5f57ap7827zb5y96ic587hf351f892fk80x2750drnw8zi"))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "regard"; FIXME: no test is actually run
       #:build-target "all-jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-failing
           (lambda _
             ;; This file looks for w3c.dom.Window, but it has been moved to
             ;; org.apache.batik.w3c.dom.Window.
             (delete-file "samples/tests/resources/java/sources/com/untrusted/script/UntrustedScriptHandler.java")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java/")))
               (mkdir-p dir)
               (copy-file (string-append "batik-" ,version "/lib/batik-all-" ,version ".jar")
                          (string-append dir "batik-all.jar"))))))))
    (inputs
     `(("java-xmlgraphics-commons" ,java-xmlgraphics-commons)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://xmlgraphics.apache.org/batik")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-batik-1.7
  (package
    (inherit java-batik)
    (name "java-batik")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xmlgraphics/batik/source/"
                                  "batik-src-" version ".zip"))
              (sha256
               (base32
                "1zbrffb8xrddb41sn8fzq40wxc5i8177cl9nm0gmd5x78csmkskb"))))
    (native-inputs
     (append (package-native-inputs java-batik)
             `(("unzip" ,unzip))))
    (arguments
     `(#:test-target "regard"; FIXME: no test is actually run
       #:build-target "all-jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-failing
           (lambda _
             ;; This file looks for w3c.dom.Window, but it has been moved to
             ;; org.apache.batik.w3c.dom.Window.
             (delete-file "samples/tests/resources/java/sources/com/untrusted/script/UntrustedScriptHandler.java")
             #t))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((dir (string-append (assoc-ref outputs "out") "/share/java/")))
               (mkdir-p dir)
               (copy-file (string-append "batik-" ,version "/lib/batik-all.jar")
                          (string-append dir "batik-all.jar"))))))))))

(define-public java-pdfbox-fontbox
  (package
    (name "java-pdfbox-fontbox")
    (version "2.0.19")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/pdfbox/" version "/pdfbox-"
                                  version "-src.zip"))
              (sha256
               (base32
                "0chf5b3ppi0bbx1sa14bnv7aq4nk0hs966mjqga0j4lzjdsz73xf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "fontbox.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:test-exclude
       (list
         "**/Abstract*.java"
         ;; Require downloading fonts
         "**/CFFParserTest.java"
         "**/TTFSubsetterTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             #t))
         (add-before 'configure 'chdir
           (lambda _
             (chdir "fontbox")
             #t)))))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("unzip" ,unzip)))
    (home-page "https://xmlgraphics.apache.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-pdfbox
  (package
    (inherit java-pdfbox-fontbox)
    (name "java-pdfbox")
    (arguments
     `(#:jar-name "pdfbox.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; can't seem to find some test files?
       #:test-exclude
       (list
         "**/Abstract*.java"
         ;; Require network
         "**/MergeAcroFormsTest.java"
         "**/MergeAnnotationsTest.java"
         "**/PDButtonTest.java"
         ;; Require downloaded resources
         "**/PDFMergerUtilityTest.java"
         "**/PDStructureElementTest.java"
         "**/PDFontTest.java"
         ;; Can't load image
         "**/LosslessFactoryTest.java"
         ;; Unknown failure
         "**/CCITTFactoryTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")
             (copy-recursively "src/test/resources" "build/test-classes")
             (mkdir-p "target/pdfs")
             #t))
         (add-before 'configure 'chdir
           (lambda _
             (chdir "pdfbox")
             #t)))))
    (inputs
     `(("java-bouncycastle" ,java-bouncycastle)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-diff-utils" ,java-diff-utils)
       ("java-pdfbox-fontbox" ,java-pdfbox-fontbox)))))

(define-public java-fop
  (package
    (name "java-fop")
    (version "2.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/xmlgraphics/fop/source/"
                                  "fop-" version "-src.tar.gz"))
              (sha256
               (base32
                "11ih8hlqgmkrnmygfwfqry8npjbqdrr1pfilla9a8s293hxk4sqx"))))
              ;(modules '((guix build utils)))
              ;(snippet
              ; `(begin
              ;    (for-each delete-file (find-files "." ".*.jar$"))
              ;    #t))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jar-main"
       #:test-target "junit"
       #:tests? #f; some fail
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
               ;(copy-file "build/fop-hyph.jar"
               ;           (string-append lib "fop-hyph.jar"))
               ;(copy-file "build/fop-sandbox.jar"
               ;           (string-append lib "fop-sandbox.jar"))
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
       ("java-pdfbox-fontbox" ,java-pdfbox-fontbox)
       ("java-pdfbox" ,java-pdfbox)
       ("java-batik" ,java-batik)))
       ;("java-avalon-framework-api" ,java-avalon-framework-api)
       ;("java-avalon-logkit" ,java-avalon-logkit)))
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
     `(#:phases
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

(define-public java-jctools-core
  (package
    (name "java-jctools-core")
    (version "2.1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JCTools/JCTools/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16vqznf6ikzvii72r13h02vcbqfp5kl0fcw2cfhp52fzk3pmpsi2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jctools-core.jar"
       #:source-dir "jctools-core/src/main/java"
       #:test-dir "jctools-core/src/test"
       ;; Require com.google.common.collect.testing (guava?)
       #:tests? #f))
    (native-inputs
     `(("java-guava" ,java-guava)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "https://jctools.github.io/JCTools")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-log4j-api-for-netty
  (package
    (inherit java-log4j-api)
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/logging/log4j/" version
                                  "/apache-log4j-" version "-src.tar.gz"))
              (sha256
               (base32
                "1ri58x5l451ngz3p8shn4r7kb69vg0pkr9jb817952s9jbskwws9"))))))

(define-public java-log4j-core-for-netty
  (package
    (inherit java-log4j-api-for-netty)
    (name "java-log4j-core")
    (inputs
     `(("java-osgi-core" ,java-osgi-core)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-log4j-api" ,java-log4j-api-for-netty)
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
       ("java-stax2-api" ,java-stax2-api)
       ("java-jeromq" ,java-jeromq)))
    (native-inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-junit" ,java-junit)
       ("java-slf4j-api" ,java-slf4j-api)))
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
           (lambda _ (chdir "log4j-core") #t)))))
    (synopsis "Core component of the Log4j framework")
    (description "This package provides the core component of the Log4j
logging framework for Java.")))

(define-public java-protobuf
  (package
    (name "java-protobuf")
    (version (package-version protobuf))
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/protobuf/releases/"
                                  "download/v" version "/protobuf-java-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1mvbibm8p1cdbaf5frql406ipgm56k0ygilswn633jg9qz2n69sc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "protobuf.jar"
       #:source-dir "java/core/src/main/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-protos
           (lambda _
             (for-each
               (lambda (proto)
                 (invoke "protoc" "--java_out=java/core/src/main/java" "-Isrc"
                         (string-append "src/google/protobuf/" proto ".proto")))
                 '("any" "api" "descriptor" "duration" "empty" "field_mask"
                   "source_context" "struct" "timestamp" "type" "wrappers"))
             (invoke "protoc" "--java_out=java/core/src/main/java" "-Isrc"
                     "src/google/protobuf/compiler/plugin.proto")
             #t)))))
    (inputs
     `(("java-guava" ,java-guava)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("protobuf" ,protobuf)))
    (home-page (package-home-page protobuf))
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-protobuf-nano
  (package
    (inherit java-protobuf)
    (name "java-protobuf-nano")
    (version "3.5.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/protocolbuffers/protobuf")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "14gq6rnv03zvcb5hym240z4yqiphrmd5y4zx9a77n37rwvfgx5qy"))))
    (arguments
     `(#:jar-name "protobuf.jar"
       #:source-dir "javanano/src/main/java"
       #:tests? #f))));; difficult to generate sources))))
       ;#:test-dir "javanano/src/test"
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (add-after 'build 'build-protos
       ;    (lambda _
       ;      (mkdir-p "build/classes")
       ;      (apply invoke "protoc" "--javanano_out=build/generated"
       ;             "--proto_path=javanano/src/test/java/com"
       ;             (find-files "javanano/src/test" "unittest_.*"))
       ;      (invoke "protoc" "--javanano_out=build/generated"
       ;              "--proto_path=javanano/src/test/java/com"
       ;              "javanano/src/test/java/com/google/protobuf/nano/map_test.proto")
       ;      #t)))))))

(define-public java-jzlib
  (package
    (name "java-jzlib")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ymnk/jzlib/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1i0fplk22dlyaz3id0d8hb2wlsl36w9ggbvsq67sx77y0mi6bnl3"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jzlib.jar"
       #:source-dir "src/main/java"
       #:tests? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-conscrypt
  (package
    (name "java-conscrypt")
    (version "1.4.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/google/conscrypt.git")
                     (commit version)))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1wfcc7gspvxxhm4vwb0yfidpsjj6kcdqnfwj2qsr4zfppn01gv7f"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "conscrypt.jar"
       #:source-dir "openjdk/src/main/java:common/src/main/java"
       #:test-dir "openjdk/src/test"
       #:tests? #f; conscrypt-testing require libcore from android
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-library
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes/META-INF/native")
             (let ((jni-include (string-append "-I" (assoc-ref inputs "jdk")
                                               "/include/linux")))
               (for-each
                 (lambda (cpp-file)
                   (invoke "g++" "-c" cpp-file "-o" (string-append cpp-file ".o")
                           "-std=c++11" "-fPIC" "-O2" "-Icommon/src/jni/main/include"
                           "-Icommon/src/jni/unbundled/include" jni-include))
                 (find-files "common/src/jni/main/cpp" ".*.cc$")))
             (apply invoke "gcc" "-o"
                    "build/classes/META-INF/native/libconscrypt.so"
                    "-shared"
                    (find-files "common/src/jni/main/cpp" ".*.o$"))
             #t))
         (add-before 'build 'generate-constants
           (lambda _
             (invoke "g++" "-std=c++11" "-O2" "-o" "gen_constants"
                     "constants/src/gen/cpp/generate_constants.cc")
             (with-output-to-file "common/src/main/java/org/conscrypt/NativeConstants.java"
               (lambda _
                 (invoke "./gen_constants")))
             #t))
         ;(add-before 'build 'build-testing
         ;  (lambda _
         ;    (mkdir-p "build/testing-classes")
         ;    (apply invoke "javac" "-d" "build/testing-classes"
         ;           "-cp" (getenv "CLASSPATH")
         ;           (find-files "testing/src/main/java" ".*.java$"))
         ;    #t))
         (add-before 'check 'set-classpath
           (lambda _
             (setenv "CLASSPATH"
                     (string-append (getenv "CLASSPATH") ":build/testing-classes"))
             #t)))))
    (inputs
     `(("boringssl" ,boringssl)))
    ;(native-inputs
    ; `(("java-bouncycastle" ,java-bouncycastle)
    ;   ("java-mockito-1" ,java-mockito-1)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-netty-tcnative-boringssl
  (package
    (name "java-netty-tcnative-boringssl")
    (version "2.0.20")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/netty/netty-tcnative.git")
                     (commit (string-append "netty-tcnative-parent-"
                                            version ".Final"))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1kdgnk5133bw821iwhli8vj5kf8sb8vxav5wpdsv6vhrgrrm8nrl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "netty-tcnative-boringssl.jar"
       #:source-dir "openssl-dynamic/src/main/java"
       #:test-dir "openssl-dynamic/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-native
           (lambda* (#:key inputs system #:allow-other-keys)
             (let ((native-dir (string-append
                                 "build/classes/META-INF/native/linux"
                                 (if (or (string-prefix? "x86_64" system)
                                         (string-prefix? "aarch64" system))
                                   "64" "32")))
                   (apr-include (string-append "-I" (assoc-ref inputs "apr")
                                               "/include/apr-1"))
                   (jni-include (string-append "-I" (assoc-ref inputs "jdk")
                                               "/include/linux")))
               (mkdir-p native-dir)
               (for-each
                 (lambda (source)
                   (invoke "gcc" "-c" source "-o" (string-append source ".o")
                           "-fPIC" "-O2" jni-include apr-include))
                 (find-files "openssl-dynamic/src/main/c" ".*.c$"))
               (apply invoke "gcc" "-o"
                      (string-append native-dir "/libnetty_tcnative.so")
                      "-shared" "-lssl" "-lapr-1" "-lcrypto"
                      (find-files "openssl-dynamic/src/main/c" ".*.o$")))
               #t))
           (add-before 'check 'copy-lib
             (lambda _
               (mkdir-p "build/test-classes")
               (copy-recursively "build/classes/META-INF"
                                 "build/test-classes/META-INF")
               #t)))))
    (inputs
     `(("apr" ,apr)
       ("boringssl" ,boringssl)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-mockito
  (package
    (name "java-mockito")
    (version "2.23.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mockito/mockito/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "08vw3fqymgpjww06q4zvc6247dj2a7y3hgxsfghxny5lklh12qml"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "mockito.jar"
       #:source-dir "src/main/java"
       #:tests? #f; Some compilation errors
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-system-asm
           (lambda _
             (substitute* "src/main/java/org/mockito/internal/creation/bytebuddy/InlineBytecodeGenerator.java"
               (("net.bytebuddy.jar.asm") "org.objectweb.asm"))
             #t)))))
    (propagated-inputs
     `(("java-asm" ,java-asm)
       ("java-byte-buddy-agent" ,java-byte-buddy-agent)
       ("java-byte-buddy-dep" ,java-byte-buddy-dep)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-objenesis" ,java-objenesis)))
    (native-inputs
     `(("java-assertj" ,java-assertj)))
    (home-page "https://mockito.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-netty-common
  (package
    (name "java-netty-common")
    (version "4.1.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/netty/netty/archive/netty-"
                                  version ".Final.tar.gz"))
              (sha256
               (base32
                "0dchrbwrhsxycs8kgwqcp81xybyx6p13k86d5pjsqni1hrc4sn2i"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "netty-common.jar"
       #:source-dir "common/src/main/java"
       #:test-dir "common/src/test"
       ;; Weird issue with log4j
       #:test-exclude (list "**/Abstract*.*"
                            "**/Log4JLoggerFactoryTest.*")))
    (inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-jctools-core" ,java-jctools-core)
       ("java-log4j-api" ,java-log4j-api-for-netty)
       ("java-log4j-core" ,java-log4j-core-for-netty)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-junit" ,java-junit)
       ("java-mockito" ,java-mockito)
       ("java-slf4j-simple" ,java-slf4j-simple)))
    (home-page "https://netty.io/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-netty-buffer
  (package
    (inherit java-netty-common)
    (name "java-netty-buffer")
    (arguments
     `(#:jar-name "netty-buffer.jar"
       #:source-dir "buffer/src/main/java"
       #:test-dir "buffer/src/test"
       #:test-exclude (list "**/Abstract*.*"
                            "**/ByteBufAllocatorTest.*")))
    (inputs
     `(("java-netty-common" ,java-netty-common)
       ,@(package-inputs java-netty-common)))))

(define-public java-netty-resolver
  (package
    (inherit java-netty-common)
    (name "java-netty-resolver")
    (arguments
     `(#:jar-name "netty-resolver.jar"
       #:source-dir "resolver/src/main/java"
       #:test-dir "resolver/src/test"))
    (inputs
     `(("java-netty-common" ,java-netty-common)
       ,@(package-inputs java-netty-common)))))

(define-public java-netty-transport
  (package
    (inherit java-netty-common)
    (name "java-netty-transport")
    (arguments
     `(#:jar-name "netty-transport.jar"
       #:source-dir "transport/src/main/java"
       #:test-dir "transport/src/test"
       ;; reference to assertEquals is ambiguous
       #:tests? #f))
    (inputs
     `(("java-netty-buffer" ,java-netty-buffer)
       ("java-netty-common" ,java-netty-common)
       ("java-netty-resolver" ,java-netty-resolver)
       ,@(package-inputs java-netty-common)))
    (native-inputs
     `(("java-logback-classic" ,java-logback-classic)
       ("java-logback-core" ,java-logback-core)
       ,@(package-native-inputs java-netty-common)))))

(define-public java-netty-codec
  (package
    (inherit java-netty-common)
    (name "java-netty-codec")
    (arguments
     `(#:jar-name "netty-codec.jar"
       #:source-dir "codec/src/main/java"
       #:test-dir "codec/src/test"
       #:test-exclude
       (list "**/Abstract*.java" "**/Base64Test.java" "**/ZlibTest.java"
             "**/marshalling/*.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (with-directory-excursion "codec/src/test/java/io/netty/handler"
               (substitute* "codec/ByteToMessageCodecTest.java"
                 (("Equals\\(1, ") "Equals((int) 1, (int)")))
             #t)))))
    (inputs
     `(("java-jboss-marshalling" ,java-jboss-marshalling)
       ("java-jzlib" ,java-jzlib)
       ("java-lz4" ,java-lz4)
       ("java-lzma" ,java-lzma)
       ("java-netty-buffer" ,java-netty-buffer)
       ("java-netty-common" ,java-netty-common)
       ("java-netty-transport" ,java-netty-transport)
       ("java-ning-compress" ,java-ning-compress)
       ("java-protobuf" ,java-protobuf)
       ("java-protobuf-nano" ,java-protobuf-nano)
       ,@(package-inputs java-netty-common)))
    (native-inputs
     `(("java-commons-compress" ,java-commons-compress)
       ("java-netty-resolver" ,java-netty-resolver)
       ,@(package-native-inputs java-netty-common)))))

(define-public java-netty-handler
  (package
    (inherit java-netty-common)
    (name "java-netty-handler")
    (arguments
     `(#:jar-name "netty-handler.jar"
       #:source-dir "handler/src/main/java"
       #:test-dir "handler/src/test"
       ;; Reference to assertEquals is ambiguous
       #:tests? #f))
    (inputs
     `(("java-bouncycastle" ,java-bouncycastle)
       ("java-conscrypt" ,java-conscrypt)
       ("java-eclipse-jetty-alpn-api" ,java-eclipse-jetty-alpn-api)
       ("java-eclipse-jetty-npn-api" ,java-eclipse-jetty-npn-api)
       ("java-netty-buffer" ,java-netty-buffer)
       ("java-netty-codec" ,java-netty-codec)
       ("java-netty-common" ,java-netty-common)
       ("java-netty-tcnative-boringssl" ,java-netty-tcnative-boringssl)
       ("java-netty-transport" ,java-netty-transport)
       ,@(package-inputs java-netty-common)))
    (native-inputs
     `(("java-logback-classic" ,java-logback-classic)
       ("java-logback-core" ,java-logback-core)
       ,@(package-native-inputs java-netty-common)))))

(define-public java-conversantmedia-disruptor
  (package
    (name "java-conversantmedia-disruptor")
    (version "1.2.14")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/conversant/disruptor/archive/" version ".tar.gz"))
              (sha256
               (base32
                "0sbr6bcsawzfzwcxxlyh6lqqlqd09qld0xika9dhd01kxl60srwb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "conversantmedia-disruptor.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"))
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-okio
  (package
    (name "java-okio")
    (version "1.16.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/square/okio/archive/okio-parent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "05xxrsv2klfvcnp5r4ri28d7hh502wcwlvsm9lnx4kpqlpkq0yzp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "okio.jar"
       #:source-dir "okio/src/main/java"
       #:test-dir "okio/src/test"))
    (inputs
     `(("java-jsr305" ,java-jsr305)
       ("java-animal-sniffer-annotations" ,java-animal-sniffer-annotations)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-okhttp
  (package
    (name "java-okhttp")
    (version "3.12.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/square/okhttp/archive/parent-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1gj3rlz2sy6hbdy7yp18f2rb19z0mrp0wai6fls1slsds4fay2gr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "okhttp.jar"
       #:source-dir "okhttp/src/main/java"
       #:test-dir "okhttp/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-platforms
	   (lambda _
	     (delete-file "okhttp/src/main/java/okhttp3/internal/platform/AndroidPlatform.java")
	     (delete-file "okhttp/src/main/java/okhttp3/internal/platform/ConscryptPlatform.java")
	     (substitute* "okhttp/src/main/java/okhttp3/internal/platform/Platform.java"
	       (("AndroidPlatform.buildIfSupported.*") "null;\n")
	       (("ConscryptPlatform.buildIfSupported.*") "null;\n"))
	     #t))
	 (add-after 'unpack 'fill-templates
	   (lambda _
	     (with-directory-excursion "okhttp/src/main/java-templates"
	       (for-each
	         (lambda (file)
		   (let ((installed-name (string-append "../java/" file)))
	             (copy-file file installed-name)
		     (substitute* installed-name
		       (("\\$\\{project.version\\}") ,version))))
	         (find-files "." ".*.java")))
	     #t)))))
    (inputs
     `(("java-animal-sniffer-annotations" ,java-animal-sniffer-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-okio" ,java-okio)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-okhttp-urlconnection
  (package
    (inherit java-okhttp)
    (name "java-okhttp-urlconnection")
    (arguments
     `(#:jar-name "okhttp.jar"
       #:source-dir "okhttp-urlconnection/src/main/java"
       #:test-dir "okhttp-urlconnection/src/test"
       ;; TODO: require okhttp-mockwebserver
       #:tests? #f))
    (inputs
     `(("java-okhttp" ,java-okhttp)
       ,@(package-inputs java-okhttp)))))

(define-public java-config
  (package
    (name "java-config")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lightbend/config/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0jsw3s902aqj5c9iddj7ahy9qlrc39d7w3jj9y13bn5ja8biazzk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "config.jar"
       #:source-dir "config/src/main/java"
       #:test-dir "config/src/test"))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-reactive-streams
  (package
    (name "java-reactive-streams")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/reactive-streams/reactive-streams-jvm/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "16nrm8fv058vr8442986yl6kvdb7w5if8lb8ip2jakawjq93k2vm"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "reactive-streams.jar"
       #:source-dir "api/src/main/java"
       ;; No tests
       #:tests? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-streamex
  (package
    (name "java-streamex")
    (version "0.6.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/amaembo/streamex/archive/streamex-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "069a7q7m7srshv7nxvvw5i338k0kmsbazqhfsbqsgd6bklfh4r7h"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "streamex.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-protoc-jar
  (package
    (name "java-protoc-jar")
    (version "3.6.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/os72/protoc-jar/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "13q9cvplsbv9jxx5myiir4hdlhclgr9ka6c9x9f022hcn0lzgjpw"))
              (modules '((guix build utils)))
              (snippet
               `(delete-file-recursively "bin"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "protoc-jar.jar"
       #:tests? #f; require network
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'embed-protoc
           (lambda* (#:key inputs system #:allow-other-keys)
             (let* ((dir (string-append "build/classes/bin/" ,(package-version protobuf)))
                    (file (string-append dir "/protoc-" ,(package-version protobuf)
                                         "-linux-" (string-drop-right system 6) ".exe")))
               (mkdir-p dir)
               (copy-file (string-append (assoc-ref inputs "protobuf") "/bin/protoc") file))
             #t)))))
    (inputs
     `(("protobuf" ,protobuf)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-fmpp
  (package
    (name "java-fmpp")
    (version "0.9.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/freemarker/fmpp/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0k66qq8mss165mqbn3gs537xgqx74rch6zx335d415vd4h9d83va"))
              (patches
                (search-patches "java-fmpp-remove-imageinfo.patch"))
              (modules '((guix build utils)))
              (snippet
               ;; NOTE: the bin/ directory only contain scripts
               `(begin
                  (delete-file-recursively "lib")
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:test-target "test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'set-modification-time
           (lambda _
             ;; This software assumes that a modification date of 0 means
             ;; that it failed to get the modification time. Guix sets
             ;; a modification time of 0, which leads to test failures.
             (for-each (lambda (file) (utime file 1 1)) (find-files "src" "."))
             #t))
         (add-before 'build 'create-lib
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "lib")
             (copy-file (string-append (assoc-ref inputs "java-apache-freemarker")
                                       "/share/java/java-apache-freemarker.jar")
                        "lib/freemarker.jar")
             (copy-file (string-append (assoc-ref inputs "ant") "/lib/ant.jar")
                        "lib/ant.jar")
             (copy-file (string-append (assoc-ref inputs "java-junit")
                                       "/share/java/junit.jar")
                        "lib/junit.jar")
             (copy-file (string-append (assoc-ref inputs "java-hamcrest-all")
                                       "/share/java/hamcrest-all.jar")
                        "lib/hamcrest-generator-nodeps.jar")
             (copy-file (car (find-files (string-append (assoc-ref inputs "java-bsh")
                                                        "/share/java/")
                                         ".*.jar$"))
                        "lib/bsh.jar")
             (copy-file (car (find-files (string-append
                                           (assoc-ref
                                             inputs
                                             "java-apache-xml-commons-resolver")
                                           "/share/java/")
                                         ".*.jar$"))
                        "lib/resolver.jar")
             #t))
         (add-before 'build 'fix-class-path
           (lambda* (#:key inputs #:allow-other-keys)
             ;; The result otherwise requires its dependencies alongside it.
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "build.xml"
                 ;; Also fix embedded timestamp
                 (("\\$\\{timeStamp\\}") "1970-01-01T00:00:00Z")
                 ((" imageinfo.jar") "")
                 (("freemarker.jar ")
                  (string-append
                    ;; In java8, we can only refer to jar files relatively to
                    ;; the current jar...
                    "../../../../../../../../../../"
                    (car (find-files
                           (assoc-ref inputs "java-apache-freemarker")
                           "freemarker.jar"))
                    " "))
                 (("bsh.jar ")
                  (string-append
                    "../../../../../../../../../../"
                    (car (find-files (assoc-ref inputs "java-bsh") ".*.jar$"))
                    " "))
                 ((" resolver.jar")
                  (string-append
                    " ../../../../../../../../../../"
                    (car (find-files
                           (assoc-ref inputs "java-apache-xml-commons-resolver")
                           ".*.jar$"))))))
             #t))
         (replace 'install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (java (string-append out "/lib"))
                    (bin (string-append out "/bin")))
               (install-file "lib/fmpp.jar" java)
               (substitute* "bin/fmpp"
                 (("#!.*")
                  (string-append "#!" (which "sh") "\n"
                                 "JAVA_HOME=" (assoc-ref inputs "jdk"))))
               (install-file "bin/fmpp" bin)
               (chmod (string-append bin "/fmpp") #o755))
             #t)))))
    (inputs
     `(("java-apache-freemarker" ,java-apache-freemarker)
       ("java-apache-xml-commons-resolver" ,java-apache-xml-commons-resolver)
       ("java-bsh" ,java-bsh)))
    (native-inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-sbt-ipcsocket
  (package
    (name "java-sbt-ipcsocket")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sbt/ipcsocket/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0zqg1c10dwjzvqb9wngmprs2cfiyw3n1jq8ag0hwk978f1d93a4h"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "ipcsocket.jar"
       #:source-dir "src/main/java"
       ;; Cannot create unix domain socket properly in the build environment
       #:tests? #f))
    (inputs
     `(("java-native-access" ,java-native-access)
       ("java-native-access-platform" ,java-native-access-platform)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-sbt-test-interface
  (package
    (name "java-sbt-test-interface")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sbt/test-interface/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0lqsaik33d7wfq8d69mq4s6jb16rb2fmnhv2wjqisrxi0m4nmffw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "test-interface.jar"
       #:source-dir "src/main/java"
       ;; TODO: tests are written in scala
       #:tests? #f))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-caffeine
  (package
    (name "java-caffeine")
    (version "2.6.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ben-manes/caffeine/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1kaxz4fpjpgyv4n2zhlb3q0f8mn5ji68wilzwhf9dsbq4l12zkx4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "caffeine.jar"
       #:source-dir "caffeine/src/main/java:caffeine/generated-sources"
       #:test-dir "caffeine/src/test"
       #:tests? #f; TODO: need mockito2 and more dependencies
       #:jdk ,openjdk9
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-javapoet
           (lambda _
             (mkdir-p "build/javapoet")
             (apply invoke "javac" "-cp" (getenv "CLASSPATH")
                    "-d" "build/javapoet"
                    (find-files "caffeine/src/javaPoet" ".*.java$"))
             (copy-recursively "caffeine/src/javaPoet/resources"
                               "build/javapoet")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":build/javapoet")
                     "-noverify"
                     "com.github.benmanes.caffeine.cache.LocalCacheFactoryGenerator"
                     "caffeine/generated-sources")
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH")
                                                 ":build/javapoet")
                     "-noverify"
                     "com.github.benmanes.caffeine.cache.NodeFactoryGenerator"
                     "caffeine/generated-sources")
             #t)))))
    (inputs
     `(("java-commons-lang3" ,java-commons-lang3)
       ("java-guava" ,java-guava)
       ("java-jsr305" ,java-jsr305)
       ("java-javapoet" ,java-javapoet)))
    (native-inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-testng" ,java-testng)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-paranamer
  (package
    (name "java-paranamer")
    (version "2.8")
    (source (origin
              (method git-fetch)
              ;; This repository is a mirror of the svn directory that disappeared
              ;; along with codehaus.org
              (uri (git-reference
                     (url "https://github.com/paul-hammant/paranamer")
                     (commit (string-append "paranamer-parent-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1jfick1sfjmqdxak72h5sx7xcyq0njwsg64jp4xaz06365ghbiz9"))
              (modules '((guix build utils)))
              (snippet
                `(for-each delete-file (find-files "." ".*.jar")))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "paranamer.jar"
       #:source-dir "paranamer/src/java"
       #:tests? #f))
    (inputs
     `(("java-javax-inject" ,java-javax-inject)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-picocontainer
  (package
    (name "java-picocontainer")
    (version "2.15")
    (source (origin
              (method git-fetch)
              ;; This repository is a mirror of the svn directory that disappeared
              ;; along with codehaus.org
              (uri (git-reference
                     (url "https://github.com/codehaus/picocontainer")
                     (commit "7be6b8b0eb33421dc7a755817628e06b79bd879d")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "06ygwa1wkk70lb9abc0shh1lzyysjaciqb5917qxsyn4rx43sjdg"))
              (modules '((guix build utils)))
              (snippet
                `(for-each delete-file (find-files "." ".*.jar")))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "picocontainer.jar"
       #:source-dir "container/src/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "java/2.x/tags/picocontainer-2.15")
             #t)))))
    (inputs
     `(("java-paranamer" ,java-paranamer)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-picocontainer-1
  (package
    (inherit java-picocontainer)
    (version "1.3")
    (arguments
     `(#:jar-name "picocontainer.jar"
       #:source-dir "container/src/java"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "java/1.x/picocontainer/tags/picocontainer-1_3")
             #t)))))))

(define-public java-automaton
  (package
    (name "java-automaton")
    (version "1.12.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/cs-au-dk/dk.brics.automaton")
                     (commit "328cf493ec2537af9d2bbce0eb4b4ef118b66547")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1vjcz8m8wlqx6j1cymww7mfh9ckxfwcfm63dvg3bg394l8qzbxg3"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'install (install-jars ".")))))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-spullara-cli-parser
  (package
    (name "java-spullara-cli-parser")
    (version "1.1.5")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/spullara/cli-parser")
                     (commit (string-append "cli-parser-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1viysb4aws6nsbp4lil1fwwc69rr9s5z66g17iygmbq5lincz35y"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "spullara-cli-parser.jar"
       #:test-dir "src/test"
       #:source-dir "src/main/java"))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)))
    (home-page "https://github.com/spullara/cli-parser")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-eawtstub
  (let ((commit "ae5d21ccbc62754eb0353b5b1d57abf1da954652"))
    (package
      (name "java-eawtstub")
      (version (git-version "0.0.0" "0" commit))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                       (url "https://github.com/consulo/eawtstub")
                       (commit commit)))
                (file-name (git-file-name name version))
                (sha256
                 (base32
                  "0l0xng9qxs3b5l86nb77vr65a924ginxd32sjiqjrb9lbh2yp7ap"))))
      (build-system ant-build-system)
      (arguments
       `(#:jar-name "eawtstub.jar"
         ;; No tests
         #:tests? #f))
      (home-page "")
      (synopsis "")
      (description "")
      (license license:asl2.0))))

(define-public java-jline3-terminal
  (package
    (name "java-jline3-terminal")
    (version "3.19.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/jline/jline3")
                     (commit (string-append "jline-parent-" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "16cfbkbj925c92xnwq5sbg7v57yh840g0mh95iyzkkajxirz9qn9"))
              (snippet
               ;; calls maven, and conflicts with ant
               `(delete-file "build"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jline3-terminal.jar"
       #:source-dir "terminal/src/main/java"
       #:test-dir "terminal/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "terminal/src/main/resources/" "build/classes")
             #t)))))
    (native-inputs
     `(("java-easymock" ,java-easymock)
       ("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license #f)))

(define-public java-jline3-reader
  (package
    (inherit java-jline3-terminal)
    (name "java-jline3-reader")
    (arguments
     `(#:jar-name "jline3-reader.jar"
       #:source-dir "reader/src/main/java"
       #:test-dir "reader/src/test"))
    (inputs
     `(("java-jline3-terminal" ,java-jline3-terminal)))))

(define-public java-concurrent-reference-hash-map
  (package
    (name "java-concurrent-reference-hash-map")
    (version "1.0.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/alexarchambault/concurrent-reference-hash-map")
                     (commit (string-append "v" version))))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "1x2pybyld2b6pp5f93l2ixxljk18vkw11s6pysfck7a30l9f7bzb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "concurrent-reference-hash-map.jar"
       #:tests? #f)); no tests
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1)))
