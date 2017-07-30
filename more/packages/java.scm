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
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java))

(define-public java-tomcat
  (package
    (name "java-tomcat")
    (version "8.5.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.nextag.com/apache/tomcat/tomcat-8/v"
                                  version "/src/apache-tomcat-" version "-src.tar.gz"))
              (sha256
               (base32
                "08vc859z9f0787nhikbdsj6i441d4qk5xv17c1564hxxg6bmilqd"))))
    (build-system ant-build-system)
    (inputs
     `(("java-eclipse-jdt-core" ,java-eclipse-jdt-core)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (arguments
     `(#:build-target "package"
       ;#:test-target "test"
       #:tests? #f; requires downloading some files.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'properties
           (lambda _
             (mkdir "downloads")
             (substitute* "build.xml"
               (("download-compile,") ""))
             (with-output-to-file "build.properties"
               (lambda _
                 (display
                   (string-append "base.path=" (getcwd) "/downloads\n"))))))
         (replace 'install
           (install-jars "output/build/lib")))))
    (home-page "https://tomcat.apache.org")
    (synopsis "Java Servlet, JavaServer Pages, Java Expression Language and Java
WebSocket")
    (description "Apache Tomcat is an open source implementation of the Java
Servlet, JavaServer Pages, Java Expression Language and Java WebSocket
technologies. The Java Servlet, JavaServer Pages, Java Expression Language and
Java WebSocket specifications are developed under the Java Community Process.")
    (license license:asl2.0)))

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
     `(("junit" ,java-junit)))
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
     `(("dec" ,java-brotli-dec)
       ,@(package-inputs java-commons-compress)))))

(define-public java-jmapviewer
  (package
    (name "java-jmapviewer")
    (version "2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://svn.openstreetmap.org/applications/viewer/jmapviewer/releases/"
                                  version "/JMapViewer-" version "-Source.zip"))
              (sha256
               (base32
                "00jxqc4fzy7hpdi0007f0a84aa630brvam7vxqirdk9j4za4p0d8"))))
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

(define-public java-josm
  (package
    (name "java-josm")
    (version "12450")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstreetmap/josm.git")
                    (commit "61a9c27e59976805b7cce4fae5a48c8e04b19373")))
              ;;(uri (svn-reference
              ;;      (url "https://svn.openstreetmap.org/applications/editors/josm")
              ;;      (revision 12039)))
              (sha256
               (base32
                "1hab27b8s6h2b1g07pghr4nkx6rx2gx75488rwppdfc0ymzpjzrp"))
              (file-name (string-append name "-" version))))
    (build-system ant-build-system)
    (native-inputs
     `(("java-javacc" ,java-javacc)))
    (propagated-inputs
     `(("java-jmapviewer" ,java-jmapviewer)
       ("java-tomcat" ,java-tomcat)
       ("java-brotli-dec" ,java-brotli-dec)
       ("java-xz" ,java-xz)
       ("java-velocity" ,java-velocity)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-compress" ,java-commons-compress-latest)))
    (arguments
     `(;#:build-target "dist"
       #:tests? #f
       #:jdk ,icedtea-8
       #:jar-name "josm.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'rm-build.xml
           (lambda* _
             (delete-file "build.xml")))
         (add-before 'build 'fix-revision
           (lambda* _
             (with-output-to-file "REVISION.XML"
               (lambda _
                 (display
                   (string-append "<info><entry><commit revision=\"" ,version "\">"
                                  "<date>1970-01-01 00:00:00 +0000</date>"
                                  "</commit></entry></info>"))))))
         (add-before 'build 'generate-parser
           (lambda* _
             (let* ((dir "src/org/openstreetmap/josm/gui/mappaint/mapcss")
                    (out (string-append dir "/parsergen"))
                    (file (string-append dir "/MapCSSParser.jj")))
               (mkdir-p "src/org/openstreetmap/josm/gui/mappaint/mapcss/parsergen")
               (zero? (system* "javacc" "-DEBUG_PARSER=false"
                               "-DEBUG_TOKEN_MANAGER=false" "-JDK_VERSION=1.8"
                               "-GRAMMAR_ENCODING=UTF-8"
                               (string-append "-OUTPUT_DIRECTORY=" out)
                               file)))))
)))
         ;(add-before 'build 'fix-compiler
         ;  (lambda* _
         ;    (substitute* "build.xml"
         ;      (("UNKNOWN") ,version)
         ;      (("<touch.*epsg.output.*") "<mkdir dir=\"${epsg.output}/..\" /><touch file=\"${epsg.output}\"/>\n")
         ;      ((".*com.google.errorprone.ErrorProneAntCompilerAdapter.*") "")
         ;      (("compiler=\"[^\"]*\" ") ""))))
         ;(replace 'install
         ;  (lambda* (#:key outputs inputs #:allow-other-keys)
         ;    (let* ((out (assoc-ref outputs "out"))
         ;           (bin (string-append out "/bin"))
         ;           (lib (string-append out "/lib/josm")))
         ;      (mkdir-p bin)
         ;      (mkdir-p lib)
         ;      (copy-file "dist/josm-custom.jar"
         ;                 (string-append lib "/josm.jar"))
         ;      (with-output-to-file (string-append bin "/josm")
         ;        (lambda _
         ;          (display
         ;            (string-append "#!/bin/sh\n"
         ;                           (assoc-ref inputs "jdk") "/bin/java"
         ;                           " -jar " lib "/josm.jar"))))
         ;      (chmod (string-append bin "/josm") #o755)))))))
    (home-page "https://josm.openstreetmap.de")
    (synopsis "OSM editor")
    (description "OSM editor.")
    (license license:gpl2+)))

(define-public java-commons-collections
  (package
    (inherit java-commons-collections4)
    (name "java-commons-collections")
    (version "3.2.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/collections/source/"
                                  "commons-collections-" version "-src.tar.gz"))
              (sha256
               (base32
                "055r51a5lfc3z7rkxnxmnn1npvkvda7636hjpm4qk7cnfzz98387"))))
    (arguments
      (substitute-keyword-arguments (package-arguments java-commons-collections4)
        ((#:phases phases)
          `(modify-phases ,phases
            (add-before 'build 'add-manifest
              (lambda _
                (mkdir-p "build/conf")
                (call-with-output-file "build/conf/MANIFEST.MF"
                  (lambda (file)
                    (format file "Manifest-Version: 1.0\n")
                    (format file "Ant-Version: Apache Ant 1.9.9\n")
                    (format file "Created-By: 1.8.0_131-b11 (Oracle Corporation)")))))
            (replace 'install
              (install-jars "build"))))))))

(define-public java-jdom2
  (package
    (name "java-jdom")
    (version "2.0.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/hunterhacker/jdom/archive/JDOM-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0p8n7inqq2a25wk9ljinl3ixlx1x2la9qaman8ngd75xxjb02yc1"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f; junit run with "package" target
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description " Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-jakarta-oro
  (package
    (name "java-jakarta-oro")
    (version "2.0.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/jakarta/oro/"
                                  "jakarta-oro-" version ".tar.gz"))
              (sha256
               (base32
                "0rpmnsskiwmsy8r0sckz5n5dbvh3vkxx8hpm177c754r8xy3qksc"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f; junit run with "package" target
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'remove-bin
           (lambda _
             (delete-file (string-append "jakarta-oro-" ,version ".jar"))))
         (replace 'install
           (install-jars ,(string-append "jakarta-oro-" version))))))
    (home-page "https://jakarta.apache.org/oro/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jdom
  (package
    (name "java-jdom")
    (version "1.1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://jdom.org/dist/binary/archive/jdom-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "07wdpm3jwwc9q38kmdw40fvbmv6jzjrkrf8m0zqs58f79a672wfl"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "package"
       #:tests? #f; junit run with "package" target
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description " Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-commons-logging
  (package
    (inherit java-commons-logging-minimal)
    (arguments
     `(#:tests? #f ; avoid dependency on logging frameworks
       #:jar-name "commons-logging.jar"
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'delete-adapters-and-tests
           (lambda _
             ;; Delete all adapters except for NoOpLog, SimpleLog, and
             ;; LogFactoryImpl.  NoOpLog is required to build; LogFactoryImpl
             ;; is used by applications; SimpleLog is the only actually usable
             ;; implementation that does not depend on another logging
             ;; framework.
             (for-each
              (lambda (file)
                (delete-file (string-append
                              "src/main/java/org/apache/commons/logging/impl/" file)))
              (list "Jdk13LumberjackLogger.java"
                    "WeakHashtable.java"
                    "Log4JLogger.java"
                    "ServletContextCleaner.java"
                    "Jdk14Logger.java"
                    "AvalonLogger.java"
                    "LogKitLogger.java"))
             (delete-file-recursively "src/test")
             #t)))))
    (inputs
     `(("log4j" ,java-log4j-api)
       ,@(package-inputs java-commons-logging-minimal)))))

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
                "0rk7s04hkrr2k3glccx0yrglzqzj4qbipcrxhglk46yhx92vravc"))))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "src/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'prepare
           (lambda* (#:key inputs #:allow-other-keys)
             (delete-file-recursively "lib")
             (mkdir-p "bin/lib")
             ;; Don't download anything
             (substitute* "build/build.xml"
               ((".*download.xml.*") ""))
             (chdir "build"))))))
    (native-inputs
     `(("javacc" ,java-javacc)
       ("antlr" ,antlr2)))
    (propagated-inputs
     `(("java-commons-collections" ,java-commons-collections)
       ("java-jakarta-oro" ,java-jakarta-oro)
       ("java-jdom" ,java-jdom)
       ("java-tomcat" ,java-tomcat)
       ("java-log4j-api" ,java-log4j-api)
       ("java-commons-logging-minimal" ,java-commons-logging)
       ("java-commons-lang" ,java-commons-lang)))
    (home-page "https://velocity.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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
       #:source-dir "commons-jcs-core/src/main/java"))
    (propagated-inputs
     `(("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-velocity" ,java-velocity)))
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
         ;    (let* ((out (assoc-ref outputs "out"))
         ;           (bin (string-append out "/bin"))
         ;           (lib (string-append out "/lib/josm")))
         ;      (mkdir-p bin)
         ;      (mkdir-p lib)
         ;      (copy-file "dist/josm-custom.jar"
         ;                 (string-append lib "/josm.jar"))
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
                                    " -cp " dir "/javacc.jar" " javacc" " $*"))))
               (chmod (string-append bin "/javacc") #o755)))))))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://javacc.org")
    (synopsis "")
    (description "")
    (license license:bsd-3)))
    

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
                "0djdw7j66lqjx8bx9zja0hsx10c6nsj3z0z20jmavwfr6bpp0345"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         ;; icu4j archive contains its sources directly at the top, not in
         ;; a subdirectory as usual.
         (add-after 'unpack 'chdir
           (lambda _
             (chdir "org.abego.treelayout"))))))
    (inputs
     `(("junit" ,java-junit)))
    (home-page "http://treelayout.sourceforge.net")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

; propose update
(define-public java-commons-cli
  (package
    (name "java-commons-cli")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://mirrors.ircam.fr/pub/apache/commons/"
                                  "cli/source/commons-cli-" version "-src.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "05hgi2z01fqz374y719gl1dxzqvzci5af071zm7vxrjg9vczipm1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-cli-1.4.jar"
       #:tests? #f))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-cli")
    (synopsis "Java API for parsing command line options passed to programs")
    (description "Apache Commons CLI library provides an API for parsing command
line options passed to programs. It's also able to print help messages detailing
the options available for a command line tool.")
    (license license:asl2.0)))

; propose update
(define-public java-jsr305
  (package
    (name "java-jsr305")
    (version "3.0.2")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/amaembo/jsr-305.git")
                     (commit "d7734b13c61492982784560ed5b4f4bd6cf9bb2c")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "1wk159136pgc6i54drbq2whazfmdilvfqlxj3k19s9dfwbayf621"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "ri/src/main/java"
       #:tests? #f))
    (home-page "https://github.com/amaembo/jsr-305")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

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
       #:jdk ,icedtea-8
       #:tests? #f))
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
       #:jdk ,icedtea-8
       #:tests? #f))
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
     `(("guava" ,java-guava)
       ("javapoet" ,java-javapoet)))
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
     `(("junit" ,java-junit)))
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
                "00igy7a6aylswxdcklj9021g2s8bvsvrysagqyd8cibm4pimxrnk"))))
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
     `(("asm" ,java-asm)))
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

(define-public java-guava
  (package
    (name "java-guava")
    (version "20.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/guava/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1kasavj973iblj1fj35gzbywhkljrnbjpymgqyqaibbbmmbzff8s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "guava/src"
       #:tests? #f))
    (inputs
     `(("java-jsr305" ,java-jsr305)
       ("java-j2objc-annotations" ,java-j2objc-annotations)
       ("java-animal-sniffer-annotations" ,java-animal-sniffer-annotations)
       ("java-error-prone-annotations" ,java-error-prone-annotations)))
    (home-page "https://github.com/google/guava")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-joda-convert
  (package
    (name "java-joda-convert")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JodaOrg/joda-convert/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1di9chp0pgvd2gxsmdaxhldwns9a2ss9705jmn97mdd69cg5zcnc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "src/main/java"
       #:tests? #f))
    (inputs
     `(("java-google-collect" ,java-google-collect)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-joda-time
  (package
    (name "java-joda-time")
    (version "2.9.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JodaOrg/joda-time/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1i9x91mi7yg2pasl0k3912f1pg46n37sps6rdb0v1gs8hj9ppwc1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "src/main/java"
       #:tests? #f))
    (inputs
     `(("java-joda-convert" ,java-joda-convert)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-xstream
  (package
    (name "java-xstream")
    (version "1.4.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/com/thoughtworks"
                                  "/xstream/xstream-distribution/" version
                                  "/xstream-distribution-" version "-src.zip"))
              (file-name (string-append name "-" version ".zip"))
              (sha256
               (base32
                "1cq9j9h839wc6pkrgd9bd7y94a3zrj1j741i134izqs9xx2b54fi"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:tests? #f
       #:source-dir "xstream/src/java"))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://x-stream.github.io")
    (synopsis "")
    (description "")
    (license license:x11)))

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
                   ;; actually CDDL 1.1
                   license:cddl1.0))))

;; Can only be built with gradle.
(define-public groovy
  (package
    (name "groovy")
    (version "2.4.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/groovy/archive/GROOVY_"
                                  "2_4_10.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0wapzqwpx4bh2fsqpzf3haakjz6wvfjx1vd9a4spavhlrjqk2pbb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "groovy.jar"
       #:tests? #f))
    (native-inputs
     `(("junit" ,java-junit)))
    (inputs
     `(("commons-cli" ,java-commons-cli)
       ("antlr" ,antlr3)
       ("asm" ,java-asm)))
    (home-page "")
    (synopsis "")
    (description "")
    (license (list license:gpl2
                   ;; actually CDDL 1.1
                   license:cddl1.0))))

;; requires jline, javax.servlet, org.fusesource.jansi, org.livetribe,
;;   com.thoughtworks.xstream, org.apache.ivy, bsf
;;   antlr
(define-public groovy-1.8.9
  (package
    (inherit groovy)
    (name "groovy")
    (version "1.8.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/groovy/archive/GROOVY_"
                                  "1_8_9.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "16z3jv5yw11wwwzbs6x41g83gqazhngg30ys2kpy7cpfm3rsqi71"))))
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:tests? #f
       #:source-dir "src/main"))))


(define-public antlr3-3.4
  (package
    (name "antlr3")
    (version "3.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/website-antlr3/raw/"
                                  "gh-pages/download/antlr-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1cwfswpk3jlzl1dhc6b6586srza8q0bbzwlxcq136p29v62fjrb3"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "tool/src/main/java:runtime/Java/src/main/java:tool/src/main/antlr3"
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-after 'install 'bin-install
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let ((jar (string-append (assoc-ref outputs "out") "/share/java"))
                   (bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/antlr3")
                 (lambda _
                   (display
                     (string-append "#!/bin/sh\n"
                                    "java -cp " jar "/antlr3-3.3.jar:"
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "stringtemplate")
                                                  ".*\\.jar"))
                                    ":"
                                    (string-concatenate
                                      (find-files (string-append (assoc-ref inputs "antlr") "/lib")
                                                  ".*\\.jar"))
                                    " org.antlr.Tool $*"))))
               (chmod (string-append bin "/antlr3") #o755))))
         (add-before 'build 'generate-grammar
           (lambda _
             (chdir "tool/src/main/antlr3/org/antlr/grammar/v3/")
             (for-each (lambda (file)
                         (display file)
                         (newline)
                         (system* "antlr3" file))
                       '("ActionAnalysis.g" "ActionTranslator.g" "ANTLR.g"
                         "ANTLRTreePrinter.g" "ANTLRv3.g" "ANTLRv3Tree.g"
                         "AssignTokenTypesWalker.g" "CodeGenTreeWalker.g"
                         "DefineGrammarItemsWalker.g" "LeftRecursiveRuleWalker.g"
                         "TreeToNFAConverter.g"))
             (chdir "../../../../../../../..")
             (system* "antlr" "-o" "tool/src/main/java/org/antlr/tool"
                      "tool/src/main/java/org/antlr/tool/serialize.g")
             (substitute* "tool/src/main/java/org/antlr/tool/LeftRecursiveRuleAnalyzer.java"
               (("import org.antlr.grammar.v3.\\*;") "import org.antlr.grammar.v3.*;
import org.antlr.grammar.v3.ANTLRTreePrinter;"))
             (substitute* "tool/src/main/java/org/antlr/tool/Grammar.java"
               (("import org.antlr.grammar.v3.\\*;")
                "import org.antlr.grammar.v3.*;\n
import org.antlr.grammar.v3.TreeToNFAConverter;\n
import org.antlr.grammar.v3.DefineGrammarItemsWalker;\n
import org.antlr.grammar.v3.ANTLRTreePrinter;"))
             (substitute* "tool/src/main/java/org/antlr/tool/ErrorManager.java"
               (("case NO_SUCH_ATTRIBUTE_PASS_THROUGH:") ""))
             (substitute* "tool/src/main/antlr3/org/antlr/grammar/v3/ANTLRParser.java"
               (("public Object getTree") "public GrammarAST getTree"))
             (substitute* "tool/src/main/antlr3/org/antlr/grammar/v3/ANTLRv3Parser.java"
               (("public Object getTree") "public CommonTree getTree"))))
         (add-before 'build 'fix-build-xml
           (lambda _
             (substitute* "build.xml"
               (("<exec") "<copy todir=\"${classes.dir}\">
<fileset dir=\"tool/src/main/resources\">
<include name=\"**/*.stg\"/>
<include name=\"**/*.st\"/>
<include name=\"**/*.sti\"/>
<include name=\"**/STLexer.tokens\"/>
</fileset>
</copy><exec")))))))
    (native-inputs
     `(("antlr" ,antlr2)
       ("antlr3" ,antlr3-3.3)))
    (inputs
     `(("junit" ,java-junit)))
    (propagated-inputs
     `(("stringtemplate" ,stringtemplate3)
       ("stringtemplate4" ,stringtemplate4)
       ("antlr" ,antlr2)
       ("antlr3" ,antlr3-3.1)))
    (home-page "http://www.stringtemplate.org")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

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
     `(("antlr" ,antlr3)))
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
       #:tests? #f
       #:jdk ,icedtea-8
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
    (version "4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/antlr/antlr4/archive/"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0y7lzkvx9wbbmwg45mb4icx7i66z6894qfygrbbs26sr5xxyml9h"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append ,name "-" ,version ".jar")
       #:source-dir "runtime/Java/src:tool/src"
       #:jdk ,icedtea-8
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
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
       ("icu4j" ,java-icu4j)
       ("java-json" ,java-json)
       ("treelayout" ,java-treelayout)
       ("stringtemplate4" ,stringtemplate4)))
    (home-page "https://antlr.org")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

;; requires groovy 2.4.7.
;(define-public gradle
;  (package
;    (name "gradle")
;    (version "3.4.1")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/gradle/gradle/archive/v"
;                                  version ".tar.gz"))
;              (file-name (string-append name "-" version ".tar.gz"))
;              (sha256
;               (base32 "0fq30k51mkixg31z3d4fjq3zbnyjml4i530px6n1n947mqk3rgyl"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:phases
;       (modify-phases %standard-phases
;         (replace 'build
;           (lambda* _
;             (system* "sh" "-x" "gradlew" "prBuild" "-x" "integTest" "--continue"
;                      "--stacktrace"))))))
;             ;(system* "sh" "-x" "travisci_build.sh"))))))
;    (home-page "")
;    (synopsis "Build system")
;    (description "Build system")
;    (license license:asl2.0)))
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
