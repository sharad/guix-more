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
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages base)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (more packages python))

(define-public java-tomcat
  (package
    (name "java-tomcat")
    (version "8.5.20")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/tomcat/tomcat-8/v"
                                  version "/src/apache-tomcat-" version "-src.tar.gz"))
              (sha256
               (base32
                "0436glw5knwnlj5636vsb966zfdfcw3jpnwbpwjn743p3gk0mi3b"))))
    (build-system ant-build-system)
    (inputs
     `(("java-eclipse-jdt-core" ,java-eclipse-jdt-core)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (arguments
     `(#:build-target "package"
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
     `(("antlr" ,antlr3)
       ("ST4" ,java-stringtemplate)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
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
     `(("graphics" ,java-openjfx-graphics)
       ("base" ,java-openjfx-base)
       ("openjfx" ,java-openjfx)))
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
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://svn.openstreetmap.org/applications/viewer/jmapviewer/releases/"
                                  version "/JMapViewer-" version "-Source.zip"))
              (sha256
               (base32
                "0xalq8bacn8ibz3xiaqvj5pg6pxk9irvwx5f1lb0y2z5gsny3l1x"))))
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
    (version "12545")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstreetmap/josm.git")
                    (commit "3534f00bd3a58fe82f6b2e2685f5b9d17cc13886")))
              ;;(uri (svn-reference
              ;;      (url "https://svn.openstreetmap.org/applications/editors/josm")
              ;;      (revision 12039)))
              (sha256
               (base32
                "10j1p9xsi6g11qspcfj280n8bsm77skw9v50cyhj378v6bcxjh41"))
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
       ("java-openjfx" ,java-openjfx)
       ("java-openjfx-base" ,java-openjfx-base)
       ("java-openjfx-media" ,java-openjfx-media)
       ("java-openjfx-graphics" ,java-openjfx-graphics)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-jcs" ,java-commons-jcs)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-compress" ,java-commons-compress-latest)))
    (arguments
     `(#:tests? #f
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
         (add-after 'build 'generate-epsg
           (lambda _
             (system* "javac" "scripts/BuildProjectionDefinitions.java"
                      "-cp" "build/classes")
             (mkdir-p "data/projection")
             (with-output-to-file "data/projection/custom-epsg"
               (lambda _ (display "")))
             (zero? (system* "java" "-cp" "build/classes:scripts:."
                             "BuildProjectionDefinitions" "."))))
         (add-after 'generate-epsg 'copy-data
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "data" "build/classes/data")))
         (add-before 'install 'regenerate-jar
           (lambda _
             ;; We need to regenerate the jar file with to add data.
             (delete-file "build/jar/josm.jar")
             (zero? (system* "jar" "-cf" "build/jar/josm.jar" "-C"
                             "build/classes" "."))))
         (add-before 'build 'copy-styles
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "styles" "build/classes/styles")))
         (add-before 'build 'copy-images
           (lambda _
             (mkdir-p "build/classes")
             (rename-file "images" "build/classes/images")))
         (add-before 'build 'copy-revision
           (lambda _
             (mkdir-p "build/classes")
             (with-output-to-file "build/classes/REVISION"
               (lambda _
                 (display
                   (string-append "Revision: " ,version "\n"
                                  "Is-Local-Build: true\n"
                                  "Build-Date: 1970-01-01 00:00:00 +0000\n"))))))
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
               (chmod (string-append bin "/josm") #o755)))))))
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
       #:tests? #f; tests are run as part of the build process
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
       #:tests? #f; tests are run as part of the build process
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
       #:tests? #f; tests are run as part of the build process
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (install-jars "build")))))
    (home-page "http://jdom.org/")
    (synopsis "Access, manipulate, and output XML data")
    (description " Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

(define-public java-mvel2
  (package
    (name "java-mvel2")
    (version "2.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/mvel/mvel/archive/mvel2-"
                                  version ".Final.tar.gz"))
              (sha256
               (base32
                "01ph5s9gm16l2qz58lg21w6fna7xmmrj7f9bzqr1jim7h9557d3z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "mvel2.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'exclude-non-tests
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/Abstract*Test.java\" />"
                               "<exclude name=\"**/MVELThreadTest.java\" />")))))
         (add-after 'install 'install-bin
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (mkdir-p bin)
               (with-output-to-file (string-append bin "/mvel2")
                 (lambda _
                   (display (string-append "java -Dout.dir=$2 -cp "
                                           (getenv "CLASSPATH")
                                           ":" (assoc-ref outputs "out")
                                           "/share/java/mvel2.jar"
                                           " org.mvel2.sh.Main $1"))))
               (chmod (string-append bin "/mvel2") #o755)))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/mvel/mvel")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-lz4
  (package
    (name "java-lz4")
    (version "1.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/lz4/lz4-java/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "096dm57p2lzqk28n0j2p52x2j3cvnsd2dfqn43n7vbwrkjsy7y54"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "lz4.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/java:src/java-unsafe"
       #:tests? #f; FIXME: needs deps
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-source
           (lambda _
             (with-directory-excursion "src/build/source_templates"
               (zero? (system* "mvel2" "../gen_sources.mvel" "../../java"))))))))
    (native-inputs
     `(("mvel" ,java-mvel2)))
    (home-page "https://jpountz.github.io/lz4-java"); or http://blog.jpountz.net/
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-bouncycastle-bcprov
  (package
    (name "java-bouncycastle-bcprov")
    (version "1.58")
    (source (origin
              (method url-fetch)
              (uri "https://bouncycastle.org/download/bcprov-jdk15on-158.tar.gz")
              (sha256
               (base32
                "1hgkg96llbvgs8i0krwz2n0j7wlg6jfnq8w8kg0cc899j0wfmf3n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "bouncycastle-bcprov.jar"
       #:tests? #f; no tests
       #:source-dir "src"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unzip-src
           (lambda _
             (mkdir-p "src")
             (with-directory-excursion "src"
               (zero? (system* "unzip" "../src.zip"))))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("junit" ,java-junit)))
    (home-page "https://www.bouncycastle.org")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-bouncycastle-bcpkix
  (package
    (name "java-bouncycastle-bcpkix")
    (version "1.58")
    (source (origin
              (method url-fetch)
              (uri "https://bouncycastle.org/download/bcpkix-jdk15on-158.tar.gz")
              (sha256
               (base32
                "0is7qay02803s9f7lhnfcjlz61ni3hq5d7apg0iil7nbqkbfbcq2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "bouncycastle-bcpkix.jar"
       #:tests? #f; no tests
       #:source-dir "src"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unzip-src
           (lambda _
             (mkdir-p "src")
             (with-directory-excursion "src"
               (zero? (system* "unzip" "../src.zip"))))))))
    (native-inputs
     `(("unzip" ,unzip)
       ("junit" ,java-junit)))
    (inputs
     `(("bcprov" ,java-bouncycastle-bcprov)))
    (home-page "https://www.bouncycastle.org")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-bouncycastle-bctls
  (package
    (name "java-bouncycastle-bctls")
    (version "1.58")
    (source (origin
              (method url-fetch)
              (uri "https://bouncycastle.org/download/bctls-jdk15on-158.tar.gz")
              (sha256
               (base32
                "0riyd4iy9q9gk8spf0pqp64hrfihrdfm445aqr9n2zfb7n4jz2v3"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "bouncycastle.jar"
       #:tests? #f; no tests
       #:source-dir "src"))
    (inputs
     `(("bcprov" ,java-bouncycastle-bcprov)
       ("bcpkix" ,java-bouncycastle-bcpkix)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://www.bouncycastle.org")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-powermock-core
  (package
    (name "java-powermock-core")
    (version "1.7.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/powermock/powermock/"
                                  "archive/powermock-" version ".tar.gz"))
              (sha256
               (base32
                "09rdklqm1c2zp45c8x9596g8r9m5ab8aalxh447kljcrzajz49ri"))
              (patches
                (search-patches "java-powermock-fix-java-files.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-core.jar"
       #:source-dir "powermock-core/src/main/java"
       #:test-dir "powermock-core/src/test"
       #:tests? #f; requires powermock-api
       #:jdk ,icedtea-8))
    (inputs
     `(("reflect" ,java-powermock-reflect)
       ("javassist" ,java-jboss-javassist)))
    (native-inputs
     `(("junit" ,java-junit)
       ("assertj" ,java-assertj)
       ("mockito" ,java-mockito-1)))
    (home-page "https://github.com/powermock/powermock")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-powermock-reflect
  (package
    (inherit java-powermock-core)
    (name "java-powermock-reflect")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-reflect.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-reflect/src/main/java"
       #:test-dir "powermock-reflect/src/test"))
    (inputs
     `(("java-objenesis" ,java-objenesis)))
    (native-inputs
     `(("junit" ,java-junit)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("hamcrest" ,java-hamcrest-core)
       ("assertj" ,java-assertj)))))

(define-public java-powermock-api-support
  (package
    (inherit java-powermock-core)
    (name "java-powermock-api-support")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-api-support.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-api/powermock-api-support/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("core" ,java-powermock-core)
       ("reflect" ,java-powermock-reflect)))))

(define-public java-powermock-api-easymock
  (package
    (inherit java-powermock-core)
    (name "java-powermock-api-easymock")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-api-easymock.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-api/powermock-api-easymock/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-file
           (lambda _
             ;; FIXME: This should not be necessary
             (substitute* "powermock-api/powermock-api-easymock/src/main/java/org/powermock/api/easymock/PowerMock.java"
               (("classLoader instanceof MockClassLoader") "false")
               (("\\(\\(MockClassLoader\\) classLoader\\).cache\\(mock.getClass\\(\\)\\);") ";")))))))
    (inputs
     `(("core" ,java-powermock-core)
       ("easymock" ,java-easymock)
       ("reflect" ,java-powermock-reflect)
       ("support" ,java-powermock-api-support)
       ("cglib" ,java-cglib)))))

(define-public java-powermock-modules-junit4-common
  (package
    (inherit java-powermock-core)
    (name "java-powermock-modules-junit4-common")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-modules-junit4-common.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-modules/powermock-module-junit4-common/src/main/java"
       #:test-dir "powermock-modules/powermock-module-junit4-common/src/test"))
    (inputs
     `(("core" ,java-powermock-core)
       ("easymock" ,java-easymock)
       ("reflect" ,java-powermock-reflect)
       ("hamcrest" ,java-hamcrest-core)
       ("cglib" ,java-cglib)))))

(define-public java-powermock-modules-junit4
  (package
    (inherit java-powermock-core)
    (name "java-powermock-modules-junit4")
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-powermock-modules-junit4.jar"
       #:jdk ,icedtea-8
       #:source-dir "powermock-modules/powermock-module-junit4/src/main/java"
       #:test-dir "powermock-modules/powermock-module-junit4/src/test"))
    (inputs
     `(("core" ,java-powermock-core)
       ("reflect" ,java-powermock-reflect)
       ("common" ,java-powermock-modules-junit4-common)
       ("cglib" ,java-cglib)))
    (native-inputs
     `(("easymock" ,java-easymock)
       ("hamcrest" ,java-hamcrest-core)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)
       ("junit" ,java-junit)))))

(define-public java-xerial-core
  (package
    (name "java-xerial-core")
    (version "2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xerial/xerial-java/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0d3g863i41bgalpa4xr3vm1h140l091n8iwgq5qvby5yivns9y8d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xerial-core.jar"
       #:source-dir "xerial-core/src/main/java"
       #:test-dir "xerial-core/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (let ((dir (string-append (getcwd) "/build/classes/")))
               (with-directory-excursion "xerial-core/src/main/resources"
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*")))))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/xerial/xerial-java")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-classworlds
  (package
    (name "java-plexus-classworlds")
    (version "2.5.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/codehaus-plexus/"
                                  "plexus-classworlds/archive/plexus-classworlds-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1qm4p0rl8d82lzhsiwnviw11jnq44s0gflg78zq152xyyr2xmh8g"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-classworlds.jar"
       #:source-dir "src/main"
       #:tests? #f));; FIXME: needs to study pom.xml for resource generation
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "http://codehaus-plexus.github.io/plexus-classworlds/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public bitshuffle-for-snappy
  (package
    (inherit bitshuffle)
    (name "bitshuffle-for-snappy")
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (replace 'configure
           (lambda* (#:key outputs #:allow-other-keys)
             (with-output-to-file "Makefile"
               (lambda _
                 (display
                   (string-append
                     "libbitshuffle.so: src/bitshuffle.o src/bitshuffle_core.o "
                     "src/iochain.o lz4/lz4.o\n"
                     "\tgcc -O3 -ffast-math -std=c99 -o $@ -shared -fPIC $^\n"
                     "\n"
                     "%.o: %.c\n"
                     "\tgcc -O3 -ffast-math -std=c99 -fPIC -Isrc -Ilz4 -c $< -o $@\n"
                     "\n"
                     "PREFIX:=" (assoc-ref outputs "out") "\n"
                     "LIBDIR:=$(PREFIX)/lib\n"
                     "INCLUDEDIR:=$(PREFIX)/include\n"
                     "install: libbitshuffle.so\n"
                     "\tinstall -dm755 $(LIBDIR)\n"
                     "\tinstall -dm755 $(INCLUDEDIR)\n"
                     "\tinstall -m755 libbitshuffle.so $(LIBDIR)\n"
                     "\tinstall -m644 src/bitshuffle.h $(INCLUDEDIR)\n"
                     "\tinstall -m644 src/bitshuffle_core.h $(INCLUDEDIR)\n"
                     "\tinstall -m644 src/iochain.h $(INCLUDEDIR)\n"
                     "\tinstall -m644 lz4/lz4.h $(INCLUDEDIR)\n")))))))))
    (inputs '())
    (native-inputs '())))

(define-public java-snappy
  (package
    (name "java-snappy")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xerial/snappy-java/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1w58diryma7qz7aa24yv8shf3flxcbbw8jgcn2lih14wgmww58ww"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "snappy.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-bins
           (lambda _
             (delete-file "lib/org/xerial/snappy/OSInfo.class")
             (delete-file-recursively "src/main/resources/org/xerial/snappy/native")
             #t))
         (add-before 'build 'build-jni
           (lambda _
             (system* "javac" "src/main/java/org/xerial/snappy/OSInfo.java"
                      "-d" "lib")
             (substitute* "Makefile.common"
               (("-shared")
                "-shared -lbitshuffle -lsnappy"))
             (substitute* "Makefile"
               (("\\$\\(SNAPPY_GIT_UNPACKED\\) ")
                "")
               ((": \\$\\(SNAPPY_GIT_UNPACKED\\)")
                ":")
               (("SNAPPY_OBJ:=.*")
                "SNAPPY_OBJ:=$(addprefix $(SNAPPY_OUT)/, SnappyNative.o BitShuffleNative.o)\n")
               (("NAME\\): \\$\\(SNAPPY_OBJ\\)")
                "NAME): $(SNAPPY_OBJ)\n\t@mkdir -p $(@D)")
               (("\\$\\(BITSHUFFLE_UNPACKED\\) ")
                "")
               ((": \\$\\(SNAPPY_SOURCE_CONFIGURED\\)") ":"))
             (zero? (system* "make" "native"))))
         (add-after 'build-jni 'copy-jni
           (lambda _
             (let ((dir (string-append (getcwd) "/build/classes/org/xerial/snappy/native/")))
               (with-directory-excursion "src/main/resources/org/xerial/snappy/native"
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*"))))))
         (add-before 'check 'disable-failing
           (lambda _
             (substitute* "src/test/java/org/xerial/snappy/SnappyLoaderTest.java"
               (("target/classes") "build/classes"))
             ;; FIXME: probably an error
             (substitute* "src/test/java/org/xerial/snappy/SnappyOutputStreamTest.java"
               (("91080") "91013")))))))
    (inputs
     `(("osgi-framework" ,java-osgi-framework)))
    (propagated-inputs
     `(("bitshuffle" ,bitshuffle-for-snappy)
       ("snappy" ,snappy)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("xerial-core" ,java-xerial-core)
       ("classworlds" ,java-plexus-classworlds)
       ("perl" ,perl)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-kafka-clients
  (package
    (name "java-kafka-clients")
    (version "0.11.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/kafka/" version "/kafka-"
                                  version "-src.tgz"))
              (sha256
               (base32
                "01mbi12bdxhrv4iadb3179cqrg689jva8hh8nig4n747arsbgiby"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-kafka-clients.jar"
       #:jdk ,icedtea-8
       #:source-dir "clients/src/main/java"
       #:test-dir "clients/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'exclude-base
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               ;; not a class
                               "<exclude name=\"**/IntegrationTest.java\" />"
                               ;; requires network
                               "<exclude name=\"**/ClientUtilsTest.java\" />"
                               ;; something is wrong with our powermock
                               "<exclude name=\"**/KafkaProducerTest.java\" />"
                               "<exclude name=\"**/BufferPoolTest.java\" />"))))))))
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)
       ("java-lz4" ,java-lz4)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-all)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)
       ("cglib" ,java-cglib)
       ("javassist" ,java-jboss-javassist)
       ("snappy" ,java-snappy)
       ("easymock" ,java-easymock)
       ("powermock" ,java-powermock-core)
       ("powermock-easymock" ,java-powermock-api-easymock)
       ("powermock-junit4-common" ,java-powermock-modules-junit4-common)
       ("powermock-junit4" ,java-powermock-modules-junit4)
       ("powermock-support" ,java-powermock-api-support)
       ("bouncycastle" ,java-bouncycastle-bcprov)
       ("bouncycastle-bcpkix" ,java-bouncycastle-bcpkix)))
    (home-page "https://kafka.apache.org")
    (synopsis "")
    (description "")
    (license (list license:cddl1.0; actually cddl1.1
                   license:gpl2)))); with classpath exception

(define-public java-bsh
  (package
    (name "java-bsh")
    (version "2.0b6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/beanshell/beanshell/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1bawkxk6jyc75hxvzkpz689h73cn3f222m0ar3nvb0dal2b85kfv"))))
    (build-system ant-build-system)
    (arguments
     `(#:build-target "jarall"
       #:test-target "junit-tests-all"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((share (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p share)
               (copy-file "dist/bsh-2.0b6.jar" (string-append share "/bsh-2.0b6.jar"))))))))
    (home-page "http://beanshell.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-hdrhistogram
  (package
    (name "java-hdrhistogram")
    (version "2.1.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/HdrHistogram/HdrHistogram/"
                                  "archive/HdrHistogram-" version ".tar.gz"))
              (sha256
               (base32
                "1sicbmc3sr42nw93qbkb26q9rn33ag33k6k77phjc3j5h5gjffqv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-hdrhistogram.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'set-version
           (lambda _
             (let* ((version-java "src/main/java/org/HdrHistogram/Version.java")
                    (template (string-append version-java ".template")))
               (copy-file template version-java)
               (substitute* version-java
                 (("\\$VERSION\\$") ,version)
                 (("\\$BUILD_TIME\\$") "0"))))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://hdrhistogram.github.io/HdrHistogram")
    (synopsis "")
    (description "")
    (license license:bsd-2)))

(define-public java-jmock
  (package
    (name "java-jmock")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jmock-developers/"
                                  "jmock-library/archive/" version ".tar.gz"))
              (sha256
               (base32
                "18650a9g8xffcsdb6w91pbswa7f40fp2sh6s3nclkclz5dbzq8f0"))))
    (build-system ant-build-system)
    (inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-asm" ,java-asm)
       ("java-bsh" ,java-bsh)
       ("java-jumit" ,java-junit)))
    (arguments
     `(#:jar-name "java-jmock.jar"
       #:source-dir "jmock/src/main/java"
       #:test-dir "jmock/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             ;; Otherwise the abstract class is tested, but junit cannot create
             ;; an instance of it. Then remove dependent.
             (for-each (lambda (file) (delete-file file))
               '("jmock/src/test/java/org/jmock/test/unit/lib/AbstractMatcherTest.java"
                 "jmock/src/test/java/org/jmock/test/unit/lib/CurrentStateMatcherTests.java")))))))
    (native-inputs
     `(("cglib" ,java-cglib)))
    (home-page "https://github.com/jmock-developers/jmock-library")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-jmock-junit4
  (package
    (inherit java-jmock)
    (name "java-jmock-junit4")
    (arguments
     `(#:jar-name "java-jmock-junit4.jar"
       #:source-dir "jmock-junit4/src/main/java"
       #:test-dir "jmock-junit4/src/test"))
    (inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-asm" ,java-asm)
       ("java-bsh" ,java-bsh)
       ("java-jmock" ,java-jmock)
       ("java-jumit" ,java-junit)))))

(define-public java-jmock-legacy
  (package
    (inherit java-jmock)
    (name "java-jmock-legacy")
    (arguments
     `(#:jar-name "java-jmock-legacy.jar"
       #:source-dir "jmock-legacy/src/main/java"
       #:test-dir "jmock-legacy/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-tests
           (lambda _
             ;; This file is a dependance of some tests
             (copy-file "jmock/src/test/java/org/jmock/test/acceptance/PackageProtectedType.java"
                        "jmock-legacy/src/test/java/org/jmock/test/acceptance/PackageProtectedType.java"))))))
    (inputs
     `(("java-hamcrest-all" ,java-hamcrest-all)
       ("java-objenesis" ,java-objenesis)
       ("java-cglib" ,java-cglib)
       ("java-jmock" ,java-jmock)
       ("java-asm" ,java-asm)
       ("java-bsh" ,java-bsh)
       ("java-junit" ,java-junit)))
    (native-inputs
     `(("java-jmock-junit4" ,java-jmock-junit4)))))

(define-public java-lmax-disruptor
  (package
    (name "java-lmax-disruptor")
    (version "3.3.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/LMAX-Exchange/disruptor/"
                                  "archive/" version ".tar.gz"))
              (sha256
               (base32
                "19c7c5cf3lby4fy7vl3b6a9hds1g0j7xgfbskqbdlcai1x82hh8i"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-lmax-disruptor.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); tests hang
    (inputs
     `(("junit" ,java-junit)
       ("java-hdrhistogram" ,java-hdrhistogram)
       ("java-jmock" ,java-jmock)
       ("java-jmock-legacy" ,java-jmock-legacy)
       ("java-jmock-junit4" ,java-jmock-junit4)
       ("java-hamcrest-all" ,java-hamcrest-all)))
    (native-inputs
     `(("cglib" ,java-cglib)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)))
    (home-page "https://www.lmax.com/disruptor")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-datanucleus-javax-persistence
  (package
    (name "java-datanucleus-javax-persistence")
    (version "2.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/datanucleus/"
                                  "javax.persistence/archive/javax.persistence-"
                                  version "-release.tar.gz"))
              (sha256
               (base32
                "11jx0fjwgc2hhbqqgdd6m1pf2fplf9vslppygax0y1z5csnqjhpx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-datanucleus-javax-persistence.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (home-page "https://github.com/datanucleus/javax.persistence")
    (synopsis "")
    (description "")
    (license (list license:edl1.0 license:epl1.0))))

(define-public java-jboss-javassist
  (package
    (name "java-jboss-javassist")
    (version "3.21.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss-javassist/javassist/"
                                  "archive/rel_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x)) version)
                                  "_ga.tar.gz"))
              (sha256
               (base32
                "10lpcr3sbf7y6fq6fc2h2ik7rqrivwcy4747bg0kxhwszil3cfmf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-javassist.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main"
       #:tests? #f; FIXME: requires junit-awtui and junit-swingui from junit3
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'remove-binary
           (lambda _
             (delete-file "javassist.jar"))))))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/jboss-javassist/javassist")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

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
                "0zvglvscq177lahvp8n9nlm0vkdxlf6db0fs8jcy8zf82z6k4d2n"))))
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

(define-public java-jboss-interceptors-api-spec
  (package
    (name "java-jboss-interceptors-api-spec")
    (version "1.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-interceptors-api_spec/"
                                  "archive/jboss-interceptors-api_" version
                                  "_spec-1.0.0.Final.tar.gz"))
              (sha256
               (base32
                "15h3486rcqg46sfz99kilwidhgv085gyjx6fdpxa1c5j75s8qc3k"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-interceptors-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "."
       #:tests? #f)); no tests
    (home-page "https://github.com/jboss/jboss-interceptors-api_spec")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-jboss-el-api-spec
  (package
    (name "java-jboss-el-api-spec")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-el-api_spec/"
                                  "archive/jboss-el-api_" version
                                  "_spec-1.0.7.Final.tar.gz"))
              (sha256
               (base32
                "1j45ljxalwlibxl7g7iv952sjxkw275m8vyxxij8l6wdd5pf0pdh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-el-api_spec.jar"
       #:jdk ,icedtea-8))
    (inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/jboss/jboss-el-api_spec")
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
                "0crfl4f5m2sm59kdsivrw2dy63w02al1li88bhlhp7mxnicfmxv7"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-transaction-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("cdi-api" ,java-cdi-api)
       ("interceptors" ,java-jboss-interceptors-api-spec)))
    (home-page "https://github.com/jboss/jboss-transaction-api_spec")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-jboss-jms-api-spec
  (package
    (name "java-jboss-jms-api-spec")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jboss/jboss-jms-api_spec/"
                                  "archive/jboss-jms-api_" version
                                  "_spec-1.0.1.Final.tar.gz"))
              (sha256
               (base32
                "07bqblw9kq2i8q92bz70fvavq5xjfkaixl8xa0m0cypjgy82rb7m"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jboss-jms-api_spec.jar"
       #:jdk ,icedtea-8
       #:source-dir "."
       #:tests? #f)); no tests
    (home-page "https://github.com/jboss/jboss-jms-api_spec")
    (synopsis "")
    (description "")
    (license (list license:gpl2 license:cddl1.0)))); either gpl2 only or cddl.

(define-public java-mail
  (package
    (name "java-mail")
    (version "1.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javaee/javamail/archive/"
                                  "JAVAMAIL-1_6_0.tar.gz"))
              (sha256
               (base32
                "1b4rg7fpj50ld90a71iz2m4gm3f5cnw18p3q3rbrrryjip46kx92"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-mail.jar"
       #:jdk ,icedtea-8
       #:source-dir "mail/src/main/java"
       #:test-dir "mail/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'move-version.java
           (lambda _
             ;; this is done in build.xml (init target)
             (copy-file "mail/src/main/resources/javax/mail/Version.java"
                        "mail/src/main/java/javax/mail/Version.java")))
         (add-before 'check 'remove-failing
           (lambda _
             ;; This fails
             (delete-file "mail/src/test/java/com/sun/mail/util/logging/CollectorFormatterTest.java")
             ;; This one needs the previous one
             (delete-file "mail/src/test/java/com/sun/mail/util/logging/CompactFormatterTest.java")
             ;; This fails
             (delete-file "mail/src/test/java/com/sun/mail/util/logging/DurationFilterTest.java")
             (delete-file "mail/src/test/java/com/sun/mail/util/logging/MailHandlerTest.java")
             (delete-file "mail/src/test/java/javax/mail/internet/GetLocalAddressTest.java")
             ;; FIXME: ends with:
             ;; java.lang.ClassNotFoundException: javax.mail.internet.MimeMultipartParseTest
             (delete-file "mail/src/test/java/javax/mail/internet/MimeMultipartParseTest.java")
             ;; FIXME: same here
             (delete-file "mail/src/test/java/javax/mail/search/SearchTermSerializationTest.java")
             ))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "mail/src/main/resources/META-INF/" ".*")))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://javaee.github.io/javamail")
    (synopsis "")
    (description "")
    (license (list license:cddl1.0; actually cddl1.1
                   license:gpl2)))); with classpath exception

(define-public java-fasterxml-jackson-annotations
  (package
    (name "java-fasterxml-jackson-annotations")
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-annotations/archive/"
                                  "jackson-annotations-" version ".tar.gz"))
              (sha256
               (base32
                "0lh6ngld2sgspy3zy6yk7wfd7a1lqy0a7kl90krz49l6wyx440ny"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-annotations.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/FasterXML/jackson-annotations")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-core
  (package
    (name "java-fasterxml-jackson-core")
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-core/archive/"
                                  "jackson-core-" version ".tar.gz"))
              (sha256
               (base32
                "05r742510lwclv51lwbca3753hppzvq38pnsalp1hccn3102lhy2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-core.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/core/json/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.core.json")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.core")
                 (("@projectartifactid@") "jackson-core")))))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "src/main/resources/META-INF/" ".*"))))
         (add-before 'check 'copy-test-resources
           (lambda _
             (mkdir-p "build/test-classes")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes"
                                                        (basename file))))
               (find-files "src/test/resources/" ".*\\.json"))))
         (add-before 'check 'exclude-base
           (lambda _
             ;; not really tests
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/failing/**\" />"
                               "<exclude name=\"**/BaseTest.java\" />"
                               "<exclude name=\"**/ConcurrencyReadTest.java\" />"
                               "<exclude name=\"**/ManualCharAccessTest.java\" />"
                               "<exclude name=\"**/TrailingCommasTest.java\" />"
                               "<exclude name=\"**/AsyncMissingValuesInObjectTest.java\" />"
                               "<exclude name=\"**/AsyncMissingValuesInArrayTest.java\" />")))))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "src/main/resources/META-INF/" ".*")))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/FasterXML/jackson-core")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-databind
  (package
    (name "java-fasterxml-jackson-databind")
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-databind/archive/"
                                  "jackson-databind-" version ".tar.gz"))
              (sha256
               (base32
                "01a60dwy1q11xizvimphmhjfbk41rwar3rl1q3xcwlhic2vdw147"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-databind.jar"
       #:source-dir "src/main/java"
       #:tests? #f; requires javax.measures for which I can't find a free implementation
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/databind/cfg/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.databind.cfg")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.databind")
                 (("@projectartifactid@") "jackson-databind")))))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "src/main/resources/META-INF/" ".*")))))))
    (inputs
     `(("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/FasterXML/jackson-databind")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-fasterxml-jackson-modules-base
  (package
    (name "java-fasterxml-jackson-modules-base")
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-modules-base/archive/"
                                  "jackson-modules-base-" version ".tar.gz"))
              (sha256
               (base32
                "054f1rk3k7way7d2a67q2j81yi33n1q9l4rp14jb864y46zdaayy"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-modules-base.jar"
       #:source-dir "jaxb/src/main/java"
       #:test-dir "jaxb/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out (string-append "jaxb/src/main/java/com/fasterxml/"
                                        "jackson/module/jaxb/PackageVersion.java"))
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.module.jaxb")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.module.jaxb")
                 (("@projectartifactid@") "jackson-module-jaxb")))))
         (add-before 'check 'disable-failing
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               ;; The base class for tests, not a test in itself
                               "<exclude name=\"**/BaseJaxbTest.java\" />")))))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "jaxb/src/main/resources/META-INF/" ".*")))))))
    (inputs
     `(("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "https://github.com/FasterXML/jackson-dataformat-xml")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-woodstox-core
  (package
    (name "java-woodstox-core")
    (version "5.0.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/woodstox/archive/woodstox-core-5.0.3.tar.gz"))
              (sha256
               (base32
                "1i7pdgb8jbw6gdy5kmm0l6rz109n2ns92pqalpyp24vb8vlvdfd4"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "woodstox.jar"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-msv-dep
           (lambda _
             ;; we don't need osgi, and it depends on msv
             (delete-file-recursively "src/main/java/com/ctc/wstx/osgi")
             ;; msv's latest release is from 2011
             (delete-file-recursively "src/main/java/com/ctc/wstx/msv")
             (delete-file-recursively "src/test/java/wstxtest/osgi")
             (delete-file-recursively "src/test/java/wstxtest/msv")))
         (add-before 'check 'remove-failing
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/Base*.java\" />"
                               "<exclude name=\"failing/**\" />")))))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "src/main/resources/META-INF/" ".*")))))))
    (inputs
     `(("stax2" ,java-stax2-api)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/FasterXML/woodstox")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-fasterxml-jackson-dataformat-xml
  (package
    (name "java-fasterxml-jackson-dataformat-xml")
    (version "2.9.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-dataformat-xml/archive/"
                                  "jackson-dataformat-xml-" version ".tar.gz"))
              (sha256
               (base32
                "1iv8brs68szk2d4fghji1bgg5g8fcc4y1yp363phiy794f40vxfc"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-dataformat-xml.jar"
       #:source-dir "src/main/java"
       ;; FIXME: tests fail to find the SAX API implementation in woodstox.
       ;; It probably means this package is broken.
       #:tests? #f
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/dataformat/xml/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (newline)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.dataformat.xml")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.dataformat.xml")
                 (("@projectartifactid@") "jackson-dataformat-xml")))))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/classes/META-INF/"
                                                        (basename file))))
               (find-files "src/main/resources/META-INF/" ".*")))))))
    (inputs
     `(("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-modules-base" ,java-fasterxml-jackson-modules-base)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
       ("java-stax2-api" ,java-stax2-api)
       ("woodstox" ,java-woodstox-core)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/FasterXML/jackson-dataformat-xml")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define-public java-stax2-api
  (package
    (name "java-stax2-api")
    (version "4.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/stax2-api/archive/"
                                  "stax2-api-" version ".tar.gz"))
              (sha256
               (base32
                "1amc1si0l0hyyw2sawmnzy4hkna3z6fp195y4nm5m9wb9ld5awkq"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-stax2-api.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (home-page "https://github.com/FasterXML/stax2-api")
    (synopsis "Stax2 API")
    (description "Stax2 API is an extension to basic Stax 1.0 API that adds
significant new functionalities, such as full-featured bi-direction validation
interface and high-performance Typed Access API.")
    (license license:bsd-2)))

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
     `(("commons-logging" ,java-commons-logging-minimal)
       ("asm" ,java-asm)))
    (home-page "https://www.eclipse.org/aspectj")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define java-xmlunit-test-resources
  (origin
    (method git-fetch)
    (uri (git-reference
           (url "https://github.com/xmlunit/test-resources.git")
           (commit "a590d2ae865c3e0455691d76ba8eefccc2215aec")))
    (file-name "java-xmlunit-test-resources")
    (sha256
     (base32
      "0r0glj37pg5l868yjz78gckr91cs8fysxxbp9p328dssssi91agr"))))

(define-public java-xmlunit
  (package
    (name "java-xmlunit")
    (version "2.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/xmlunit/xmlunit/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0gy9wbrg682m5j4p7xw2lvvp1p86vrki83kcl59h084z262ks2pl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-xmlunit.jar"
       #:source-dir "xmlunit-core/src/main/java"
       #:test-dir "xmlunit-core/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dir (string-append (getcwd) "/../test-resources/")))
               (with-directory-excursion (assoc-ref inputs "resources")
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*"))))))
         (add-before 'check 'disable-non-tests
           (lambda _
             ;; not really tests
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/Abstract*Test.java\" />"))))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("mockito" ,java-mockito-1)
       ("hamcrest" ,java-hamcrest-all)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)
       ("cglib" ,java-cglib)
       ("resources" ,java-xmlunit-test-resources)))
    (home-page "https://github.com/xmlunit/xmlunit")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-xmlunit-legacy
  (package
    (inherit java-xmlunit)
    (name "java-xmlunit-legacy")
    (arguments
     `(#:jar-name "java-xmlunit-legacy.jar"
       #:source-dir "xmlunit-legacy/src/main/java"
       #:test-dir "xmlunit-legacy/src/test"))
    (inputs
     `(("xmlunit" ,java-xmlunit)
       ("junit" ,java-junit)))
    (native-inputs
     `(("mockito" ,java-mockito-1)))))

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
                "13vbshq61cb6r37yb87rky1q16dzh5l76l9iiskgbzqpzp4igjk5"))))
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
             (chdir "spring-core")))
         (add-before 'configure 'rename-dep
           (lambda _
             (substitute* "src/main/java/org/springframework/objenesis/SpringObjenesis.java"
               (("org.springframework.objenesis") "org.objenesis"))))
         (add-before 'configure 'add-import
           (lambda _
             (substitute* "src/main/java/org/springframework/cglib/core/SpringNamingPolicy.java"
               (("public class")
                "import net.sf.cglib.core.DefaultNamingPolicy;\npublic class"))))
         (add-before 'check 'remove-log4j-1-dep
           (lambda _
             ;; this tests requires log4j-1 (not log4j-1.2-api)
             (delete-file "src/test/java/org/springframework/util/MockLog4jAppender.java")
             (delete-file "src/test/java/org/springframework/util/Log4jConfigurerTests.java")))
         (add-before 'check 'select-tests
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Tests.java\" />"
                               ;; these tests fail
                               "<exclude name=\"**/LocalVariableTableParameterNameDiscovererTests.java\" />"
                               "<exclude name=\"**/StandardReflectionParameterNameDiscoverTests.java\" />"
                               "<exclude name=\"**/SpringFactoriesLoaderTests.java\" />"
                               "<exclude name=\"**/PropertySourceTests.java\" />"
                               "<exclude name=\"**/StaxEventXMLReaderTests.java\" />"
                               "<exclude name=\"**/StaxStreamHandlerTests.java\" />"
                               ;; Unable to set MockitoNamingPolicy on cglib generator which creates FastClasses
                               "<exclude name=\"**/util/StreamUtilsTests.java\" />"
                               "<exclude name=\"**/Abstract*.java\" />")))))
         (add-before 'check 'copy-test-resources
           (lambda* (#:key inputs #:allow-other-keys)
             (let ((dir (string-append (getcwd) "/build/test-classes/")))
               (with-directory-excursion "src/test/resources"
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*")))))))))
    (inputs
     `(("logging" ,java-commons-logging-minimal)
       ("java-jopt-simple" ,java-jopt-simple)
       ("java-commons-codec" ,java-commons-codec)
       ("java-log4j-1.2-api" ,java-log4j-1.2-api)
       ("objenesis" ,java-objenesis)
       ("cglib" ,java-cglib)
       ("weaver" ,java-aspectj-weaver)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-all)
       ("asm" ,java-asm)
       ("java-jboss-annotations-api-spec" ,java-jboss-annotations-api-spec)
       ("java-xmlunit-legacy" ,java-xmlunit-legacy)
       ("java-xmlunit" ,java-xmlunit)
       ("java-mockito" ,java-mockito-1)))
    (build-system ant-build-system)
    (home-page "https://projects.spring.io/spring-framework/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;(define-public java-spring-framework-web
;  (package (inherit java-spring-framework-core)
;    (name "java-spring-framework-web")
;    (arguments
;     `(#:jar-name "java-spring-framework-web.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "spring-web/src/main/java"
;       #:test-dir "spring-web/src/test"))
;    (inputs
;     `(("core" ,java-spring-framework-core)
;       ("aspectj" ,java-aspectj-weaver)
;       ("tomcat" ,java-tomcat)
;       ("logging" ,java-commons-logging-minimal)))
;    (native-inputs '())))
;
;(define-public java-spring-framework-test
;  (package (inherit java-spring-framework-core)
;    (name "java-spring-framework-test")
;    (arguments
;     `(#:jar-name "java-spring-framework-test.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "spring-test/src/main/java"
;       #:test-dir "spring-test/src/test"))
;    (inputs
;     `(("core" ,java-spring-framework-core)
;       ("http" ,java-spring-framework-web)
;       ("aspectj" ,java-aspectj-weaver)
;       ("tomcat" ,java-tomcat)
;       ("logging" ,java-commons-logging-minimal)))
;    (native-inputs '())))
;
;(define-public java-spring-framework-aop
;  (package (inherit java-spring-framework-core)
;    (name "java-spring-framework-aop")
;    (arguments
;     `(#:jar-name "java-spring-framework-aop.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "spring-aop/src/main/java"
;       #:test-dir "spring-aop/src/test"))
;    (inputs
;     `(("core" ,java-spring-framework-core)
;       ("beans" ,java-spring-framework-beans)
;       ("aspectj" ,java-aspectj-weaver)
;       ("logging" ,java-commons-logging-minimal)))
;    (native-inputs '())))
;
;(define-public java-spring-framework-beans
;  (package (inherit java-spring-framework-core)
;    (name "java-spring-framework-beans")
;    (arguments
;     `(#:jar-name "java-spring-framework-beans.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "spring-beans/src/main/java"
;       #:test-dir "spring-beans/src/test"
;       #:phases
;       (modify-phases %standard-phases
;         (add-before 'configure 'fix-dep
;           (lambda _
;             (substitute* "spring-beans/src/main/java/org/springframework/beans/factory/support/CglibSubclassingInstantiationStrategy.java"
;               (("org.springframework.cglib.proxy") "net.sf.cglib.proxy")
;               (("org.springframework.cglib.core.ClassGenerator")
;                "net.sf.cglib.core.ClassGenerator")
;               (("org.springframework.cglib.core.DefaultGeneratorStrategy")
;                "net.sf.cglib.core.DefaultGeneratorStrategy")))))))
;    (inputs
;     `(("core" ,java-spring-framework-core)
;       ("cglib" ,java-cglib)
;       ("inject" ,java-javax-inject)
;       ("snakeyaml" ,java-snakeyaml-notests)
;       ("el" ,java-jboss-el-api-spec)
;       ("aspectj" ,java-aspectj-weaver)
;       ("logging" ,java-commons-logging-minimal)))
;    (native-inputs
;     `(("junit" ,java-junit)
;       ("hamcrest" ,java-hamcrest-all)
;       ("test" ,java-spring-framework-test)
;       ("mockito" ,java-mockito-1)))))
;
;(define-public java-spring-framework-context
;  (package (inherit java-spring-framework-core)
;    (name "java-spring-framework-context")
;    (arguments
;     `(#:jar-name "java-spring-framework-context.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "spring-context/src/main/java"
;       #:test-dir "spring-context/src/test"))
;    (inputs
;     `(("java-aopalliance" ,java-aopalliance)
;       ("java-commons-logging" ,java-commons-logging-minimal)
;       ("java-spring-framework-aop" ,java-spring-framework-aop)))
;    (native-inputs '())))


(define-public java-snakeyaml
  (package
    (name "java-snakeyaml")
    (version "1.18")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://bitbucket.org/asomov/snakeyaml/get/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0rf5ha6w0waz50jz2479jsrbgmd0dnx0gs337m126j5z7zlmg7mg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-snakeyaml.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("joda-time" ,java-joda-time)
       ("java-velocity" ,java-velocity)))
    ;; FIXME: requires java-spring-framework-context
    (home-page "https://bitbucket.org/asomov/snakeyaml")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

(define java-snakeyaml-notests
  (package (inherit java-snakeyaml)
    (native-inputs '())
    (arguments
      `(#:tests? #f
        ,@(package-arguments java-snakeyaml)))))

(define-public java-ops4j-lang
  (package
    (name "java-ops4j-lang")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ops4j/org.ops4j.base/"
                                  "archive/base-" version ".tar.gz"))
              (sha256
               (base32
                "18hl3lpchgpv8yh5rlk39l2gif5dlfgb8gxjmncf39pr2dprkniw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-ops4j-lang.jar"
       #:source-dir "ops4j-base-lang/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-test-file
           (lambda _
             ;; That file is required by a test in ops4j-pax-exam-core-spi
             (mkdir-p "build/classes/META-INF/maven/org.ops4j.base/ops4j-base-lang")
             (with-output-to-file "build/classes/META-INF/maven/org.ops4j.base/ops4j-base-lang/pom.properties"
               (lambda _
                 (display
                   (string-append
                     "This jar was not created by maven!\n"
                     "This file was created to satisfy a test dependency in "
                     "ops4j-pax-exam-core-spi.\n")))))))))
    (home-page "https://ops4j1.jira.com/wiki/spaces/base/overview")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-ops4j-monitors
  (package
    (inherit java-ops4j-lang)
    (name "java-ops4j-monitors")
    (arguments
     `(#:jar-name "java-ops4j-monitors.jar"
       #:source-dir "ops4j-base-monitors/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("lang" ,java-ops4j-lang)))))

(define-public java-ops4j-io
  (package
    (inherit java-ops4j-lang)
    (name "java-ops4j-io")
    (arguments
     `(#:jar-name "java-ops4j-io.jar"
       #:source-dir "ops4j-base-io/src/main/java"
       #:test-dir "ops4j-base-io/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'disable-failing
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/ListerTest.java\" />"))))))))
    (inputs
     `(("lang" ,java-ops4j-monitors)
       ("lang" ,java-ops4j-lang)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))))

(define-public java-ops4j-util
  (package
    (inherit java-ops4j-lang)
    (name "java-ops4j-util")
    (arguments
     `(#:jar-name "java-ops4j-util.jar"
       #:source-dir "ops4j-base-util/src/main/java"
       #:test-dir "ops4j-base-util/src/test"))
    (inputs
     `(("lang" ,java-ops4j-lang)))
    (native-inputs
     `(("junit" ,java-junit)))))

(define-public java-ops4j-util-property
  (package
    (inherit java-ops4j-lang)
    (name "java-ops4j-util-property")
    (arguments
     `(#:jar-name "java-ops4j-util-property.jar"
       #:source-dir "ops4j-base-util-property/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("lang" ,java-ops4j-lang)
       ("util" ,java-ops4j-util)))))

(define-public java-ops4j-store
  (package
    (inherit java-ops4j-lang)
    (name "java-ops4j-store")
    (arguments
     `(#:jar-name "java-ops4j-store.jar"
       #:source-dir "ops4j-base-store/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("lang" ,java-ops4j-lang)
       ("slf4j" ,java-slf4j-api)
       ("io" ,java-ops4j-io)))))

(define-public java-ops4j-spi
  (package
    (inherit java-ops4j-lang)
    (name "java-ops4j-spi")
    (arguments
     `(#:jar-name "java-ops4j-spi.jar"
       #:source-dir "ops4j-base-spi/src/main/java"
       #:test-dir "ops4j-base-spi/src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))))

(define java-asm-old
  (package
    (inherit java-asm)
    (version "3.3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://download.forge.ow2.org/asm/asm-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1xcp06wbqqq4jm93pafjw5jc08vy30qiw9s7kff9gmw23ka279b9"))))))

(define-public java-microemulator
  (package
    (name "java-microemulator")
    (version "2.0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://sourceforge/microemulator/microemulator/"
                                  version "/microemulator-" version ".zip"))
              (sha256
               (base32
                "0x9a4xqw6747c130y2znfwg945jgpjnd4bzj5gdamxmi7848dslb"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "microemulator.jar"
       #:source-dir "src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'unpack-jar
           (lambda _
             (mkdir-p "src")
             (with-directory-excursion "src"
               (zero? (system* "jar" "xf" "../microemulator-sources.jar")))))
         (add-before 'configure 'remove-old-dep
           (lambda _
             ;; requires netscape.javascript for which I can't find a free implementation
             (delete-file "src/org/microemu/applet/CookieRecordStoreManager.java")
             ;; requires an old version of swt
             (delete-file "src/org/microemu/app/Swt.java"))))))
    (inputs
     `(("java-swt" ,java-swt)
       ("asm" ,java-asm-old)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://sourceforge.net/projects/microemulator/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-cmpn
  (package
    (name "java-osgi-cmpn")
    (version "6.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/"
                                  "org/osgi/osgi.cmpn/" version "/osgi.cmpn-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1lmb6xyrmkqdhv1kayf0514rlwq6ypvs4m44ibrck3snp8241wys"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-cmpn.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("core" ,java-osgi-core)
       ("java-datanucleus-javax-persistence" ,java-datanucleus-javax-persistence)
       ("microemulator" ,java-microemulator)
       ("tomcat" ,java-tomcat)))
    (home-page "http://www.osgi.org")
    (synopsis "Compendium specification module of OSGi framework")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the compendium specification module, providing interfaces and classes for use
in compiling bundles.")
    (license license:asl2.0)))

(define-public java-osgi-service-component-annotations
  (package
    (name "java-osgi-service-component-annotations")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.component.annotations/"
                                  version "/org.osgi.service.component.annotations-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "15rq9cmp4fpn74q44m4j35qsqmjf5lx3hcrk6pzvbhc08igic2f0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-component-annotations.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "http://www.osgi.org")
    (synopsis "Support annotations for osgi-service-component")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the support annotations for osgi-service-component.")
    (license license:asl2.0)))

(define-public java-osgi-dto
  (package
    (name "java-osgi-dto")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.dto/" version "/org.osgi.dto-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0f4bqjzadn0hwk6sd3h5gvbyfp3yci1s6r0v770cc15p0pg627yr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-resource.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-resource
  (package
    (name "java-osgi-resource")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.resource/"
                                  version "/org.osgi.resource-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0hi0fsc5v99q22bd7lrkvpz1y0ds4w9arjldpwsrcpqvz2js7q2d"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-resource.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("dto" ,java-osgi-dto)))
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-namespace-contract
  (package
    (name "java-osgi-namespace-contract")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.namespace.contract/"
                                  version "/org.osgi.namespace.contract-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1iz4f2i0fvqrlq90ki9nfzcfpvy2av434ri25bglywqssx8mmp36"))))
    (build-system ant-build-system)
    (inputs
     `(("resource" ,java-osgi-resource)
       ("annotation" ,java-osgi-annotation)))
    (arguments
     `(#:jar-name "osgi-namespace-contract.jar"
       #:tests? #f)); no tests
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-namespace-extender
  (package
    (name "java-osgi-namespace-extender")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.namespace.extender/"
                                  version "/org.osgi.namespace.extender-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0jgqiak2i05qv6j3gd33xlaifzzc0ylxxk376v2x0apfg3vvixmz"))))
    (build-system ant-build-system)
    (inputs
     `(("resource" ,java-osgi-resource)
       ("annotation" ,java-osgi-annotation)))
    (arguments
     `(#:jar-name "osgi-namespace-contract.jar"
       #:tests? #f)); no tests
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-namespace-service
  (package
    (name "java-osgi-namespace-service")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.namespace.service/"
                                  version "/org.osgi.namespace.service-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0qmw8n2449nkmm56d1znz9zhazb6ya3vsimd5bf5jg23zzhgl8c8"))))
    (build-system ant-build-system)
    (inputs
     `(("resource" ,java-osgi-resource)
       ("annotation" ,java-osgi-annotation)))
    (arguments
     `(#:jar-name "osgi-namespace-contract.jar"
       #:tests? #f)); no tests
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-util-function
  (package
    (name "java-osgi-util-function")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.util.function/"
                                  version "/org.osgi.util.function-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "04l7j3hwmmj28w23m7paca0afzncs42j2mdr3liqq8kvp548sc6x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-util-function.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-util-promise
  (package
    (name "java-osgi-util-promise")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.util.promise/"
                                  version "/org.osgi.util.promise-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0y34dwiflg1c4ahvkswpf9z02xph2sr9fm04ia5493x3lshpw22c"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-util-promise.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("function" ,java-osgi-util-function)))
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-service-metatype-annotations
  (package
    (name "java-osgi-service-metatype-annotations")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.metatype.annotations/"
                                  version "/org.osgi.service.metatype.annotations-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "12rwm3349wk80vm88rcdgs4435m4jxkpkj5mrx326skkz2c6hyw6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-metatype-annotations.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)))
    (home-page "http://www.osgi.org")
    (synopsis "Support annotations for metatype")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the support annotations for metatype.")
    (license license:asl2.0)))

(define-public java-osgi-service-repository
  (package
    (name "java-osgi-service-repository")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.repository/"
                                  version "/org.osgi.service.repository-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1k41mhg7b58pd8nsghr2qwcjrxdnf1p9spsw9v11k4257g6rl06n"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-repository.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("promise" ,java-osgi-util-promise)
       ("resource" ,java-osgi-resource)))
    (home-page "http://www.osgi.org")
    (synopsis "service-repository")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the service-repository.")
    (license license:asl2.0)))

(define-public java-osgi-framework
  (package
    (name "java-osgi-framework")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.framework/" version "/org.osgi.framework-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1lwp2zfad3rybcc6q9bwz8xsgkc92ypzy5p6x54387f1qj65m73s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-framework.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("resource" ,java-osgi-resource)
       ("dto" ,java-osgi-dto)))
    (home-page "http://www.osgi.org")
    (synopsis "OSGi framework")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.")
    (license license:asl2.0)))

(define-public java-osgi-service-log
  (package
    (name "java-osgi-service-log")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.log/"
                                  version "/org.osgi.service.log-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1029j30dzcwializzca0j3fkhwwz08kmmsha5agw1iccscimj6r0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-log.jar"
       #:tests? #f)); no tests
    (inputs
     `(("java-osgi-framework" ,java-osgi-framework)))
    (home-page "http://www.osgi.org")
    (synopsis "service-log")
    (description
      "OSGi, for Open Services Gateway initiative framework, is a module system
and service platform for the Java programming language.  This package contains
the service-log.")
    (license license:asl2.0)))

(define-public java-osgi-service-jdbc
  (package
    (name "java-osgi-service-jdbc")
    (version "1.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.jdbc/"
                                  version "/org.osgi.service.jdbc-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "11iln5v7bk469cgb9ddkrz9sa95b3733gqgaqw9xf5g6wq652yjz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-jdbc.jar"
       #:tests? #f)); no tests
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description
      "")
    (license license:asl2.0)))

(define-public java-osgi-service-resolver
  (package
    (name "java-osgi-service-resolver")
    (version "1.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.resolver/"
                                  version "/org.osgi.service.resolver-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1dzqn1ryfi2rq4zwsgp44bmj2wlfydjg1qbxw2b0z4xdjjy55vxd"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-resolver.jar"
       #:tests? #f)); no tests
    (inputs
     `(("annotation" ,java-osgi-annotation)
       ("resource" ,java-osgi-resource)))
    (home-page "http://www.osgi.org")
    (synopsis "Support annotations for osgi-service-component")
    (description "")
    (license license:asl2.0)))

(define-public java-osgi-util-tracker
  (package
    (name "java-osgi-util-tracker")
    (version "1.5.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.util.tracker/"
                                  version "/org.osgi.util.tracker-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "0c4fh9vxwzsx59r8dygda0gq2gx3z5vfhc3jsphlqwf5w0h403lz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-util-tracker.jar"
       #:tests? #f)); no tests
    (inputs
     `(("framework" ,java-osgi-framework)
       ("annotation" ,java-osgi-annotation)))
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description
      "")
    (license license:asl2.0)))

(define-public java-osgi-service-cm
  (package
    (name "java-osgi-service-cm")
    (version "1.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/org/osgi/"
                                  "org.osgi.service.cm/"
                                  version "/org.osgi.service.cm-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "1z8kap48y3xi0ggj8v6czglfnpnd94mmismgi2wbqhj1nl5fzbp6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "osgi-service-cm.jar"
       #:tests? #f)); no tests
    (inputs
     `(("framework" ,java-osgi-framework)
       ("annotation" ,java-osgi-annotation)))
    (home-page "http://www.osgi.org")
    (synopsis "")
    (description
      "")
    (license license:asl2.0)))

(define-public java-aqute-bndlib
  (package
    (name "java-aqute-bndlib")
    (version "3.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/bndtools/bnd/archive/"
                                  version ".REL.tar.gz"))
              (sha256
               (base32
                "158c9250v1q07hvj6v30lja4gq1s3y0v94j281rghz82lilwzb07"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-bndlib.jar"
       #:source-dir "biz.aQute.bndlib/src"
       #:tests? #f)); no tests
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("osgi-annot" ,java-osgi-annotation)
       ("java-aqute-libg" ,java-aqute-libg)
       ("java-aqute-bnd-annotation" ,java-aqute-bnd-annotation)
       ("java-osgi-service-component-annotations" ,java-osgi-service-component-annotations)
       ("java-osgi-service-repository" ,java-osgi-service-repository)
       ("java-osgi-service-log" ,java-osgi-service-log)
       ("java-osgi-service-metatype-annotations" ,java-osgi-service-metatype-annotations)
       ("java-osgi-namespace-contract" ,java-osgi-namespace-contract)
       ("java-osgi-namespace-extender" ,java-osgi-namespace-extender)
       ("java-osgi-namespace-service" ,java-osgi-namespace-service)
       ("promise" ,java-osgi-util-promise)
       ("osgi" ,java-osgi-core)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-aqute-libg
  (package
    (inherit java-aqute-bndlib)
    (name "java-aqute-libg")
    (arguments
     `(#:jar-name "java-aqute-libg.jar"
       #:source-dir "aQute.libg/src"
       #:tests? #f)); actually in "aQute.libg/test", not in .../java
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("osgi-annot" ,java-osgi-annotation)
       ("java-osgi-cmpn" ,java-osgi-cmpn)
       ("osgi" ,java-osgi-core)))))

(define-public java-aqute-bnd-annotation
  (package
    (inherit java-aqute-bndlib)
    (name "java-aqute-bnd-annotation")
    (arguments
     `(#:jar-name "java-aqute-bnd-annotation.jar"
       #:source-dir "biz.aQute.bnd.annotation/src"
       #:tests? #f)); empty test dir
    (inputs '())))

(define-public java-ops4j-pax-tinybundles
  (package
    (name "java-ops4j-pax-tinybundles")
    (version "2.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ops4j/org.ops4j.pax.tinybundles/"
                                  "archive/tinybundles-" version ".tar.gz"))
              (sha256
               (base32
                "0y0gq3pvv0iir2b885lmlwnvr724vv7vklzhhr4fs27d7mdkj871"))))
    (arguments
     `(#:jar-name "java-ops4j-pax-tinybundles.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-version
           (lambda _
             ;; I think the test has a hard reference to an old version of bndlib
             ;; we are not using. This is the version referenced in the pom.xml.
             (substitute* "src/test/java/org/ops4j/pax/tinybundles/bnd/BndTest.java"
               (("2.4.0.201411031534") "3.4.0"))
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               ;; Base classes for other tests
                               "<exclude name=\"**/BndTest.java\" />"
                               "<exclude name=\"**/CoreTest.java\" />"))))))))
    (inputs
     `(("lang" ,java-ops4j-lang)
       ("io" ,java-ops4j-io)
       ("store" ,java-ops4j-store)
       ("slf4j" ,java-slf4j-api)
       ("libg" ,java-aqute-libg)
       ("bndlib" ,java-aqute-bndlib)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("log4j" ,java-log4j-api)
       ("bndannotation" ,java-aqute-bnd-annotation)
       ("framework" ,java-osgi-framework)))
    (build-system ant-build-system)
    (home-page "https://ops4j1.jira.com/wiki/spaces/ops4j/pages/12060312/Tinybundles")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-ops4j-pax-exam-core
  (package
    (name "java-ops4j-pax-exam-core")
    (version "4.11.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/ops4j/org.ops4j.pax.exam2/"
                                  "archive/exam-reactor-" version ".tar.gz"))
              (sha256
               (base32
                "0vk87df6m4shhqwd0wmkjklhnnqy98cxbhns06wzvb8rfgcl6wp5"))))
    (arguments
     `(#:jar-name "java-ops4j-pax-exam-core.jar"
       #:source-dir "core/pax-exam/src/main/java"
       #:test-dir "core/pax-exam/src/test"))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("lang" ,java-ops4j-lang)
       ("io" ,java-ops4j-io)
       ("util-property" ,java-ops4j-util-property)
       ("util-store" ,java-ops4j-store)
       ("java-osgi-core" ,java-osgi-core)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (build-system ant-build-system)
    (home-page "https://ops4j1.jira.com/wiki/spaces/PAXEXAM4/overview")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-ops4j-pax-exam-core-spi
  (package
    (inherit java-ops4j-pax-exam-core)
    (name "java-ops4j-pax-exam-core-spi")
    (arguments
     `(#:jar-name "java-ops4j-pax-exam-spi.jar"
       #:source-dir "core/pax-exam-spi/src/main/java"
       #:test-dir "core/pax-exam-spi/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (mkdir-p "build/classes/META-INF/maven/org.ops4j.pax.exam/pax-exam-spi")
             (with-output-to-file "build/classes/META-INF/maven/org.ops4j.pax.exam/pax-exam-spi/pom.properties"
               (lambda _
                 (display "This jar was not created by maven!\n")))
             (substitute* "core/pax-exam-spi/src/test/java/org/ops4j/pax/exam/spi/war/FileFinderTest.java"
               (("src/main") "core/pax-exam-spi/src/main")
               (("src/test") "core/pax-exam-spi/src/test")
               (("\"src\"") "\"core/pax-exam-spi/src\""))
             (substitute* "core/pax-exam-spi/src/test/java/org/ops4j/pax/exam/spi/war/JarBuilderTest.java"
               (("src/test") "core/pax-exam-spi/src/test"))
             (substitute* "core/pax-exam-spi/src/test/java/org/ops4j/pax/exam/spi/war/WarBuilderTest.java"
               (("src/test") "core/pax-exam-spi/src/test")
               (("target/") "build/"))
             (substitute* "core/pax-exam-spi/src/test/java/org/ops4j/pax/exam/spi/war/WarTestProbeBuilderTest.java"
               (("src/test") "core/pax-exam-spi/src/test")
               (("target/") "build/"))
             (substitute* "core/pax-exam-spi/src/test/java/org/ops4j/pax/exam/spi/war/ZipBuilderTest.java"
               (("target") "build"))
             (substitute* "core/pax-exam-spi/src/test/java/org/ops4j/pax/exam/spi/reactors/BaseStagedReactorTest.java"
               (("AssertionError") "IllegalArgumentException"))
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               ;; Base class, not a test
                               "<exclude name=\"**/BaseStagedReactorTest.java\" />"
                               ;; Depends on org.mortbay.jetty.testwars:test-war-dump
                               "<exclude name=\"**/WarBuilderTest.java\" />"))))))))
    (inputs
     `(("java-ops4j-pax-exam-core" ,java-ops4j-pax-exam-core)
       ("lang" ,java-ops4j-lang)
       ("monitors" ,java-ops4j-monitors)
       ("store" ,java-ops4j-store)
       ("io" ,java-ops4j-io)
       ("spi" ,java-ops4j-spi)
       ("osgi" ,java-osgi-core)
       ("slf4j" ,java-slf4j-api)
       ("tinybundles" ,java-ops4j-pax-tinybundles)))
    (native-inputs
     `(("mockito" ,java-mockito-1)
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("cglib" ,java-cglib)
       ("objenesis" ,java-objenesis)
       ("asm" ,java-asm)))))

(define-public java-ops4j-pax-exam-core-junit
  (package
    (inherit java-ops4j-pax-exam-core)
    (name "java-ops4j-pax-exam-core-junit")
    (arguments
     `(#:jar-name "ops4j-pax-exam-core-junit.jar"
       #:source-dir "drivers/pax-exam-junit4/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("junit" ,java-junit)
       ("slf4j" ,java-slf4j-api)
       ("core" ,java-ops4j-pax-exam-core)
       ("spi" ,java-ops4j-pax-exam-core-spi)))
    (native-inputs '())))

(define-public java-fasterxml-jackson-dataformat-yaml
  (package
    (name "java-fasterxml-jackson-dataformat-yaml")
    (version "2.8.9")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/FasterXML/"
                                  "jackson-dataformat-yaml/archive/"
                                  "jackson-dataformat-yaml-" version ".tar.gz"))
              (sha256
               (base32
                "1qsqkbm9myq2bgqp58b64i2791gcl9njvrgfsy0kdmfhsxy975a9"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jackson-dataformat-yaml.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'generate-PackageVersion.java
           (lambda _
             (let* ((out "src/main/java/com/fasterxml/jackson/dataformat/yaml/PackageVersion.java")
                    (in (string-append out ".in")))
               (copy-file in out)
               (substitute* out
                 (("@package@") "com.fasterxml.jackson.dataformat.yaml")
                 (("@projectversion@") ,version)
                 (("@projectgroupid@") "com.fasterxml.jackson.dataformat.yaml")
                 (("@projectartifactid@") "jackson-dataformat-yaml")))))
         (add-before 'check 'activate-all-tests
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               "<exclude name=\"**/failing/**.java\" />"))))))))
    (inputs
     `(("java-fasterxml-jackson-annotations" ,java-fasterxml-jackson-annotations)
       ("java-fasterxml-jackson-core" ,java-fasterxml-jackson-core)
       ("java-fasterxml-jackson-databind" ,java-fasterxml-jackson-databind)
       ("java-snakeyaml" ,java-snakeyaml-notests)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("java-ops4j-pax-exam-core-spi" ,java-ops4j-pax-exam-core-spi)
       ("java-ops4j-pax-exam-core-junit" ,java-ops4j-pax-exam-core-junit)
       ("java-ops4j-pax-exam" ,java-ops4j-pax-exam-core)))
    (home-page "https://github.com/FasterXML/jackson-dataformat-yaml")
    (synopsis "")
    (description "")
    (license license:asl2.0))); found on wiki.fasterxml.com/JacksonLicensing

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
     `(("junit" ,java-junit)))
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
     `(("lucene" ,java-lucene-core)))))

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
     `(("lucene" ,java-lucene-core)
       ("queries" ,java-lucene-queries)))))

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
     `(("lucene" ,java-lucene-core)
       ("sandbox" ,java-lucene-sandbox)
       ("queries" ,java-lucene-queries)))))

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
     `(("junit" ,java-junit)))
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
     `(("osgi" ,java-osgi-framework)
       ("tomcat" ,java-tomcat)
       ("jts" ,java-jts)
       ("lucene" ,java-lucene-core)
       ("lucene-queryparser" ,java-lucene-queryparser)
       ("slf4j" ,java-slf4j-api)
       ("osgi-service-jdbc" ,java-osgi-service-jdbc)))
    (home-page "http://h2database.com")
    (synopsis "")
    (description "")
    (license '(license:mpl2.0 license:epl1.0))))

(define-public java-commons-csv
  (package
    (name "java-commons-csv")
    (version "1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/csv/source/"
                                  "commons-csv-" version "-src.tar.gz"))
              (sha256
               (base32
                "1l89m0fm2s3xx3v3iynvangymfg2vlyngaj6fgsi457nmsw7m7ij"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-csv.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); FIXME: requires java-h2
    (inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("junit" ,java-junit)))
    (home-page "https://commons.apache.org/proper/commons-csv/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-javax-inject
  (package
    (name "java-javax-inject")
    (version "tck-1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/javax-inject/javax-inject/"
                                  "archive/javax.inject-" version ".tar.gz"))
              (sha256
               (base32
                "1ydrlvh2r7vr1g7lhjwy3w2dggpj9h6pix1lakkkgdywb365n6g0"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-javax-inject.jar"
       #:jdk ,icedtea-8
       #:tests? #f)); no tests
    (home-page "http://github.org/javax-inject/javax-inject")
    (synopsis "JSR-330: Dependency Injection for Java")
    (description "This package specifies a means for obtaining objects in such
a way as to maximize reusability, testability and maintainability compared to
traditional approaches such as constructors, factories, and service locators
(e.g., JNDI).  This process, known as dependency injection, is beneficial to
most nontrivial applications.

Many types depend on other types.  For example, a @var{Stopwatch} might depend
on a @var{TimeSource}.  The types on which a type depends are known as its
dependencies.  The process of finding an instance of a dependency to use at run
time is known as resolving the dependency.  If no such instance can be found,
the dependency is said to be unsatisfied, and the application is broken.")
    (license license:asl2.0)))

(define-public java-aopalliance
  (package
    (name "java-aopalliance")
    (version "1.0")
    (source (origin
              (method git-fetch)
              ;; Note: this git repository is not official, but contains the
              ;; source code that is in the CVS repository.  Downloading the
              ;; tarball from sourceforge is undeterministic, and the cvs download
              ;; fails.
              (uri (git-reference
                     (url "https://github.com/hoverruan/aopalliance")
                     (commit "0d7757ae204e5876f69431421fe9bc2a4f01e8a0")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0rsg2b0v3hxlq2yk1i3m2gw3xwq689j3cwx9wbxvqfpdcjbca0qr"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-aopalliance.jar"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:source-dir "aopalliance/src/main"))
    (home-page "http://aopalliance.sourceforge.net")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-guice
  (package
    (name "java-guice")
    (version "4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/guice/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0dwmqjzlavb144ywqqglj3h68hqszkff8ai0a42hyb5il0qh4rbp"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-guice.jar"
       #:jdk ,icedtea-8
       #:tests? #f; FIXME: tests are not in a java sub directory
       #:source-dir "core/src"))
    (inputs
     `(("guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-aopalliance" ,java-aopalliance)
       ("java-javax-inject" ,java-javax-inject)
       ("java-asm" ,java-asm)))
    (home-page "http://github.org/google/guice")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jcommander
  (package
    (name "java-jcommander")
    (version "1.71")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cbeust/jcommander/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1f5k2ckay6qjc3d3w3d7bc0p3cx3c7n6p6zxvw1kibqdr0q98wlx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jcommander.jar"
       #:jdk ,icedtea-8
       #:tests? #f; requires testng which depends on jcommander
       #:source-dir "src/main/java"))
    (home-page "http://jcommander.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-assertj
  (package
    (name "java-assertj")
    (version "3.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/joel-costigliola/assertj-core/archive/"
                                  "assertj-core-" version ".tar.gz"))
              (sha256
               (base32
                "1kf124fxskf548rklkg86294w2x6ajqrff94rrhyqns31danqkfz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-assertj.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f)); depends on tng-junit which depends on assertj
    (inputs
     `(("cglib" ,java-cglib)
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (native-inputs
     `(("mockito" ,java-mockito-1)))
    (home-page "https://joel-costigliola.github.io/assertj/index.html")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-testng
  (package
    (name "java-testng")
    (version "6.12")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cbeust/testng/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "01j2x47wkj7n5w6gpcjfbwgc88ai5654b23lb87w7nsrj63m3by6"))))
    (build-system ant-build-system)
    (arguments
     `(#:jdk ,icedtea-8; java.util.function
       #:jar-name "java-testng.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (let ((dir (string-append (getcwd) "/build/classes/")))
               (with-directory-excursion "src/main/resources"
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*"))))))
         (add-before 'check 'copy-test-resources
           (lambda _
             (let ((dir (string-append (getcwd) "/build/test-classes/")))
               (with-directory-excursion "src/test/resources"
                 (for-each (lambda (file)
                             (mkdir-p (dirname (string-append dir file)))
                             (copy-file file (string-append dir file)))
                   (find-files "." ".*"))))))
         (replace 'check
           (lambda _
             (system* "ant" "compile-tests")
             ;; we don't have groovy
             (substitute* "src/test/resources/testng.xml"
               (("<class name=\"test.groovy.GroovyTest\" />") ""))
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":build/classes"
                                                         ":build/test-classes")
                             "-Dtest.resources.dir=src/test/resources"
                             "org.testng.TestNG" "src/test/resources/testng.xml")))))))
    (propagated-inputs
     `(("junit" ,java-junit)
       ("java-jsr305" ,java-jsr305)
       ("java-bsh" ,java-bsh)
       ("java-jcommander" ,java-jcommander)
       ("java-guice" ,java-guice)
       ("snakeyaml" ,java-snakeyaml-notests)))
    (native-inputs
     `(("guava" ,java-guava)
       ("java-javax-inject" ,java-javax-inject)
       ("java-hamcrest" ,java-hamcrest-all)
       ("java-assertj" ,java-assertj)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("aopalliance" ,java-aopalliance)))
    (home-page "http://testng.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-fest-util
  (package
    (name "java-fest-util")
    (version "1.2.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alexruiz/fest-util/"
                                  "archive/fest-util-" version ".tar.gz"))
              (sha256
               (base32
                "05g6hljz5mdaakk8d7g32klbhz9bdwp3qlj6rdaggdidxs3x1sb8"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-fest-util.jar"
       #:source-dir "src/main/java"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/alexruiz/fest-util")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; required by java-jnacl
;(define java-fest-util-1.3
;  (package
;    (inherit java-fest-util)
;    (name "java-fest-util")
;    (version "1.3.0-SNAPSHOT")
;    (source (origin
;              (method git-fetch)
;              (uri (git-reference
;                    (url "https://github.com/alexruiz/fest-util")
;                    (commit "6bca64f2673fbc255bab6c289dc65333f28734f6")))
;              (file-name (string-append name "-" version))
;              (sha256
;               (base32
;                "1gfvy0s47xvg7rf1dx174pcq9mmsrcg172sprn7s9859hcy9r8y8"))))
;    (inputs
;     `(("java-error-prone-annotations" ,java-error-prone-annotations)))))

(define-public java-fest-test
  (package
    (name "java-fest-test")
    (version "2.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alexruiz/fest-test/"
                                  "archive/fest-test-" version ".tar.gz"))
              (sha256
               (base32
                "1rxfbw6l9vc65iy1x3fb617qc6y4w2k430pgf1mfbxfdlxbm0f7g"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-fest-test.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("junit" ,java-junit)))
    (home-page "https://github.com/alexruiz/fest-test")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;(define-public java-fest-assert-1
;  (package
;    (name "java-fest-assert")
;    (version "1.4")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/alexruiz/fest-assert-1.x/"
;                                  "archive/" version ".tar.gz"))
;              (sha256
;               (base32
;                "0q4jvjydrd0pl10cp9vl18kph2wg429qallicfrqhh7mdr8dpvw5"))))
;    (inputs
;     `(("java-fest-util" ,java-fest-util-1.3)))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name "java-fest-assert.jar"
;       #:source-dir "src/main/java"))
;    (home-page "https://github.com/alexruiz/fest-assert-2.x")
;    (synopsis "")
;    (description "")
;    (license license:asl2.0)))

(define-public java-fest-assert
  (package
    (name "java-fest-assert")
    (version "2.0M10")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/alexruiz/fest-assert-2.x/"
                                  "archive/fest-assert-core-" version ".tar.gz"))
              (sha256
               (base32
                "1bi0iqavikzww6rxvz5jyg7y6bflv95s6ibryxx0xfcxrrw6i5lw"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-fest-assert.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'activate-all-tests
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               ;; Unable to set MockitoNamingPolicy on cglib
                               ;; generator which creates FastClasses
                               "<exclude name=\"**/MessageFormatter_format_Test.java\" />"
                               "<exclude name=\"**/internal/*/*_assert*_Test.java\" />"
                               ;; Not tests
                               "<exclude name=\"**/Abstract*.java\" />"
                               "<exclude name=\"**/*BaseTest.java\" />"))))))))
    (inputs
     `(("java-fest-util" ,java-fest-util)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-fest-test" ,java-fest-test)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-mockito" ,java-mockito-1)
       ("java-cglib" ,java-cglib)
       ("java-objenesis" ,java-objenesis)
       ("java-asm" ,java-asm)))
    (home-page "https://github.com/alexruiz/fest-assert-2.x")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jnacl
  (package
    (name "java-jnacl")
    (version "0.1.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/neilalexander/jnacl")
                     (commit "40c322e0a42637ab17cdf941138eeaf2494055f8")))
              (sha256
               (base32
                "1pspnmp44q61a2q4bpslpxw86rfn8s5l0xgvyrikqgdvg7ypx597"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jnacl.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* "src/test/java/com/neilalexander/jnacl/NaClTest.java"
               (("assertions.Assertions") "assertions.api.Assertions"))
             (substitute* "src/test/java/com/neilalexander/jnacl/NaclSecretBoxTest.java"
               (("assertions.Assertions") "assertions.api.Assertions"))))
         (replace 'check
           (lambda _
             (system* "ant" "compile-tests")
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":build/classes"
                                                         ":build/test-classes")
                             "org.testng.TestNG" "-testclass"
                             "build/test-classes/com/neilalexander/jnacl/NaclSecretBoxTest.class"))
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":build/classes"
                                                         ":build/test-classes")
                             "org.testng.TestNG" "-testclass"
                             "build/test-classes/com/neilalexander/jnacl/NaClTest.class")))))))
    (native-inputs
     `(("java-testng" ,java-testng)
       ("java-fest-util" ,java-fest-util)
       ("java-fest-assert" ,java-fest-assert)))
    (home-page "https://github.com/neilalexander/jnacl")
    (synopsis "")
    (description "")
    (license license:mpl2.0)))

(define-public java-jeromq
  (package
    (name "java-jeromq")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/zeromq/jeromq/archive/v"
                                  version ".tar.gz"))
              (sha256
               (base32
                "17wx8dlyqmbw77xf6d6wxnhiyky6181zpf1a48jqzz9hidz0j841"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "java-jeromq.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-failing
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/*Test.java\" />"
                               ;; Requires network
                               "<exclude name=\"**/ZBeaconTest.java\" />"
                               ;; FIXME: I don't know why it fails
                               "<exclude name=\"**/CustomDecoderTest.java\" />"
                               "<exclude name=\"**/CustomEncoderTest.java\" />"
                               ;; Not tests
                               "<exclude name=\"**/Abstract*.java\" />"))))))))
    (inputs
     `(("java-jnacl" ,java-jnacl)))
    (native-inputs
     `(("java-hamcrest-core" ,java-hamcrest-core)
       ("junit" ,java-junit)))
    (home-page "http://zeromq.org/bindings:java")
    (synopsis "")
    (description "")
    (license license:mpl2.0)))

(define-public java-log4j-core
  (package
    (inherit java-log4j-api)
    (name "java-log4j-core")
    (inputs
     `(("java-osgi-core" ,java-osgi-core)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-log4j-api" ,java-log4j-api)
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
     `(#:tests? #f ; tests require unpackaged software
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
           (lambda _ (chdir "log4j-core") #t)))))))

(define-public java-apache-felix-utils
  (package
    (name "java-apache-felix-utils")
    (version "1.10.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://apache.mindstudios.com/felix/"
                                  "org.apache.felix.utils-" version
                                  "-source-release.tar.gz"))
              (sha256
               (base32
                "11hb1in1dzgwmi27iwksl45a7nibc8mi5175jhg94yq63svcjkrh"))))
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
     `(("framework" ,java-osgi-framework)
       ("log" ,java-osgi-service-log)
       ("cm" ,java-osgi-service-cm)
       ("tracker" ,java-osgi-util-tracker)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
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
     `(("resource" ,java-osgi-resource)
       ("framework" ,java-osgi-framework)))
    (native-inputs
     `(("junit" ,java-junit)
       ("mockito" ,java-mockito-1)
       ("utils" ,java-apache-felix-utils)
       ("hamcrest" ,java-hamcrest-core)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("objenesis" ,java-objenesis)))
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
     `(("osgi-annotation" ,java-osgi-annotation)
       ("resolver" ,java-apache-felix-resolver)
       ("osgi-service-resolver" ,java-osgi-service-resolver)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("asm" ,java-asm)
       ("easymock" ,java-easymock)
       ("mockito" ,java-mockito-1)))
    (home-page "https://felix.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-log4j-1.2-api
  (package
    (inherit java-log4j-api)
    (name "java-log4j-1.2-api")
    (arguments
     `(#:jar-name "java-log4j-1.2-api.jar"
       #:source-dir "log4j-1.2-api/src/main/java"
       #:test-dir "log4j-1.2-api/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; requires maven
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'copy-classes
           (lambda _
             (mkdir-p "log4j-1.2-api/src/test/java/org/apache/logging/log4j/test/appender")
             (copy-file "log4j-core/src/test/java/org/apache/logging/log4j/test/appender/ListAppender.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/test/appender/ListAppender.java")
             (mkdir-p "log4j-1.2-api/src/test/java/org/apache/logging/log4j/junit")
             (copy-file "log4j-core/src/test/java/org/apache/logging/log4j/junit/LoggerContextRule.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/junit/LoggerContextRule.java")
             (mkdir-p "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/equinox")
             (copy-file "log4j-api/src/test/java/org/apache/logging/log4j/osgi/equinox/AbstractEquinoxLoadBundleTest.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/equinox/AbstractEquinoxLoadBundleTest.java")
             (mkdir-p "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/felix")
             (copy-file "log4j-api/src/test/java/org/apache/logging/log4j/osgi/OsgiRule.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/OsgiRule.java")
             (copy-file "log4j-api/src/test/java/org/apache/logging/log4j/osgi/BundleTestInfo.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/BundleTestInfo.java")
             (copy-file "log4j-api/src/test/java/org/apache/logging/log4j/osgi/AbstractLoadBundleTest.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/AbstractLoadBundleTest.java")
             (copy-file "log4j-api/src/test/java/org/apache/logging/log4j/osgi/felix/AbstractFelixLoadBundleTest.java"
                        "log4j-1.2-api/src/test/java/org/apache/logging/log4j/osgi/felix/AbstractFelixLoadBundleTest.java"))))))
    (inputs
     `(("log4j-api" ,java-log4j-api)
       ("log4j-core" ,java-log4j-core)
       ("osgi-core" ,java-osgi-core)
       ("eclipse-osgi" ,java-eclipse-osgi)
       ("java-lmax-disruptor" ,java-lmax-disruptor)))
    (native-inputs
     `(("junit" ,java-junit)
       ("velocity" ,java-velocity)
       ("felix" ,java-apache-felix)
       ("io" ,java-commons-io)))))

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
     `(#:tests? #f ; tests require unpackaged software
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
     `(("log" ,java-avalon-logkit)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
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
             (chdir "build")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (dir (string-append out "/share/java")))
               (mkdir-p dir)
               (copy-file "../bin/velocity-1.7.jar"
                          (string-append dir "/velocity-1.7.jar"))))))))
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

;; Older version, unmaintained, for jcs.
(define-public java-commons-httpclient
  (package
    (name "java-commons-httpclient")
    (version "3.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/httpcomponents/"
                                  "commons-httpclient/source/commons-httpclient-"
                                  version "-src.tar.gz"))
              (sha256
               (base32
                "1wlpn3cfy3d4inxy6g7wxcsa8p7sshn6aldk9y4ia3lb879rd97r"))))
    (arguments
     `(#:build-target "compile"
       #:test-target "test"
       #:tests? #f; requires junit-textui (junit 3)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-accent
           (lambda _
             (for-each (lambda (file)
                         (with-fluids ((%default-port-encoding "ISO-8859-1"))
                          (substitute* file
                            (("\\* @author Ortwin .*") "* @author Ortwin Gluck\n"))))
               '("src/java/org/apache/commons/httpclient/HttpContentTooLargeException.java"
                 "src/examples/TrivialApp.java" "src/examples/ClientApp.java"
                 "src/test/org/apache/commons/httpclient/TestHttps.java"
                 "src/test/org/apache/commons/httpclient/TestURIUtil2.java"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (zero? (system* "ant" "dist"
                             (string-append "-Ddist.home=" (assoc-ref outputs "out")
                                            "/share/java"))))))))
    (propagated-inputs
     `(("java-commons-logging" ,java-commons-logging-minimal)
       ("java-commons-codec" ,java-commons-codec)))
    (build-system ant-build-system)
    (home-page "https://hc.apache.org")
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
     `(("cglib" ,java-cglib)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest" ,java-hamcrest-core)
       ("asm" ,java-asm)
       ("objenesis" ,java-objenesis)))
    (home-page "https://commons.apache.org/proper/commons-pool")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-cdi-api
  (package
    (name "java-cdi-api")
    (version "2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/cdi-spec/cdi/archive/"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1iv8b8bp07c5kmqic14jsr868vycjv4qv02lf3pkgp9z21mnfg5y"))))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "api/src/main/java"
       #:jar-name "java-cdi-api.jar"
       #:test-dir "api/src/test"
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
                             "build/test-classes/org/jboss/cdi/api/test/CDITest.class")
             #t)))))
    (inputs
     `(("java-javax-inject" ,java-javax-inject)
       ("javax-el" ,java-jboss-el-api-spec)
       ("javax-interceptors" ,java-jboss-interceptors-api-spec)))
    (native-inputs
     `(("testng" ,java-testng)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "")
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
     `(("junit" ,java-junit)))
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
       #:source-dir "org.abego.treelayout/src/main/java"
       #:test-dir "org.abego.treelayout/src/test"))
    (inputs
     `(("junit" ,java-junit)))
    (native-inputs
     `(("hamcrest" ,java-hamcrest-core)))
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
       ("junit" ,java-junit)))
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

;(define-public java-guava
;  (package
;    (name "java-guava")
;    (version "20.0")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/google/guava/archive/v"
;                                  version ".tar.gz"))
;              (file-name (string-append name "-" version ".tar.gz"))
;              (sha256
;               (base32
;                "1kasavj973iblj1fj35gzbywhkljrnbjpymgqyqaibbbmmbzff8s"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name (string-append ,name "-" ,version ".jar")
;       #:source-dir "guava/src"
;       #:tests? #f))
;    (inputs
;     `(("java-jsr305" ,java-jsr305)
;       ("java-j2objc-annotations" ,java-j2objc-annotations)
;       ("java-animal-sniffer-annotations" ,java-animal-sniffer-annotations)
;       ("java-error-prone-annotations" ,java-error-prone-annotations)))
;    (home-page "https://github.com/google/guava")
;    (synopsis "")
;    (description "")
;    (license license:asl2.0)))

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
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'activate-all-tests
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/Test*.java\" />"
                               "<exclude name=\"**/test*/**.java\" />"))))))))
    (inputs
     `(("java-guava" ,java-guava)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
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
     `(#:jar-name "java-joda-time.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'build-resources
           (lambda _
             (mkdir-p "build/classes/org/joda/time/tz/data")
             (mkdir-p "build/classes/org/joda/time/format")
             ;; This will produce an exception, but it's all right.
             (zero? (system* "java" "-cp" (string-append "build/classes:" (getenv "CLASSPATH"))
                             "org.joda.time.tz.ZoneInfoCompiler"
                             "-src" "src/main/java/org/joda/time/tz/src"
                             "-dst" "build/classes/org/joda/time/tz/data"
                             "africa" "antarctica" "asia" "australasia"
                             "europe" "northamerica" "southamerica"
                             "pacificnew" "etcetera" "backward" "systemv"))
             (for-each (lambda (f)
                         (copy-file f (string-append
                                        "build/classes/org/joda/time/format/"
                                        (basename f))))
               (find-files "src/main/java/org/joda/time/format" ".*.properties"))))
         (add-before 'install 'regenerate-jar
           (lambda _
             ;; We need to regenerate the jar file to add generated data.
             (delete-file "build/jar/java-joda-time.jar")
             (zero? (system* "jar" "-cf" "build/jar/java-joda-time.jar" "-C"
                             "build/classes" "."))))
         (add-before 'check 'copy-test-resources
           (lambda _
             (mkdir-p "build/test-classes/org/joda/time/tz/data")
             (copy-file "src/test/resources/tzdata/ZoneInfoMap"
                        "build/test-classes/org/joda/time/tz/data/ZoneInfoMap")
             (for-each (lambda (file)
                         (copy-file file (string-append "build/test-classes/"
                                                        (basename file))))
               (find-files "src/test/resources/" ".*"))))
         (add-before 'check 'exclude-non-tests
           (lambda _
             (substitute* "build.xml"
               (("<include name=\"\\*\\*/\\*Test.java\" />")
                (string-append "<include name=\"**/Test*.java\" />"
                               "<exclude name=\"**/Test*Chronology.java\" />"
                               "<exclude name=\"**/Test*Field.java\" />"))))))))
    (inputs
     `(("java-joda-convert" ,java-joda-convert)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("tzdata" ,tzdata)))
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
       #:source-dir "src/main"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-parser
           (lambda _
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr/java"
               (zero? (system* "antlr4" "java.g"))))))))))


;(define-public antlr3-3.4
;  (package
;    (name "antlr3")
;    (version "3.4")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/antlr/website-antlr3/raw/"
;                                  "gh-pages/download/antlr-"
;                                  version ".tar.gz"))
;              (sha256
;               (base32
;                "1cwfswpk3jlzl1dhc6b6586srza8q0bbzwlxcq136p29v62fjrb3"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name (string-append ,name "-" ,version ".jar")
;       #:source-dir "tool/src/main/java:runtime/Java/src/main/java:tool/src/main/antlr3"
;       #:tests? #f
;       #:phases
;       (modify-phases %standard-phases
;         (add-after 'install 'bin-install
;           (lambda* (#:key inputs outputs #:allow-other-keys)
;             (let ((jar (string-append (assoc-ref outputs "out") "/share/java"))
;                   (bin (string-append (assoc-ref outputs "out") "/bin")))
;               (mkdir-p bin)
;               (with-output-to-file (string-append bin "/antlr3")
;                 (lambda _
;                   (display
;                     (string-append "#!/bin/sh\n"
;                                    "java -cp " jar "/antlr3-3.3.jar:"
;                                    (string-concatenate
;                                      (find-files (assoc-ref inputs "stringtemplate")
;                                                  ".*\\.jar"))
;                                    ":"
;                                    (string-concatenate
;                                      (find-files (string-append (assoc-ref inputs "antlr") "/lib")
;                                                  ".*\\.jar"))
;                                    " org.antlr.Tool $*"))))
;               (chmod (string-append bin "/antlr3") #o755))))
;         (add-before 'build 'generate-grammar
;           (lambda _
;             (chdir "tool/src/main/antlr3/org/antlr/grammar/v3/")
;             (for-each (lambda (file)
;                         (display file)
;                         (newline)
;                         (system* "antlr3" file))
;                       '("ActionAnalysis.g" "ActionTranslator.g" "ANTLR.g"
;                         "ANTLRTreePrinter.g" "ANTLRv3.g" "ANTLRv3Tree.g"
;                         "AssignTokenTypesWalker.g" "CodeGenTreeWalker.g"
;                         "DefineGrammarItemsWalker.g" "LeftRecursiveRuleWalker.g"
;                         "TreeToNFAConverter.g"))
;             (chdir "../../../../../../../..")
;             (system* "antlr" "-o" "tool/src/main/java/org/antlr/tool"
;                      "tool/src/main/java/org/antlr/tool/serialize.g")
;             (substitute* "tool/src/main/java/org/antlr/tool/LeftRecursiveRuleAnalyzer.java"
;               (("import org.antlr.grammar.v3.\\*;") "import org.antlr.grammar.v3.*;
;import org.antlr.grammar.v3.ANTLRTreePrinter;"))
;             (substitute* "tool/src/main/java/org/antlr/tool/Grammar.java"
;               (("import org.antlr.grammar.v3.\\*;")
;                "import org.antlr.grammar.v3.*;\n
;import org.antlr.grammar.v3.TreeToNFAConverter;\n
;import org.antlr.grammar.v3.DefineGrammarItemsWalker;\n
;import org.antlr.grammar.v3.ANTLRTreePrinter;"))
;             (substitute* "tool/src/main/java/org/antlr/tool/ErrorManager.java"
;               (("case NO_SUCH_ATTRIBUTE_PASS_THROUGH:") ""))
;             (substitute* "tool/src/main/antlr3/org/antlr/grammar/v3/ANTLRParser.java"
;               (("public Object getTree") "public GrammarAST getTree"))
;             (substitute* "tool/src/main/antlr3/org/antlr/grammar/v3/ANTLRv3Parser.java"
;               (("public Object getTree") "public CommonTree getTree"))))
;         (add-before 'build 'fix-build-xml
;           (lambda _
;             (substitute* "build.xml"
;               (("<exec") "<copy todir=\"${classes.dir}\">
;<fileset dir=\"tool/src/main/resources\">
;<include name=\"**/*.stg\"/>
;<include name=\"**/*.st\"/>
;<include name=\"**/*.sti\"/>
;<include name=\"**/STLexer.tokens\"/>
;</fileset>
;</copy><exec")))))))
;    (native-inputs
;     `(("antlr" ,antlr2)
;       ("antlr3" ,antlr3-3.3)))
;    (inputs
;     `(("junit" ,java-junit)))
;    (propagated-inputs
;     `(("stringtemplate" ,java-stringtemplate-3)
;       ("stringtemplate4" ,java-stringtemplate)
;       ("antlr" ,antlr2)
;       ("antlr3" ,antlr3-3.1)))
;    (home-page "http://www.stringtemplate.org")
;    (synopsis "")
;    (description "")
;    (license license:bsd-3)))

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
       ("stringtemplate4" ,java-stringtemplate)))
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
     `(("xmlgraphics" ,java-xmlgraphics-commons)))
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
       #:tests? #f)); FIXME: need commons-xml-resolver
    (inputs
     `(("commons-io" ,java-commons-io)
       ("commons-logging" ,java-commons-logging-minimal)))
    (native-inputs
     `(("junit" ,java-junit)
       ("mockito" ,java-mockito-1)))
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
     `(("apache-logging" ,java-commons-logging-minimal)
       ("commons-io" ,java-commons-io)
       ("xmlgraphics" ,java-xmlgraphics-commons)
       ("tomcat" ,java-tomcat)
       ("batik" ,java-batik)
       ("avalon" ,java-avalon-framework-api)
       ("avalon" ,java-avalon-logkit)))
    (native-inputs
     `(("junit" ,java-junit)))
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
     `(("io" ,java-commons-io)
       ("xmlgraphics" ,java-xmlgraphics-commons)
       ("logging" ,java-commons-logging-minimal)))))

(define-public java-fop-events
  (package
    (inherit java-fop)
    (name "java-fop-events")
    (arguments
     `(#:jar-name "fop-events.jar"
       #:source-dir "fop-events/src/main/java"
       #:test-dir "fop-events/src/test"))
    (inputs
     `(("io" ,java-commons-io)
       ("xmlgraphics" ,java-xmlgraphics-commons)
       ("logging" ,java-commons-logging-minimal)
       ("fop-util" ,java-fop-util)))))

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

(define-public java-geronimo-xbean-reflect
  (package
    (name "java-geronimo-xbean-reflect")
    (version "1807386")
    (source (origin
              (method svn-fetch)
              (uri (svn-reference
                     ;(url "https://svn.apache.org/repos/asf/geronimo/xbean/trunk/")
                     (url "https://svn.apache.org/repos/asf/geronimo/xbean/tags/xbean-4.5/")
                     (revision 1807396)))
              (sha256
               (base32
                "18q3i6jgm6rkw8aysfgihgywrdc5nvijrwnslmi3ww497jvri6ja"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "geronimo-xbean-reflect.jar"
       #:source-dir "xbean-reflect/src/main/java"
       #:test-dir "xbean-reflect/src/test"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/Abstract*.java" "**/AsmParameterNameLoaderTest.java"
                            "**/ObjectRecipeTest.java" "**/ParameterNameLoaderTest.java"
                            "**/RecipeHelperTest.java" "**/XbeanAsmParameterNameLoaderTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-source
           (lambda _
             ;; org.apache.xbean.asm6 is actually repackaged java-asm
             (substitute* "xbean-reflect/src/main/java/org/apache/xbean/recipe/XbeanAsmParameterNameLoader.java"
               (("org.apache.xbean.asm5") "org.objectweb.asm")))))))
    (inputs
     `(("asm" ,java-asm)
       ("log4j" ,java-log4j-api)
       ("log4j-1.2" ,java-log4j-1.2-api)
       ("log4j-core" ,java-log4j-core)
       ("logging" ,java-commons-logging-minimal)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-io
  (package
    (name "java-plexus-io")
    (version "3.0.0")
    (source (codehaus-plexus-origin
             "plexus-io" version
             "0f2j41kihaymxkpbm55smpxjja235vad8cgz94frfy3ppcp021dw"
             ""))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-io.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (copy-file "src/main/resources/META-INF/plexus/components.xml"
                        "build/classes/META-INF/plexus/components.xml")
             #t)))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("commons-io" ,java-commons-io)
       ("java-jsr305" ,java-jsr305)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("guava" ,java-guava)
       ("classworlds" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("container-default" ,java-plexus-container-default-bootstrap)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-iq80-snappy
  (package
    (name "java-iq80-snappy")
    (version "0.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dain/snappy/archive/snappy-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0rb3zhci7w9wzd65lfnk7p3ip0n6gb58a9qpx8n7r0231gahyamf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "container-default.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:test-exclude (list "**/Abstract*.java"
                            "**/SnappyFramedStreamTest.java"); No runnable method
       #:phases
       (modify-phases %standard-phases
         (replace 'check
           (lambda _
             (define (test class)
               (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                           ":build/classes"
                                                           ":build/test-classes")
                               "-Dtest.resources.dir=src/test/resources"
                               "org.testng.TestNG" "-testclass"
                               class)))
             (system* "ant" "compile-tests")
             (and
               (test "org.iq80.snappy.SnappyFramedStreamTest")
               (test "org.iq80.snappy.SnappyStreamTest"))))
               ;(test "org.iq80.snappy.SnappyTest"))))
         (add-before 'build 'remove-dep
           (lambda _
             ;; We don't have hadoop
             (delete-file "src/main/java/org/iq80/snappy/HadoopSnappyCodec.java")
             (delete-file "src/test/java/org/iq80/snappy/TestHadoopSnappyCodec.java")
             #t)))))
    (home-page "https://github.com/dain/snappy")
    (native-inputs
     `(("guava" ,java-guava)
       ("java-snappy" ,java-snappy)
       ("hamcrest" ,java-hamcrest-core)
       ("testng" ,java-testng)))
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-tukaani-xz
  (package
    (name "java-tukaani-xz")
    (version "1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://tukaani.org/xz/xz-java-" version ".zip"))
              (sha256
               (base32
                "1z3p1ri1gvl07inxn0agx44ck8n7wrzfmvkz8nbq3njn8r9wba8x"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'chdir
           (lambda _
             ; The package is not unzipped in a subdirectory
             (chdir "..")))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Do we want to install *Demo.jar?
             (install-file "build/jar/xz.jar"
                           (string-append
                             (assoc-ref outputs "out")
                             "/share/java/xz.jar")))))))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://tukaani.org")
    (synopsis "")
    (description "")
    (license license:public-domain)))

(define-public java-plexus-archiver
  (package
    (name "java-plexus-archiver")
    (version "3.5")
    (source (codehaus-plexus-origin
             "plexus-archiver" version
             "0iv1j7khra6icqh3jndng3iipfmkc7l5jq2y802cm8r575v75pyv"
             ""))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "container-default.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:test-exclude (list "**/Abstract*.java" "**/Base*.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-failing
           (lambda _
             ;; Requires an older version of plexus container
             (delete-file "src/test/java/org/codehaus/plexus/archiver/DuplicateFilesTest.java")))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (copy-file "src/main/resources/META-INF/plexus/components.xml"
                        "build/classes/META-INF/plexus/components.xml")
             #t)))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("commons-io" ,java-commons-io)
       ("snappy" ,java-iq80-snappy)
       ("io" ,java-plexus-io)
       ("compress" ,java-commons-compress)
       ("container-default" ,java-plexus-container-default-bootstrap)
       ("snappy" ,java-snappy)
       ("java-jsr305" ,java-jsr305)))
    (native-inputs
     `(("junit" ,java-junit)
       ("classworld" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("xz" ,java-tukaani-xz)
       ("guava" ,java-guava)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-container-default-bootstrap
  (package
    (name "java-plexus-container-default-bootstrap")
    (version "1.7.1")
    (source (codehaus-plexus-origin
             "plexus-containers" version
             "07l7wfi0kxnabd175yvbkilb26mndnba0a1g0ac1rpfagv3qpnzw"
             ""))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "container-default.jar"
       #:source-dir "plexus-container-default/src/main/java"
       #:test-dir "plexus-container-default/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; requires plexus-archiver, which depends on this package
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (copy-file "plexus-container-default/src/main/resources/META-INF/plexus/components.xml"
                        "build/classes/META-INF/plexus/components.xml")
             #t)))))
    (inputs
     `(("worldclass" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("utils" ,java-plexus-utils)
       ("junit" ,java-junit)
       ("guava" ,java-guava)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-container-default
  (package
    (inherit java-plexus-container-default-bootstrap)
    (name "java-plexus-container-default")
    (arguments
     `(#:jar-name "container-default.jar"
       #:source-dir "plexus-container-default/src/main/java"
       #:test-dir "plexus-container-default/src/test"
       #:test-exclude (list "**/*Test.java"
                            "**/ComponentRealmCompositionTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (copy-file "plexus-container-default/src/main/resources/META-INF/plexus/components.xml"
                        "build/classes/META-INF/plexus/components.xml")
             #t))
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* "plexus-container-default/src/test/java/org/codehaus/plexus/component/composition/ComponentRealmCompositionTest.java"
               (("src/test") "plexus-container-default/src/test")))))))
    (inputs
     `(("worldclass" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("utils" ,java-plexus-utils)
       ("junit" ,java-junit)
       ("guava" ,java-guava)))
    (native-inputs
     `(("archiver" ,java-plexus-archiver)
       ("hamcrest" ,java-hamcrest-core)))))

(define-public java-plexus-component-annotations
  (package
    (inherit java-plexus-container-default)
    (name "java-plexus-component-annotations")
    (arguments
     `(#:jar-name "plexus-component-annotations.jar"
       #:source-dir "plexus-component-annotations/src/main/java"
       #:tests? #f)); no tests
    (inputs '())
    (native-inputs '())))

(define-public java-plexus-component-metadata
  (package
    (inherit java-plexus-container-default)
    (name "java-plexus-component-metadata")
    (arguments
     `(#:jar-name "plexus-component-metadata.jar"
       #:source-dir "plexus-component-metadata/src/main/java"
       #:test-dir "plexus-component-metadata/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("container" ,java-plexus-container-default)
       ("annotations" ,java-plexus-component-annotations)
       ("utils" ,java-plexus-utils)
       ("classworlds" ,java-plexus-classworlds)
       ("cli" ,java-commons-cli)
       ("qdox" ,java-qdox-1.12); TODO: package latest version
       ("jdom2" ,java-jdom2)
       ("asm" ,java-asm)))
    (native-inputs '())))

(define-public java-sisu-build-api
  (package
    (name "java-sisu-build-api")
    (version "0.0.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/sonatype/sisu-build-api/"
                                  "archive/plexus-build-api-" version ".tar.gz"))
              (sha256
               (base32
                "1c3rrpma3x634xp2rm2p5iskfhzdyc7qfbhjzr70agrl1jwghgy2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "sisu-build-api.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; I don't know how to run these tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/org/sonatype/plexus/build/incremental")
             (copy-file "src/main/resources/org/sonatype/plexus/build/incremental/version.properties"
                        "build/classes/org/sonatype/plexus/build/incremental/version.properties")
             #t))
         (add-before 'build 'generate-plexus-compontent
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             ;; This file is required for plexus to inject this package.
             ;; It is absent from the source code, so maybe it is generated?
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda _
                 (display
                   (string-append
                     "<component-set>\n"
                     "  <components>\n"
                     "    <component>\n"
                     "      <role>org.sonatype.plexus.build.incremental.BuildContext</role>\n"
                     "      <role-hint>default</role-hint>\n"
                     "      <implementation>org.sonatype.plexus.build.incremental.DefaultBuildContext</implementation>\n"
                     "      <description>Filesystem based non-incremental build context implementation which behaves as if all files\n"
                     "were just created.</description>\n"
                     "    </component>\n"
                     "  </components>\n"
                     "</component-set>\n")))))))))
    (inputs
     `(("plexus-utils" ,java-plexus-utils)
       ("plexus-container-default" ,java-plexus-container-default)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-compiler-api
  (package
    (name "java-plexus-compiler-api")
    (version "2.8.2")
    (source (codehaus-plexus-origin
             "plexus-compiler" version
             "13lxk1yg8fzv4ihby1jmfjda60dkxx4rg89k9i6glddd78q1xl4h"
             ""))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-compiler-api.jar"
       #:source-dir "plexus-compiler-api/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "plexus-compiler-api/src/test"))
    (inputs
     `(("container" ,java-plexus-container-default)
       ("util" ,java-plexus-utils)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-compiler-javac
  (package
    (inherit java-plexus-compiler-api)
    (name "java-plexus-compiler-javac")
    (arguments
     `(#:jar-name "plexus-compiler-javac.jar"
       #:source-dir "plexus-compilers/plexus-compiler-javac/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; depends on compiler-test -> maven-core -> ... -> this package.
       #:test-dir "plexus-compilers/plexus-compiler-javac/src/test"))
    (inputs
     `(("api" ,java-plexus-compiler-api)
       ("utils" ,java-plexus-utils)
       ("logging" ,java-plexus-container-default)))
    (native-inputs
     `(("junit" ,java-junit)))))

(define-public java-modello-core
  (package
    (name "java-modello-core")
    (version "1.9.1")
    (source (codehaus-plexus-origin
             "modello" version
             "1nqa1arvyc84i8wn3vk08k46vf8bpqqnf5a6szdj2lc3s1yamlv8"
             "")) ;; no prefix
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "modello-core.jar"
       #:source-dir "modello-core/src/main/java"
       #:test-dir "modello-core/src/test"
       #:main-class "org.codehaus.modello.ModelloCli"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (copy-file "modello-core/src/main/resources/META-INF/plexus/components.xml"
                        "build/classes/META-INF/plexus/components.xml")
             #t))
         (add-before 'check 'fix-tests
           (lambda _
             (substitute* '("modello-core/src/test/java/org/codehaus/modello/core/DefaultModelloCoreTest.java"
                            "modello-core/src/test/java/org/codehaus/modello/core/io/ModelReaderTest.java")
               (("src/test") "modello-core/src/test")))))))
    (inputs
     `(("plexus-utils" ,java-plexus-utils)
       ("container" ,java-plexus-container-default-bootstrap)
       ("sisu" ,java-sisu-build-api)))
    (native-inputs
     `(("junit" ,java-junit)
       ("classworlds" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("guava" ,java-guava)))
    (home-page "http://codehaus-plexus.github.io/modello/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-modello-plugins-java
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-java")
    (arguments
     `(#:jar-name "modello-plugins-java.jar"
       #:source-dir "modello-plugins/modello-plugin-java/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-java/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; requires maven-model, which depends on this package
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "modello-plugins/modello-plugin-java/src/main/resources"
                               "build/classes")
             #t)))))
    (inputs
     `(("core" ,java-modello-core)
       ,@(package-inputs java-modello-core)))))

(define-public java-modello-plugins-xml
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-xml")
    (arguments
     `(#:jar-name "modello-plugins-xml.jar"
       #:source-dir "modello-plugins/modello-plugin-xml/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-xml/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "modello-plugins/modello-plugin-xml/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* "modello-plugins/modello-plugin-xml/src/test/java/org/codehaus/modello/plugins/xml/XmlModelloPluginTest.java"
               (("src/test") "modello-plugins/modello-plugin-xml/src/test")))))))
    (inputs
     `(("core" ,java-modello-core)
       ("java" ,java-modello-plugins-java)
       ,@(package-inputs java-modello-core)))))

(define-public java-modello-test
  (package
    (inherit java-modello-core)
    (name "java-modello-test")
    (arguments
     `(#:jar-name "modello-test.jar"
       #:source-dir "modello-test/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("compiler-api" ,java-plexus-compiler-api)
       ("compiler-javac" ,java-plexus-compiler-javac)
       ("container" ,java-plexus-container-default)))))

(define-public java-modello-plugins-xpp3
  (package
    (inherit java-modello-core)
    (name "java-modello-plugins-xpp3")
    (arguments
     `(#:jar-name "modello-plugins-xpp3.jar"
       #:source-dir "modello-plugins/modello-plugin-xpp3/src/main/java"
       #:test-dir "modello-plugins/modello-plugin-xpp3/src/test"
       #:tests? #f; I can find some of its dependencies, for instance org.codehaus.modello.test.features.io.xpp3.ModelloFeaturesTestXpp3Reader
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes")
             (copy-recursively "modello-plugins/modello-plugin-xpp3/src/main/resources"
                               "build/classes")
             #t)))))
    (inputs
     `(("core" ,java-modello-core)
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ,@(package-inputs java-modello-core)))
    (native-inputs
     `(("xmlunit" ,java-xmlunit)
       ("test" ,java-modello-test)
       ,@(package-native-inputs java-modello-core)))))

(define-public java-plexus-cipher
  (package
    (name "java-plexus-cipher")
    (version "1.7")
    (source (codehaus-plexus-origin
              "cipher" version
              "1j3r8xzlxlk340snkjp6lk2ilkxlkn8qavsfiq01f43xmvv8ymk3"))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-cipher.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; TODO: org.sonatype.guice.bean.containers.InjectedTestCase
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes"))))))
    (inputs
     `(("cdi-api" ,java-cdi-api)
       ("inject" ,java-javax-inject)))
    (home-page "http://codehaus-plexus.github.io/plexus-cipher/")
    (synopsis "Encryption/decryption Component")
    (description "")
    (license license:asl2.0)))

(define-public java-plexus-sec-dispatcher
  (package
    (name "java-plexus-sec-dispatcher")
    (version "1.4") ;; Newest release listed at the Maven Central Repository.
    (source (origin
              ;; This project doesn't tag releases or publish tarballs, so we take
              ;; the "prepare release plexus-sec-dispatcher-1.4" git commit.
        (method url-fetch)
        (uri (string-append "https://github.com/sonatype/plexus-sec-dispatcher/"
                            "archive/" "7db8f88048" ".tar.gz"))
        (sha256
         (base32
          "1nvlwj2090nn7f0144pyamp3lfygahlcp09dx0faqgla57lr11hj"))
        (file-name (string-append name "-" version ".tar.gz"))))
    (arguments
     `(#:jar-name "plexus-sec-dispatcher.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "src/main/java" version
                               "false" "true")))
             (let ((file "src/main/mdo/settings-security.mdo"))
               (and
               (modello-single-mode file "1.0.0" "java")
               (modello-single-mode file "1.0.0" "xpp3-reader")
               (modello-single-mode file "1.0.0" "xpp3-writer")))))
         (add-before 'check 'fix-paths
           (lambda _
             (mkdir-p "target")
             (copy-file "src/test/resources/test-sec.xml"
                        "target/sec.xml"))))))
    (inputs
     `(("cipher" ,java-plexus-cipher)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("container" ,java-plexus-container-default)
       ("classworlds" ,java-plexus-classworlds)
       ("utils" ,java-plexus-utils)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("junit" ,java-junit)))
    (build-system ant-build-system)
    (home-page "http://spice.sonatype.org/plexus-sec-dispatcher/")
    (synopsis "Plexus Security Dispatcher Component")
    (description "")
    (license license:asl2.0)))

(define-public java-eclipse-aether-api
  (package
    (name "java-eclipse-aether-api")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/aether-core/"
                                  "archive/aether-1.0.2.v20150114.tar.gz"))
              (sha256
               (base32
                "192x32hlyxs4p6xzaz1r1jrsqqr56akcl0lncq3av1zpbil6kqhh"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-aether-api.jar"
       #:source-dir "aether-api/src/main/java"
       #:test-dir "aether-api/src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://projects.eclipse.org/projects/technology.aether")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven
  (package
    (name "maven")
    (version "3.3.9")
    (source (origin 
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "1g0iavyb34kvs3jfrx2hfnr8lr11m39sj852cy7528wva1glfl4i"))
              (patches
                (search-patches "maven-generate-component-xml.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven.jar"
       #:source-dir "apache-maven"))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-artifact
  (package
    (inherit maven)
    (name "maven-artifact")
    (arguments
     `(#:jar-name "maven-artifact.jar"
       #:source-dir "maven-artifact/src/main/java"
       #:test-dir "maven-artifact/src/test"
       #:main-class "org.apache.maven.artifact.versioning.ComparableVersion"))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("junit" ,java-junit)))))

(define-public maven-model
  (package
    (inherit maven)
    (name "maven-model")
    (arguments
     `(#:jar-name "maven-model.jar"
       #:source-dir "maven-model/src/main/java"
       #:test-dir "maven-model/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "maven-model/src/main/java" version
                               "false" "true")))
             (let ((file "maven-model/src/main/mdo/maven.mdo"))
               (and
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xpp3-extended-reader"))))))))
    (inputs
     `(("lang3" ,java-commons-lang3)
       ("utils" ,java-plexus-utils)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("container" ,java-plexus-container-default)
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("junit" ,java-junit)))))

(define-public maven-settings
  (package
    (inherit maven)
    (name "maven-settings")
    (arguments
     `(#:jar-name "maven-settings.jar"
       #:source-dir "maven-settings/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "maven-settings/src/main/java" version
                               "false" "true")))
             (let ((file "maven-settings/src/main/mdo/settings.mdo"))
               (and
                 (modello-single-mode file "1.1.0" "java")
                 (modello-single-mode file "1.1.0" "xpp3-reader")
                 (modello-single-mode file "1.1.0" "xpp3-writer"))))))))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ("container" ,java-plexus-container-default)
       ("classworlds" ,java-plexus-classworlds)
       ("utils" ,java-plexus-utils)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)))))

(define-public maven-builder-support
  (package
    (inherit maven)
    (name "maven-builder-support")
    (arguments
     `(#:jar-name "maven-builder-support.jar"
       #:source-dir "maven-builder-support/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-builder-support/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* '("maven-builder-support/src/test/java/org/apache/maven/building/FileSourceTest.java"
                            "maven-builder-support/src/test/java/org/apache/maven/building/UrlSourceTest.java")
               (("target/test-classes") "maven-builder-support/src/test/resources"))
             #t)))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("lang3" ,java-commons-lang3)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))))

(define-public maven-settings-builder
  (package
    (inherit maven)
    (name "maven-settings-builder")
    (arguments
     `(#:jar-name "maven-settings-builder.jar"
       #:source-dir "maven-settings-builder/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-settings-builder/src/test"))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("annotations" ,java-plexus-component-annotations)
       ("interpolation" ,java-plexus-interpolation)
       ("sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("support" ,maven-builder-support)
       ("settings" ,maven-settings)
       ("lang3" ,java-commons-lang3)))
    (native-inputs
     `(("junit" ,java-junit)))))

(define-public maven-plugin-lifecycle
  (package
    (inherit maven)
    (name "maven-plugin-api")
    (arguments
     `(#:jar-name "maven-plugin-api.jar"
       #:source-dir "maven-plugin-api/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-plugin-api/test"))
    (inputs
     `(("artifact" ,maven-artifact)
       ("container" ,java-plexus-container-default)
       ("utils" ,java-plexus-utils)
       ("classworlds" ,java-plexus-classworlds)))
    (native-inputs '())))

(define-public maven-plugin-api
  (package
    (inherit maven)
    (name "maven-plugin-api")
    (arguments
     `(#:jar-name "maven-plugin-api.jar"
       #:source-dir "maven-plugin-api/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-plugin-api/test"))
    (inputs
     `(("artifact" ,maven-artifact)
       ("container" ,java-plexus-container-default)
       ("utils" ,java-plexus-utils)))
    (native-inputs '())))

(define-public maven-model-builder
  (package
    (inherit maven)
    (name "maven-model-builder")
    (arguments
     `(#:jar-name "maven-model-builder.jar"
       #:source-dir "maven-model-builder/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-model-builder/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "maven-model-builder/src/main/resources"
                               "build/classes")))
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (chmod "components.sh" #o755)
             (zero? (system* "./components.sh" "maven-model-builder/src/main/java"
                             "build/classes/META-INF/plexus/components.xml"))))
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* (find-files "maven-model-builder/src/test/java" ".*.java")
               (("src/test") "maven-model-builder/src/test"))
             #t)))))
    (inputs
     `(("model" ,maven-model)
       ("artifact" ,maven-artifact)
       ("support" ,maven-builder-support)
       ("annotations" ,java-plexus-component-annotations)
       ("utils" ,java-plexus-utils)
       ("interpolation" ,java-plexus-interpolation)
       ("lang3" ,java-commons-lang3)
       ("guava" ,java-guava)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("container" ,java-plexus-container-default)
       ("xmlunit" ,java-xmlunit)
       ("xmlunit" ,java-xmlunit-legacy)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("classworlds" ,java-plexus-classworlds)))))

(define-public maven-core
  (package
    (inherit maven)
    (name "maven-core")
    (arguments
     `(#:jar-name "maven-core.jar"
       #:source-dir "maven-core/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-core/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "maven-core/src/main/java" version
                               "false" "true")))
             (let ((file "maven-core/src/main/mdo/toolchains.mdo"))
               (and
                 (modello-single-mode file "1.1.0" "java")
                 (modello-single-mode file "1.1.0" "xpp3-reader")
                 (modello-single-mode file "1.1.0" "xpp3-writer"))))))))
    (inputs
     `(("artifact" ,maven-artifact)
       ("model" ,maven-model)
       ("model-builder" ,maven-model-builder)
       ("settings" ,maven-settings)
       ("settings-builder" ,maven-settings-builder)
       ("container" ,java-plexus-container-default)
       ("annotations" ,java-plexus-component-annotations)
       ("utils" ,java-plexus-utils)
       ("lang3" ,java-commons-lang3)
       ("guava" ,java-guava)
       ("aether" ,java-eclipse-aether-api)
       ("java-javax-inject" ,java-javax-inject)
       ("classworld" ,java-plexus-classworlds)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ("classworlds" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)))))
