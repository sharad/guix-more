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
  #:use-module (gnu packages docbook)
  #:use-module (gnu packages java)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages python))

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
    (version "12921")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                    (url "https://github.com/openstreetmap/josm.git")
                    (commit "82b3a6bc44ce02ce8398b3842fa4ec2647bb5e4a")))
              ;;(uri (svn-reference
              ;;      (url "https://svn.openstreetmap.org/applications/editors/josm")
              ;;      (revision 12039)))
              (sha256
               (base32
                "1v2mf8y42r3jplc72xy481a3263j9p2g72d76szcsvnh3cg5kd3i"))
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
       ("java-avalon-framework-api" ,java-avalon-framework-api)
       ("java-httpcomponents-client" ,java-httpcomponents-client)
       ("java-httpcomponents-core" ,java-httpcomponents-core)
       ("java-commons-jcs" ,java-commons-jcs)
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
         (add-after 'unpack 'rm-jcs-embedded
           (lambda _
             (delete-file-recursively "src/org/apache/commons/jcs")))
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
    (description "Java-based solution for accessing, manipulating, and
outputting XML data from Java code.")
    (license license:bsd-4)))

;; As of 2010-09-01, the ORO project is retired
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
    (synopsis "Text-processing for Java")
    (description "The Jakarta-ORO Java classes are a set of text-processing
Java classes that provide Perl5 compatible regular expressions, AWK-like
regular expressions, glob expressions, and utility classes for performing
substitutions, splits, filtering filenames, etc.  This library is the successor
of the OROMatcher, AwkTools, PerlTools, and TextTools libraries originally
from ORO, Inc.")
    (license license:asl2.0)))

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
    (synopsis "Cryptographic library")
    (description "")
    (license license:expat)))

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
                "0wv8x0jp9a5qxlrgkhb5jdk2gr6vi87b4j4kjb8ryxiy9gn8g51z"))))
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
             (copy-recursively (assoc-ref inputs "resources") "../test-resources"))))))
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
             ;; These tests require log4j-1 (log4j-1.2-api doesn't work)
             (delete-file "src/test/java/org/springframework/util/MockLog4jAppender.java")
             (delete-file "src/test/java/org/springframework/util/Log4jConfigurerTests.java")))
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
;       ("snakeyaml" ,java-snakeyaml)
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
     `(("log" ,java-avalon-logkit)))
    (native-inputs
     `(("junit" ,java-junit)))
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
     `(("logging" ,java-commons-logging-minimal)
       ("cglib" ,java-cglib)
       ("beanutils", java-commons-beanutils)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
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
     `(("digester" ,java-commons-digester)
       ("beanutils" ,java-commons-beanutils)
       ("collections" ,java-commons-collections)
       ("logging" ,java-commons-logging-minimal)))
    (native-inputs
     `(("junit" ,java-junit)))
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
               (("</files>") ""))))
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (install-file "build/ognl-2.7.2.jar" (string-append (assoc-ref outputs "out") "/share/java")))))))
             ;(zero? (system* "ant" "dist" "-Dcompile.version=7"
             ;                (string-append "-Ddist="
             ;                               (assoc-ref outputs "out")
             ;                               "/share/java")
             ;                (string-append "-Ddocbook.xsl.path="
             ;                               (assoc-ref inputs "docbook-xsl"))
             ;                (string-append "-Ddocbook.xml.path="
             ;                               (assoc-ref inputs "docbook-xml"))
             ;                               )))))))
    (inputs
     `(("javassist" ,java-jboss-javassist)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
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
    (version "2.5.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/struts/" version "/struts-"
                                  version "-src.zip"))
              (sha256
               (base32
                "1pi3ymql7d9axxzi6pd8iqap1d3s2pij88mc7zywbw7mva61y8qy"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "apache-struts.jar"
       #:source-dir "src/core/src/main/java"
       #:test-dir "src/core/src/test"))
    (inputs
     `(("log4j" ,java-log4j-api)
       ("lang3" ,java-commons-lang3)
       ("io" ,java-commons-io)
       ("logging" ,java-commons-logging-minimal)
       ("servlet" ,java-classpathx-servletapi)
       ("ognl" ,java-ognl)
       ("velocity" ,java-velocity)
       ("testng" ,java-testng)))
    (native-inputs
     `(("junit" ,java-junit)
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
                               "build/classes")))
         (add-before 'build 'generate-parser
           (lambda _
             (and
               (zero?
                 (system*
                   "jjtree" "-STATIC=false" "-MULTI=true"
                   "-NODE_PACKAGE=org.apache.velocity.runtime.parser.node"
                   "-BUILD_NODE_FILES=false" "-NODE_USES_PARSER=true"
                   ;(string-append "-OUTPUT_DIRECTORY=velocity-engine-core/src"
                   ;               "/main/java/org/apache/velocity/runtime/parser")
                   "velocity-engine-core/src/main/parser/Parser.jjt"))
               (begin
                 (rename-file "Parser.jj"
                              "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/Parser.jj")
                 (rename-file "ParserTreeConstants.java"
                              "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/node/ParserTreeConstants.java")
                 (rename-file "JJTParserState.java"
                              "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/node/JJTParserState.java")
                 #t)
               (zero?
                 (system*
                   "javacc" "-STATIC=false" "-JDK_VERSION=1.8"
                   (string-append "-OUTPUT_DIRECTORY=velocity-engine-core/src"
                                  "/main/java/org/apache/velocity/runtime/parser")
                   "velocity-engine-core/src/main/java/org/apache/velocity/runtime/parser/Parser.jj"))))))))
    (native-inputs
     `(("javacc" ,java-javacc-5)))
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
                          (string-append dir "/velocity-tools-2.0.jar"))))))))
    (inputs
     `(("dom4j" ,java-dom4j)
       ("velocity" ,java-velocity)
       ("digester" ,java-commons-digester)
       ("validator" ,java-commons-validator)
       ("beanutils", java-commons-beanutils)))
    ;; apache struts
    (home-page "https://velocity.apache.org/tools/devel")
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
    (build-system ant-build-system)
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
    (home-page "https://hc.apache.org")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-httpcomponents-core
  (package
    (name "java-httpcomponents-core")
    (version "4.4.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/httpcomponents/httpcore/"
                                  "source/httpcomponents-core-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "18isvannj51a3lz71qn8wmj9x9l0ryw6x5sc56pp12nxnpnf175a"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "httpcomponents-core.jar"
       #:source-dir "httpcore/src/main"
       #:test-dir "httpcore/src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("logging" ,java-commons-logging-minimal)
       ("lang3" ,java-commons-lang3)
       ("mockito" ,java-mockito-1)))
    (home-page "https://hc.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-httpcomponents-client
  (package
    (name "java-httpcomponents-client")
    (version "4.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/httpcomponents/httpclient/"
                                  "source/httpcomponents-client-" version
                                  "-src.tar.gz"))
              (sha256
               (base32
                "1428399s7qy3cim5wc6f3ks4gl9nf9vkjpfmnlap3jflif7g2pj1"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "httpcomponents-client.jar"
       #:source-dir "httpclient/src/main"
       #:test-dir "httpclient/src/test"))
    (inputs
     `(("core" ,java-httpcomponents-core)
       ("codec" ,java-commons-codec)
       ("logging" ,java-commons-logging-minimal)))
    (native-inputs
     `(("junit" ,java-junit)
       ("mockito" ,java-mockito-1)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://hc.apache.org/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-gson
  (package
    (name "java-gson")
    (version "2.8.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/gson/archive/"
                                  "gson-parent-" version ".tar.gz"))
              (sha256
               (base32
                "1j4qnp7v046q0k48c4kyf69sxaasx2h949d3cqwsm3kzxms3x0f9"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "gson.jar"
       #:source-dir "gson/src/main/java"
       #:test-dir "gson/src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jsoup
  (package
    (name "java-jsoup")
    (version "1.10.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jhy/jsoup/archive/jsoup-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0xbzw7rjv7s4nz1xk9b2cnin6zkpaldmc3svk71waa7hhjgp0a20"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jsoup.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (let ((classes-dir (string-append (getcwd) "/build/classes")))
               (with-directory-excursion "src/main/java"
                 (for-each (lambda (file)
                             (let ((dist (string-append classes-dir "/" file)))
                               (mkdir-p (dirname dist))
                               (copy-file file dist)))
                   (find-files "." ".*.properties")))))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("gson" ,java-gson)))
    (home-page "https://jsoup.org")
    (synopsis "HTML Parser")
    (description "jsoup is a Java library for working with real-world HTML.  It
provides a very convenient API for extracting and manipulating data, using the
best of DOM, CSS, and jquery-like methods.")
    (license license:expat)))

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
                                    " -cp " dir "/javacc.jar" " `basename $0`" " $*"))))
               (chmod (string-append bin "/javacc") #o755)
               (symlink (string-append bin "/javacc")
                        (string-append bin "/jjdoc"))
               (symlink (string-append bin "/javacc")
                        (string-append bin "/jjtree"))))))))
    (native-inputs
     `(("junit" ,java-junit)))
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
       #:test-exclude (list "**/test*/**.java")))
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
       #:test-include (list "**/Test*.java")
       #:test-exclude (list "**/Test*Chronology.java" "**/Test*Field.java")
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
               (find-files "src/test/resources/" ".*")))))))
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

(define-public java-jettison
  (package
    (name "java-jettison")
    (version "1.3.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/codehaus/jettison/archive/"
                                  "jettison-" version ".tar.gz"))
              (sha256
               (base32
                "0rdhfyxywvga5wiwasc04iqnxyixn3rd8wj01c9ymhvwc3h6dpqg"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jettison.jar"
       #:source-dir "src/main/java"
       #:test-exclude (list "**/Abstract*.java"
                            ;; Abstract class
                            "**/DOMTest.java"
                            "**/BadgerFishDOMTest.java"
                            "**/MappedDOMTest.java")))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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

(define-public java-apache-xml-commons-resolver
  (package
    (name "java-apache-xml-commons-resolver")
    (version "1.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/"
                           "xml-commons-resolver-" version ".tar.gz"))
       (sha256
        (base32 "1zhy4anc3fg9f8y348bj88vmab15aavrg6nf419ifb25asyygnsm"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (for-each delete-file (find-files "." ".*\\.(jar|zip)"))
           #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append "xml-resolver.jar")
       #:tests? #f)); no tests
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "http://xerces.apache.org/xml-commons/")
    (synopsis "")
    (description "TODO")
    (license license:asl2.0)))

(define-public java-jaxp
  (package
    (name "java-jaxp")
    (version "1.4.01")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/xml-commons/source/"
                           "xml-commons-external-" version "-src.tar.gz"))
       (sha256
        (base32 "0rhq32a7dl9yik7zx9h0naz2iz068qgcdiayak91wp4wr26xhjyk"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxp.jar"
       #:jdk ,icedtea-8
       #:source-dir ".."
       #:tests? #f)); no tests
    ;; TODO: Debian builds several jars out of this: jaxp-1.4.jar,
    ;; xml-apis.jar and xml-apis-1.4.01.jar. Additionally it adds maven-repo
    ;; data (.pom). The poms get fetch directly using wget, see
    ;; <https://anonscm.debian.org/viewvc/pkg-java/trunk/libjaxp1.3-java/debian/rules?revision=15402&view=markup>
    (home-page "http://xerces.apache.org/xml-commons/")
    (synopsis "Java XML parser and transformer APIs (DOM, SAX, JAXP, TrAX)")
    (description "xml-apis.jar from the Apache XML Commons project is used by
the Xerces-J XML parser and Xalan-J XSLT processor and specifies these APIs:

@itemize
@item Document Object Model (DOM) level 3
@item Simple API for XML (SAX) 2.0.2
@item Java APIs for XML Processing (JAXP) 1.3.04
@item Transformation API for XML (TrAX) 1.3.04
@item Document Object Model (DOM) Level 3 Load and Save
@item JSR 206 Java API for XML Processing 1.3
@end itemize

These classes are also used in Sun's reference implementation. A GPL'ed
implementation of these APIs can be found in the java-gnujaxp package. ")
    (license license:asl2.0)))

(define-public java-xerces
  (package
    (name "java-xerces")
    (version "2.11.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://apache/xerces/j/source/"
                           "Xerces-J-src." version ".tar.gz"))
       (sha256
        (base32 "1006igwy2lqrmjvdk64v8dg6qbk9c29pm8xxx7r87n0vnpvmx6pm"))
       (patches (search-patches
                 "java-xerces-xjavac_taskdef.patch"
                 "java-xerces-build_dont_unzip.patch"
                 "java-xerces-bootclasspath.patch"
                 "java-xerces-manifest_classpath.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'create-build.properties
          (lambda* (#:key inputs #:allow-other-keys)
            (let ((jaxp (assoc-ref inputs "java-jaxp"))
                  (resolver (assoc-ref inputs "resolver")))
              (with-output-to-file "build.properties"
                (lambda _
                  (format #t
                   "jar.jaxp = ~a/share/java/jaxp.jar~@
                   jar.apis-ext = ~a/share/java/jaxp.jar~@
                   jar.resolver = ~a/share/java/xml-resolver.jar~%"
                   jaxp jaxp resolver))))))
         (replace 'install (install-jars "build")))))
    (native-inputs
     `(("resolver" ,java-apache-xml-commons-resolver)
       ("java-jaxp" ,java-jaxp)))
    (home-page "https://xerces.apache.org/xerces2-j/")
    (synopsis "Validating XML parser for Java with DOM level 3 support")
    (description "The Xerces2 Java parser is the reference implementation of
XNI, the Xerces Native Interface, and also a fully conforming XML Schema
processor.

Xerces2-J supports the following standards and APIs:

@itemize
@item eXtensible Markup Language (XML) 1.0 Second Edition Recommendation
@item Namespaces in XML Recommendation
@item Document Object Model (DOM) Level 2 Core, Events, and Traversal and
      Range Recommendations
@item Simple API for XML (SAX) 2.0.1 Core and Extension
@item Java APIs for XML Processing (JAXP) 1.2.01
@item XML Schema 1.0 Structures and Datatypes Recommendations
@item Experimental implementation of the Document Object Model (DOM) Level 3
      Core and Load/Save Working Drafts
@item Provides a partial implementation of the XML Inclusions (XInclude) W3C
      Candidate Recommendation
@end itemize

Xerces is now able to parse documents written according to the XML 1.1
Candidate Recommendation, except that it does not yet provide an option to
enable normalization checking as described in section 2.13 of this
specification. It also handles namespaces according to the XML Namespaces 1.1
Candidate Recommendation, and will correctly serialize XML 1.1 documents if
the DOM level 3 load/save API's are in use.

TODO Verify signature")
    (license license:asl2.0)))

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

(define-public java-xmlpull2
  (package
    (name "java-xmlpull2")
    (version "2.1.10")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.extreme.indiana.edu/xgws/xsoap/"
                                  "PullParser/PullParser" version ".tgz"))
              (sha256
               (base32
                "1kw9nhyqb7bzhn2zjbwlpi5vp5rzj89amzi3hadw2acyh2dmd0md"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "impl"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p out)
               (copy-file "build/lib/PullParser-standard-2.1.10.jar"
                          (string-append out "/xmlpull2-standard.jar"))
               (copy-file "build/lib/PullParser-2.1.10.jar"
                          (string-append out "/xmlpull2.jar"))))))))
    (home-page ""); No available homepage
    (synopsis "")
    (description "")
    (license license:public-domain)))

(define-public java-xpp3
  (package
    (name "java-xpp3")
    (version "1.1.4")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://www.extreme.indiana.edu/dist/"
                                  "java-repository/xpp3/distributions/xpp3-"
                                  version "_src.tgz"))
              (sha256
               (base32
                "1b99zrhyij5qwyhilyjdl1ykxvhk902vsvflh6gx4fir8hfvdl5p"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:build-target "jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (string-append (assoc-ref outputs "out") "/share/java")))
               (mkdir-p out)
               (copy-file (string-append "build/xpp3-" ,version ".jar")
                          (string-append out "/xpp3.jar"))))))))
    (home-page ""); No available homepage
    (synopsis "")
    (description "")
    (license license:public-domain)))

;; for com.sun.msv
(define-public java-xsdlib
  (package
    (name "java-xsdlib")
    (version "2013.2")
    (source (origin
              (method url-fetch)
              (uri (string-append "http://central.maven.org/maven2/com/sun/msv/"
                                  "datatype/xsd/xsdlib/" version "/xsdlib-"
                                  version "-sources.jar"))
              (sha256
               (base32
                "185i48p1xp09wbq03i9zgfl701qa262rq46yf4cajzmk3336kqim"))))
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f; no tests
       #:jar-name "xsdlib.jar"
       #:jdk ,icedtea-8))
    (inputs
     `(("xerces" ,java-xerces)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-2)))

(define-public java-commons-bcel
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
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       ;; FIXME: requires org.openjdk.jmh.* and com.sun.jna.platform.win32 for tests
       #:tests? #f))
    (native-inputs
     `(("junit" ,java-junit)
       ("collections" ,java-commons-collections4)
       ("lang3" ,java-commons-lang3)
       ("io" ,java-commons-io)))
    (home-page "https://commons.apache.org/proper/commons-bcel/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;(define-public java-jflex
;  (package
;    (name "java-jflex")
;    (version "1.6.1")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/jflex-de/jflex/archive/"
;                                  version ".tar.gz"))
;              (sha256
;               (base32
;                "1wdfx4yl8cy2karbm4vpmk29xjlv6vn8y9b0sgfax26bl0bx7zxs"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name "jflex.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "jflex/src/main/java"
;       #:test-dir "jflex/src/test"))
;    (home-page "jflex.de")
;    (synopsis "")
;    (description "")
;    (license license:bsd-3)))

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

;; Requires java-cup, but it requires jflex which in turn requires java-cup.
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
     `(("bcel" ,java-commons-bcel)
       ("xerces" ,java-xerces)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-2)))

(define-public java-dom4j
  (package
    (name "java-dom4j")
    (version "2.0.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/dom4j/dom4j/archive/"
                                  "version-" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "04snvb8m4290l0z1nkzdhlgxhfzi36wfkk674fn90q75s80s72j6"))
              (modules '((guix build utils)))
              (snippet
                '(begin ;; Delete bundled jar archives.
                   (for-each delete-file (find-files "." ".*\\.jar"))
                   #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "dom4j.jar"
       #:jdk ,icedtea-8
       #:source-dir "src/main/java"
       #:tests? #f; we can't build xalan
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-jaxen-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "jaxen-sources")
             (with-directory-excursion "jaxen-sources"
               (system* "jar" "xf" (assoc-ref inputs "jaxen-sources")))
             (mkdir-p "src/main/java/org/jaxen/dom4j")
             (copy-file "jaxen-sources/org/jaxen/dom4j/DocumentNavigator.java"
                        "src/main/java/org/jaxen/dom4j/DocumentNavigator.java")
             (copy-file "jaxen-sources/org/jaxen/dom4j/Dom4jXPath.java"
                        "src/main/java/org/jaxen/dom4j/Dom4jXPath.java")))
         (add-before 'build 'fix-old-xpp2
           (lambda _
             ;; This package normally depends on xpp2 2.0, but we can only package
             ;; version 2.1.10.
             (substitute* "src/main/java/org/dom4j/xpp/ProxyXmlStartTag.java"
               (("public void resetStartTag")
                (string-append
                  "public boolean removeAttributeByRawName(String name) {\n"
                  "  return false;\n"
                  "}\n"
                  "public boolean removeAttributeByName(String name, String name2) {\n"
                  "  return false;\n"
                  "}\n"
                  "\n"
                  "public void resetStartTag"))
               (("Atttribute") "Attribute")))))))
    (inputs
     `(("jaxen" ,java-jaxen-no-rec-deps)
       ("jaxen-sources" ,(package-source java-jaxen-no-rec-deps))
       ("xmlpull" ,java-xmlpull)
       ("xmlpull2" ,java-xmlpull2)
       ("xsdlib" ,java-xsdlib)))
    (native-inputs
     `(("testng" ,java-testng)
       ("xerces" ,java-xerces)))
    (home-page "https://github.com/dom4j/dom4j/")
    ; Note: dom4j.org is outdated
    (synopsis "Flexible XML framework for Java")
    (description "")
    (license license:expat))) ;; TODO: some 4-clause license

(define-public java-jaxen-no-rec-deps
  (package
    (name "java-jaxen-no-rec-deps")
    (version "1.1.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/jaxen/jaxen/"
                                  version "/jaxen-" version "-sources.jar"))
              (sha256
               (base32
                "18pa8mks3gfhazmkyil8wsp6j1g1x7rggqxfv4k2mnixkrj5x1kx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jaxen.jar"
       #:source-dir "src"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-dom4j
           (lambda _
             (delete-file-recursively "src/org/jaxen/dom4j")
             (delete-file-recursively "src/org/jaxen/xom"))))))
    (inputs
     `(("jdom" ,java-jdom)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-jaxen
  (package
    (inherit java-jaxen-no-rec-deps)
    (name "java-jaxen")
    (inputs
     `(("jodm" ,java-jdom)
       ("xom" ,java-xom)
       ("dom4j" ,java-dom4j)))))

(define-public java-xom
  (package
    (name "java-xom")
    (version "127")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/elharo/xom/archive/XOM_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "04m69db1irqja12a9rfxrac8cbn9psqa1k136wh4ls4pxfsdr5wg"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "xom.jar"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-tagsoup-dep
           (lambda _
             (delete-file "src/nu/xom/tools/XHTMLJavaDoc.java"))))))
    (inputs
     `(("jdom" ,java-jdom)
       ("junit" ,java-junit)
       ("servlet" ,java-tomcat)
       ("jaxen" ,java-jaxen-no-rec-deps)
       ("xerces" ,java-xerces)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:lgpl2.1)))

(define-public java-kxml2
  (package
    (name "java-kxml2")
    (version "2.4.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/stefanhaustein/kxml2/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0afahxsifpxgwyp3ybr1ablfwswxw8n2msfhsrrj22rpb3yzai3x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "kxml2.jar"
       #:tests? #f)); no tests
    (inputs
     `(("xmlpull" ,java-xpp3)))
    (home-page "http://kxml.org")
    (synopsis "")
    (description "")
    (license license:expat)))

(define-public java-stax
  (package
    (name "java-stax")
    (version "1.2.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://repo1.maven.org/maven2/stax/stax/"
                                  version "/stax-" version "-sources.jar"))
              (sha256
               (base32
                "04ba4qvbrps45j8bldbakxq31k7gjlsay9pppa9yn13fr00q586z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "stax.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'fix-utf8
           (lambda _
             (with-fluids ((%default-port-encoding "ISO-8859-1"))
               (substitute* "src/com/wutka/dtd/Scanner.java"
                 (("//.*") "\n"))))))))
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
       #:jdk ,icedtea-8
       #:source-dir "xstream/src/java"))
    (inputs
     `(("jdom" ,java-jdom)
       ("jdom2" ,java-jdom2)
       ("cglib" ,java-cglib)
       ("time" ,java-joda-time)
       ("jettison" ,java-jettison)
       ("xom" ,java-xom)
       ("xmlpull3" ,java-xpp3)
       ("dom4j" ,java-dom4j)
       ("stax2" ,java-stax2-api)
       ("woodstox" ,java-woodstox-core)
       ("kxml2" ,java-kxml2)
       ("stax" ,java-stax)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "https://x-stream.github.io")
    (synopsis "")
    (description "")
    (license license:x11)))

(define-public java-hawtjni
  (package
    (name "java-hawtjni")
    (version "1.15")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fusesource/hawtjni/archive/"
                                  "hawtjni-project-" version ".tar.gz"))
              (sha256
               (base32
                "1bqfd732rmh6svyx17fpw9175gc9gzkcbyps2yyrf50c3zzjas6g"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "hawtjni.jar"
       #:source-dir "hawtjni-generator/src/main/java:hawtjni-runtime/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-native
           (lambda* (#:key inputs #:allow-other-keys)
             (with-directory-excursion "hawtjni-generator/src/main/resources/"
               (system* "gcc" "-c" "hawtjni.c" "-o" "hawtjni.o"
                        "-fPIC" "-O2"
                        (string-append "-I" (assoc-ref inputs "jdk") "/include/linux"))
               (system* "gcc" "-c" "hawtjni-callback.c" "-o" "hawtjni-callback.o"
                        "-fPIC" "-O2"
                        (string-append "-I" (assoc-ref inputs "jdk") "/include/linux"))
               (zero? (system* "gcc" "-o" "libhawtjni.so" "-shared"
                               "hawtjni.o" "hawtjni-callback.o")))))
         (add-after 'install 'install-native
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (lib (string-append out "/lib"))
                    (inc (string-append out "/include")))
               (mkdir-p lib)
               (mkdir-p inc)
               (with-directory-excursion "hawtjni-generator/src/main/resources/"
                 (copy-file "libhawtjni.so" (string-append lib "/libhawtjni.so"))
                 (copy-file "hawtjni.h" (string-append inc "/hawtjni.h")))))))))
    (inputs
     `(("cli" ,java-commons-cli)
       ("asm" ,java-asm)
       ("finder" ,java-geronimo-xbean-finder)))
    (home-page "https://fusesource.github.io/hawtjni/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jansi-native
  (package
    (name "java-jansi-native")
    (version "1.7")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fusesource/jansi-native/"
                                  "archive/jansi-native-" version ".tar.gz"))
              (sha256
               (base32
                "0j2ydlgxbzbgshqkwghbxxxnbnx1mmjgd6k5fw6xfvxw1z956yqf"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jansi-native.jar"
       #:source-dir "src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'build-native
           (lambda* (#:key inputs #:allow-other-keys)
             (substitute* "src/main/native-package/src/jansi_ttyname.c"
               (("#include \"jansi_.*") ""))
             ;; TODO: there are more required files for windows in windows/
             (with-directory-excursion "src/main/native-package/src"
               (system* "gcc" "-c" "jansi_ttyname.c" "-o" "jansi_ttyname.o"
                        (string-append "-I" (assoc-ref inputs "hawtjni") "/include")
                        (string-append "-I" (assoc-ref inputs "jdk") "/include/linux")
                        "-fPIC" "-O2")
               (system* "gcc" "-o" "libjansi.so" "-shared" "jansi_ttyname.o"))
             ;; TODO: detect one of linux{32,64}, freebsd{32,64}, osx, windows{32,64}
             (mkdir-p "build/classes/META-INF/native/linux64")
             (copy-file "src/main/native-package/src/libjansi.so"
                        "build/classes/META-INF/native/linux64/libjansi.so")))
         (add-after 'install 'install-native
           (lambda* (#:key outputs #:allow-other-keys)
             (mkdir-p (string-append (assoc-ref outputs "out") "/bin"))
             (copy-file "src/main/native-package/src/jansi.h"
                        (string-append (assoc-ref outputs "out")
                                       "/bin/jansi.h")))))))
    (inputs
     `(("hawtjni" ,java-hawtjni)))
    (home-page "https://fusesource.github.io/jansi/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jansi
  (package
    (name "java-jansi")
    (version "1.16")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/fusesource/jansi/archive/"
                                  "jansi-project-" version ".tar.gz"))
              (sha256
               (base32
                "11kh3144i3fzp21dpy8zg52mjmsr214k7km9p8ly0rqk2px0qq2z"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jansi.jar"
       #:source-dir "jansi/src/main/java"
       #:test-dir "jansi/src/test"))
    (inputs
     `(("jansi-native" ,java-jansi-native)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://fusesource.github.io/jansi/")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-jline
  (package
    (name "java-jline")
    (version "1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/jline/jline1/archive/jline-"
                                  version ".tar.gz"))
              (sha256
               (base32
                "0bi3p6vrh7a6v0fbpb6rx9plpmx5zk3lr352xzdbz2jcxg499wir"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "jline.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/jline")
             (for-each (lambda (f) (copy-file (string-append "src/main/resources/jline/" f)
                                              (string-append "build/classes/jline/" f)))
               '("CandidateListCompletionHandler.properties"
                 "keybindings-mac.properties"
                 "keybindings.properties"
                 "windowsbindings.properties")))))))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "https://jline.github.io")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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
                "15c9xmf7rhr5w4qk2jcb6swds336l4l5gyb51pcjay2ywnigk8sa"))
              (patches
                (search-patches
                  "groovy-Add-exceptionutilsgenerator.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "groovy.jar"
       #:source-dir "src/main:subprojects/groovy-test/src/main/java"
       #:test-dir "src/test"
       #:tests? #f
       #:main-class "groovy.ui.GroovyMain"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-parser
           (lambda _
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr/java"
               (zero? (system* "antlr" "java.g")))
             (with-directory-excursion "src/main/org/codehaus/groovy/antlr"
               (mkdir "parser")
               (with-directory-excursion "parser"
                 (zero? (system* "antlr" "../groovy.g"))))))
         (add-before 'build 'generate-exception-utils
           (lambda _
             (system* "javac" "-cp" (getenv "CLASSPATH")
                      "config/ant/src/org/codehaus/groovy/ExceptionUtilsGenerator.java")
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":config/ant/src")
                             "org.codehaus.groovy.ExceptionUtilsGenerator"
                             "build/classes/org/codehaus/groovy/runtime/ExceptionUtils.class"))))
         ;(add-before 'check 'compile-groovy
         ;  (lambda _
         ;    (zero? (apply system* "java" "-cp"
         ;                  (string-append (getenv "CLASSPATH") ":build/classes")
         ;                  "org.codehaus.groovy.tools.FileSystemCompiler"
         ;                  (find-files "src/test" ".*\\.(groovy|java)$")))))
         (add-after 'install 'install-sh
           (lambda* (#:key outputs #:allow-other-keys)
             (substitute* "src/bin/startGroovy"
               ((" -classpath .*")
                (string-append " -classpath " (getenv "CLASSPATH") ":"
                               (assoc-ref outputs "out") "/share/java/groovy.jar \\")))
             (let ((bin (string-append (assoc-ref outputs "out") "/bin")))
               (for-each (lambda (script)
                           (install-file (string-append "src/bin/" script) bin)
                           (chmod (string-append bin "/" script) #o755))
                 '("grape" "groovy" "groovyc" "groovyConsole" "groovydoc"
                   "groovysh" "java2groovy" "startGroovy")))
             (install-file "src/conf/groovy-starter.conf"
                           (string-append (assoc-ref outputs "out") "/conf"))
             #t))
         (add-before 'check 'add-groovy-classes
           (lambda _
             (substitute* "build.xml"
               (("a") "a")))))))
    (native-inputs
     `(("junit" ,java-junit)
       ("antlr" ,antlr2)
       ("jmock" ,java-jmock-1)
       ("xmlunit" ,java-xmlunit-legacy)))
    (inputs
     `(("commons-cli" ,java-commons-cli)
       ("asm" ,java-asm)
       ("servlet" ,java-tomcat)
       ("xstream" ,java-xstream)
       ("jansi" ,java-jansi)
       ("jline" ,java-jline)))
    (home-page "")
    (synopsis "")
    (description "")
    (license (list license:gpl2
                   ;; actually CDDL 1.1
                   license:cddl1.0))))

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
                "0y7lzkvx9wbbmwg45mb4icx7i66z6894qfygrbbs26sr5xxyml9h"))
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
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "stringtemplate")
                                                  ".*\\.jar"))
                                    ":"
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "stringtemplate4")
                                                  ".*\\.jar"))
                                    ":"
                                    (string-concatenate
                                      (find-files (assoc-ref inputs "antlr3")
                                                  ".*\\.jar"))
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
       ("icu4j" ,java-icu4j)
       ("java-json" ,java-json)
       ("treelayout" ,java-treelayout)
       ("stringtemplate4" ,java-stringtemplate)))
    (native-inputs
     `(("runtime" ,java-antlr4-runtime)))
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

(define-public java-commons-jxpath
  (package
    (name "java-commons-jxpath")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "mirror://apache/commons/jxpath/source/"
                                  "commons-jxpath-" version "-src.tar.gz"))
              (sha256
               (base32
                "1rpgg31ayn9fwr4bfi2i1ij0npcg79ad2fv0w9hacvawsyc42cfs"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-jxpath.jar"
       ;; tests require more dependencies, including mockrunner which depends on old software
       #:tests? #f
       #:source-dir "src/java"))
    (inputs
     `(("servlet" ,java-tomcat)
       ("jdom" ,java-jdom)
       ("beanutils" ,java-commons-beanutils)))
    (native-inputs
     `(("junit" ,java-junit)))
    (home-page "http://commons.apache.org/jxpath/")
    (synopsis "Simple interpreter of an expression language called XPath.")
    (description "The org.apache.commons.jxpath package defines a simple
interpreter of an expression language called XPath.  JXPath applies XPath
expressions to graphs of objects of all kinds: JavaBeans, Maps, Servlet
contexts, DOM etc, including mixtures thereof.")
    (license license:asl2.0)))

(define-public java-geronimo-xbean-bundleutils
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-xbean-bundleutils")
    (arguments
     `(#:jar-name "geronimo-xbean-bundleutils.jar"
       #:source-dir "xbean-bundleutils/src/main/java"
       #:test-dir "xbean-bundleutils/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-java
           (lambda _
             (substitute* "xbean-bundleutils/src/main/java/org/apache/xbean/osgi/bundle/util/DelegatingBundleContext.java"
               (("import org.osgi.framework.ServiceRegistration;")
                (string-append "import org.osgi.framework.ServiceRegistration;\n"
                               "import org.osgi.framework.ServiceFactory;\n"
                               "import java.util.Collection;\n"
                               "import org.osgi.framework.ServiceObjects;"))
               (("public Bundle getBundle\\(\\)")
                (string-append "@Override\n"
                               "public <S> ServiceObjects<S> getServiceObjects(ServiceReference<S> reference) {\n"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "@Override\n"
                               "public <S> ServiceRegistration<S> registerService(Class<S> clazz, ServiceFactory<S> factory, Dictionary<String, ?> properties) {"
                               " throw new UnsupportedOperationException();\n"
                               "}\n"
                               "public Bundle getBundle()"))))))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("asm" ,java-asm)
       ("framework" ,java-osgi-framework)
       ("eclipse-osgi" ,java-eclipse-osgi)
       ("packageadmin" ,java-osgi-service-packageadmin)))))

(define-public java-geronimo-xbean-asm-util
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-xbean-asm-util")
    (arguments
     `(#:jar-name "geronimo-xbean-asm-util.jar"
       #:source-dir "xbean-asm-util/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("asm" ,java-asm)))))

(define-public java-geronimo-xbean-finder
  (package
    (inherit java-geronimo-xbean-reflect)
    (name "java-geronimo-xbean-finder")
    (arguments
     `(#:jar-name "geronimo-xbean-finder.jar"
       #:source-dir "xbean-finder/src/main/java"
       #:test-dir "xbean-finder/src/test"))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("asm" ,java-asm)
       ("bundleutils" ,java-geronimo-xbean-bundleutils)
       ("asm-util" ,java-geronimo-xbean-asm-util)
       ("packageadmin" ,java-osgi-service-packageadmin)
       ("framework" ,java-osgi-framework)))
    (native-inputs
     `(("junit" ,java-junit)
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

;(define-public java-qdox
;  (package
;    (name "java-qdox")
;    (version "2.0-M5")
;    (source
;     (origin
;       (method url-fetch)
;       ;; 2.0-M4, -M5 at https://github.com/paul-hammant/qdox
;       ;; Older releases at https://github.com/codehaus/qdox/
;       (uri (string-append "http://central.maven.org/maven2/"
;                           "com/thoughtworks/qdox/qdox/" version
;                           "/qdox-" version "-sources.jar"))
;       (sha256
;        (base32 "10ny800qmfishrqdxmngv0jylhaxjh340bia8csghl9a9cbxfrjs"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name "qdox.jar"))
;    (home-page "http://qdox.codehaus.org/")
;    (synopsis "Parse definitions from Java source files")
;    (description "QDox is a high speed, small footprint parser for extracting
;class/interface/method definitions from source files complete with JavaDoc
;@code{@@tags}.  It is designed to be used by active code generators or
;documentation tools.")
;    (license license:asl2.0)))
;
;(define-public java-qdox-1.12
;  (package (inherit java-qdox)
;    (version "1.12.1")
;    (source
;     (origin
;       (method url-fetch)
;       (uri (string-append "http://central.maven.org/maven2/"
;                           "com/thoughtworks/qdox/qdox/" version
;                           "/qdox-" version "-sources.jar"))
;       (sha256
;        (base32 "0hlfbqq2avf5s26wxkksqmkdyk6zp9ggqn37c468m96mjv0n9xfl"))))
;    (arguments
;     `(#:jar-name "qdox.jar"
;       #:phases
;       (modify-phases %standard-phases
;         (add-after 'unpack 'delete-tests
;           (lambda _
;             (delete-file-recursively "src/com/thoughtworks/qdox/junit")
;             #t)))))))

(define-public java-plexus-cli
  (package
    (name "java-plexus-cli")
    (version "1.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/sonatype/plexus-cli")
                     (commit "a776afa6bca84e5107bedb69440329cdb24ed645")))
              (file-name (string-append name "-" version))
              (sha256
               (base32
                "0xjrlay605rypv3zd7y24vlwf0039bil3n2cqw54r1ddpysq46vx"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "plexus-cli.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"))
    (inputs
     `(("cli" ,java-commons-cli)
       ("container" ,java-plexus-container-default)
       ("classworlds" ,java-plexus-classworlds)))
    (native-inputs
     `(("utils" ,java-plexus-utils)
       ("junit" ,java-junit)
       ("guava" ,java-guava)))
    (home-page "https://codehaus-plexus.github.io/plexus-cli")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public java-qdox
  (package
    (name "java-qdox")
    ; Newer version exists, but this version is required by java-plexus-component-metadata
    (version "2.0-M2")
    (source
     (origin
       (method url-fetch)
       ;; 2.0-M4, -M5 at https://github.com/paul-hammant/qdox
       ;; Older releases at https://github.com/codehaus/qdox/
       (uri (string-append "http://central.maven.org/maven2/"
                           "com/thoughtworks/qdox/qdox/" version
                           "/qdox-" version "-sources.jar"))
       (sha256
        (base32 "10xxrcaicq6axszcr2jpygisa4ch4sinyx5q7kqqxv4lknrmxp5x"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "qdox.jar"
       #:tests? #f)); no tests
    (home-page "http://qdox.codehaus.org/")
    (synopsis "Parse definitions from Java source files")
    (description "QDox is a high speed, small footprint parser for extracting
class/interface/method definitions from source files complete with JavaDoc
@code{@@tags}.  It is designed to be used by active code generators or
documentation tools.")
    (license license:asl2.0)))

(define-public java-plexus-component-metadata
  (package
    (inherit java-plexus-container-default)
    (name "java-plexus-component-metadata")
    (arguments
     `(#:jar-name "plexus-component-metadata.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "plexus-component-metadata")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources"
                               "build/classes/")
             #t)))))
    (inputs
     `(("container" ,java-plexus-container-default)
       ("annotations" ,java-plexus-component-annotations)
       ("utils" ,java-plexus-utils)
       ("cli" ,java-plexus-cli)
       ("classworlds" ,java-plexus-classworlds)
       ("plugin-api" ,maven-plugin-api)
       ("plugin-annotations" ,maven-plugin-annotations)
       ("core" ,maven-core-boot)
       ("model" ,maven-model)
       ("cli" ,java-commons-cli)
       ("qdox" ,java-qdox)
       ("jdom2" ,java-jdom2)
       ("asm" ,java-asm)))
    (native-inputs
     `(("junit" ,java-junit)
       ("guava" ,java-guava)
       ("xbean" ,java-geronimo-xbean-reflect)))))

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
             "0g3x26pymcdnfnwv2a1i57pd5s26f5zqfi1rdy98z1bn01klx25k"
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
             "0l2pvns8pmlrmjm3iknp7gpg3654y1m8qhy55b19sdwdchdcyxfh"
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
             (copy-recursively "src/main/resources" "build/classes")
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.sonatype.plexus.components.cipher.DefaultPlexusCipher\n")))
             #t)))))
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
          "1smfrk4n7xbrsxpxcp2j4i0j8q86j73w0w6xg7qz83dp6dagdjgp"))
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
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda _
                 (display
                   (string-append
                     "<component-set>\n"
                     "  <components>\n"
                     "    <component>\n"
                     "      <role>org.sonatype.plexus.components.sec.dispatcher.SecDispatcher</role>\n"
                     "      <role-hint>default</role-hint>\n"
                     "      <implementation>org.sonatype.plexus.components.sec.dispatcher.DefaultSecDispatcher</implementation>\n"
                     "      <description></description>\n"
                     "      <requirements>\n"
                     "        <requirement>\n"
                     "          <role>org.sonatype.plexus.components.cipher.PlexusCipher</role>\n"
                     "          <field-name>_cipher</field-name>\n"
                     "        </requirement>\n"
                     "        <requirement>\n"
                     "          <role>org.sonatype.plexus.components.sec.dispatcher.PasswordDecryptor</role>\n"
                     "          <field-name>_decryptors</field-name>\n"
                     "        </requirement>\n"
                     "      </requirements>\n"
                     "      <configuration>\n"
                     "        <_configuration-file>~/.settings-security.xml</_configuration-file>\n"
                     "      </configuration>\n"
                     "    </component>\n"
                     "  </components>\n"
                     "</component-set>\n"))))))
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

(define-public java-eclipse-sisu-inject
  (package
    (name "java-eclipse-sisu-inject")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/sisu.inject/"
                                  "archive/releases/" version ".tar.gz"))
              (sha256
               (base32
                "11rg6yw5nl13i65xsp4jxxgr341qcnnaan48p767h28kb07s0ajn"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-sisu-inject.jar"
       #:source-dir "org.eclipse.sisu.inject/src"
       #:jdk ,icedtea-8
       #:tests? #f)); no tests
    (inputs
     `(("guice" ,java-guice)
       ("guice-servlet" ,java-guice-servlet)
       ("javax-inject" ,java-javax-inject)
       ("servlet" ,java-tomcat)
       ("junit" ,java-junit)
       ("slf4j" ,java-slf4j-api)
       ("java-jsr305" ,java-jsr305)
       ("java-jsr250" ,java-jsr250)
       ("cdi-api" ,java-cdi-api)
       ("osgi-framework" ,java-osgi-framework)
       ("osgi-util-tracker" ,java-osgi-util-tracker)
       ("testng" ,java-testng)))
    (home-page "https://www.eclipse.org/sisu/")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

(define-public java-eclipse-sisu-plexus
  (package
    (name "java-eclipse-sisu-plexus")
    (version "0.3.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/eclipse/sisu.plexus/"
                                  "archive/releases/" version ".tar.gz"))
              (sha256
               (base32
                "0lbj7nxy5j0z71k407zbb82icfqh7midrfk0fb3fa3jzdjz0d9d9"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "eclipse-sisu-plexus.jar"
       #:source-dir "org.eclipse.sisu.plexus/src"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "org.eclipse.sisu.plexus/META-INF/plexus/components.xml"
                           "build/classes/META-INF/plexus")
             #t)))))
    (inputs
     `(("classworlds" ,java-plexus-classworlds)
       ("util" ,java-plexus-utils)
       ("annotations" ,java-plexus-component-annotations)
       ("framework" ,java-osgi-framework)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("guice" ,java-guice)
       ("javax-inject" ,java-javax-inject)
       ("slf4j" ,java-slf4j-api)
       ("junit" ,java-junit)))
    (home-page "https://www.eclipse.org/sisu/")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

(define-public java-eclipse-jetty-xml
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-xml")
    (arguments
     `(#:jar-name "eclipse-jetty-xml.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; most tests require network
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-xml"))))))
    (inputs
     `(("util" ,java-eclipse-jetty-util)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io)
       ,@(package-native-inputs java-eclipse-jetty-util)))))

(define-public java-eclipse-jetty-xml-9.2
  (package
    (inherit java-eclipse-jetty-xml)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-xml.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; most tests require network
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-xml"))))))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

(define-public java-eclipse-jetty-webapp
  (package
    (inherit java-eclipse-jetty-util)
    (name "java-eclipse-jetty-webapp")
    (arguments
     `(#:jar-name "eclipse-jetty-webapp.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude (list
                        ;; Fails, but I don't know why
                        "**/WebAppContextTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-webapp"))))))
    (inputs
     `(("util" ,java-eclipse-jetty-util)
       ("http" ,java-eclipse-jetty-http)
       ("server" ,java-eclipse-jetty-server)
       ("servlet" ,java-eclipse-jetty-servlet)
       ("security" ,java-eclipse-jetty-security)
       ("xml" ,java-eclipse-jetty-xml)
       ("servlet" ,java-tomcat)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io)
       ,@(package-native-inputs java-eclipse-jetty-util)))))

(define-public java-eclipse-jetty-webapp-9.2
  (package
    (inherit java-eclipse-jetty-webapp)
    (version (package-version java-eclipse-jetty-util-9.2))
    (source (package-source java-eclipse-jetty-util-9.2))
    (arguments
     `(#:jar-name "eclipse-jetty-webapp.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-exclude (list
                        ;; Fails, but I don't know why
                        "**/WebAppContextTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "jetty-webapp"))))))
    (inputs
     `(("util" ,java-eclipse-jetty-util-9.2)
       ("http" ,java-eclipse-jetty-http-9.2)
       ("server" ,java-eclipse-jetty-server-9.2)
       ("servlet" ,java-eclipse-jetty-servlet-9.2)
       ("security" ,java-eclipse-jetty-security-9.2)
       ("xml" ,java-eclipse-jetty-xml-9.2)
       ("servlet" ,java-tomcat)
       ,@(package-inputs java-eclipse-jetty-util-9.2)))
    (native-inputs
     `(("io" ,java-eclipse-jetty-io-9.2)
       ,@(package-native-inputs java-eclipse-jetty-util-9.2)))))

;(define-public java-eclipse-aether-api
;  (package
;    (name "java-eclipse-aether-api")
;    (version "1.0.2")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/eclipse/aether-core/"
;                                  "archive/aether-1.0.2.v20150114.tar.gz"))
;              (sha256
;               (base32
;                "192x32hlyxs4p6xzaz1r1jrsqqr56akcl0lncq3av1zpbil6kqhh"))))
;    (build-system ant-build-system)
;    (arguments
;     `(#:jar-name "eclipse-aether-api.jar"
;       #:source-dir "aether-api/src/main/java"
;       #:test-dir "aether-api/src/test"))
;    (native-inputs
;     `(("junit" ,java-junit)
;       ("hamcrest" ,java-hamcrest-core)))
;    (home-page "https://projects.eclipse.org/projects/technology.aether")
;    (synopsis "")
;    (description "")
;    (license license:epl1.0)))
;
;(define-public java-eclipse-aether-spi
;  (package
;    (inherit java-eclipse-aether-api)
;    (name "java-eclipse-aether-spi")
;    (arguments
;     `(#:jar-name "eclipse-aether-spi.jar"
;       #:source-dir "aether-spi/src/main/java"
;       #:test-dir "aether-spi/src/test"))
;    (inputs
;     `(("api" ,java-eclipse-aether-api)))))
;
;(define-public java-eclipse-aether-test-util
;  (package
;    (inherit java-eclipse-aether-api)
;    (name "java-eclipse-aether-test-util")
;    (arguments
;     `(#:jar-name "eclipse-aether-test-util.jar"
;       #:source-dir "aether-test-util/src/main/java"
;       #:test-dir "aether-test-util/src/test"))
;    (inputs
;     `(("api" ,java-eclipse-aether-api)
;       ("spi" ,java-eclipse-aether-spi)))))
;
;(define-public java-eclipse-aether-util
;  (package
;    (inherit java-eclipse-aether-api)
;    (name "java-eclipse-aether-util")
;    (arguments
;     `(#:jar-name "eclipse-aether-util.jar"
;       #:source-dir "aether-util/src/main/java"
;       #:test-dir "aether-util/src/test"))
;    (inputs
;     `(("api" ,java-eclipse-aether-api)))
;    (native-inputs
;     `(("junit" ,java-junit)
;       ("hamcrest" ,java-hamcrest-core)
;       ("test-util" ,java-eclipse-aether-test-util)))))
;
;(define-public java-eclipse-aether-impl
;  (package
;    (inherit java-eclipse-aether-api)
;    (name "java-eclipse-aether-impl")
;    (arguments
;     `(#:jar-name "eclipse-aether-impl.jar"
;       #:jdk ,icedtea-8
;       #:source-dir "aether-impl/src/main/java"
;       #:test-dir "aether-impl/src/test"
;       #:phases
;       (modify-phases %standard-phases
;         (add-before 'build 'generate-sisu
;           (lambda _
;             (mkdir-p "build/classes/META-INF/sisu")
;             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
;               (lambda _
;                 (display
;                   (string-append
;                     "org.eclipse.aether.internal.impl.DefaultRemoteRepositoryManager\n"
;                     "org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory\n"
;                     "org.eclipse.aether.internal.impl.DefaultLocalRepositoryProvider\n"
;                     "org.eclipse.aether.internal.impl.DefaultOfflineController\n"
;                     "org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory\n"
;                     "org.eclipse.aether.internal.impl.DefaultRepositoryLayoutProvider\n"
;                     "org.eclipse.aether.internal.impl.DefaultFileProcessor\n"
;                     "org.eclipse.aether.internal.impl.LoggerFactoryProvider\n"
;                     "org.eclipse.aether.internal.impl.DefaultTransporterProvider\n"
;                     "org.eclipse.aether.internal.impl.DefaultSyncContextFactory\n"
;                     "org.eclipse.aether.internal.impl.EnhancedLocalRepositoryManagerFactory\n"
;                     "org.eclipse.aether.internal.impl.DefaultArtifactResolver\n"
;                     "org.eclipse.aether.internal.impl.DefaultRepositoryEventDispatcher\n"
;                     "org.eclipse.aether.internal.impl.DefaultUpdatePolicyAnalyzer\n"
;                     "org.eclipse.aether.internal.impl.DefaultInstaller\n"
;                     "org.eclipse.aether.internal.impl.DefaultMetadataResolver\n"
;                     "org.eclipse.aether.internal.impl.DefaultRepositorySystem\n"
;                     "org.eclipse.aether.internal.impl.DefaultChecksumPolicyProvider\n"
;                     "org.eclipse.aether.internal.impl.DefaultDeployer\n"
;                     "org.eclipse.aether.internal.impl.DefaultDependencyCollector\n"
;                     "org.eclipse.aether.internal.impl.DefaultUpdateCheckManager\n"
;                     "org.eclipse.aether.internal.impl.slf4j.Slf4jLoggerFactory\n"
;                     "org.eclipse.aether.internal.impl.DefaultRepositoryConnectorProvider")))))))))
;    (inputs
;     `(("api" ,java-eclipse-aether-api)
;       ("spi" ,java-eclipse-aether-spi)
;       ("util" ,java-eclipse-aether-util)
;       ("javax-inject" ,java-javax-inject)
;       ("sisu-inject" ,java-eclipse-sisu-inject)
;       ("guice" ,java-guice)
;       ("slf4j" ,java-slf4j-api)))
;    (native-inputs
;     `(("junit" ,java-junit)
;       ("hamcrest" ,java-hamcrest-core)
;       ("guava" ,java-guava)
;       ("cglib" ,java-cglib)
;       ("asm" ,java-asm)
;       ("aopalliance" ,java-aopalliance)
;       ("test-util" ,java-eclipse-aether-test-util)))))

(define-public java-commons-compiler
  (package
    (name "java-commons-compiler")
    (version "3.0.7")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/janino-compiler/janino")
                     (commit "6fa05ea19323f46a4f2611671d2ad453c0866564")))
              (sha256
               (base32
                "04kzh2pbrb1011fprnpgy1nwrx0k5aky382k9n9j2w1pj17qplz4"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file
                            (find-files "." "\\.jar$"))
                  #t))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "commons-compiler.jar"
       #:source-dir "commons-compiler/src/main"
       #:tests? #f)); no tests
    (home-page "")
    (synopsis "")
    (description "")
    (license license:bsd-3)))

(define-public java-janino
  (package
    (inherit java-commons-compiler)
    (name "java-janino")
    (arguments
     `(#:jar-name "janino.jar"
       #:source-dir "src/main/java"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "janino"))))))
    (inputs
     `(("compiler" ,java-commons-compiler)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))))

(define-public java-logback-core
  (package
    (name "java-logback-core")
    (version "1.2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/qos-ch/logback/archive/v_"
                                  version ".tar.gz"))
              (sha256
               (base32
                "1x6ga74yfgm94cfx98gybakbrlilx8i2gn6dx13l40kasmys06mi"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "logback.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; One test fails with Unable to set MockitoNamingPolicy on cglib generator which creates FastClasses
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "logback-core"))))))
    (inputs
     `(("java-mail" ,java-javax-mail)
       ("servlet" ,java-tomcat)
       ("compiler" ,java-commons-compiler)
       ("janino" ,java-janino)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("mockito" ,java-mockito-1)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("objenesis" ,java-objenesis)
       ("joda-time" ,java-joda-time)))
    (home-page "https://logback.qos.ch")
    (synopsis "")
    (description "")
    (license (list license:epl1.0 license:lgpl2.1))))

(define java-logback-core-tests
  (package
    (inherit java-logback-core)
    (name "java-logback-core-tests")
    (arguments
     `(#:jar-name "logback-tests.jar"
       #:source-dir "logback-core/src/test/java"
       #:tests? #f))
    (inputs
     `(("core" ,java-logback-core)
       ,@(package-native-inputs java-logback-core)
       ,@(package-inputs java-logback-core)))))

(define-public java-logback-classic
  (package
    (inherit java-logback-core)
    (name "java-logback-classic")
    (arguments
     `(#:jar-name "logback-classic.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; tests require more packages: h2, greenmail, hsql, subethamail, slf4j, log4j, felix
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "logback-classic")))
         ;(add-after 'configure 'build-groovy
         (replace 'build
           (lambda _
             ;(delete-file "src/main/java/ch/qos/logback/classic/boolex/GEventEvaluator.java"))))))
             (mkdir-p "build/classes")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/classes"
                             "-j"; joint compilation
                             (find-files "src/main/" ".*\\.(groovy|java)$")))
               (zero? (system* "ant" "jar"))))))))
    (inputs
     `(("core" ,java-logback-core)
       ("slf4j" ,java-slf4j-api)
       ,@(package-inputs java-logback-core)))
    (native-inputs
     `(("groovy" ,groovy)
       ;; for groovy
       ("cli" ,java-commons-cli)
       ("asm" ,java-asm)
       ("antlr" ,antlr2)
       ;; for tests
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("dom4j" ,java-dom4j)
       ("log4j" ,java-log4j-api)
       ("osgi-framework" ,java-osgi-framework)
       ("assertj" ,java-assertj)
       ("mockito" ,java-mockito-1)
       ("slf4j" ,java-slf4j-api)
       ("core" ,java-logback-core)
       ("logback-tests" ,java-logback-core-tests)))))

(define java-slf4j-api-tests
  (package
    (inherit java-slf4j-api)
    (name "java-slf4j-api-tests")
    (arguments
     `(#:jar-name "slf4j-api-tests.jar"
       #:source-dir "slf4j-api/src/test/java"
       #:tests? #f))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ,@(package-inputs java-slf4j-api)
       ,@(package-native-inputs java-slf4j-api)))))

;; Needs maven-core
(define-public maven-plugin-testing-harness
  (package
    (name "maven-plugin-testing-harness")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/plugin-"
                                  "testing/maven-plugin-testing-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "15vgjar275nkz47c05mpqb4g2rha0abc752xhxcxc34z2z47z6p5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-testing-harness.jar"
       #:source-dir "maven-plugin-testing-harness/src/main/java"
       #:jdk ,icedtea-8))
    (inputs
     `(("artifact" ,maven-artifact)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-shared-utils
  (package
    (name "maven-shared-utils")
    ; latest is 3.2.0, but is not supported by maven-embedder
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "0vfaas4g09ch0agrd1dcxcmhdd3w971ssvfr9mx9gi2lp5nv8w66"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-shared-utils.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'remove-cyclic-dep
           (lambda _
             (delete-file "src/test/java/org/apache/maven/shared/utils/introspection/ReflectionValueExtractorTest.java")
             #t)))))
    (inputs
     `(("jansi" ,java-jansi)
       ("io" ,java-commons-io)
       ("jsr305" ,java-jsr305)
       ("container" ,java-plexus-container-default)))
    (native-inputs
     `(("unzip" ,unzip)
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("lang3" ,java-commons-lang3)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven
  (package
    (name "maven")
    (version "3.5.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "1xw0g85w1rnxrjia3d7knpnwh8jg5052qsnqjl6nyly2k8320qrg"))
              (patches
                (search-patches "maven-generate-component-xml.patch"
                                "maven-generate-javax-inject-named.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (substitute* "apache-maven/src/bin/mvn"
               (("cygwin=false;")
                (string-append
                  "CLASSPATH=" (getenv "CLASSPATH") "\n"
                  "cygwin=false;"))
               (("-classpath.*") "-classpath ${CLASSPATH} \\\n"))))
         (delete 'check)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out") "/bin/"))
                   (conf (string-append (assoc-ref outputs "out") "/conf/")))
               (mkdir-p (string-append (assoc-ref outputs "out") "/lib"))
               (for-each (lambda (file)
                           (install-file (string-append "apache-maven/src/bin/" file)
                                         bin)
                           (chmod (string-append bin file) #o755))
                '("mvn" "mvnDebug" "mvnyjp"))
               (install-file "apache-maven/src/bin/m2.conf" bin)
               (copy-recursively "apache-maven/src/conf" conf)))))))
    (inputs
     `(("classworlds" ,java-plexus-classworlds)
       ("artifact" ,maven-artifact)
       ("embedder" ,maven-embedder)
       ("core" ,maven-core)
       ("compat" ,maven-compat)
       ("builder" ,maven-builder-support)
       ("model" ,maven-model)
       ("model-builder" ,maven-model-builder)
       ("settings" ,maven-settings)
       ("settings-builder" ,maven-settings-builder)
       ("plugin-api" ,maven-plugin-api)
       ("metadata", maven-repository-metadata)
       ("shared-utils" ,maven-shared-utils)
       ("api" ,maven-resolver-api)
       ("spi" ,maven-resolver-spi)
       ("util" ,maven-resolver-util)
       ("impl" ,maven-resolver-impl)
       ("resolver-connector-basic" ,maven-resolver-connector-basic)
       ("resolver-provider" ,maven-resolver-provider)
       ("transport-wagon" ,maven-resolver-transport-wagon)
       ("wagon" ,maven-wagon-provider-api)
       ("wagon-file" ,maven-wagon-file)
       ("wagon-http" ,maven-wagon-http)
       ("logging" ,java-commons-logging-minimal)
       ("httpclient" ,java-httpcomponents-client)
       ("httpcore" ,java-httpcomponents-core)
       ("wagon-http-shared" ,maven-wagon-http-shared)
       ("wagon-tck-http" ,maven-wagon-tck-http)
       ("container" ,java-eclipse-sisu-plexus)
       ("guice" ,java-guice)
       ("aop" ,java-aopalliance)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("annots" ,java-plexus-component-annotations)
       ("plexus-utils" ,java-plexus-utils)
       ("interpolation" ,java-plexus-interpolation)
       ("sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("cipher" ,java-plexus-cipher)
       ("guava" ,java-guava)
       ("jansi" ,java-jansi)
       ("jsr250" ,java-jsr250)
       ("cdi" ,java-cdi-api)
       ("cli" ,java-commons-cli)
       ("io" ,java-commons-io)
       ("lang3" ,java-commons-lang3)
       ("slf4j" ,java-slf4j-api)
       ;; TODO: replace with maven-slf4j-provider
       ("simple" ,java-slf4j-simple)))
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
       ;("container" ,java-plexus-container-default)
       ("container" ,java-eclipse-sisu-plexus)
       ("annots" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
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
    (inputs '())
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ;("container" ,java-plexus-container-default)
       ("container" ,java-eclipse-sisu-plexus)
       ("annots" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
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
       #:test-dir "maven-settings-builder/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (chmod "components.sh" #o755)
             (zero? (system* "./components.sh" "maven-settings-builder/src/main/java"
                             "build/classes/META-INF/plexus/components.xml")))))))
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

(define-public maven-plugin-api
  (package
    (inherit maven)
    (name "maven-plugin-api")
    (arguments
     `(#:jar-name "maven-plugin-api.jar"
       #:source-dir "maven-plugin-api/src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "maven-plugin-api/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "maven-plugin-api/src/main/java" version
                               "false" "true")))
             (let ((file "maven-plugin-api/src/main/mdo/lifecycle.mdo"))
               (and
                 (modello-single-mode file "1.0.0" "java")
                 (modello-single-mode file "1.0.0" "xpp3-reader")
                 (modello-single-mode file "1.0.0" "xpp3-writer"))))))))
    (inputs
     `(("artifact" ,maven-artifact)
       ("model" ,maven-model)
       ;("container" ,java-plexus-container-default)
       ("container" ,java-eclipse-sisu-plexus)
       ("annots" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("utils" ,java-plexus-utils)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
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
       ("container" ,java-eclipse-sisu-plexus)
       ("annots" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("xmlunit" ,java-xmlunit)
       ("xmlunit" ,java-xmlunit-legacy)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("classworlds" ,java-plexus-classworlds)))))

(define-public maven-repository-metadata
  (package
    (inherit maven)
    (name "maven-repository-metadata")
    (arguments
     `(#:jar-name "maven-repository-metadata.jar"
       #:source-dir "maven-repository-metadata/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "maven-repository-metadata/src/main/java" version
                               "false" "true")))
             (let ((file "maven-repository-metadata/src/main/mdo/metadata.mdo"))
               (and
                 (modello-single-mode file "1.1.0" "java")
                 (modello-single-mode file "1.1.0" "xpp3-reader")
                 (modello-single-mode file "1.1.0" "xpp3-writer"))))))))
    (inputs '())
    (native-inputs
     `(("modello" ,java-modello-core)
       ;; for modello:
       ;("container" ,java-plexus-container-default)
       ("container" ,java-eclipse-sisu-plexus)
       ("annots" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)
       ("utils" ,java-plexus-utils)
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

(define-public maven-resolver-provider
  (package
    (inherit maven)
    (name "maven-resolver-provider")
    (arguments
     `(#:jar-name "maven-resolver-provider.jar"
       #:source-dir "maven-resolver-provider/src/main/java"
       #:test-dir "maven-resolver-provider/src/test"
       #:jdk ,icedtea-8
       #:tests? #f; dependency loop on maven-core (@Component RepositorySystem)
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (chmod "components.sh" #o755)
             (zero? (system* "./components.sh" "maven-resolver-provider/src/main/java"
                             "build/classes/META-INF/plexus/components.xml")))))))
    (inputs
     `(("spi" ,maven-resolver-spi)
       ("api" ,maven-resolver-api)
       ("impl" ,maven-resolver-impl)
       ("util" ,maven-resolver-util)
       ("model" ,maven-model)
       ("model-builder" ,maven-model-builder)
       ("support" ,maven-builder-support)
       ("repository" ,maven-repository-metadata)
       ("utils" ,java-plexus-utils)
       ("annotations" ,java-plexus-component-annotations)
       ("lang3" ,java-commons-lang3)
       ("guice" ,java-guice)
       ("guava" ,java-guava)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("javax-inject" ,java-javax-inject)))))

;(define-public maven-resolver-provider
;  (package
;    (inherit maven-resolver-provider-boot)
;    (arguments
;     `(#:jar-name "maven-resolver-provider.jar"
;       #:source-dir "src/main/java"
;       #:jdk ,icedtea-8
;       #:test-exclude (list "**/DefaultArtifactDescriptorReaderTest.java")
;       #:phases
;       (modify-phases %standard-phases
;         (add-before 'configure 'chdir
;           (lambda _
;             ;; Tests assume we're in this directory
;             (chdir "maven-resolver-provider")))
;         (add-before 'build 'generate-components.xml
;           (lambda _
;             (mkdir-p "build/classes/META-INF/plexus")
;             (chmod "../components.sh" #o755)
;             (zero? (system* "../components.sh" "src/main/java"
;                             "build/classes/META-INF/plexus/components.xml"))))
;         (add-before 'check 'fix-assumptions
;           (lambda _
;             ;; Errors about the version of some files
;             (substitute* "src/test/java/org/apache/maven/repository/internal/DefaultArtifactDescriptorReaderTest.java"
;               (("20130404.090532-2") "SNAPSHOT")))))))
;         ;(add-before 'check 'copy-test-classes
;         ;  (lambda _
;         ;    (system* "ant" "compile-tests")
;         ;    (mkdir-p "target/test-classes")
;         ;    (copy-recursively "build/test-classes" "target/test-classes")
;         ;    ;(copy-recursively "src/test/resources/repo" "target/test-classes/repo")
;         ;    #t)))))
;    (native-inputs
;     `(;; For building tests:
;       ("maven-core" ,maven-core-boot)
;       ("container" ,java-eclipse-sisu-plexus)
;       ("junit" ,java-junit)
;       ("mockito" ,java-mockito-1)
;       ;; For running tests:
;       ("hamcrest" ,java-hamcrest-core)
;       ("aop" ,java-aopalliance)
;       ("classworlds" ,java-plexus-classworlds)
;       ("plugin" ,maven-plugin-api)
;       ("cglib" ,java-cglib)
;       ("asm" ,java-asm)
;       ("interpolation" ,java-plexus-interpolation)
;       ("artifact" ,maven-artifact)
;       ("objenesis" ,java-objenesis)
;       ("transport-wagon" ,maven-resolver-transport-wagon)
;       ("wagon-file" ,maven-wagon-file)
;       ("wagon-api" ,maven-wagon-provider-api)
;       ("connector-basic" ,maven-resolver-connector-basic)))))

(define maven-core-boot
  (package
    (inherit maven)
    (name "maven-core-boot")
    (arguments
     `(#:jar-name "maven-core.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       ;; Tests fail with
       ;; org.codehaus.plexus.component.repository.exception.ComponentLookupException: java.util.NoSuchElementException
       ;;   role: org.apache.maven.repository.RepositorySystem
       ;; It seems they need maven-compat, which requires maven-core
       #:tests? #f
       ;#:fake-maven? #t
       ;#:version ,(package-version maven)
       ;#:pom-file "pom.xml"
       ;#:group-id "org.apache.maven"
       ;#:artifact-id "maven-core"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             ;; Required for generating components.xml in maven-core
             (chdir "maven-core")))
         (add-before 'build 'copy-resources
           (lambda _
             (mkdir-p "build/classes/")
             (copy-recursively "src/main/resources" "build/classes")))
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "../sisu.sh" #o755)
             (zero? (system* "../sisu.sh" "src/main/java"
                             "build/classes/META-INF/sisu/javax.inject.Named"))))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "src/main/java" version
                               "false" "true")))
             (let ((file "src/main/mdo/toolchains.mdo"))
               (and
                 (modello-single-mode file "1.1.0" "java")
                 (modello-single-mode file "1.1.0" "xpp3-reader")
                 (modello-single-mode file "1.1.0" "xpp3-writer"))))))))
    (inputs
     `(("artifact" ,maven-artifact)
       ("resolver" ,maven-resolver-provider)
       ("support" ,maven-builder-support)
       ("model" ,maven-model)
       ("model-builder" ,maven-model-builder)
       ("settings" ,maven-settings)
       ("settings-builder" ,maven-settings-builder)
       ("plugin-api" ,maven-plugin-api)
       ("repository-metadat" ,maven-repository-metadata)
       ("shared-utils" ,maven-shared-utils)
       ("annotations" ,java-plexus-component-annotations)
       ("utils" ,java-plexus-utils)
       ("lang3" ,java-commons-lang3)
       ("guava" ,java-guava)
       ("guice" ,java-guice)
       ("aether-api" ,maven-resolver-api)
       ("aether-spi" ,maven-resolver-spi)
       ("aether-util" ,maven-resolver-util)
       ("aether-impl" ,maven-resolver-impl)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("container" ,java-eclipse-sisu-plexus)
       ("java-javax-inject" ,java-javax-inject)
       ("classworld" ,java-plexus-classworlds)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("classworlds" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("junit" ,java-junit)
       ("mockito" ,java-mockito-1)
       ("jxpath" ,java-commons-jxpath)))))

(define-public maven-core
  (package
    (inherit maven-core-boot)
    (name "maven-core")
    (arguments
      (substitute-keyword-arguments (package-arguments maven-core-boot)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-after 'build 'generate-metadata
              (lambda _
                (delete-file "build/classes/META-INF/plexus/components.xml")
                (and (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                                     "--source" "build/classes/META-INF/plexus"
                                     "--output" "build/classes/META-INF/plexus/components.t.xml"
                                     ;; I don't know what these two options do, but if
                                     ;; not present, it ends with a NullPointerException.
                                     "--classes" "build/classes"
                                     "--descriptors" "build/classes"))
                     ;; Now we merge all other components from hand-written xml
                     ;; FIXME: This should be taken care of by plexus-component-metadata directly
                     (zero? (system* "sh" "-c"
                                     (string-append "(cat build/classes/META-INF/plexus/components.t.xml |"
                                                    "sed -e 's|</component-set>||' -e 's|</components>||' ; "
                                                    "cat src/main/resources/META-INF/plexus/artifact-handlers.xml |"
                                                    " sed -e 's|<?xml.*||' -e 's|<component-set>||' -e 's|<components>||'"
                                                    " -e 's|</component-set>||' -e 's|</components>||'; "
                                                    "cat src/main/resources/META-INF/plexus/components.xml |"
                                                    " sed -e 's|<?xml.*||' -e 's|<component-set>||' -e 's|<components>||'"
                                                    " -e 's|</component-set>||' -e 's|</components>||'; "
                                                    "cat src/main/resources/META-INF/plexus/default-bindings.xml |"
                                                    " sed -e 's|<?xml.*||' -e 's|<component-set>||' -e 's|<components>||' )>"
                                                    "build/classes/META-INF/plexus/components.xml"))))))
                     ;(substitute* "build/classes/META-INF/plexus/components.xml"
                     ;  (("</components>")
                     ;   (string-append
                     ;     "  <component>\n"
                     ;     "    <role>org.sonatype.plexus.components.sec.dispatcher.SecDispatcher</role>\n"
                     ;     "    <role-hint>maven</role-hint>\n"
                     ;     "    <implementation>org.sonatype.plexus.components.sec.dispatcher.DefaultSecDispatcher</implementation>\n"
                     ;     "    <description>Maven Security dispatcher</description>\n"
                     ;     "    <requirements>\n"
                     ;     "      <requirement>\n"
                     ;     "        <role>org.sonatype.plexus.components.cipher.PlexusCipher</role>\n"
                     ;     "        <field-name>_cipher</field-name>\n"
                     ;     "      </requirement>\n"
                     ;     "      <requirement>\n"
                     ;     "        <role>org.sonatype.plexus.components.sec.dispatcher.PasswordDecryptor</role>\n"
                     ;     "        <field-name>_decryptors</field-name>\n"
                     ;     "      </requirement>\n"
                     ;     "    </requirements>\n"
                     ;     "    <configuration>\n"
                     ;     "      <_configuration-file>~/.m2/settings-security.xml</_configuration-file>\n"
                     ;     "    </configuration>\n"
                     ;     "  </component>\n"
                     ;     "</components>")))
                     ;#t)))
            ;(add-after 'generate-metadata 'fix-metadata
            ;  (lambda _
            ;    (substitute* "build/classes/META-INF/plexus/components.xml"
            ;      (("<field-name>") "<role-hint /><field-name>"))
            ;    #t))
            (add-after 'generate-metadata 'rebuild
              (lambda _
                (zero? (system* "ant" "jar"))))))))
    (native-inputs
     `(("metadata" ,java-plexus-component-metadata)
       ("cli" ,java-commons-cli)
       ("plexus-cli" ,java-plexus-cli)
       ("jdom2" ,java-jdom2)
       ("qdox" ,java-qdox)
       ("core" ,maven-core-boot)
       ,@(package-native-inputs maven-core-boot)))))

(define-public maven-wagon-provider-api
  (package
    (name "maven-wagon-provider-api")
    (version "3.0.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/wagon/"
                                  "wagon-" version "-source-release.zip"))
              (sha256 (base32 "1qb0q4m7vmf290xp3fnfdi3pwl3hkskia5g3z2v82q1ch3y2knqv"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-wagon-provider-api.jar"
       #:source-dir "wagon-provider-api/src/main/java"
       #:test-dir "wagon-provider-api/src/test"))
    (inputs
     `(("utils" ,java-plexus-utils)))
    (native-inputs
     `(("unzip" ,unzip)
       ("junit" ,java-junit)
       ("easymock" ,java-easymock)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-wagon-provider-test
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-provider-test")
    (arguments
     `(#:jar-name "maven-wagon-provider-test.jar"
       #:source-dir "wagon-provider-test/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("containers" ,java-plexus-container-default)
       ("jetty-util" ,java-eclipse-jetty-util-9.2)
       ("jetty-security" ,java-eclipse-jetty-security-9.2)
       ("jetty-server" ,java-eclipse-jetty-server-9.2)
       ("jetty-servlet" ,java-eclipse-jetty-servlet-9.2)
       ("slf4j" ,java-slf4j-api)
       ("servlet" ,java-tomcat)
       ("provider-api" ,maven-wagon-provider-api)))))

(define-public maven-wagon-file
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-file")
    (arguments
     `(#:jar-name "maven-wagon-file.jar"
       #:source-dir "wagon-providers/wagon-file/src/main/java"
       #:test-dir "wagon-providers/wagon-file/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'check 'fix-paths
           (lambda _
             (substitute* "wagon-providers/wagon-file/src/test/java/org/apache/maven/wagon/providers/file/FileWagonTest.java"
               (("target") "build"))))
         (add-after 'build 'generate-metadata
           (lambda _
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                             "--source" "wagon-providers/wagon-file/src/main/java"
                             "--output" "build/classes/META-INF/plexus/components.xml"
                             ;; I don't know what these two options do, but if
                             ;; not present, it ends with a NullPointerException.
                             "--classes" "build/classes"
                             "--descriptors" "build/classes/META-INF"))))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (zero? (system* "ant" "jar")))))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("provider-test" ,maven-wagon-provider-test)
       ("metadata" ,java-plexus-component-metadata)
       ("annotations" ,java-plexus-component-annotations)
       ("container" ,java-eclipse-sisu-plexus)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("guice" ,java-guice)
       ("inject" ,java-javax-inject)
       ("cglib" ,java-cglib)
       ("slf4j" ,java-slf4j-api)
       ("utils" ,java-plexus-utils)
       ("cli" ,java-plexus-cli)
       ("plugin-api" ,maven-plugin-api)
       ("plugin-annotations" ,maven-plugin-annotations)
       ("core" ,maven-core)
       ("model" ,maven-model)
       ("cli" ,java-commons-cli)
       ("qdox" ,java-qdox)
       ("jdom2" ,java-jdom2)
       ("asm" ,java-asm)
       ("xbean" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))))

(define-public maven-wagon-http-shared
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http-shared")
    (arguments
     `(#:jar-name "maven-wagon-http-shared.jar"
       #:source-dir "wagon-providers/wagon-http-shared/src/main/java"
       #:test-dir "wagon-providers/wagon-http-shared/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                             "--source" "wagon-providers/wagon-http-shared/src/main/java"
                             "--output" "build/classes/META-INF/plexus/components.xml"
                             ;; I don't know what these two options do, but if
                             ;; not present, it ends with a NullPointerException.
                             "--classes" "build/classes"
                             "--descriptors" "build/classes/META-INF"))))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (zero? (system* "ant" "jar")))))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("httpclient" ,java-httpcomponents-client)
       ("httpcore" ,java-httpcomponents-core)
       ("io" ,java-commons-io)
       ("jsoup" ,java-jsoup)
       ("provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("provider-test" ,maven-wagon-provider-test)
       ("metadata" ,java-plexus-component-metadata)
       ("annotations" ,java-plexus-component-annotations)
       ("container" ,java-eclipse-sisu-plexus)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("guice" ,java-guice)
       ("inject" ,java-javax-inject)
       ("cglib" ,java-cglib)
       ("slf4j" ,java-slf4j-api)
       ("utils" ,java-plexus-utils)
       ("cli" ,java-plexus-cli)
       ("plugin-api" ,maven-plugin-api)
       ("plugin-annotations" ,maven-plugin-annotations)
       ("core" ,maven-core)
       ("model" ,maven-model)
       ("cli" ,java-commons-cli)
       ("qdox" ,java-qdox)
       ("jdom2" ,java-jdom2)
       ("asm" ,java-asm)
       ("xbean" ,java-geronimo-xbean-reflect)
       ,@(package-native-inputs maven-wagon-provider-api)))))

(define-public maven-wagon-tck-http
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-tck-http")
    (arguments
     `(#:jar-name "maven-wagon-tck-http.jar"
       #:source-dir "wagon-tcks/wagon-tck-http/src/main/java"
       #:tests? #f; no tests
       #:jdk ,icedtea-8))
    (inputs
     `(("util" ,java-plexus-utils)
       ("provider-api" ,maven-wagon-provider-api)
       ("servlet" ,java-tomcat)
       ("slf4j" ,java-slf4j-api)
       ("codec" ,java-commons-codec)
       ("container" ,java-eclipse-sisu-plexus)
       ("classworlds" ,java-plexus-classworlds)
       ("jetty-util" ,java-eclipse-jetty-util-9.2)
       ("jetty-webapp" ,java-eclipse-jetty-webapp-9.2)
       ("jetty-security" ,java-eclipse-jetty-security-9.2)
       ("jetty-server" ,java-eclipse-jetty-server-9.2)
       ("jetty-servlet" ,java-eclipse-jetty-servlet-9.2)))))

(define-public maven-wagon-http-lightweight
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http-lightweight")
    (arguments
     `(#:jar-name "maven-wagon-http-lightweight.jar"
       #:source-dir "wagon-providers/wagon-http-lightweight/src/main/java"
       #:test-dir "wagon-providers/wagon-http-lightweight/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-after 'build 'generate-metadata
           (lambda _
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                             "--source" "wagon-providers/wagon-http-lightweight/src/main/java"
                             "--output" "build/classes/META-INF/plexus/components.xml"
                             ;; I don't know what these two options do, but if
                             ;; not present, it ends with a NullPointerException.
                             "--classes" "build/classes"
                             "--descriptors" "build/classes/META-INF"))))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (zero? (system* "ant" "jar"))))
         (add-before 'check 'fix-resource-path
           (lambda _
             (substitute* "wagon-providers/wagon-http-lightweight/src/test/java/org/apache/maven/wagon/providers/http/LightweightHttpsWagonTest.java"
               (("src/test") "wagon-providers/wagon-http-lightweight/src/test"))
             #t)))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("httpclient" ,java-httpcomponents-client)
       ("httpcore" ,java-httpcomponents-core)
       ("wagon-http-shared" ,maven-wagon-http-shared)
       ("wagon-tck-http" ,maven-wagon-tck-http)
       ("provider-api" ,maven-wagon-provider-api)
       ("io" ,java-commons-io)))
    (native-inputs
     `(("provider-test" ,maven-wagon-provider-test)
       ("metadata" ,java-plexus-component-metadata)
       ("annotations" ,java-plexus-component-annotations)
       ("container" ,java-eclipse-sisu-plexus)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("guice" ,java-guice)
       ("inject" ,java-javax-inject)
       ("cglib" ,java-cglib)
       ("slf4j" ,java-slf4j-api)
       ("utils" ,java-plexus-utils)
       ("cli" ,java-plexus-cli)
       ("plugin-api" ,maven-plugin-api)
       ("plugin-annotations" ,maven-plugin-annotations)
       ("core" ,maven-core)
       ("model" ,maven-model)
       ("cli" ,java-commons-cli)
       ("qdox" ,java-qdox)
       ("jdom2" ,java-jdom2)
       ("asm" ,java-asm)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("servlet" ,java-tomcat)
       ("jetty-util" ,java-eclipse-jetty-util-9.2)
       ("jetty-io" ,java-eclipse-jetty-io-9.2)
       ("jetty-http" ,java-eclipse-jetty-http-9.2)
       ("jetty-server" ,java-eclipse-jetty-server-9.2)
       ("jetty-servlet" ,java-eclipse-jetty-servlet-9.2)
       ("jetty-security" ,java-eclipse-jetty-security-9.2)
       ("jsoup" ,java-jsoup)
       ("hamcrest" ,java-hamcrest-core)
       ("logging" ,java-commons-logging-minimal)
       ("simple" ,java-slf4j-simple)
       ,@(package-native-inputs maven-wagon-provider-api)))))

(define-public maven-wagon-http
  (package
    (inherit maven-wagon-provider-api)
    (name "maven-wagon-http")
    (arguments
     `(#:jar-name "maven-wagon-http.jar"
       #:source-dir "wagon-providers/wagon-http/src/main/java"
       #:test-dir "wagon-providers/wagon-http/src/test"
       #:test-exclude (list
                        "**/Abstract*.java"
                        ;; FIXME: javax.net.ssl.SSLHandshakeException:
                        ;; sun.security.validator.ValidatorException:
                        ;; PKIX path building failed:
                        ;; sun.security.provider.certpath.SunCertPathBuilderException:
                        ;; unable to find valid certification path to requested target
                        "**/HttpsWagonPreemptiveTest.java"
                        "**/HttpsWagonTest.java"
                        ;; Injection errors
                        "**/TckTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "wagon-providers/wagon-http/src/main/resources/META-INF/plexus/components.xml"
                           "build/classes/META-INF/plexus")
             #t))
         (add-before 'check 'fix-resource-path
           (lambda _
             (substitute* '("wagon-providers/wagon-http/src/test/java/org/apache/maven/wagon/providers/http/HttpsWagonPreemptiveTest.java"
                            "wagon-providers/wagon-http/src/test/java/org/apache/maven/wagon/providers/http/HttpsWagonTest.java")
               (("src/test") "wagon-providers/wagon-http/src/test"))
             #t)))))
    (inputs
     `(("utils" ,java-plexus-utils)
       ("httpclient" ,java-httpcomponents-client)
       ("httpcore" ,java-httpcomponents-core)
       ("wagon-http-shared" ,maven-wagon-http-shared)
       ("wagon-tck-http" ,maven-wagon-tck-http)
       ("provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("provider-test" ,maven-wagon-provider-test)
       ("metadata" ,java-plexus-component-metadata)
       ("annotations" ,java-plexus-component-annotations)
       ("container" ,java-eclipse-sisu-plexus)
       ("container" ,java-plexus-container-default)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("classworlds" ,java-plexus-classworlds)
       ("guava" ,java-guava)
       ("guice" ,java-guice)
       ("inject" ,java-javax-inject)
       ("cglib" ,java-cglib)
       ("slf4j" ,java-slf4j-api)
       ("utils" ,java-plexus-utils)
       ("cli" ,java-plexus-cli)
       ("plugin-api" ,maven-plugin-api)
       ("plugin-annotations" ,maven-plugin-annotations)
       ("core" ,maven-core)
       ("model" ,maven-model)
       ("cli" ,java-commons-cli)
       ("qdox" ,java-qdox)
       ("jdom2" ,java-jdom2)
       ("asm" ,java-asm)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("servlet" ,java-tomcat)
       ("jetty-util" ,java-eclipse-jetty-util-9.2)
       ("jetty-io" ,java-eclipse-jetty-io-9.2)
       ("jetty-http" ,java-eclipse-jetty-http-9.2)
       ("jetty-server" ,java-eclipse-jetty-server-9.2)
       ("jetty-servlet" ,java-eclipse-jetty-servlet-9.2)
       ("jetty-security" ,java-eclipse-jetty-security-9.2)
       ("hamcrest" ,java-hamcrest-core)
       ("logging" ,java-commons-logging-minimal)
       ("codec" ,java-commons-codec)
       ("io" ,java-commons-io)
       ("jsoup" ,java-jsoup)
       ("simple" ,java-slf4j-simple)
       ,@(package-native-inputs maven-wagon-provider-api)))))

(define-public maven-embedder
  (package
    (inherit maven)
    (name "maven-embedder")
    (arguments
     `(#:jar-name "maven-embedder.jar"
       #:source-dir "maven-embedder/src/main/java"
       #:test-dir "maven-embedder/src/test"
       #:test-exclude (list "**/MavenCliTest.java")
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "sisu.sh" #o755)
             (zero? (system* "./sisu.sh" "maven-embedder/src/main/java"
                             "build/classes/META-INF/sisu/javax.inject.Named"))))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "maven-embedder/src/main/java" version
                               "false" "true")))
             (let ((file "maven-embedder/src/main/mdo/core-extensions.mdo"))
               (and
                 (modello-single-mode file "1.0.0" "java")
                 (modello-single-mode file "1.0.0" "xpp3-reader")
                 (modello-single-mode file "1.0.0" "xpp3-writer")))))
         (add-before 'check 'fix-test-paths
           (lambda _
             (substitute* "maven-embedder/src/test/java/org/apache/maven/cli/CLIManagerDocumentationTest.java"
               (("target/test-classes") "build/test-classes"))))
         (add-before 'check 'fix-compilation
           (lambda _
             ;; Tests are in the java/ subdir. Other subdirectories contain
             ;; additional test plugins, with duplicate classes, so we can't
             ;; compile them. Also, they are meant to be built with maven, to
             ;; test its build process.
             (substitute* "build.xml"
               (("srcdir=\"maven-embedder/src/test\"")
                "srcdir=\"maven-embedder/src/test/java\"")))))))
    (inputs
     `(("core" ,maven-core)
       ("artifact" ,maven-artifact)
       ("plugin-api" ,maven-plugin-api)
       ("support" ,maven-builder-support)
       ("model" ,maven-model)
       ("model-builder" ,maven-model-builder)
       ("settings" ,maven-settings)
       ("settings-builder" ,maven-settings-builder)
       ("maven-shared-utils" ,maven-shared-utils)
       ("classworlds" ,java-plexus-classworlds)
       ("util" ,java-plexus-utils)
       ("container" ,java-eclipse-sisu-plexus)
       ("cipher" ,java-plexus-cipher)
       ("annotations" ,java-plexus-component-annotations)
       ("sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("aether-util" ,maven-resolver-util)
       ("aether-api" ,maven-resolver-api)
       ("logback" ,java-logback-core)
       ("logback-classic" ,java-logback-classic)
       ("cli" ,java-commons-cli)
       ("io" ,java-commons-io)
       ("lang3" ,java-commons-lang3)
       ("guava" ,java-guava)
       ("guice" ,java-guice)
       ("inject" ,java-javax-inject)
       ("slf4j" ,java-slf4j-api)
       ("slf4j-simple" ,java-slf4j-simple)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ("container" ,java-eclipse-sisu-plexus)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))))

(define-public maven-resolver-api
  (package
    (name "maven-resolver-api")
    (version "1.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-resolver/"
                                  "archive/maven-resolver-" version ".tar.gz"))
              (sha256
               (base32
                "1kvb64f6anmn93vdyb4s90189jrrm31agrp2gf0n9njwjp5vcddz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resolver-api.jar"
       #:source-dir "maven-resolver-api/src/main/java"
       #:test-dir "maven-resolver-api/src/test"))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)))
    (home-page "https://github.com/apache/maven-resolver")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-resolver-spi
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-spi")
    (arguments
     `(#:jar-name "maven-resolver-spi.jar"
       #:source-dir "maven-resolver-spi/src/main/java"
       #:test-dir "maven-resolver-spi/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("api" ,maven-resolver-api)))))

(define-public maven-resolver-test-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-test-util")
    (arguments
     `(#:jar-name "maven-resolver-test-util.jar"
       #:source-dir "maven-resolver-test-util/src/main/java"
       #:test-dir "maven-resolver-test-util/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("api" ,maven-resolver-api)
       ("spi" ,maven-resolver-spi)))))

(define-public maven-resolver-util
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-util")
    (arguments
     `(#:jar-name "maven-resolver-util.jar"
       #:source-dir "maven-resolver-util/src/main/java"
       #:test-dir "maven-resolver-util/src/test"
       #:jdk ,icedtea-8))
    (inputs
     `(("api" ,maven-resolver-api)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("test-util" ,maven-resolver-test-util)))))

(define-public maven-resolver-impl
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-impl")
    (arguments
     `(#:jar-name "maven-resolver-impl.jar"
       #:source-dir "maven-resolver-impl/src/main/java"
       #:test-dir "maven-resolver-impl/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display
                   (string-append
                     "org.eclipse.aether.internal.impl.DefaultArtifactResolver\n"
                     "org.eclipse.aether.internal.impl.DefaultTransporterProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultUpdatePolicyAnalyzer\n"
                     "org.eclipse.aether.internal.impl.slf4j.Slf4jLoggerFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositorySystem\n"
                     "org.eclipse.aether.internal.impl.LoggerFactoryProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultFileProcessor\n"
                     "org.eclipse.aether.internal.impl.DefaultLocalRepositoryProvider\n"
                     "org.eclipse.aether.internal.impl.SimpleLocalRepositoryManagerFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultDeployer\n"
                     "org.eclipse.aether.internal.impl.DefaultMetadataResolver\n"
                     "org.eclipse.aether.internal.impl.DefaultInstaller\n"
                     "org.eclipse.aether.internal.impl.Maven2RepositoryLayoutFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultSyncContextFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultOfflineController\n"
                     "org.eclipse.aether.internal.impl.EnhancedLocalRepositoryManagerFactory\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryLayoutProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultRemoteRepositoryManager\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryEventDispatcher\n"
                     "org.eclipse.aether.internal.impl.DefaultRepositoryConnectorProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultUpdateCheckManager\n"
                     "org.eclipse.aether.internal.impl.DefaultChecksumPolicyProvider\n"
                     "org.eclipse.aether.internal.impl.DefaultDependencyCollector\n")))))))))
    (inputs
     `(("api" ,maven-resolver-api)
       ("spi" ,maven-resolver-spi)
       ("util" ,maven-resolver-util)
       ("sisu" ,java-eclipse-sisu-inject)
       ("inject" ,java-javax-inject)
       ("guice" ,java-guice)
       ("guava" ,java-guava)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("aopalliance" ,java-aopalliance)
       ("slf4j" ,java-slf4j-api)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("test-util" ,maven-resolver-test-util)))))

(define-public maven-resolver-connector-basic
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-connector-basic")
    (arguments
     `(#:jar-name "maven-resolver-connector-basic.jar"
       #:source-dir "maven-resolver-connector-basic/src/main/java"
       #:test-dir "maven-resolver-connector-basic/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory\n"))))))))
    (inputs
     `(("api" ,maven-resolver-api)
       ("spi" ,maven-resolver-spi)
       ("util" ,maven-resolver-util)
       ("inject" ,java-javax-inject)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("test-util" ,maven-resolver-test-util)))))

(define-public maven-resolver-transport-wagon
  (package
    (inherit maven-resolver-api)
    (name "maven-resolver-transport-wagon")
    (arguments
     `(#:jar-name "maven-resolver-transport-wagon.jar"
       #:source-dir "maven-resolver-transport-wagon/src/main/java"
       #:test-dir "maven-resolver-transport-wagon/src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-sisu
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (with-output-to-file "build/classes/META-INF/sisu/javax.inject.Named"
               (lambda _
                 (display "org.eclipse.aether.transport.wagon.WagonTransporterFactory\n")))
             #t))
         (add-before 'build 'generate-components.xml
           (lambda _
             (mkdir-p "build/classes/META-INF/plexus")
             (with-output-to-file "build/classes/META-INF/plexus/components.xml"
               (lambda _
                 (display
                   (string-append
                     "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n"
                     "<component-set>\n"
                     "  <components>\n"
                     "    <component>\n"
                     "      <role>org.eclipse.aether.transport.wagon.WagonConfigurator</role>\n"
                     "      <role-hint>plexus</role-hint>\n"
                     "      <implementation>org.eclipse.aether.internal.transport.wagon.PlexusWagonConfigurator</implementation>\n"
                     "      <description />\n"
                     "      <isolated-realm>false</isolated-realm>\n"
                     "      <requirements>\n"
                     "        <requirement>\n"
                     "          <role>org.codehaus.plexus.PlexusContainer</role>\n"
                     "          <role-hint />\n"
                     "          <field-name>container</field-name>\n"
                     "        </requirement>\n"
                     "      </requirements>\n"
                     "    </component>\n"
                     "    <component>\n"
                     "      <role>org.eclipse.aether.transport.wagon.WagonProvider</role>\n"
                     "      <role-hint>plexus</role-hint>\n"
                     "      <implementation>org.eclipse.aether.internal.transport.wagon.PlexusWagonProvider</implementation>\n"
                     "      <description />\n"
                     "      <isolated-realm>false</isolated-realm>\n"
                     "      <requirements>\n"
                     "        <requirement>\n"
                     "          <role>org.codehaus.plexus.PlexusContainer</role>\n"
                     "          <role-hint />\n"
                     "          <field-name>container</field-name>\n"
                     "        </requirement>\n"
                     "      </requirements>\n"
                     "    </component>\n"
                     "  </components>\n"
                     "</component-set>\n"))))
             #t)))))
    (inputs
     `(("api" ,maven-resolver-api)
       ("spi" ,maven-resolver-spi)
       ("util" ,maven-resolver-util)
       ("inject" ,java-javax-inject)
       ("wagon-api" ,maven-wagon-provider-api)
       ("annotation" ,java-plexus-component-annotations)
       ("classworld" ,java-plexus-classworlds)
       ("plexus-util" ,java-plexus-utils)
       ("sisu" ,java-eclipse-sisu-inject)
       ("sisu" ,java-eclipse-sisu-plexus)))
    (native-inputs
     `(("junit" ,java-junit)
       ("hamcrest" ,java-hamcrest-core)
       ("test-util" ,maven-resolver-test-util)
       ("guava" ,java-guava)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("aopalliance" ,java-aopalliance)
       ("guice" ,java-guice)))))

(define-public maven-slf4j-provider
  (package
    (inherit maven)
    (name "maven-slf4j-provider")
    (arguments
     `(#:jar-name "maven-slf4j-provider"
       #:source-dir "maven-slf4j-provider/src/main/java"
       #:jdk ,icedtea-8
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-simple-sources
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "maven-slf4j-provider/target/generated-sources")
             (with-directory-excursion "maven-slf4j-provider/target/generated-sources"
               (system* "tar" "xf" (assoc-ref inputs "simple")))
             (with-directory-excursion "maven-slf4j-provider/"
               (zero? (system* "java" "-cp" (getenv "CLASSPATH")
                               "groovy.ui.GroovyMain" "-Db/c gasedir=/tmp"
                               "src/main/script/patch-slf4j-simple.groovy"))))))))
    (inputs
     `(("slf4j" ,java-slf4j-api)
       ("simple" ,(package-source java-slf4j-simple))
       ("maven-shared-utils" ,maven-shared-utils)))
    (native-inputs
     `(("groovy" ,groovy)
       ("asm" ,java-asm)
       ("antlr" ,antlr2)
       ("cli" ,java-commons-cli)))))

(define-public maven-compat
  (package
    (inherit maven)
    (name "maven-compat")
    (arguments
     `(#:jar-name "maven-compat.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         ;; Tests assume we're in this directory
         (add-before 'configure 'chdir
           (lambda _
             (chdir "maven-compat")))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (zero? (system* "java"
                               "org.codehaus.modello.ModelloCli"
                               file mode "src/main/java" version
                               "false" "true")))
             (let ((file "src/main/mdo/profiles.mdo"))
               (and
                 (modello-single-mode file "1.0.0" "java")
                 (modello-single-mode file "1.0.0" "xpp3-reader")
                 (modello-single-mode file "1.0.0" "xpp3-writer")))
             (let ((file "src/main/mdo/paramdoc.mdo"))
               (and
                 (modello-single-mode file "1.0.0" "java")
                 (modello-single-mode file "1.0.0" "xpp3-reader")
                 (modello-single-mode file "1.0.0" "xpp3-writer")))))
         (add-after 'build 'generate-metadata
           (lambda _
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                             "--source" "src/main/java"
                             "--output" "build/classes/META-INF/plexus/components.xml"
                             ;; I don't know what these two options do, but if
                             ;; not present, it ends with a NullPointerException.
                             "--classes" "build/classes"
                             "--descriptors" "build/classes/META-INF"))))
         (add-before 'check 'build-tests
          (lambda _
            (zero? (system* "ant" "compile-tests"))))
         (add-after 'build-tests 'generate-test-metadata
           (lambda _
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":build/classes"
                                                         ":build/test-classes")
                             "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                             "--source" "src/test/java"
                             "--output" "build/test-classes/META-INF/plexus/components.xml"
                             ;; I don't know what these two options do, but if
                             ;; not present, it ends with a NullPointerException.
                             "--classes" "build/test-classes"
                             "--descriptors" "build/test-classes/META-INF"))))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (zero? (system* "ant" "jar")))))))
    (inputs
     `(("artifact" ,maven-artifact)
       ("repo" ,maven-repository-metadata)
       ("support" ,maven-builder-support)
       ("model" ,maven-model)
       ("model-builder" ,maven-model-builder)
       ("settings" ,maven-settings)
       ("settings-builder" ,maven-settings-builder)
       ("core" ,maven-core)
       ("wagon" ,maven-wagon-provider-api)
       ("wagon-file" ,maven-wagon-file)
       ("resolver-api" ,maven-resolver-api)
       ("resolver-util" ,maven-resolver-util)
       ("resolver-spi" ,maven-resolver-spi)
       ("interpolation" ,java-plexus-interpolation)))
    (native-inputs
     `(("modello" ,java-modello-core)
       ("utils" ,java-plexus-utils)
       ("annot" ,java-plexus-component-annotations)
       ("classworlds" ,java-plexus-classworlds)
       ("xbean" ,java-geronimo-xbean-reflect)
       ("build-api" ,java-sisu-build-api)
       ("container" ,java-eclipse-sisu-plexus)
       ("sisu-inject" ,java-eclipse-sisu-inject)
       ("inject" ,java-javax-inject)
       ("guice" ,java-guice)
       ("guava" ,java-guava)
       ("cglib" ,java-cglib)
       ("asm" ,java-asm)
       ("java" ,java-modello-plugins-java)
       ("xml" ,java-modello-plugins-xml)
       ("xpp3" ,java-modello-plugins-xpp3)
       ;; metadata
       ("metadata" ,java-plexus-component-metadata)
       ("cli" ,java-commons-cli)
       ("plexus-cli" ,java-plexus-cli)
       ("jdom2" ,java-jdom2)
       ("plugin-api" ,maven-plugin-api)
       ("qdox" ,java-qdox)
       ;; tests
       ("cipher" ,java-plexus-cipher)
       ("sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("jsr250", java-jsr250)
       ("cdi-api" ,java-cdi-api)
       ("junit" ,java-junit)
       ("impl" ,maven-resolver-impl)
       ("connector-basic" ,maven-resolver-connector-basic)
       ("transport-wagon" ,maven-resolver-transport-wagon)
       ("lang3" ,java-commons-lang3)
       ("aop" ,java-aopalliance)
       ("resolver-provider" ,maven-resolver-provider)
       ("slf4j" ,java-slf4j-api)
       ("slf4j-simple" ,java-slf4j-simple)
       ,@(package-inputs java-slf4j-api)))))

(define-public maven-plugin-annotations
  (package
    (name "maven-plugin-annotations")
    (version "3.5")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "plugin-tools/maven-plugin-tools-" version
                                  "-source-release.zip"))
              (sha256 (base32 "1ryqhs62j5pas93brhf5dsnvp99hxbvssf681yj5rk3r9h24hqm2"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-annotations.jar"
       #:source-dir "maven-plugin-annotations/src/main/java"
       #:tests? #f))
    (inputs
     `(("artifact" ,maven-artifact)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
