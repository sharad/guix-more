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

(define-module (more packages maven)
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
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages java))

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
     `(("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexu-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core-boot" ,maven-core-boot)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)))))

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
     `(("maven-artifact" ,maven-artifact)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-shared-utils
  (package
    (name "maven-shared-utils")
    (version "3.2.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/shared/"
                                  "maven-shared-utils-" version "-source-release.zip"))
              (sha256
               (base32
                "1kzmj68wwdcznb36hm6kfz57wbavw7g1rp236pz10znkjljn6rf6"))))
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
     `(("java-jansi" ,java-jansi)
       ("java-commons-io" ,java-commons-io)
       ("java-jsr305" ,java-jsr305)
       ("java-plexus-container-default" ,java-plexus-container-default)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-lang3" ,java-commons-lang3)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven
  (package
    (name "maven")
    (version "3.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "maven-3/" version "/source/"
                                  "apache-maven-" version "-src.tar.gz"))
              (sha256 (base32 "06by23fz207lkvsndq883irfcf4p77jzkgf7n2q7hzyw1hs4h5s7"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (for-each delete-file (find-files "." "\\.jar$"))
                  #t))
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
     `(("java-plexus-classworlds" ,java-plexus-classworlds)
       ("maven-artifact" ,maven-artifact)
       ("maven-embedder" ,maven-embedder)
       ("maven-core" ,maven-core)
       ("maven-compat" ,maven-compat)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-repository-metadata", maven-repository-metadata)
       ("maven-shared-utils" ,maven-shared-utils)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-connector-basic" ,maven-resolver-connector-basic)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("maven-resolver-transport-wagon" ,maven-resolver-transport-wagon)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("maven-wagon-file" ,maven-wagon-file)
       ("maven-wagon-http" ,maven-wagon-http)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-httpcomponents-client" ,java-httpcomponents-client)
       ("java-httpcomponents-core" ,java-httpcomponents-core)
       ("maven-wagon-http-shared" ,maven-wagon-http-shared)
       ("maven-wagon-tck-http" ,maven-wagon-tck-http)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-guice" ,java-guice)
       ("java-aopalliance" ,java-aopalliance)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-interpolation" ,java-plexus-interpolation)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-guava" ,java-guava)
       ("java-jansi" ,java-jansi)
       ("java-jsr250" ,java-jsr250)
       ("java-cdi-api" ,java-cdi-api)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-slf4j-api" ,java-slf4j-api)
       ;; TODO: replace with maven-slf4j-provider
       ("java-slf4j-simple" ,java-slf4j-simple)))
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
     `(("java-junit" ,java-junit)))))

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
     `(("java-commons-lang3" ,java-commons-lang3)
       ("java-plexus-utils" ,java-plexus-utils)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; for modello:
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("java-junit" ,java-junit)))))

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
     `(("java-modello-core" ,java-modello-core)
       ;; for modello:
       ;("container" ,java-plexus-container-default)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)))))

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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)))))

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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-interpolation" ,java-plexus-interpolation)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-settings" ,maven-settings)
       ("java-commons-lang3" ,java-commons-lang3)))
    (native-inputs
     `(("java-junit" ,java-junit)))))

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
     `(("maven-artifact" ,maven-artifact)
       ("maven-model" ,maven-model)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
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
       ("java-junit" ,java-junit)))))

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
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
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
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; for tests
       ("java-junit" ,java-junit)))))

;; In case of "null returned by binding at org.eclipse.sisu.wire.LocatorWiring"
;; this package is the probably the culprit.  Check it contains a
;; META-INF/plexus/components.xml that makes sense.
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
         (add-before 'build 'generate-sisu-named
           (lambda _
             (mkdir-p "build/classes/META-INF/sisu")
             (chmod "./sisu.sh" #o755)
             (zero? (system* "./sisu.sh" "maven-resolver-provider/src/main/java"
                             "build/classes/META-INF/sisu/javax.inject.Named")))))))
    (inputs
     `(("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-repository-metadata" ,maven-repository-metadata)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)))))

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
;       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
;       ("java-junit" ,java-junit)
;       ("java-mockito-1" ,java-mockito-1)
;       ;; For running tests:
;       ("java-hamcrest-core" ,java-hamcrest-core)
;       ("aop" ,java-aopalliance)
;       ("classworlds" ,java-plexus-classworlds)
;       ("plugin" ,maven-plugin-api)
;       ("java-cglib" ,java-cglib)
;       ("java-asm" ,java-asm)
;       ("interpolation" ,java-plexus-interpolation)
;       ("artifact" ,maven-artifact)
;       ("java-objenesis" ,java-objenesis)
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
     `(("maven-artifact" ,maven-artifact)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-repository-metadata" ,maven-repository-metadata)
       ("maven-shared-utils" ,maven-shared-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworld" ,java-plexus-classworlds)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-commons-jxpath" ,java-commons-jxpath)))))

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
            (add-after 'generate-metadata 'rebuild
              (lambda _
                (zero? (system* "ant" "jar"))))))))
    (native-inputs
     `(("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-commons-cli" ,java-commons-cli)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-jdom2" ,java-jdom2)
       ("java-qdox" ,java-qdox)
       ("maven-core-boot" ,maven-core-boot)
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
     `(("java-plexus-utils" ,java-plexus-utils)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-junit" ,java-junit)
       ("java-easymock" ,java-easymock)))
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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-tomcat" ,java-tomcat)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))))

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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-httpcomponents-client" ,java-httpcomponents-client)
       ("java-httpcomponents-core" ,java-httpcomponents-core)
       ("java-commons-io" ,java-commons-io)
       ("java-jsoup" ,java-jsoup)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
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
     `(("java-plexus-util" ,java-plexus-utils)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-tomcat" ,java-tomcat)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-commons-codec" ,java-commons-codec)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-webapp-9.2" ,java-eclipse-jetty-webapp-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)))))

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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-httpcomponents-client" ,java-httpcomponents-client)
       ("java-httpcomponents-core" ,java-httpcomponents-core)
       ("maven-wagon-http-shared" ,maven-wagon-http-shared)
       ("maven-wagon-tck-http" ,maven-wagon-tck-http)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-commons-io" ,java-commons-io)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-tomcat" ,java-tomcat)
       ("java-eclispe-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclispe-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ("java-eclispe-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclispe-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclispe-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-eclispe-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-jsoup" ,java-jsoup)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-slf4j-simple" ,java-slf4j-simple)
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
     `(("java-plexus-utils" ,java-plexus-utils)
       ("java-httpcomponents-client" ,java-httpcomponents-client)
       ("java-httpcomponents-core" ,java-httpcomponents-core)
       ("maven-wagon-http-shared" ,maven-wagon-http-shared)
       ("maven-wagon-tck-http" ,maven-wagon-tck-http)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (native-inputs
     `(("maven-wagon-provider-test" ,maven-wagon-provider-test)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-inject" ,java-javax-inject)
       ("java-cglib" ,java-cglib)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-cli" ,java-plexus-cli)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("java-commons-cli" ,java-commons-cli)
       ("java-qdox" ,java-qdox)
       ("java-jdom2" ,java-jdom2)
       ("java-asm" ,java-asm)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-tomcat" ,java-tomcat)
       ("java-eclipse-jetty-util-9.2" ,java-eclipse-jetty-util-9.2)
       ("java-eclipse-jetty-io-9.2" ,java-eclipse-jetty-io-9.2)
       ("java-eclipse-jetty-http-9.2" ,java-eclipse-jetty-http-9.2)
       ("java-eclipse-jetty-server-9.2" ,java-eclipse-jetty-server-9.2)
       ("java-eclipse-jetty-servlet-9.2" ,java-eclipse-jetty-servlet-9.2)
       ("java-eclipse-jetty-security-9.2" ,java-eclipse-jetty-security-9.2)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-io" ,java-commons-io)
       ("java-jsoup" ,java-jsoup)
       ("java-slf4j-simple" ,java-slf4j-simple)
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
     `(("maven-core" ,maven-core)
       ("maven-artifact" ,maven-artifact)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-shared-utils" ,maven-shared-utils)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-util" ,java-plexus-utils)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("maven-resolevr-util" ,maven-resolver-util)
       ("maven-resolevr-api" ,maven-resolver-api)
       ("java-logback-core" ,java-logback-core)
       ("java-logback-classic" ,java-logback-classic)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; tests
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-objenesis" ,java-objenesis)
       ("java-hamcrest-core" ,java-hamcrest-core)))))

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
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("jajva-aopalliance" ,java-aopalliance)
       ("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)))))

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
     `(("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("maven-resolver-util" ,maven-resolver-util)
       ("java-javax-inject" ,java-javax-inject)
       ("mavne-wagon-provider-api" ,maven-wagon-provider-api)
       ("java-plexus-component-annotation" ,java-plexus-component-annotations)
       ("java-plexus-classworld" ,java-plexus-classworlds)
       ("java-plexus-plexus-util" ,java-plexus-utils)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("maven-resolver-test-util" ,maven-resolver-test-util)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-aopalliance" ,java-aopalliance)
       ("java-guice" ,java-guice)))))

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
               (system* "tar" "xf" (assoc-ref inputs "java-slf4j-simple")))
             (with-directory-excursion "maven-slf4j-provider/"
               (zero? (system* "groovy"
                               "src/main/script/patch-slf4j-simple.groovy"))))))))
    (inputs
     `(("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,(package-source java-slf4j-simple))
       ("maven-shared-utils" ,maven-shared-utils)))
    (native-inputs
     `(("groovy" ,groovy)))))

(define-public maven-compat
  (package
    (inherit maven)
    (name "maven-compat")
    (arguments
     `(#:jar-name "maven-compat.jar"
       #:source-dir "src/main/java"
       #:jdk ,icedtea-8
       #:test-dir "src/test"
       #:tests? #f; TODOTODOTODOTODO
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
     `(("maven-artifact" ,maven-artifact)
       ("maven-repository-metadata" ,maven-repository-metadata)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-settings" ,maven-settings)
       ("maven-settings-builder" ,maven-settings-builder)
       ("maven-core" ,maven-core)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)
       ("maven-wagon-file" ,maven-wagon-file)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-util" ,maven-resolver-util)
       ("maven-resolver-spi" ,maven-resolver-spi)
       ("java-plexus-interpolation" ,java-plexus-interpolation)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-exclispe-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ;; metadata
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-commons-cli" ,java-commons-cli)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-jdom2" ,java-jdom2)
       ("maven-plugin-api" ,maven-plugin-api)
       ("java-qdox" ,java-qdox)
       ;; tests
       ("java-plexus-cipher" ,java-plexus-cipher)
       ("java-plexus-sec-dispatcher" ,java-plexus-sec-dispatcher)
       ("java-jsr250", java-jsr250)
       ("java-cdi-api" ,java-cdi-api)
       ("java-junit" ,java-junit)
       ("maven-resolver-impl" ,maven-resolver-impl)
       ("maven-resolver-connector-basic" ,maven-resolver-connector-basic)
       ("maven-resolver-transport-wagon" ,maven-resolver-transport-wagon)
       ("java-commons-lang3" ,java-commons-lang3)
       ("java-aop" ,java-aopalliance)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)
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
     `(("maven-artifact" ,maven-artifact)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
