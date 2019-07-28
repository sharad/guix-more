;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2017, 2019 Julien Lepiller <julien@lepiller.eu>
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
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages web)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (more packages java))

(define-public maven-polyglot-common
  (package
    (name "maven-polyglot-common")
    (version "0.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/takari/polyglot-maven/"
                                  "archive/polyglot-" version ".tar.gz"))
              (sha256
               (base32
                "0v8j2gj0s4vrddyk0ll38rbl69vm6rv2wnn0c8nlsz2824vhvzca"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-polyglot-common.jar"
       #:source-dir "polyglot-common/src/main/java"
       #:tests? #f; No test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "polyglot-common/src/main/resources-filtered/maven-polyglot.properties"
                           "build/classes")
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "polyglot-common/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     `(("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-builder-support" ,maven-builder-support)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-plugin-api" ,maven-plugin-api)))
    (native-inputs
     `(("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-commons-cli" ,java-commons-cli)
       ("java-asm" ,java-asm)
       ("java-guice" ,java-guice)
       ("java-guava" ,java-guava)
       ("java-cglib" ,java-cglib)
       ("java-jdom2" ,java-jdom2)
       ("java-qdox" ,java-qdox)
       ("java-javax-inject" ,java-javax-inject)))
    (home-page "https://github.com/takari/polyglot-maven")
    (synopsis "")
    (description "")
    (license license:epl1.0)))

(define-public maven-polyglot-java
  (package
    (inherit maven-polyglot-common)
    (name "maven-polyglot-java")
    (arguments
     `(#:jar-name "maven-polyglot-java.jar"
       #:source-dir "polyglot-java/src/main/java"
       #:tests? #f; No test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "polyglot-java/src/main/resources/META-INF/maven/extension.xml"
                           "build/classes/META-INF/maven")
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "polyglot-java/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     `(("maven-polyglot-common" ,maven-polyglot-common)
       ("java-commons-beanutils" ,java-commons-beanutils)
       ("java-commons-io" ,java-commons-io)
       ("java-guava" ,java-guava)
       ,@(package-inputs maven-polyglot-common)))))

(define-public maven-polyglot-groovy
  (package
    (inherit maven-polyglot-common)
    (name "maven-polyglot-groovy")
    (arguments
     `(#:jar-name "maven-polyglot-groovy.jar"
       #:source-dir "polyglot-groovy/src/main/java:polyglot-groovy/src/main/groovy"
       #:tests? #f; No test
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'use-groovy
           (lambda _
             (substitute* "build.xml"
               ;; Change the compiler to groovyc
               (("javac") "groovyc")
               ;; Make it fork
               (("includeantruntime=\"false\"")
                "includeantruntime=\"false\" fork=\"yes\"")
               ;; groovyc doesn't understand the --classpath argument (bug?)
               (("classpath=\"@refidclasspath\"")
                "classpathref=\"classpath\"")
               ;; To enable joint compilation
               (("classpathref=\"classpath\" />")
                "classpathref=\"classpath\"><javac source=\"1.5\" target=\"1.5\" /></groovyc>")
               ;; Actually create a definition of the groovyc task.
               ;; FIXME: Can't we use groovy-ant for that?
               (("<project basedir=\".\">")
                "<project basedir=\".\"><taskdef name=\"groovyc\"
classname=\"org.codehaus.groovy.ant.Groovyc\" />"))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             (install-file "polyglot-groovy/src/main/resources/META-INF/maven/extension.xml"
                           "build/classes/META-INF/maven")
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "polyglot-groovy/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     `(("maven-polyglot-common" ,maven-polyglot-common)
       ("groovy" ,groovy)
       ("java-slf4j-api" ,java-slf4j-api)
       ,@(package-inputs maven-polyglot-common)))))

(define-public maven-slf4j-provider
  (package
    (inherit maven-artifact)
    (name "maven-slf4j-provider")
    (arguments
     `(#:jar-name "maven-slf4j-provider.jar"
       #:source-dir "maven-slf4j-provider/src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("maven-shared-utils" ,maven-shared-utils)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-slf4j-simple" ,java-slf4j-simple)))
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-filtering
  (package
    (name "maven-filtering")
    (version "3.1.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://archive.apache.org/dist/maven/"
                                  "shared/maven-filtering-" version
                                  "-source-release.zip"))
              (sha256
               (base32
                "09wrdhchnszd2l6h4z30ra0bv1a19qyjgac9z8zf1pn0m4nw05yz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-filtering.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; tests fail for now...
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'remove-failing-test
           (lambda _
             (delete-file "src/test/java/org/apache/maven/shared/filtering/IncrementalResourceFilteringTest.java")
             (substitute* "src/test/java/org/apache/maven/shared/filtering/StubMavenSession.java"
               (("org.sonatype.aether.RepositorySystemSession")
                "org.eclipse.aether.RepositorySystemSession"))
             #t)))))
    (inputs
     `(("java-jsr305" ,java-jsr305)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-interpolation" ,java-plexus-interpolation)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("maven-settings" ,maven-settings)
       ("maven-shared-utils" ,maven-shared-utils)))
    (native-inputs
     `(("java-assertj" ,java-assertj)
       ("java-guava" ,java-guava)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-mockito-1" ,java-mockito-1)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("maven-resolver-api" ,maven-resolver-api)
       ("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-plugin-testing
  (package
    (name "maven-plugin-testing")
    (version "3.3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-plugin-testing/"
                                  "archive/maven-plugin-testing-" version ".tar.gz"))
              (sha256
               (base32
                "010jp44vq75fk6260zvqz41ai3l1sqrjz56yyzv9pgshx27vpshy"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-testing.jar"
       #:source-dir "maven-plugin-testing-harness/src/main/java"
       #:test-dir "maven-plugin-testing-harness/src/test"))
    (inputs
     `(("java-commons-io" ,java-commons-io)
       ("java-guice" ,java-guice)
       ("java-junit" ,java-junit)
       ("java-plexus-archiver" ,java-plexus-archiver)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-artifact" ,maven-artifact)
       ("maven-compat" ,maven-compat)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("maven-model-builder" ,maven-model-builder)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-resolver-api" ,maven-resolver-api)
       ("maven-resolver-provider" ,maven-resolver-provider)
       ("maven-wagon-provider-api" ,maven-wagon-provider-api)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-doxia-logging-api
  (package
    (name "maven-doxia-logging-api")
    (version "1.8")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-doxia/"
                                  "archive/doxia-" version ".tar.gz"))
              (sha256
               (base32
                "07qckb1fsycnz5b08xbm8b1hjz8295ri6xi47s47rlwxg53hvf4s"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-doxia-logging-api.jar"
       #:source-dir "doxia-logging-api/src/main/java"
       #:tests? #f)); No tests
    (inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)))
    (home-page "https://maven.apache.org/doxia")
    (synopsis "Content generation framework")
    (description "Doxia is a content generation framework for generating static
and dynamic content, that supports a variety of markup languages.")
    (license license:asl2.0)))

(define-public maven-doxia-sink-api
  (package
    (inherit maven-doxia-logging-api)
    (name "maven-doxia-sink-api")
    (arguments
     `(#:jar-name "maven-doxia-sink-api.jar"
       #:source-dir "doxia-sink-api/src/main/java"
       #:tests? #f)); No tests
    (inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("maven-doxia-logging-api" ,maven-doxia-logging-api)))))

(define-public maven-doxia-core
  (package
    (inherit maven-doxia-logging-api)
    (name "maven-doxia-core")
    (arguments
     `(#:jar-name "maven-doxia-core.jar"
       #:source-dir "doxia-core/src/main/java"
       #:test-dir "doxia-core/src/test"
       #:test-exclude
       ;; test fails for unknown reason
       (list "**/SnippetMacroTest.java"
             ;; requires network
             "**/XmlValidatorTest.java"
             "**/Abstract*Test.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "doxia-core/src/main/resources"
                               "build/classes")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "doxia-core/src/main/java" version
                       "false" "true"))
             (let ((file "doxia-core/src/main/mdo/document.mdo"))
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "xsd"))
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "doxia-core/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     `(("java-commons-lang3" ,java-commons-lang3)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("maven-doxia-logging-api" ,maven-doxia-logging-api)
       ("maven-doxia-sink-api" ,maven-doxia-sink-api)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; for modello:
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-utils" ,java-plexus-utils)
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
       ("java-modello-plugins-xsd" ,java-modello-plugins-xsd)
       ;; for tests
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-junit" ,java-junit)
       ("java-xmlunit" ,java-xmlunit)
       ("java-xmlunit-matchers" ,java-xmlunit-matchers)
       ;; for generating metadata
       ("java-commons-cli" ,java-commons-cli)
       ("java-jdom2" ,java-jdom2)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-qdox" ,java-qdox)
       ("maven-plugin-api" ,maven-plugin-api)))))

(define-public maven-doxia-module-xhtml
  (package
    (inherit maven-doxia-logging-api)
    (name "maven-doxia-module-xhtml")
    (arguments
     `(#:jar-name "maven-doxia-module-xhtml.jar"
       #:source-dir "doxia-modules/doxia-module-xhtml/src/main/java"
       #:test-dir "doxia-modules/doxia-module-xhtml/src/test"
       #:test-exclude
       ;; test fails for unknown reason
       (list "**/SnippetMacroTest.java"
             ;; requires network
             "**/XmlValidatorTest.java"
             "**/Abstract*Test.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-test-classes
           (lambda _
             (copy-recursively "doxia-core/src/test/java" "doxia-modules/doxia-module-xhtml/src/test/java")
             (copy-file "doxia-core/src/test/java/org/apache/maven/doxia/module/AbstractIdentityTest.java"
                        "doxia-modules/doxia-module-xhtml/src/test/java/org/apache/maven/doxia/module/AbstractIdentityTest.java")
             (copy-file "doxia-core/src/test/java/org/apache/maven/doxia/AbstractModuleTest.java"
                        "doxia-modules/doxia-module-xhtml/src/test/java/org/apache/maven/doxia/AbstractModuleTest.java")
             (mkdir-p "doxia-modules/doxia-module-xhtml/src/test/java/org/apache/maven/doxia/sink/impl")
             (copy-file "doxia-core/src/test/java/org/apache/maven/doxia/sink/impl/SinkTestDocument.java"
                        "doxia-modules/doxia-module-xhtml/src/test/java/org/apache/maven/doxia/sink/impl/SinkTestDocument.java")
             #t))
         (add-after 'build 'generate-metadata
           (lambda _
             (invoke "java" "-cp" (string-append (getenv "CLASSPATH") ":build/classes")
                     "org.codehaus.plexus.metadata.PlexusMetadataGeneratorCli"
                     "--source" "doxia-modules/doxia-module-xhtml/src/main/java"
                     "--output" "build/classes/META-INF/plexus/components.xml"
                     "--classes" "build/classes"
                     "--descriptors" "build/classes/META-INF")
             #t))
         (add-after 'generate-metadata 'rebuild
           (lambda _
             (invoke "ant" "jar")
             #t)))))
    (inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-doxia-core" ,maven-doxia-core)
       ("maven-doxia-logging-api" ,maven-doxia-logging-api)
       ("maven-doxia-sink-api" ,maven-doxia-sink-api)))
    (native-inputs
     `(("java-commons-lang3" ,java-commons-lang3)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-guava" ,java-guava)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-junit" ,java-junit)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-xmlunit" ,java-xmlunit)
       ("java-xmlunit-matchers" ,java-xmlunit-matchers)
       ;; for generating metadata
       ("java-asm" ,java-asm)
       ("java-commons-cli" ,java-commons-cli)
       ("java-jdom2" ,java-jdom2)
       ("java-plexus-cli" ,java-plexus-cli)
       ("java-plexus-component-metadata" ,java-plexus-component-metadata)
       ("java-qdox" ,java-qdox)
       ("maven-plugin-api" ,maven-plugin-api)))))

(define-public maven-doxia-skin-model
  (package
    (name "maven-doxia-skin-model")
    (version "1.8.1")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-doxia-sitetools/"
                                  "archive/doxia-sitetools-" version ".tar.gz"))
              (sha256
               (base32
                "02dgblm6n07jwr7r90wzc8ax9vz3ax7rh5w12wmvfyd5aybh691w"))
              (patches
                (search-patches "maven-doxia-sitetools-fix-plexus-utils.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-doxia-skin-model.jar"
       #:source-dir "doxia-skin-model/src/main/java"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "doxia-skin-model/src/main/java" version
                       "false" "true"))
             (let ((file "doxia-skin-model/src/main/mdo/skin.mdo"))
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xsd"))
             #t)))))
    (inputs
     `(("maven-doxia-sink-api" ,maven-doxia-sink-api)))
    (native-inputs
     `(("unzip" ,unzip)
       ("java-modello-core" ,java-modello-core)
       ;; for modello:
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-guice" ,java-guice)
       ("java-cglib" ,java-cglib)
       ("java-asm" ,java-asm)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-guava" ,java-guava)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)
       ("java-modello-plugins-xml" ,java-modello-plugins-xml)
       ("java-modello-plugins-xpp3" ,java-modello-plugins-xpp3)
       ("java-modello-plugins-xsd" ,java-modello-plugins-xsd)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-doxia-decoration-model
  (package
    (inherit maven-doxia-skin-model)
    (name "maven-doxia-decoration-model")
    (arguments
     `(#:jar-name "maven-doxia-decoration-model.jar"
       #:source-dir "doxia-decoration-model/src/main/java"
       #:test-dir "doxia-decoration-model/src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "doxia-decoration-model/src/main/java" version
                       "false" "true"))
             (let ((file "doxia-decoration-model/src/main/mdo/decoration.mdo"))
               (modello-single-mode file "4.0.0" "xpp3-writer")
               (modello-single-mode file "4.0.0" "java")
               (modello-single-mode file "4.0.0" "xpp3-reader")
               (modello-single-mode file "4.0.0" "xsd"))
             #t)))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ,@(package-native-inputs maven-doxia-skin-model)))
    (description "")))

(define-public maven-doxia-site-renderer
  (package
    (inherit maven-doxia-skin-model)
    (name "maven-doxia-site-renderer")
    (arguments
     `(#:jar-name "maven-doxia-site-renderer.jar"
       #:source-dir "doxia-site-renderer/src/main/java"
       #:test-dir "doxia-site-renderer/src/test"
       #:tests? #f; require gargoylesoftware-htmlunit
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "doxia-site-renderer/src/main/resources"
                               "build/classes")
             #t)))))
    (inputs
     `(("java-commons-lang3" ,java-commons-lang3)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-i18n" ,java-plexus-i18n)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-velocity-component" ,java-plexus-velocity-component)
       ("java-velocity" ,java-velocity)
       ("java-velocity-tools" ,java-velocity-tools)
       ("maven-artifact" ,maven-artifact)
       ("maven-doxia-core" ,maven-doxia-core)
       ("maven-doxia-decoration-model" ,maven-doxia-decoration-model)
       ("maven-doxia-logging-api" ,maven-doxia-logging-api)
       ("maven-doxia-module-xhtml" ,maven-doxia-module-xhtml)
       ("maven-doxia-skin-model" ,maven-doxia-skin-model)
       ("maven-doxia-sink-api" ,maven-doxia-sink-api)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (description "")))

(define-public maven-reporting-api
  (package
    (name "maven-reporting-api")
    (version "3.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-reporting-api/"
                                  "archive/maven-reporting-api-" version ".tar.gz"))
              (sha256
               (base32
                "1dc94n7czax7vrniv4xyqlza2mjqdlzzvq6wh6cmcqnnmgggya91"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-reporting-api.jar"
       #:source-dir "src/main/java"
       #:tests? #f)); no tests
    (inputs
     `(("maven-doxia-sink-api" ,maven-doxia-sink-api)))
    (native-inputs
     `(("unzip" ,unzip)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-reporting-impl
  (package
    (name "maven-reporting-impl")
    (version "3.0")
    (source (origin
              (method git-fetch)
              (uri (git-reference
                     (url "https://github.com/apache/maven-reporting-impl.git")
                     (commit "ff10bba47b9ecf728a0f16e096fb410b74711a5d")))
              (file-name (git-file-name name version))
              (sha256
               (base32
                "02da8pqa17lmkwjhqyq39ygf4lfa483bbdvhbvjginmpw3ydkbw5"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-reporting-impl.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       ;; Require junitx.util.PrivateAccessor (?)
       #:tests? #f))
    (inputs
     `(("java-plexus-utils" ,java-plexus-utils)
       ("maven-core" ,maven-core)
       ("maven-doxia-core" ,maven-doxia-core)
       ("maven-doxia-decoration-model" ,maven-doxia-decoration-model)
       ("maven-doxia-logging-api" ,maven-doxia-logging-api)
       ("maven-doxia-module-xhtml" ,maven-doxia-module-xhtml)
       ("maven-doxia-sink-api" ,maven-doxia-sink-api)
       ("maven-doxia-site-renderer" ,maven-doxia-site-renderer)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-reporting-api" ,maven-reporting-api)
       ("maven-shared-utils" ,maven-shared-utils)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-plugin-tools-api
  (package
    (name "maven-plugin-tools-api")
    (version "3.6.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/maven-plugin-tools/"
                                  "archive/maven-plugin-tools-" version ".tar.gz"))
              (sha256
               (base32
                "1miq87ywh48v0hwl3a4q95rxbgg58zpk59v6z34bl715hcawbv80"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-plugin-tools-api.jar"
       #:source-dir "maven-plugin-tools-api/src/main/java"
       #:test-dir "maven-plugin-tools-api/src/test"
       #:tests? #f)); disabled for now. Require maven-plugin-testing that doesn't build yet
    (inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-artifact" ,maven-artifact)
       ("maven-model" ,maven-model)
       ("maven-core" ,maven-core)
       ("maven-plugin-api" ,maven-plugin-api)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

(define-public maven-plugin-annotations
  (package
    (inherit maven-plugin-tools-api)
    (name "maven-plugin-annotations")
    (arguments
     `(#:jar-name "maven-plugin-annotations.jar"
       #:tests? #f; no tests
       #:source-dir "maven-plugin-annotations/src/main/java"))
    (inputs
     `(("maven-artifact" ,maven-artifact)))
    (description "")))

(define-public maven-plugin-tools-generators
  (package
    (inherit maven-plugin-tools-api)
    (name "maven-plugin-tools-generators")
    (arguments
     `(#:jar-name "maven-plugin-tools-generators.jar"
       #:source-dir "maven-plugin-tools-generators/src/main/java"
       #:test-dir "maven-plugin-tools-generators/src/test"
       #:tests? #f; require maven-plugin-testing
       #:phases
       (modify-phases %standard-phases
        (add-before 'build 'fix-exception
          (lambda _
            (substitute* "maven-plugin-tools-generators/src/main/java/org/apache/maven/tools/plugin/generator/PluginHelpGenerator.java"
              (("Properties properties = PropertyUtils.*")
               "Properties properties;
try {
properties = PropertyUtils.loadProperties( tmpPropertiesFile );
} catch(IOException e) {
e.printStackTrace();
return;
}
"))
            #t)))))
    (inputs
     `(("java-asm" ,java-asm)
       ("java-jtidy" ,java-jtidy)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-velocity-component" ,java-plexus-velocity-component)
       ("java-velocity" ,java-velocity)
       ("maven-compat" ,maven-compat)
       ("maven-core" ,maven-core)
       ("maven-model" ,maven-model)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-tools-api" ,maven-plugin-tools-api)
       ("maven-reporting-api" ,maven-reporting-api)))
    (description "")))

(define-public maven-plugin-plugin
  (package
    (inherit maven-plugin-tools-api)
    (name "maven-plugin-plugin")
    (arguments
     `(#:jar-name "maven-plugin-plugin.jar"
       #:source-dir "maven-plugin-plugin/src/main/java"
       #:test-dir "maven-plugin-plugin/src/test"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "maven-plugin-plugin/src/main/resources"
                               "build/classes")
             (copy-recursively "maven-plugin-plugin/src/main/filtered-resources"
                               "build/classes")
             #t))
         (add-before 'build 'generate-models
           (lambda* (#:key inputs #:allow-other-keys)
             (define (modello-single-mode file version mode)
               (invoke "java" "org.codehaus.modello.ModelloCli"
                       file mode "maven-plugin-plugin/src/main/java" version
                       "false" "true"))
             (modello-single-mode "maven-plugin-plugin/src/main/mdo/pluginRequirements.mdo"
                                  "1.0.0" "java"))))))
    (inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("java-plexus-velocity-component" ,java-plexus-velocity-component)
       ("java-velocity" ,java-velocity)
       ("maven-artifact" ,maven-artifact)
       ("maven-compat" ,maven-compat)
       ("maven-core" ,maven-core)
       ("maven-doxia-sink-api" ,maven-doxia-sink-api)
       ("maven-doxia-site-renderer" ,maven-doxia-site-renderer)
       ("maven-doxia-core" ,maven-doxia-core)
       ("maven-doxia-logging-api" ,maven-doxia-logging-api)
       ("maven-model" ,maven-model)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-plugin-api" ,maven-plugin-api)
       ("maven-plugin-tools-api" ,maven-plugin-tools-api)
       ("maven-plugin-tools-generators" ,maven-plugin-tools-generators)
       ("maven-reporting-api" ,maven-reporting-api)
       ("maven-reporting-impl" ,maven-reporting-impl)
       ("maven-repository-metadata" ,maven-repository-metadata)))
    (native-inputs
     `(("java-modello-core" ,java-modello-core)
       ;; for modello:
       ("java-asm" ,java-asm)
       ("java-cglib" ,java-cglib)
       ("java-eclipse-sisu-inject" ,java-eclipse-sisu-inject)
       ("java-eclipse-sisu-plexus" ,java-eclipse-sisu-plexus)
       ("java-geronimo-xbean-reflect" ,java-geronimo-xbean-reflect)
       ("java-guava" ,java-guava)
       ("java-guice" ,java-guice)
       ("java-javax-inject" ,java-javax-inject)
       ("java-plexus-classworlds" ,java-plexus-classworlds)
       ("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-sisu-build-api" ,java-sisu-build-api)
       ;; modello plugins:
       ("java-modello-plugins-java" ,java-modello-plugins-java)))
    (description "")))

(define-public maven-resources-plugin
  (package
    (name "maven-resources-plugin")
    (version "3.1.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/"
                                  "maven-resources-plugin/archive/"
                                  "maven-resources-plugin-" version ".tar.gz"))
              (sha256
               (base32
                "1f5gnjg2xmqfxml6k0ydyd1sxxwzgnb24qn6avcc4mijwd8a84pl"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "maven-resources-plugin.jar"
       #:source-dir "src/main/java"
       #:test-dir "src/test"
       #:tests? #f; test depends on maven-plugin-test-harness
       #:imported-modules
       ((more build maven pom)
        (more build maven java)
        (more build maven plugin)
        ,@%ant-build-system-modules)
       #:modules
       ((more build maven pom)
        (more build maven java)
        (more build maven plugin)
        (sxml simple)
        (guix build ant-build-system)
        (guix build java-utils)
        (guix build utils))
       ;; Need maven-plugin-tools and a corresponding phase
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-pom
           (lambda _
             (mkdir-p "build/classes/META-INF/maven")
             (copy-file "pom.xml" "build/classes/META-INF/pom.xml")
             #t))
         (add-before 'build 'generate-plugin.xml
           (lambda _
             (let* ((pom-content (get-pom "pom.xml"))
                    (name (pom-name pom-content))
                    (description (pom-description pom-content))
                    (dependencies (pom-dependencies pom-content))
                    (mojos
                     (with-directory-excursion "src/main/java/org/apache/maven/plugins/resources/"
                       `(mojos
                          ,(generate-mojo-from-files maven-convert-type
                                                     "ResourcesMojo.java"
                                                     "CopyResourcesMojo.java")
                          ,(generate-mojo-from-files maven-convert-type
                                                     "ResourcesMojo.java")
                          ,(generate-mojo-from-files maven-convert-type
                                                     "ResourcesMojo.java"
                                                     "TestResourcesMojo.java")))))
               (mkdir-p "build/classes/META-INF/maven")
               (with-output-to-file "build/classes/META-INF/maven/plugin.xml"
                 (lambda _
                   (sxml->xml
                     `(plugin
                        (name ,name)
                        (description ,description)
                        (groupId "org.apache.maven.plugins")
                        (artifactId "maven-resources-plugin")
                        (version ,,version)
                        (goalPrefix "resources")
                        (isolatedRealm "false")
                        (inheritedByDefault "true")
                        ,mojos
                        (dependencies
                         ,@dependencies))))))))
         (add-after 'install 'install-pom
           (lambda* (#:key outputs #:allow-other-keys)
             (install-file "pom.xml" (string-append (assoc-ref outputs "out")
                                     "/share/pom.xml")))))))
    (inputs
     `(("java-plexus-component-annotations" ,java-plexus-component-annotations)
       ("java-plexus-container-default" ,java-plexus-container-default)
       ("java-plexus-utils" ,java-plexus-utils)
       ("maven-core" ,maven-core)
       ("maven-filtering" ,maven-filtering)
       ("maven-model" ,maven-model)
       ("maven-plugin-annotations" ,maven-plugin-annotations)
       ("maven-plugin-api" ,maven-plugin-api)))
    (native-inputs
     `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
