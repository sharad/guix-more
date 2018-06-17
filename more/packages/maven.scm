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
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
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
       #:jdk ,icedtea-8
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
       #:jdk ,icedtea-8
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
       #:jdk ,icedtea-8
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
