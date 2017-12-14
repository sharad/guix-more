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

(define-module (more packages gradle)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages java)
  #:use-module (more packages groovy)
  #:use-module (more packages java))

(define (gradle-subproject subproject)
  (package
    (name (string-append "gradle-" subproject))
;    (version "2.3")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/gradle/gradle/archive/REL_"
;                                  version ".tar.gz"))
;              (file-name (string-append "gradle-" version ".tar.gz"))
;              (sha256
;               (base32
;                "1v65ac38fzj92jqgrpgma8kpdy974xi8aclgwxkdasy4lcr0k37g"))))
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gradle/gradle/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append "gradle-" version ".tar.gz"))
              (sha256
               (base32
                "026232dy578nl8gzj1nfc9r25p8alcwdbwb9b8s3bw4krxmy6saz"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append "gradle-" ,subproject ".jar")
       #:source-dir (string-append "subprojects/" ,subproject "/src/main/java")
       #:jdk ,icedtea-8
       #:tests? #f;; Ignore tests for now
       #:test-dir (string-append "subprojects/" ,subproject "/src/test")))
    (inputs '())
    (native-inputs '())
    (home-page "")
    (synopsis "Build system")
    (description "Build system")
    (license license:asl2.0)))

(define (gradle-groovy-subproject subproject)
  (let ((base (gradle-subproject subproject)))
    (package
      (inherit base)
      (arguments
       `(#:jar-name (string-append "gradle-" ,subproject ".jar")
         #:source-dir (string-append "subprojects/" ,subproject "/src/main/groovy")
         #:test-dir (string-append "subprojects/" ,subproject "/src/test")
         #:jdk ,icedtea-8
         #:tests? #f;; Ignore tests for now
         #:phases
         (modify-phases %standard-phases
           (add-before 'build 'use-groovy
             (lambda _
               (substitute* "build.xml"
                 (("javac") "groovyc")
                 (("includeantruntime=\"false\"")
                  "includeantruntime=\"false\" fork=\"yes\"")
                 (("classpath=\"@refidclasspath\"")
                  "classpathref=\"classpath\"")
                 ;; To enable joint compilation
                 (("classpathref=\"classpath\" />")
                  "classpathref=\"classpath\"><javac source=\"1.5\" target=\"1.5\" /></groovyc>")
                 (("<project basedir=\".\">")
                  "<project basedir=\".\"><taskdef name=\"groovyc\"
classname=\"org.codehaus.groovy.ant.Groovyc\" />")))))))
      (native-inputs
       `(("groovy" ,groovy)
         ,@(package-inputs groovy))))))

(define-public gradle-internal-testing
  (package
    (inherit (gradle-groovy-subproject "internal-testing"))
    (inputs
     `(("gradle-native" ,gradle-native)
       ("gradle-base-services" ,gradle-base-services)
       ("groovy-xml" ,groovy-xml)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-jmock" ,java-jmock)
       ("java-jmock-junit4" ,java-jmock-junit4)
       ("java-jmock-legacy" ,java-jmock-legacy)
       ("java-jsoup" ,java-jsoup)
       ("java-junit" ,java-junit)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-spockframework-core" ,java-spockframework-core)))))

;; Cycle with internal-testing
(define-public gradle-base-services
  (let ((base (gradle-subproject "base-services")))
    (package
      (inherit base)
      (inputs
       `(("java-guava" ,java-guava)
         ("java-slf4j-api" ,java-slf4j-api)
         ("java-commons-lang" ,java-commons-lang)
         ("java-commons-io" ,java-commons-io)
         ("java-jsr305" ,java-jsr305)
         ("java-jcip-annotations" ,java-jcip-annotations)))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core))))))

(define-public gradle-base-services-groovy
  (package
    (inherit (gradle-subproject "base-services-groovy"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("java-asm" ,java-asm)
       ("java-guava" ,java-guava)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-build-option
  (package
    (inherit (gradle-subproject "build-option"))
    (inputs
     `(("gradle-cli" ,gradle-cli)
       ("java-commons-lang" ,java-commons-lang)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-cli
  (package
    (inherit (gradle-subproject "cli"))))

(define-public gradle-native
  (package
    (inherit (gradle-subproject "native"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("java-commons-io" ,java-commons-io)
       ("java-guava" ,java-guava)
       ("java-jansi" ,java-jansi)
       ("java-jsr305" ,java-jsr305)
       ("java-native-platform" ,java-native-platform)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-messaging
  (let ((base  (gradle-subproject "messaging")))
    (package
      (inherit base)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-before 'build 'fix-kryo
             (lambda _
               ;; gradle depends on kryo-2.20 from 2013, we packaged version 4.
               (substitute* "subprojects/messaging/src/main/java/org/gradle/internal/serialize/kryo/KryoBackedEncoder.java"
                 (("public int getWritePosition")
                  "public long getWritePosition"))
               #t)))
         ,@(package-arguments base)))
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-base-services-groovy" ,gradle-base-services-groovy)
         ("java-commons-lang" ,java-commons-lang)
         ("java-guava" ,java-guava)
         ("java-jsr305" ,java-jsr305)
         ("java-kryo" ,java-kryo)
         ("java-slf4j-api" ,java-slf4j-api))))))

(define-public gradle-persistent-cache
  (package
    (inherit (gradle-subproject "persistent-cache"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-logging
  (package
    (inherit (gradle-subproject "logging"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-build-option" ,gradle-build-option)
       ("gradle-cli" ,gradle-cli)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-jansi" ,java-jansi)
       ("java-jansi-native" ,java-jansi-native)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-jul-to-slf4j" ,java-jul-to-slf4j)))))

(define-public gradle-model-core
  (package
    (inherit (gradle-subproject "model-core"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("java-asm" ,java-asm)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-resources
  (package
    (inherit (gradle-subproject "resources"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-native" ,gradle-native)
       ("java-commons-io" ,java-commons-io)
       ("java-guava" ,java-guava)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-build-cache
  (package
    (inherit (gradle-subproject "build-cache"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-logging" ,gradle-logging)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-resources" ,gradle-resources)
       ("java-commons-io" ,java-commons-io)
       ("java-guava" ,java-guava)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-process-services
  (package
    (inherit (gradle-subproject "process-services"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-guava" ,java-guava)
       ("java-jsr305" ,java-jsr305)
       ("java-native-platform" ,java-native-platform)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-core-api
  (package
    (inherit (gradle-subproject "core-api"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-build-cache" ,gradle-build-cache)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-resources" ,gradle-resources)
       ("groovy-ant" ,groovy-ant)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-model-groovy
  (package
    (inherit (gradle-subproject "model-groovy"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-model-core" ,gradle-model-core)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("java-asm" ,java-asm)
       ("java-guava" ,java-guava)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-jvm-services
  (package
    (inherit (gradle-subproject "jvm-services"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-process-services" ,gradle-process-services)))))

(define java-guava-for-gradle
  (package
    (inherit java-guava)
    (version "17.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/google/guava/"
                                  "archive/v" version ".tar.gz"))
              (sha256
               (base32
                "0kg2n0dfdncbm3kgf8fa6kig8djfhar24vf0yf287x27w5sqzhnb"))))
    (arguments
     `(#:jar-name "guava.jar"
       #:source-dir "guava/src/"
       #:tests? #f)))); Not in a "java" subdirectory

(define-public gradle-core
  (package
    (inherit (gradle-subproject "core"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-build-cache" ,gradle-build-cache)
       ("gradle-build-option" ,gradle-build-option)
       ("gradle-cli" ,gradle-cli)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-jvm-services" ,gradle-jvm-services)
       ("gradle-logging" ,gradle-logging)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-model-groovy" ,gradle-model-groovy)
       ("gradle-native" ,gradle-native)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-resources" ,gradle-resources)
       ("groovy-ant" ,groovy-ant)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-json" ,groovy-json)
       ("groovy-templates" ,groovy-templates)
       ("groovy-xml" ,groovy-xml)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-native-platform" ,java-native-platform)
       ("java-slf4j-api" ,java-slf4j-api)
       ("groovy" ,groovy)))))
