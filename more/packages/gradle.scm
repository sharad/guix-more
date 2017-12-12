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
  #:use-module (more packages java))

;    (version "4.3.1")
;    (source (origin
;              (method url-fetch)
;              (uri (string-append "https://github.com/gradle/gradle/archive/v"
;                                  version ".tar.gz"))
;              (file-name (string-append "gradle-" version ".tar.gz"))
;              (sha256
;               (base32
;                "0lc093zxadzf1hbgsxrk6bpgaxws25pkja1d4xxw179x83ifib7p"))))

(define (gradle-subproject subproject)
  (package
    (name (string-append "gradle-" subproject))
    (version "2.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gradle/gradle/archive/REL_"
                                  version ".tar.gz"))
              (file-name (string-append "gradle-" version ".tar.gz"))
              (sha256
               (base32
                "1v65ac38fzj92jqgrpgma8kpdy974xi8aclgwxkdasy4lcr0k37g"))))
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
       `(("groovy-bootstrap" ,groovy-bootstrap)
         ,@(package-inputs groovy-bootstrap))))))

(define-public gradle-internal-testing
  (package
    (inherit (gradle-groovy-subproject "internal-testing"))
    (inputs
     `(("java-junit" ,java-junit)
       ("java-jsoup" ,java-jsoup)
       ("java-guava" ,java-guava)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-commons-lang" ,java-commons-lang)
       ("java-commons-io" ,java-commons-io)
       ("gradle-base-services-bootstrap" ,gradle-base-services-bootstrap)))))

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
    (inherit (gradle-groovy-subproject "base-services-groovy"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("java-byte-buddy-dep" ,java-byte-buddy-dep)
       ("java-guava" ,java-guava)))
    (arguments
      (substitute-keyword-arguments (package-arguments java-javacc)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'add-bytebuddy
              (lambda _
                (substitute* "subprojects/base-services-groovy/src/main/groovy/org/gradle/groovy/scripts/internal/AstUtils.java"
                  (("package org.gradle.groovy.scripts.internal;")
                   "package org.gradle.groovy.scripts.internal;
import net.bytebuddy.implementation.MethodCall;"))
                #t))))))))

;(define-public gradle-build-option
;  (package
;    (inherit (gradle-subproject "build-option"))
;    (inputs
;     `(("gradle-cli" ,gradle-cli)
;       ("java-commons-lang" ,java-commons-lang)
;       ("java-jsr305" ,java-jsr305)))))

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
               (substitute* "subprojects/messaging/src/main/java/org/gradle/messaging/serialize/kryo/KryoBackedEncoder.java"
                 (("public int getWritePosition")
                  "public long getWritePosition"))
               #t)))
         ,@(package-arguments base)))
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("java-guava" ,java-guava)
         ("java-jsr305" ,java-jsr305)
         ("java-kryo" ,java-kryo)
         ("java-slf4j-api" ,java-slf4j-api))))))

(define-public gradle-core
  (package
    (inherit (gradle-groovy-subproject "core"))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-cli" ,gradle-cli)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava" ,java-guava)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-slf4j-api" ,java-slf4j-api)))))
