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
  #:use-module (guix build-system trivial)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages groovy)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maven)
  #:use-module (gnu packages web)
  #:use-module (gnu packages version-control)
  #:use-module (gnu packages xml)
  #:use-module (more packages java))

;; Gradle requires guava@17.
;; TODO: patch gradle to support at least guava@20 and send it upstream.
(define-public java-guava-for-gradle
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

(define (gradle-subproject subproject projects runtime)
  "Gradle is made of a lot of subprojects. Each subproject can be compiled in
a certain order and in the same way.

This procedure builds the java source of @code{subproject}.

Each subproject contains at least a text file, gradle-*-classpath.properties
that contain dependency information. This file is created using the
@code{projects} and @code{runtime} parameters."
  (package
    (name (string-append "gradle-" subproject))
    (version "4.8.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gradle/gradle/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append "gradle-" version ".tar.gz"))
              (sha256
               (base32
                "1jbw9044g0czn8pm46i6j4y0gx6l3b3iwamh9d7ja14i2wrx5shf"))
              (patches
                (search-patches
                  "gradle-match-files-without-version-number.patch"))))
    (build-system ant-build-system)
    (arguments
     ;; The jar-name must be this exactly: gradle will not find its jar files
     ;; if they are named differently.
     `(#:jar-name (string-append "gradle-" ,subproject "-4.8.jar")
       #:source-dir (string-append "subprojects/" ,subproject "/src/main/java")
       #:jdk ,icedtea-8
       #:tests? #f;; Ignore tests for now
       #:test-dir (string-append "subprojects/" ,subproject "/src/test")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-implementation-info
           (lambda _
             ;; Add implementation information to the MANIFEST.MF file.  We can
             ;; substitute this in the manifest phase.
             (substitute* "build.xml"
               (("message=\"")
                (string-append "message=\"Implementation-Title: Gradle"
                               "${line.separator}"
                               "Implementation-Version: 4.8"
                               "${line.separator}")))
             #t))
         (add-before 'build 'add-properties
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
             ;; This file contains dependency information.
             ;; The projects list is a list of gradle-subprojects that this
             ;; subproject depends directly on at runtime. This information
             ;; can be found in the *.gradle file in the subproject's directory.
             ;;
             ;; The runtime list is a list of external projects this subproject
             ;; depends on. This list must be a list of jar files, so we transform
             ;; the project name into a list of jar files the package contains.
             ;; This information can also be found in the *.gradle file of the
             ;; subproject.
             (with-output-to-file (string-append "build/classes/gradle-"
                                                 ,subproject
                                                 "-classpath.properties")
               (lambda _
                 (format #t "projects=~a\nruntime=~a\n"
                         (string-join ',projects ",")
                         (string-join
                           (map basename
                             (apply append
                               (map (lambda (lib) (find-files (assoc-ref inputs lib)
                                                              ".*.jar"))
                                 ',runtime)))
                           ","))))
             #t))
         (add-before 'build 'copy-resources
           (lambda _
             ;; If the subproject has a resources directory, copy it to the jar
             ;; file.
             (let ((resources (string-append "subprojects/" ,subproject
                                             "/src/main/resources")))
               (if (file-exists? resources)
                 (copy-recursively resources "build/classes")))
             #t)))))
    (inputs '())
    (native-inputs
      `(("java-junit" ,java-junit)))
    (home-page "")
    (synopsis "Build system")
    (description "Build system")
    (license license:asl2.0)))

(define (gradle-groovy-subproject subproject projects runtime)
  "This procedure is similar to @code{gradle-groovy-subproject}, but it
builds a module containing groovy source code."
  (let ((base (gradle-subproject subproject projects runtime)))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:source-dir source-dir)
          `(string-append "subprojects/" ,subproject "/src/main/groovy"))
         ((#:phases phases)
          `(modify-phases ,phases
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
classname=\"org.codehaus.groovy.ant.Groovyc\" />")
                   ;; Tests are in test.home/groovy, not /java
                   (("\\}/java") "}/groovy"))))))))
      (native-inputs
       `(("groovy" ,groovy)
         ,@(package-inputs groovy))))))

;; This gradle plugin is not a subproject, but it is required by buildsrc.
(define-public gradle-japicmp-plugin
  (package
    (name "gradle-japicmp-plugin")
    (version "0.2.6")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/melix/japicmp-gradle-plugin/"
                                  "archive/RELEASE_"
                                  (string-map (lambda (x) (if (eq? x #\.) #\_ x))
                                              version)
                                  ".tar.gz"))
              (sha256
               (base32
                "09frla9a1f33s31di5wcirm50ddhgww8wanlygc8068xnb5ch357"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "gradle-japicmp-plugin.jar"
       #:source-dir "src/main/java"
       #:tests? #f; FIXME: can't compile
       #:jdk ,icedtea-8))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-logging" ,gradle-logging)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-japicmp" ,java-japicmp)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jboss-javassist" ,java-jboss-javassist)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

;; This package is not meant to be available at runtime: it a build dependency
;; only. It contains task definitions used to build submodules. Unfortunately,
;; it depends on many subprojects at runtime, and cannot be used without a
;; complete gradle.
(define-public gradle-buildsrc
  (package
    (inherit (gradle-subproject "buildsrc" '() '()))
    (name "gradle-buildsrc")
    (arguments
     `(#:jar-name "gradle-buildsrc.jar"
       #:source-dir "buildSrc/src/main/groovy:buildSrc/src/main/java"
       #:test-dir "buildSrc/src/test"
       #:jdk ,icedtea-8
       #:tests? #f;; Ignore tests for now
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "buildSrc/src/main/resources" "build/classes")
             #t))
         (add-before 'build 'remove-deps
           (lambda _
             ;; Requires asciidoctor, which is implemented with jruby :/
             (delete-file "buildSrc/src/main/groovy/org/gradle/build/docs/CacheableAsciidoctorTask.groovy")
             ;; Requires http-builder, but it requires old software
             (delete-file "buildSrc/src/main/groovy/org/gradle/testing/DistributedPerformanceTest.groovy")
             #t))
         (add-before 'build 'remove-jarjar
           (lambda _
             (substitute* '("buildSrc/src/main/groovy/org/gradle/build/docs/dsl/source/SourceMetaDataVisitor.java"
                            "buildSrc/src/main/groovy/org/gradle/build/docs/dsl/source/ExtractDslMetaDataTask.groovy")
               (("groovyjarjarantlr") "antlr"))
             #t))
         (add-before 'build 'fixes
           (lambda _
             (substitute* "buildSrc/src/main/groovy/org/gradle/binarycompatibility/rules/SinceAnnotationMissingRule.java"
               ;; Fix for java8 (cannot convert to String)
               (("Declaration.getName\\(\\)") "Declaration.getName().toString()")
               ;; Fix for guava
               (("getComment\\(\\).getContent\\(\\)") "getComment().get().toString()")
               ;; javaparser is more recent than expected
               (("getVariables\\(\\).get\\(0\\).getId\\(\\).getName\\(\\)")
                "getVariables().get(0).getName().toString()"))
             (substitute* "buildSrc/src/main/groovy/org/gradle/binarycompatibility/rules/AbstractGradleViolationRule.groovy"
               (("it.name.name") "it.name.asString()"))
             #t))
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
                "classpathref=\"classpath\"><javac source=\"7\" target=\"7\" /></groovyc>")
               (("<project basedir=\".\">")
                "<project basedir=\".\"><taskdef name=\"groovyc\"
classname=\"org.codehaus.groovy.ant.Groovyc\" />"))
             #t)))))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-japicmp-plugin" ,gradle-japicmp-plugin)
       ("gradle-logging" ,gradle-logging)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-native" ,gradle-native)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-platform-jvm" ,gradle-platform-jvm)
       ("gradle-platform-native" ,gradle-platform-native)
       ("gradle-plugins" ,gradle-plugins)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-reporting" ,gradle-reporting)
       ("gradle-testing-base" ,gradle-testing-base)
       ("gradle-testing-jvm" ,gradle-testing-jvm)
       ("groovy" ,groovy)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-japicmp" ,java-japicmp)
       ("java-javaparser" ,java-javaparser)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jboss-javassist" ,java-jboss-javassist)
       ("java-jsoup" ,java-jsoup)
       ("java-parboiled" ,java-parboiled)
       ("java-pegdown" ,java-pegdown)
       ("java-slf4j-api" ,java-slf4j-api)))
    (native-inputs
     `(("groovy" ,groovy)
       ,@(package-inputs groovy)))))

(define-public gradle-internal-testing
  (package
    (inherit (gradle-groovy-subproject "internal-testing" '() '()))
    (inputs
     `(("gradle-native" ,gradle-native)
       ("gradle-base-services" ,gradle-base-services)
       ("groovy" ,groovy)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
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
  (let ((base (gradle-subproject
                "base-services" '()
                '("java-guava-for-gradle" "java-slf4j-api" "java-commons-lang"
                  "java-commons-io" "java-jsr305" "java-jcip-annotations"))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
         ((#:phases phases)
          `(modify-phases ,phases
             (add-before 'build 'create-build-receipt
               (lambda _
                 (mkdir-p "build/classes/org/gradle")
                 (with-output-to-file "build/classes/org/gradle/build-receipt.properties"
                   (lambda _
                     (format #t "baseVersion=4.8
commitId=cf7821a6f79f8e2a598df21780e3ff7ce8db2b82
buildTimestamp=19710101000000+0000
buildTimestampIso=1971-01-01 00\\:00\\:00 UTC
versionNumber=4.8
isSnapshot=false")))
                 #t))))))
      (inputs
       `(("java-guava-for-gradle" ,java-guava-for-gradle)
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
    (inherit (gradle-subproject "base-services-groovy" '("gradle-base-services")
                                '("groovy")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-build-option
  (package
    (inherit (gradle-subproject "build-option" '("gradle-cli") '()))
    (inputs
     `(("gradle-cli" ,gradle-cli)
       ("java-commons-lang" ,java-commons-lang)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-cli
  (gradle-subproject "cli" '() '()))

(define-public gradle-native
  (package
    (inherit (gradle-subproject "native" '("gradle-base-services")
                                '("java-native-platform" "java-commons-io"
                                  "java-slf4j-api" "java-jansi"
                                  "java-guava-for-gradle")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("java-commons-io" ,java-commons-io)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jansi" ,java-jansi)
       ("java-jsr305" ,java-jsr305)
       ("java-native-platform" ,java-native-platform)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-messaging
  (let ((base  (gradle-subproject "messaging" '("gradle-base-services")
                                  '("java-slf4j-api" "java-kryo" "java-objenesis"
                                    "java-minlog" "java-reflectasm" "java-asm-6"))))
    (package
      (inherit base)
      (arguments
        (substitute-keyword-arguments (package-arguments base)
          ((#:phases phases)
           `(modify-phases ,phases
              (add-before 'build 'fix-kryo
                (lambda _
                  ;; gradle depends on kryo-2.20 from 2013, we packaged version 4.
                  (substitute* "subprojects/messaging/src/main/java/org/gradle/internal/serialize/kryo/KryoBackedEncoder.java"
                    (("public int getWritePosition")
                     "public long getWritePosition"))
                  #t))))))
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-base-services-groovy" ,gradle-base-services-groovy)
         ("java-asm-6" ,java-asm-6)
         ("java-commons-lang" ,java-commons-lang)
         ("java-fastutil" ,java-fastutil)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-jsr305" ,java-jsr305)
         ("java-kryo" ,java-kryo)
         ("java-minlog" ,java-minlog)
         ("java-objenesis" ,java-objenesis)
         ("java-reflectasm" ,java-reflectasm)
         ("java-slf4j-api" ,java-slf4j-api))))))

(define-public gradle-persistent-cache
  (package
    (inherit (gradle-subproject
               "persistent-cache"
               '("gradle-base-services" "gradle-messaging" "gradle-native")
               '("java-jcip-annotations" "java-commons-collections"
                 "java-commons-io" "java-commons-lang")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-logging
  (package
    (inherit (gradle-subproject
               "logging"
               '("gradle-native" "gradle-base-services" "gradle-messaging"
                 "gradle-cli" "gradle-build-option")
               ;; Maybe log4j-over-slf4j and jcl-over-slf4j
               '("java-slf4j-api" "java-jul-to-slf4j" "ant" "java-commons-lang"
                 "java-guava-for-gradle" "java-jansi" "java-jansi-native"
                 "java-jcip-annotations")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-build-option" ,gradle-build-option)
       ("gradle-cli" ,gradle-cli)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jansi" ,java-jansi)
       ("java-jansi-native" ,java-jansi-native)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-jul-to-slf4j" ,java-jul-to-slf4j)))))

(define-public gradle-model-core
  (package
    (inherit (gradle-subproject
               "model-core"
               '("gradle-base-services-groovy" "gradle-base-services")
               '("groovy" "java-slf4j-api" "java-guava-for-gradle"
                 "java-jcip-annotations" "java-commons-lang" "java-asm-6")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-resources
  (package
    (inherit (gradle-subproject
               "resources"
               '("gradle-base-services" "gradle-messaging" "gradle-native"
                 "gradle-model-core")
               '("java-guava-for-gradle" "java-commons-io")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-native" ,gradle-native)
       ("java-commons-io" ,java-commons-io)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-build-cache
  (package
    (inherit (gradle-subproject
               "build-cache"
               '("gradle-messaging" "gradle-native" "gradle-persistent-cache"
                 "gradle-resources" "gradle-logging" "gradle-base-services")
               '("java-javax-inject" "java-commons-io")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-logging" ,gradle-logging)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-resources" ,gradle-resources)
       ("java-commons-io" ,java-commons-io)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-process-services
  (package
    (inherit (gradle-subproject
               "process-services"
               '("gradle-messaging" "gradle-native" "gradle-base-services")
               '("java-guava-for-gradle" "java-slf4j-api")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jsr305" ,java-jsr305)
       ("java-native-platform" ,java-native-platform)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-core-api
  (package
    (inherit (gradle-subproject
               "core-api"
               '("gradle-base-services" "gradle-base-services-groovy"
                 "gradle-build-cache" "gradle-logging" "gradle-model-core"
                 "gradle-persistent-cache" "gradle-process-services"
                 "gradle-resources")
               '("ant" "java-commons-io" "java-commons-lang"
                 "java-jcip-annotations")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-build-cache" ,gradle-build-cache)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-resources" ,gradle-resources)
       ("groovy" ,groovy)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-model-groovy
  (package
    (inherit (gradle-subproject
               "model-groovy"
               '("gradle-base-services-groovy" "gradle-base-services"
                 "gradle-model-core")
               '("groovy" "java-jcip-annotations" "java-guava-for-gradle")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-model-core" ,gradle-model-core)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-jvm-services
  (package
    (inherit (gradle-subproject
               "jvm-services"
               '("gradle-base-services" "gradle-process-services")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-process-services" ,gradle-process-services)))))

(define-public gradle-plugin-use
  (let ((base (gradle-subproject
                "plugin-use"
                '("gradle-resources-http" "gradle-core" "gradle-dependency-management")
                '())))
    (package
      (inherit base)
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-base-services-groovy" ,gradle-base-services-groovy)
         ("gradle-core" ,gradle-core)
         ("gradle-core-api" ,gradle-core-api)
         ("gradle-dependency-management" ,gradle-dependency-management)
         ("gradle-logging" ,gradle-logging)
         ("gradle-messaging" ,gradle-messaging)
         ("groovy" ,groovy)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-jsr305" ,java-jsr305))))))

(define-public gradle-core
  (let ((base (gradle-subproject
                "core"
                '("gradle-docs" "gradle-model-groovy" "gradle-base-services"
                  "gradle-base-services-groovy" "gradle-messaging"
                  "gradle-logging" "gradle-resources" "gradle-cli"
                  "gradle-build-option" "gradle-native" "gradle-persistent-cache"
                  "gradle-build-cache" "gradle-core-api" "gradle-process-services"
                  "gradle-jvm-services" "gradle-model-core")
                '("groovy" "ant" "java-guava-for-gradle"
                  "java-javax-inject" "java-asm-6" "java-slf4j-api"
                  "java-commons-collections" "java-commons-io"
                  "java-commons-lang" "java-jcip-annotations" "java-jaxp"
                  "java-native-platform" "java-commons-compress"))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'create-properties
              (lambda _
                (mkdir-p "build/classes")
                (with-directory-excursion "build/classes"
                  (with-output-to-file "gradle-implementation-plugins.properties"
                    (lambda _
                      (format #t "plugins=gradle-tooling-api-builders")))
                  (with-output-to-file "gradle-plugins.properties"
                    (lambda _
                      (format #t (string-append
                                   "plugins=gradle-announce,gradle-antlr,"
                                   "gradle-build-cache-http,gradle-build-comparison,"
                                   "gradle-build-init,gradle-code-quality,"
                                   "gradle-composite-builds,gradle-diagnostics,"
                                   "gradle-ear,gradle-ide,gradle-ide-native,"
                                   "gradle-ide-play,gradle-ivy,gradle-jacoco,"
                                   "gradle-javascript,gradle-language-groovy,"
                                   "gradle-language-java,gradle-language-jvm,"
                                   "gradle-language-native,gradle-language-scala,"
                                   "gradle-maven,gradle-osgi,gradle-platform-base,"
                                   "gradle-platform-jvm,gradle-platform-native,"
                                   "gradle-platform-play,gradle-plugin-development,"
                                   "gradle-plugin-use,gradle-plugins,gradle-publish,"
                                   "gradle-reporting,gradle-resources-gcs,"
                                   "gradle-resources-http,gradle-resources-s3,"
                                   "gradle-resources-sftp,gradle-scala,"
                                   "gradle-signing,gradle-testing-base,"
                                   "gradle-testing-jvm,gradle-testing-native,"
                                   "gradle-wrapper"))))
                #t)))))))
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
         ("groovy" ,groovy)
         ("java-asm-6" ,java-asm-6)
         ("java-commons-collections" ,java-commons-collections)
         ("java-commons-compress" ,java-commons-compress)
         ("java-commons-io" ,java-commons-io)
         ("java-commons-lang" ,java-commons-lang)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-javax-inject" ,java-javax-inject)
         ("java-jaxp" ,java-jaxp)
         ("java-jcip-annotations" ,java-jcip-annotations)
         ("java-jsr305" ,java-jsr305)
         ("java-native-platform" ,java-native-platform)
         ("java-slf4j-api" ,java-slf4j-api))))))

(define-public gradle-wrapper
  (package
    (inherit (gradle-subproject "wrapper" '("gradle-cli") '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-cli" ,gradle-cli)))))

(define-public gradle-tooling-api
  (package
    (inherit (gradle-subproject
               "tooling-api"
               '("gradle-core" "gradle-messaging" "gradle-wrapper"
                 "gradle-base-services")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-logging" ,gradle-logging)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-wrapper" ,gradle-wrapper)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-launcher
  (let ((base (gradle-subproject
                "launcher"
                '("gradle-base-services" "gradle-core-api" "gradle-core" "gradle-tooling-api")
                '("java-asm-6" "java-commons-io" "java-slf4j-api"))))
    (package
      (inherit base)
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-build-option" ,gradle-build-option)
         ("gradle-cli" ,gradle-cli)
         ("gradle-core" ,gradle-core)
         ("gradle-core-api" ,gradle-core-api)
         ("gradle-jvm-services" ,gradle-jvm-services)
         ("gradle-logging" ,gradle-logging)
         ("gradle-messaging" ,gradle-messaging)
         ("gradle-native" ,gradle-native)
         ("gradle-persistent-cache" ,gradle-persistent-cache)
         ("gradle-process-services" ,gradle-process-services)
         ("gradle-tooling-api" ,gradle-tooling-api)
         ("groovy" ,groovy)
         ("java-asm-6" ,java-asm-6)
         ("java-commons-io" ,java-commons-io)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-jcip-annotations" ,java-jcip-annotations)
         ("java-jsr305" ,java-jsr305)
         ("java-slf4j-api" ,java-slf4j-api)))
      (arguments
       `(#:main-class "org.gradle.launcher.GradleMain"
         ,@(substitute-keyword-arguments (package-arguments base)
            ((#:phases phases)
             `(modify-phases ,phases
                (add-before 'build 'add-classpath
                  (lambda _
                   (substitute* "build.xml"
                     (("message=\"")
                      (string-append "message=\"Class-Path: "
                                     "gradle-base-services-4.8.jar "
                                     "gradle-core-api-4.8.jar "
                                     "gradle-core-4.8.jar"
                                     "${line.separator}")))
                   #t))
                ;; This phase fails, because the jar files are not actually
                ;; present in the output directory.  This is because gradle
                ;; uses class loading: we need to put all its dependencies
                ;; in the same package, which is done later.  Then the
                ;; classpath becomes correct.
                (delete 'generate-jar-indices)))))))))

(define-public gradle-installation-beacon
  (package
    (inherit (gradle-subproject "installation-beacon" '() '()))))

(define-public gradle-workers
  (package
    (inherit (gradle-subproject "workers" '("gradle-core") '("java-jcip-annotations")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-logging" ,gradle-logging)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-native" ,gradle-native)
       ("gradle-process-services" ,gradle-process-services)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-reporting
  (package
    (inherit (gradle-subproject
               "reporting"
               '("gradle-core")
               '("groovy" "java-jatl" "java-commons-lang")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("groovy" ,groovy)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jatl" ,java-jatl)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-resources-http
  (package
    (inherit (gradle-subproject
               "resources-http"
               '("gradle-resources" "gradle-base-services" "gradle-core")
               ;; TODO: jcl-over-slf4j
               '("java-httpcomponents-httpclient" "java-httpcomponents-httpcore"
                 "java-commons-codec" "java-jcifs" "java-slf4j-api"
                 "java-guava-for-gradle" "java-commons-lang" "java-commons-io"
                 "java-nekohtml" "java-xerces" "java-jaxp")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-resources" ,gradle-resources)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-jaxp" ,java-jaxp)
       ("java-jcifs" ,java-jcifs)
       ("java-jsr305" ,java-jsr305)
       ("java-nekohtml" ,java-nekohtml)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-xerces" ,java-xerces)))))

(define-public gradle-version-control
  (let ((base (gradle-subproject
                "version-control"
                '("gradle-core" "gradle-core-api")
                ;TODO: jcl-over-slf4j-1.7.16.jar
                '("java-httpcomponents-httpclient" "java-httpcomponents-httpcore"
                  "java-commons-codec" "java-jgit" "java-jsch"
                  "java-slf4j-api"))))
    (package
      (inherit base)
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-core" ,gradle-core)
         ("gradle-core-api" ,gradle-core-api)
         ("gradle-logging" ,gradle-logging)
         ("gradle-persistent-cache" ,gradle-persistent-cache)
         ("java-commons-codec" ,java-commons-codec)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
         ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
         ("java-jgit" ,java-jgit)
         ("java-jsch" ,java-jsch)
         ("java-jsr305" ,java-jsr305)
         ("java-slf4j-api" ,java-slf4j-api))))))

(define-public gradle-dependency-management
  (let ((base (gradle-subproject
                 "dependency-management"
                 '("gradle-installation-beacon" "gradle-core" "gradle-resources"
                   "gradle-version-control" "gradle-resources-http"
                   "gradle-runtime-api-info")
                 ;; TODO: maven-core aether-connector-wagon aether-util aether-spi
                 ;; aether-impl aether-api wagon-http-shared wagon-provider-api
                 ;; wagon-http wagon-file maven-aether-provider maven-plugin-api
                 ;; maven-repository-metadata maven-artifact maven-moel
                 ;; maven-model-builder maven-compat plexus-sec-dispacher
                 ;; plexus-cipher plexus-classworlds plexus-container-default
                 ;; plexus-component-annotations plexus-interpolation plexus-utils
                 ;; maven-settings maven-settings-builder xbean-reflect
                 '("java-asm-6" "java-commons-lang" "java-commons-io"
                   "java-apache-ivy" "java-slf4j-api" "java-gson"
                   "java-jcip-annotations" "java-bouncycastle"
                   "java-jsch"))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'fix
              (lambda _
                (substitute* "subprojects/dependency-management/src/main/java/org/gradle/api/internal/artifacts/ivyservice/ivyresolve/parser/IvyXmlModuleDescriptorParser.java"
                  (("new NamespaceId\\(uri, localName\\)")
                   "new NamespaceId(uri, localName).toString()"))
                (substitute* "subprojects/dependency-management/src/main/java/org/gradle/api/internal/artifacts/ivyservice/resolveengine/store/DefaultBinaryStore.java"
                  (("offset = encoder") "offset = (int)encoder"))
                #t))))))
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-base-services-groovy" ,gradle-base-services-groovy)
         ("gradle-build-cache" ,gradle-build-cache)
         ("gradle-core" ,gradle-core)
         ("gradle-core-api" ,gradle-core-api)
         ("gradle-logging" ,gradle-logging)
         ("gradle-messaging" ,gradle-messaging)
         ("gradle-model-core" ,gradle-model-core)
         ("gradle-native" ,gradle-native)
         ("gradle-persistent-cache" ,gradle-persistent-cache)
         ("gradle-resources" ,gradle-resources)
         ("gradle-resources-http" ,gradle-resources-http)
         ("gradle-version-control" ,gradle-version-control)
         ("groovy" ,groovy)
         ("java-apache-ivy" ,java-apache-ivy)
         ("java-asm-6" ,java-asm-6)
         ("java-bouncycastle" ,java-bouncycastle)
         ("java-commons-io" ,java-commons-io)
         ("java-commons-lang" ,java-commons-lang)
         ("java-gson" ,java-gson)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-javax-inject" ,java-javax-inject)
         ("java-jcip-annotations" ,java-jcip-annotations)
         ("java-jsch" ,java-jsch)
         ("java-jsr305" ,java-jsr305)
         ("java-slf4j-api" ,java-slf4j-api)
         ("maven-settings" ,maven-settings)
         ("maven-settings-builder" ,maven-settings-builder))))))

(define-public gradle-platform-base
  (package
    (inherit (gradle-subproject
               "platform-base"
               '("gradle-core" "gradle-dependency-management" "gradle-workers")
               '("java-commons-collections" "groovy")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-testing-base
  (let ((base (gradle-subproject
                "testing-base"
                '("gradle-core" "gradle-reporting" "gradle-platform-base")
                '("java-kryo" "java-objenesis" "java-minlog" "java-reflectasm"
                  "java-asm-6"))))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
        ((#:phases phases)
         `(modify-phases ,phases
            (add-before 'build 'fix
              (lambda _
                (substitute* "subprojects/testing-base/src/main/java/org/gradle/api/internal/tasks/testing/junit/result/TestOutputStore.java"
                  (("int total = output")
                   "int total = (int)output"))
                #t))))))
      (inputs
       `(("gradle-base-services" ,gradle-base-services)
         ("gradle-base-services-groovy" ,gradle-base-services-groovy)
         ("gradle-core" ,gradle-core)
         ("gradle-core-api" ,gradle-core-api)
         ("gradle-logging" ,gradle-logging)
         ("gradle-messaging" ,gradle-messaging)
         ("gradle-model-core" ,gradle-model-core)
         ("gradle-platform-base" ,gradle-platform-base)
         ("gradle-process-services" ,gradle-process-services)
         ("gradle-reporting" ,gradle-reporting)
         ("groovy" ,groovy)
         ("java-asm-6" ,java-asm-6)
         ("java-bouncycastle" ,java-bouncycastle)
         ("java-commons-io" ,java-commons-io)
         ("java-commons-lang" ,java-commons-lang)
         ("java-guava-for-gradle" ,java-guava-for-gradle)
         ("java-javax-inject" ,java-javax-inject)
         ("java-jsr305" ,java-jsr305)
         ("java-kryo" ,java-kryo)
         ("java-minlog" ,java-minlog)
         ("java-objenesis" ,java-objenesis)
         ("java-reflectasm" ,java-reflectasm)
         ("java-slf4j-api" ,java-slf4j-api))))))

(define-public gradle-diagnostics
  (package
    (inherit (gradle-subproject
               "diagnostics"
               '("gradle-dependency-management" "gradle-base-services-groovy"
                 "gradle-core" "gradle-reporting" "gradle-platform-base")
               '("groovy" "java-guava-for-gradle" "java-jatl"
                 "java-commons-lang" "java-commons-collections")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-reporting" ,gradle-reporting)
       ("groovy" ,groovy)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jatl" ,java-jatl)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-platform-jvm
  (package
    (inherit (gradle-subproject
               "platform-jvm"
               '("gradle-platform-base" "gradle-core" "gradle-diagnostics")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-diagnostics" ,gradle-diagnostics)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-native" ,gradle-native)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-process-services" ,gradle-process-services)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-testing-jvm
  (package
    (inherit (gradle-subproject
               "testing-jvm"
               '("gradle-core" "gradle-platform-jvm" "gradle-language-java"
                 "gradle-testing-base")
               '("java-junit" "java-bsh" "java-testng" "java-snakeyaml"
                 "java-jcommander" "java-hamcrest-core")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-diagnostics" ,gradle-diagnostics)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-jvm-services" ,gradle-jvm-services)
       ("gradle-logging" ,gradle-logging)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-platform-jvm" ,gradle-platform-jvm)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-reporting" ,gradle-reporting)
       ("gradle-testing-base" ,gradle-testing-base)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-bsh" ,java-bsh)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jcommander" ,java-jcommander)
       ("java-junit" ,java-junit)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-testng" ,java-testng)))))

(define-public gradle-platform-native
  (package
    (inherit (gradle-subproject
               "platform-native"
               '("gradle-core" "gradle-platform-base" "gradle-diagnostics")
               '("groovy" "java-commons-io")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-diagnostics" ,gradle-diagnostics)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-native" ,gradle-native)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-gson" ,java-gson)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-native-platform" ,java-native-platform)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-composite-builds
  (package
    (inherit (gradle-subproject
               "composite-builds"
               '("gradle-core" "gradle-dependency-management" "gradle-launcher")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-launcher" ,gradle-launcher)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("groovy" ,groovy)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-language-jvm
  (package
    (inherit (gradle-subproject
               "language-jvm"
               '("gradle-core" "gradle-platform-jvm" "gradle-platform-base")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-platform-jvm" ,gradle-platform-jvm)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)))))

(define-public gradle-language-java
  (package
    (inherit (gradle-subproject
               "language-java"
               '("gradle-core" "gradle-platform-jvm" "gradle-language-jvm")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-jvm-services" ,gradle-jvm-services)
       ("gradle-language-jvm" ,gradle-language-jvm)
       ("gradle-logging" ,gradle-logging)
       ("gradle-messaging" ,gradle-messaging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-platform-jvm" ,gradle-platform-jvm)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-lang" ,java-commons-lang)
       ("java-fastutil" ,java-fastutil)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-language-groovy
  (package
    (inherit (gradle-subproject
               "language-groovy"
               '("gradle-core" "gradle-platform-jvm" "gradle-language-java")
               '()))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-language-java" ,gradle-language-java)
       ("gradle-language-jvm" ,gradle-language-jvm)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-platform-jvm" ,gradle-platform-jvm)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-plugins
  (package
    (inherit (gradle-subproject
               "plugins"
               '("gradle-composite-builds" "gradle-core" "gradle-workers"
                 "gradle-dependency-management" "gradle-reporting"
                 "gradle-platform-jvm" "gradle-language-jvm"
                 "gradle-language-java" "gradle-language-groovy"
                 "gradle-diagnostics" "gradle-testing-jvm")
               '("groovy" "ant" "java-asm-6" "java-commons-io"
                 "java-commons-lang" "java-commons-cli" "java-slf4j-api")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-build-cache" ,gradle-build-cache)
       ("gradle-composite-builds" ,gradle-composite-builds)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-diagnostics" ,gradle-diagnostics)
       ("gradle-language-groovy" ,gradle-language-groovy)
       ("gradle-language-java" ,gradle-language-java)
       ("gradle-language-jvm" ,gradle-language-jvm)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-persistent-cache" ,gradle-persistent-cache)
       ("gradle-platform-base" ,gradle-platform-base)
       ("gradle-platform-jvm" ,gradle-platform-jvm)
       ("gradle-reporting" ,gradle-reporting)
       ("gradle-testing-base" ,gradle-testing-base)
       ("gradle-testing-jvm" ,gradle-testing-jvm)
       ("gradle-process-services" ,gradle-process-services)
       ("gradle-workers" ,gradle-workers)
       ("groovy" ,groovy)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

(define-public gradle-runtime-api-info
  (let ((base (gradle-subproject "runtime-api-info" '() '())))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
        ((#:phases phases)
         `(modify-phases ,phases
           (replace 'build
             (lambda _
               (mkdir-p "build/classes/org/gradle/api/internal/runtimeshaded")
               (with-directory-excursion "build/classes/org/gradle/api/internal/runtimeshaded"
                 (with-output-to-file "api-relocated.txt"
                   (lambda _
                     (format #t "aQute/bnd/annotation
aQute/bnd/build
aQute/bnd/compatibility
aQute/bnd/component
aQute/bnd/connection/settings
aQute/bnd/differ
aQute/bnd/exporter/subsystem
aQute/bnd/filerepo
aQute/bnd/header
aQute/bnd/help
aQute/bnd/http
aQute/bnd/make
aQute/bnd/maven
aQute/bnd/metatype
aQute/bnd/obr
aQute/bnd/osgi
aQute/bnd/plugin/ant
aQute/bnd/plugin/eclipse
aQute/bnd/plugin/git
aQute/bnd/plugin/gradle
aQute/bnd/plugin/maven
aQute/bnd/properties
aQute/bnd/resource/repository
aQute/bnd/service
aQute/bnd/signing
aQute/bnd/testing
aQute/bnd/url
aQute/bnd/util/dto
aQute/bnd/util/repository
aQute/bnd/version
aQute/bnd/xmlattribute
aQute/configurable
aQute/lib/base64
aQute/lib/codec
aQute/lib/collections
aQute/lib/concurrentinit
aQute/lib/consoleapp
aQute/lib/converter
aQute/lib/data
aQute/lib/deployer
aQute/lib/env
aQute/lib/exceptions
aQute/lib/fileset
aQute/lib/filter
aQute/lib/getopt
aQute/lib/hex
aQute/lib/index
aQute/lib/io
aQute/lib/json
aQute/lib/justif
aQute/lib/log2reporter
aQute/lib/markdown
aQute/lib/mavenpasswordobfuscator
aQute/lib/persistentmap
aQute/lib/promise
aQute/lib/properties
aQute/lib/putjar
aQute/lib/settings
aQute/lib/spring
aQute/lib/strings
aQute/lib/tag
aQute/lib/utf8properties
aQute/lib/xpath
aQute/lib/zip
aQute/libg/asn1
aQute/libg/cafs
aQute/libg/classdump
aQute/libg/classloaders
aQute/libg/clauses
aQute/libg/command
aQute/libg/cryptography
aQute/libg/fileiterator
aQute/libg/filelock
aQute/libg/filters
aQute/libg/forker
aQute/libg/generics
aQute/libg/glob
aQute/libg/gzip
aQute/libg/log
aQute/libg/map
aQute/libg/qtokens
aQute/libg/remote
aQute/libg/reporter
aQute/libg/sax
aQute/libg/sed
aQute/libg/shacache
aQute/libg/slf4j
aQute/libg/tarjan
aQute/libg/tuple
aQute/libg/uri
aQute/libg/xslt
aQute/service/reporter
bsh
com/amazonaws
com/beust/jcommander
com/beust/testng
com/dd/plist
com/esotericsoftware/kryo
com/esotericsoftware/minlog
com/esotericsoftware/reflectasm
com/fasterxml/jackson/annotation
com/fasterxml/jackson/core
com/fasterxml/jackson/databind
com/google/api/client/auth/oauth
com/google/api/client/auth/openidconnect
com/google/api/client/googleapis
com/google/api/client/http
com/google/api/client/json
com/google/api/client/repackaged/com/google/common/annotations
com/google/api/client/repackaged/com/google/common/base
com/google/api/client/repackaged/org/apache/commons/codec
com/google/api/client/testing/http
com/google/api/client/testing/json
com/google/api/client/testing/util
com/google/api/client/util
com/google/api/services/storage
com/google/common/annotations
com/google/common/base
com/google/common/cache
com/google/common/collect
com/google/common/escape
com/google/common/eventbus
com/google/common/hash
com/google/common/html
com/google/common/io
com/google/common/math
com/google/common/net
com/google/common/primitives
com/google/common/reflect
com/google/common/util/concurrent
com/google/common/xml
com/google/gson
com/google/thirdparty/publicsuffix
com/googlecode/jatl
com/jcraft/jsch
groovyjarjarasm/asm
groovyjarjarcommonscli
javaslang
jcifs
junit/extensions
junit/framework
junit/runner
junit/textui
kotlin
net/jcip/annotations
one/util/streamex
org/apache/commons/cli
org/apache/commons/codec
org/apache/commons/collections
org/apache/commons/compress
org/apache/commons/io
org/apache/commons/lang
org/apache/html/dom
org/apache/http
org/apache/ivy
org/apache/maven
org/apache/tools/bzip2
org/apache/tools/mail
org/apache/tools/tar
org/apache/tools/zip
org/apache/wml
org/apache/xbean/propertyeditor
org/apache/xbean/recipe
org/apache/xml/serialize
org/apache/xmlcommons
org/bouncycastle
org/codehaus/classworlds
org/codehaus/plexus
org/cyberneko/html
org/eclipse/jgit/annotations
org/eclipse/jgit/api
org/eclipse/jgit/attributes
org/eclipse/jgit/blame
org/eclipse/jgit/diff
org/eclipse/jgit/dircache
org/eclipse/jgit/errors
org/eclipse/jgit/events
org/eclipse/jgit/fnmatch
org/eclipse/jgit/gitrepo
org/eclipse/jgit/hooks
org/eclipse/jgit/ignore
org/eclipse/jgit/internal
org/eclipse/jgit/lib
org/eclipse/jgit/merge
org/eclipse/jgit/nls
org/eclipse/jgit/notes
org/eclipse/jgit/patch
org/eclipse/jgit/revplot
org/eclipse/jgit/revwalk
org/eclipse/jgit/storage/file
org/eclipse/jgit/storage/pack
org/eclipse/jgit/submodule
org/eclipse/jgit/transport
org/eclipse/jgit/treewalk
org/eclipse/jgit/util
org/fusesource/hawtjni/runtime
org/fusesource/jansi
org/hamcrest
org/intellij/lang/annotations
org/iq80/snappy
org/jetbrains/annotations
org/jetbrains/ide
org/jetbrains/jps/model/java/impl
org/jetbrains/kotlin
org/jetbrains/org/objectweb/asm
org/joda/time
org/json
org/junit
org/mozilla/classfile
org/mozilla/javascript
org/objectweb/asm
org/objenesis
org/osgi/resource
org/osgi/service/component/annotations
org/osgi/service/metatype/annotations
org/osgi/service/repository
org/osgi/util/function
org/osgi/util/promise
org/simpleframework/http
org/simpleframework/transport
org/simpleframework/util
org/sonatype/aether
org/sonatype/maven/polyglot
org/sonatype/plexus/components/cipher
org/sonatype/plexus/components/sec/dispatcher
org/testng
org/yaml/snakeyaml")))
               (with-output-to-file "test-kit-relocated.txt"
                 (lambda _
                   (format #t "com/esotericsoftware/kryo
com/esotericsoftware/minlog
com/esotericsoftware/reflectasm
com/google/common/annotations
com/google/common/base
com/google/common/cache
com/google/common/collect
com/google/common/escape
com/google/common/eventbus
com/google/common/hash
com/google/common/html
com/google/common/io
com/google/common/math
com/google/common/net
com/google/common/primitives
com/google/common/reflect
com/google/common/util/concurrent
com/google/common/xml
com/google/thirdparty/publicsuffix
groovyjarjarasm/asm
groovyjarjarcommonscli
net/jcip/annotations
org/apache/commons/collections
org/apache/commons/compress
org/apache/commons/io
org/apache/commons/lang
org/apache/tools/bzip2
org/apache/tools/mail
org/apache/tools/tar
org/apache/tools/zip
org/apache/xmlcommons
org/fusesource/hawtjni/runtime
org/fusesource/jansi
org/objectweb/asm
org/objenesis
"))))
             (mkdir-p "build/jar")
             (invoke "jar" "cf" "build/jar/gradle-runtime-api-info-4.8.jar"
                             "-C" "build/classes" ".")
             #t)))))))))

;; This package doesn't work. I need to understand how api-mapping.txt and
;; default-imports.txt are generated. Currently they are generated by a custom
;; task defined in buildsrc that is run by gradle, but we don't have enough of
;; gradle to run that.
(define-public gradle-docs
  (let ((base (gradle-subproject "docs" '() '())))
    (package
      (inherit base)
      (arguments
       (substitute-keyword-arguments (package-arguments base)
        ((#:phases phases)
         `(modify-phases ,phases
            (replace 'build
              (lambda _
                (substitute* "build.xml"
                  (("compile, ") ""))
                (with-output-to-file "build/classes/default-imports.txt"
                  (lambda _
                    (format #t "import org.gradle.*
import org.gradle.api.*
import org.gradle.api.artifacts.*
import org.gradle.api.artifacts.cache.*
import org.gradle.api.artifacts.component.*
import org.gradle.api.artifacts.dsl.*
import org.gradle.api.artifacts.ivy.*
import org.gradle.api.artifacts.maven.*
import org.gradle.api.artifacts.query.*
import org.gradle.api.artifacts.repositories.*
import org.gradle.api.artifacts.result.*
import org.gradle.api.artifacts.transform.*
import org.gradle.api.artifacts.type.*
import org.gradle.api.attributes.*
import org.gradle.api.component.*
import org.gradle.api.credentials.*
import org.gradle.api.distribution.*
import org.gradle.api.distribution.plugins.*
import org.gradle.api.dsl.*
import org.gradle.api.execution.*
import org.gradle.api.file.*
import org.gradle.api.initialization.*
import org.gradle.api.initialization.dsl.*
import org.gradle.api.invocation.*
import org.gradle.api.java.archives.*
import org.gradle.api.logging.*
import org.gradle.api.logging.configuration.*
import org.gradle.api.model.*
import org.gradle.api.plugins.*
import org.gradle.api.plugins.announce.*
import org.gradle.api.plugins.antlr.*
import org.gradle.api.plugins.buildcomparison.gradle.*
import org.gradle.api.plugins.osgi.*
import org.gradle.api.plugins.quality.*
import org.gradle.api.plugins.scala.*
import org.gradle.api.provider.*
import org.gradle.api.publish.*
import org.gradle.api.publish.ivy.*
import org.gradle.api.publish.ivy.plugins.*
import org.gradle.api.publish.ivy.tasks.*
import org.gradle.api.publish.maven.*
import org.gradle.api.publish.maven.plugins.*
import org.gradle.api.publish.maven.tasks.*
import org.gradle.api.publish.plugins.*
import org.gradle.api.publish.tasks.*
import org.gradle.api.reflect.*
import org.gradle.api.reporting.*
import org.gradle.api.reporting.components.*
import org.gradle.api.reporting.dependencies.*
import org.gradle.api.reporting.dependents.*
import org.gradle.api.reporting.model.*
import org.gradle.api.reporting.plugins.*
import org.gradle.api.resources.*
import org.gradle.api.specs.*
import org.gradle.api.tasks.*
import org.gradle.api.tasks.ant.*
import org.gradle.api.tasks.application.*
import org.gradle.api.tasks.bundling.*
import org.gradle.api.tasks.compile.*
import org.gradle.api.tasks.diagnostics.*
import org.gradle.api.tasks.incremental.*
import org.gradle.api.tasks.javadoc.*
import org.gradle.api.tasks.scala.*
import org.gradle.api.tasks.testing.*
import org.gradle.api.tasks.testing.junit.*
import org.gradle.api.tasks.testing.testng.*
import org.gradle.api.tasks.util.*
import org.gradle.api.tasks.wrapper.*
import org.gradle.authentication.*
import org.gradle.authentication.aws.*
import org.gradle.authentication.http.*
import org.gradle.buildinit.plugins.*
import org.gradle.buildinit.tasks.*
import org.gradle.caching.*
import org.gradle.caching.configuration.*
import org.gradle.caching.http.*
import org.gradle.caching.local.*
import org.gradle.concurrent.*
import org.gradle.external.javadoc.*
import org.gradle.ide.visualstudio.*
import org.gradle.ide.visualstudio.plugins.*
import org.gradle.ide.visualstudio.tasks.*
import org.gradle.ide.xcode.*
import org.gradle.ide.xcode.plugins.*
import org.gradle.ide.xcode.tasks.*
import org.gradle.ivy.*
import org.gradle.jvm.*
import org.gradle.jvm.application.scripts.*
import org.gradle.jvm.application.tasks.*
import org.gradle.jvm.platform.*
import org.gradle.jvm.plugins.*
import org.gradle.jvm.tasks.*
import org.gradle.jvm.tasks.api.*
import org.gradle.jvm.test.*
import org.gradle.jvm.toolchain.*
import org.gradle.language.assembler.*
import org.gradle.language.assembler.plugins.*
import org.gradle.language.assembler.tasks.*
import org.gradle.language.base.*
import org.gradle.language.base.artifact.*
import org.gradle.language.base.compile.*
import org.gradle.language.base.plugins.*
import org.gradle.language.base.sources.*
import org.gradle.language.c.*
import org.gradle.language.c.plugins.*
import org.gradle.language.c.tasks.*
import org.gradle.language.coffeescript.*
import org.gradle.language.cpp.*
import org.gradle.language.cpp.plugins.*
import org.gradle.language.cpp.tasks.*
import org.gradle.language.java.*
import org.gradle.language.java.artifact.*
import org.gradle.language.java.plugins.*
import org.gradle.language.java.tasks.*
import org.gradle.language.javascript.*
import org.gradle.language.jvm.*
import org.gradle.language.jvm.plugins.*
import org.gradle.language.jvm.tasks.*
import org.gradle.language.nativeplatform.*
import org.gradle.language.nativeplatform.tasks.*
import org.gradle.language.objectivec.*
import org.gradle.language.objectivec.plugins.*
import org.gradle.language.objectivec.tasks.*
import org.gradle.language.objectivecpp.*
import org.gradle.language.objectivecpp.plugins.*
import org.gradle.language.objectivecpp.tasks.*
import org.gradle.language.rc.*
import org.gradle.language.rc.plugins.*
import org.gradle.language.rc.tasks.*
import org.gradle.language.routes.*
import org.gradle.language.scala.*
import org.gradle.language.scala.plugins.*
import org.gradle.language.scala.tasks.*
import org.gradle.language.scala.toolchain.*
import org.gradle.language.swift.*
import org.gradle.language.swift.plugins.*
import org.gradle.language.swift.tasks.*
import org.gradle.language.twirl.*
import org.gradle.maven.*
import org.gradle.model.*
import org.gradle.nativeplatform.*
import org.gradle.nativeplatform.platform.*
import org.gradle.nativeplatform.plugins.*
import org.gradle.nativeplatform.tasks.*
import org.gradle.nativeplatform.test.*
import org.gradle.nativeplatform.test.cpp.*
import org.gradle.nativeplatform.test.cpp.plugins.*
import org.gradle.nativeplatform.test.cunit.*
import org.gradle.nativeplatform.test.cunit.plugins.*
import org.gradle.nativeplatform.test.cunit.tasks.*
import org.gradle.nativeplatform.test.googletest.*
import org.gradle.nativeplatform.test.googletest.plugins.*
import org.gradle.nativeplatform.test.plugins.*
import org.gradle.nativeplatform.test.tasks.*
import org.gradle.nativeplatform.test.xctest.*
import org.gradle.nativeplatform.test.xctest.plugins.*
import org.gradle.nativeplatform.test.xctest.tasks.*
import org.gradle.nativeplatform.toolchain.*
import org.gradle.nativeplatform.toolchain.plugins.*
import org.gradle.normalization.*
import org.gradle.platform.base.*
import org.gradle.platform.base.binary.*
import org.gradle.platform.base.component.*
import org.gradle.platform.base.plugins.*
import org.gradle.play.*
import org.gradle.play.distribution.*
import org.gradle.play.platform.*
import org.gradle.play.plugins.*
import org.gradle.play.plugins.ide.*
import org.gradle.play.tasks.*
import org.gradle.play.toolchain.*
import org.gradle.plugin.devel.*
import org.gradle.plugin.devel.plugins.*
import org.gradle.plugin.devel.tasks.*
import org.gradle.plugin.management.*
import org.gradle.plugin.use.*
import org.gradle.plugins.ear.*
import org.gradle.plugins.ear.descriptor.*
import org.gradle.plugins.ide.api.*
import org.gradle.plugins.ide.eclipse.*
import org.gradle.plugins.ide.idea.*
import org.gradle.plugins.javascript.base.*
import org.gradle.plugins.javascript.coffeescript.*
import org.gradle.plugins.javascript.envjs.*
import org.gradle.plugins.javascript.envjs.browser.*
import org.gradle.plugins.javascript.envjs.http.*
import org.gradle.plugins.javascript.envjs.http.simple.*
import org.gradle.plugins.javascript.jshint.*
import org.gradle.plugins.javascript.rhino.*
import org.gradle.plugins.signing.*
import org.gradle.plugins.signing.signatory.*
import org.gradle.plugins.signing.signatory.pgp.*
import org.gradle.plugins.signing.type.*
import org.gradle.plugins.signing.type.pgp.*
import org.gradle.process.*
import org.gradle.testing.base.*
import org.gradle.testing.base.plugins.*
import org.gradle.testing.jacoco.plugins.*
import org.gradle.testing.jacoco.tasks.*
import org.gradle.testing.jacoco.tasks.rules.*
import org.gradle.testkit.runner.*
import org.gradle.vcs.*
import org.gradle.vcs.git.*
import org.gradle.workers.*")))
                (with-output-to-file "build/classes/api-mapping.txt"
                  (lambda _
                    (format #t "BuildAdapter:org.gradle.BuildAdapter;
BuildListener:org.gradle.BuildListener;
BuildResult:org.gradle.BuildResult;org.gradle.testkit.runner.BuildResult;
StartParameter:org.gradle.StartParameter;
TaskExecutionRequest:org.gradle.TaskExecutionRequest;
Action:org.gradle.api.Action;
ActionConfiguration:org.gradle.api.ActionConfiguration;
AntBuilder:org.gradle.api.AntBuilder;
BuildCancelledException:org.gradle.api.BuildCancelledException;
Buildable:org.gradle.api.Buildable;
BuildableComponentSpec:org.gradle.api.BuildableComponentSpec;
CheckableComponentSpec:org.gradle.api.CheckableComponentSpec;
CircularReferenceException:org.gradle.api.CircularReferenceException;
DefaultTask:org.gradle.api.DefaultTask;
Describable:org.gradle.api.Describable;
DomainObjectCollection:org.gradle.api.DomainObjectCollection;
DomainObjectSet:org.gradle.api.DomainObjectSet;
ExtensiblePolymorphicDomainObjectContainer:org.gradle.api.ExtensiblePolymorphicDomainObjectContainer;
GradleException:org.gradle.api.GradleException;
GradleScriptException:org.gradle.api.GradleScriptException;
HasImplicitReceiver:org.gradle.api.HasImplicitReceiver;
IllegalDependencyNotation:org.gradle.api.IllegalDependencyNotation;
Incubating:org.gradle.api.Incubating;
InvalidActionClosureException:org.gradle.api.InvalidActionClosureException;
InvalidUserCodeException:org.gradle.api.InvalidUserCodeException;
InvalidUserDataException:org.gradle.api.InvalidUserDataException;
JavaVersion:org.gradle.api.JavaVersion;
Named:org.gradle.api.Named;
NamedDomainObjectCollection:org.gradle.api.NamedDomainObjectCollection;
NamedDomainObjectContainer:org.gradle.api.NamedDomainObjectContainer;
NamedDomainObjectFactory:org.gradle.api.NamedDomainObjectFactory;
NamedDomainObjectList:org.gradle.api.NamedDomainObjectList;
NamedDomainObjectSet:org.gradle.api.NamedDomainObjectSet;
Namer:org.gradle.api.Namer;
NonExtensible:org.gradle.api.NonExtensible;
NonNullApi:org.gradle.api.NonNullApi;
Nullable:org.gradle.api.Nullable;
PathValidation:org.gradle.api.PathValidation;
Plugin:org.gradle.api.Plugin;
PolymorphicDomainObjectContainer:org.gradle.api.PolymorphicDomainObjectContainer;
Project:org.gradle.api.Project;
ProjectConfigurationException:org.gradle.api.ProjectConfigurationException;
ProjectEvaluationListener:org.gradle.api.ProjectEvaluationListener;
ProjectState:org.gradle.api.ProjectState;
Rule:org.gradle.api.Rule;
Script:org.gradle.api.Script;
Task:org.gradle.api.Task;
Transformer:org.gradle.api.Transformer;
UncheckedIOException:org.gradle.api.UncheckedIOException;
UnknownDomainObjectException:org.gradle.api.UnknownDomainObjectException;
UnknownProjectException:org.gradle.api.UnknownProjectException;
UnknownTaskException:org.gradle.api.UnknownTaskException;
XmlProvider:org.gradle.api.XmlProvider;
ArtifactCollection:org.gradle.api.artifacts.ArtifactCollection;
ArtifactIdentifier:org.gradle.api.artifacts.ArtifactIdentifier;
ArtifactRepositoryContainer:org.gradle.api.artifacts.ArtifactRepositoryContainer;
ArtifactView:org.gradle.api.artifacts.ArtifactView;
ClientModule:org.gradle.api.artifacts.ClientModule;
ComponentMetadata:org.gradle.api.artifacts.ComponentMetadata;
ComponentMetadataBuilder:org.gradle.api.artifacts.ComponentMetadataBuilder;
ComponentMetadataDetails:org.gradle.api.artifacts.ComponentMetadataDetails;
ComponentMetadataSupplier:org.gradle.api.artifacts.ComponentMetadataSupplier;
ComponentMetadataSupplierDetails:org.gradle.api.artifacts.ComponentMetadataSupplierDetails;
ComponentModuleMetadata:org.gradle.api.artifacts.ComponentModuleMetadata;
ComponentModuleMetadataDetails:org.gradle.api.artifacts.ComponentModuleMetadataDetails;
ComponentSelection:org.gradle.api.artifacts.ComponentSelection;
ComponentSelectionRules:org.gradle.api.artifacts.ComponentSelectionRules;
ConfigurablePublishArtifact:org.gradle.api.artifacts.ConfigurablePublishArtifact;
Configuration:org.gradle.api.artifacts.Configuration;
ConfigurationContainer:org.gradle.api.artifacts.ConfigurationContainer;
ConfigurationPublications:org.gradle.api.artifacts.ConfigurationPublications;
ConfigurationVariant:org.gradle.api.artifacts.ConfigurationVariant;
DependenciesMetadata:org.gradle.api.artifacts.DependenciesMetadata;
Dependency:org.gradle.api.artifacts.Dependency;
DependencyArtifact:org.gradle.api.artifacts.DependencyArtifact;
DependencyMetadata:org.gradle.api.artifacts.DependencyMetadata;
DependencyResolutionListener:org.gradle.api.artifacts.DependencyResolutionListener;
DependencyResolveDetails:org.gradle.api.artifacts.DependencyResolveDetails;
DependencySet:org.gradle.api.artifacts.DependencySet;
DependencySubstitution:org.gradle.api.artifacts.DependencySubstitution;
DependencySubstitutions:org.gradle.api.artifacts.DependencySubstitutions;
ExcludeRule:org.gradle.api.artifacts.ExcludeRule;
ExcludeRuleContainer:org.gradle.api.artifacts.ExcludeRuleContainer;
ExternalDependency:org.gradle.api.artifacts.ExternalDependency;
ExternalModuleDependency:org.gradle.api.artifacts.ExternalModuleDependency;
FileCollectionDependency:org.gradle.api.artifacts.FileCollectionDependency;
LenientConfiguration:org.gradle.api.artifacts.LenientConfiguration;
ModuleDependency:org.gradle.api.artifacts.ModuleDependency;
ModuleIdentifier:org.gradle.api.artifacts.ModuleIdentifier;
ModuleVersionIdentifier:org.gradle.api.artifacts.ModuleVersionIdentifier;
ModuleVersionSelector:org.gradle.api.artifacts.ModuleVersionSelector;
MutableVersionConstraint:org.gradle.api.artifacts.MutableVersionConstraint;
ProjectDependency:org.gradle.api.artifacts.ProjectDependency;
PublishArtifact:org.gradle.api.artifacts.PublishArtifact;
PublishArtifactSet:org.gradle.api.artifacts.PublishArtifactSet;
PublishException:org.gradle.api.artifacts.PublishException;
ResolutionStrategy:org.gradle.api.artifacts.ResolutionStrategy;
ResolvableDependencies:org.gradle.api.artifacts.ResolvableDependencies;
ResolveException:org.gradle.api.artifacts.ResolveException;
ResolvedArtifact:org.gradle.api.artifacts.ResolvedArtifact;
ResolvedConfiguration:org.gradle.api.artifacts.ResolvedConfiguration;
ResolvedDependency:org.gradle.api.artifacts.ResolvedDependency;
ResolvedModuleVersion:org.gradle.api.artifacts.ResolvedModuleVersion;
SelfResolvingDependency:org.gradle.api.artifacts.SelfResolvingDependency;
UnknownConfigurationException:org.gradle.api.artifacts.UnknownConfigurationException;
UnknownRepositoryException:org.gradle.api.artifacts.UnknownRepositoryException;
UnresolvedDependency:org.gradle.api.artifacts.UnresolvedDependency;
VariantMetadata:org.gradle.api.artifacts.VariantMetadata;
VersionConstraint:org.gradle.api.artifacts.VersionConstraint;
ArtifactResolutionControl:org.gradle.api.artifacts.cache.ArtifactResolutionControl;
DependencyResolutionControl:org.gradle.api.artifacts.cache.DependencyResolutionControl;
ModuleResolutionControl:org.gradle.api.artifacts.cache.ModuleResolutionControl;
ResolutionControl:org.gradle.api.artifacts.cache.ResolutionControl;
ResolutionRules:org.gradle.api.artifacts.cache.ResolutionRules;
BuildIdentifier:org.gradle.api.artifacts.component.BuildIdentifier;
ComponentArtifactIdentifier:org.gradle.api.artifacts.component.ComponentArtifactIdentifier;
ComponentIdentifier:org.gradle.api.artifacts.component.ComponentIdentifier;
ComponentSelector:org.gradle.api.artifacts.component.ComponentSelector;
LibraryBinaryIdentifier:org.gradle.api.artifacts.component.LibraryBinaryIdentifier;
LibraryComponentSelector:org.gradle.api.artifacts.component.LibraryComponentSelector;
ModuleComponentIdentifier:org.gradle.api.artifacts.component.ModuleComponentIdentifier;
ModuleComponentSelector:org.gradle.api.artifacts.component.ModuleComponentSelector;
ProjectComponentIdentifier:org.gradle.api.artifacts.component.ProjectComponentIdentifier;
ProjectComponentSelector:org.gradle.api.artifacts.component.ProjectComponentSelector;
ArtifactHandler:org.gradle.api.artifacts.dsl.ArtifactHandler;
ComponentMetadataHandler:org.gradle.api.artifacts.dsl.ComponentMetadataHandler;
ComponentModuleMetadataHandler:org.gradle.api.artifacts.dsl.ComponentModuleMetadataHandler;
DependencyHandler:org.gradle.api.artifacts.dsl.DependencyHandler;
RepositoryHandler:org.gradle.api.artifacts.dsl.RepositoryHandler;
IvyExtraInfo:org.gradle.api.artifacts.ivy.IvyExtraInfo;
IvyModuleDescriptor:org.gradle.api.artifacts.ivy.IvyModuleDescriptor;
Conf2ScopeMapping:org.gradle.api.artifacts.maven.Conf2ScopeMapping;
Conf2ScopeMappingContainer:org.gradle.api.artifacts.maven.Conf2ScopeMappingContainer;
GroovyMavenDeployer:org.gradle.api.artifacts.maven.GroovyMavenDeployer;
MavenDeployer:org.gradle.api.artifacts.maven.MavenDeployer;
MavenDeployment:org.gradle.api.artifacts.maven.MavenDeployment;
MavenPom:org.gradle.api.artifacts.maven.MavenPom;org.gradle.api.publish.maven.MavenPom;
MavenResolver:org.gradle.api.artifacts.maven.MavenResolver;
PomFilterContainer:org.gradle.api.artifacts.maven.PomFilterContainer;
PublishFilter:org.gradle.api.artifacts.maven.PublishFilter;
ArtifactResolutionQuery:org.gradle.api.artifacts.query.ArtifactResolutionQuery;
ArtifactRepository:org.gradle.api.artifacts.repositories.ArtifactRepository;
AuthenticationContainer:org.gradle.api.artifacts.repositories.AuthenticationContainer;
AuthenticationSupported:org.gradle.api.artifacts.repositories.AuthenticationSupported;
FlatDirectoryArtifactRepository:org.gradle.api.artifacts.repositories.FlatDirectoryArtifactRepository;
IvyArtifactRepository:org.gradle.api.artifacts.repositories.IvyArtifactRepository;
IvyArtifactRepositoryMetaDataProvider:org.gradle.api.artifacts.repositories.IvyArtifactRepositoryMetaDataProvider;
IvyPatternRepositoryLayout:org.gradle.api.artifacts.repositories.IvyPatternRepositoryLayout;
MavenArtifactRepository:org.gradle.api.artifacts.repositories.MavenArtifactRepository;
PasswordCredentials:org.gradle.api.artifacts.repositories.PasswordCredentials;org.gradle.api.credentials.PasswordCredentials;
RepositoryLayout:org.gradle.api.artifacts.repositories.RepositoryLayout;
RepositoryResourceAccessor:org.gradle.api.artifacts.repositories.RepositoryResourceAccessor;
ArtifactResolutionResult:org.gradle.api.artifacts.result.ArtifactResolutionResult;
ArtifactResult:org.gradle.api.artifacts.result.ArtifactResult;
ComponentArtifactsResult:org.gradle.api.artifacts.result.ComponentArtifactsResult;
ComponentResult:org.gradle.api.artifacts.result.ComponentResult;
ComponentSelectionReason:org.gradle.api.artifacts.result.ComponentSelectionReason;
DependencyResult:org.gradle.api.artifacts.result.DependencyResult;
ResolutionResult:org.gradle.api.artifacts.result.ResolutionResult;
ResolvedArtifactResult:org.gradle.api.artifacts.result.ResolvedArtifactResult;
ResolvedComponentResult:org.gradle.api.artifacts.result.ResolvedComponentResult;
ResolvedDependencyResult:org.gradle.api.artifacts.result.ResolvedDependencyResult;
ResolvedVariantResult:org.gradle.api.artifacts.result.ResolvedVariantResult;
UnresolvedArtifactResult:org.gradle.api.artifacts.result.UnresolvedArtifactResult;
UnresolvedComponentResult:org.gradle.api.artifacts.result.UnresolvedComponentResult;
UnresolvedDependencyResult:org.gradle.api.artifacts.result.UnresolvedDependencyResult;
ArtifactTransform:org.gradle.api.artifacts.transform.ArtifactTransform;
ArtifactTransformException:org.gradle.api.artifacts.transform.ArtifactTransformException;
VariantTransform:org.gradle.api.artifacts.transform.VariantTransform;
VariantTransformConfigurationException:org.gradle.api.artifacts.transform.VariantTransformConfigurationException;
ArtifactTypeContainer:org.gradle.api.artifacts.type.ArtifactTypeContainer;
ArtifactTypeDefinition:org.gradle.api.artifacts.type.ArtifactTypeDefinition;
Attribute:org.gradle.api.attributes.Attribute;
AttributeCompatibilityRule:org.gradle.api.attributes.AttributeCompatibilityRule;
AttributeContainer:org.gradle.api.attributes.AttributeContainer;
AttributeDisambiguationRule:org.gradle.api.attributes.AttributeDisambiguationRule;
AttributeMatchingStrategy:org.gradle.api.attributes.AttributeMatchingStrategy;
AttributesSchema:org.gradle.api.attributes.AttributesSchema;
CompatibilityCheckDetails:org.gradle.api.attributes.CompatibilityCheckDetails;
CompatibilityRuleChain:org.gradle.api.attributes.CompatibilityRuleChain;
DisambiguationRuleChain:org.gradle.api.attributes.DisambiguationRuleChain;
HasAttributes:org.gradle.api.attributes.HasAttributes;
HasConfigurableAttributes:org.gradle.api.attributes.HasConfigurableAttributes;
MultipleCandidatesDetails:org.gradle.api.attributes.MultipleCandidatesDetails;
Usage:org.gradle.api.attributes.Usage;
Artifact:org.gradle.api.component.Artifact;
Component:org.gradle.api.component.Component;
ComponentWithVariants:org.gradle.api.component.ComponentWithVariants;
SoftwareComponent:org.gradle.api.component.SoftwareComponent;
SoftwareComponentContainer:org.gradle.api.component.SoftwareComponentContainer;
AwsCredentials:org.gradle.api.credentials.AwsCredentials;
Credentials:org.gradle.api.credentials.Credentials;
Distribution:org.gradle.api.distribution.Distribution;
DistributionContainer:org.gradle.api.distribution.DistributionContainer;
DistributionPlugin:org.gradle.api.distribution.plugins.DistributionPlugin;
ConventionProperty:org.gradle.api.dsl.ConventionProperty;
TaskActionListener:org.gradle.api.execution.TaskActionListener;
TaskExecutionAdapter:org.gradle.api.execution.TaskExecutionAdapter;
TaskExecutionGraph:org.gradle.api.execution.TaskExecutionGraph;
TaskExecutionGraphListener:org.gradle.api.execution.TaskExecutionGraphListener;
TaskExecutionListener:org.gradle.api.execution.TaskExecutionListener;
ConfigurableFileCollection:org.gradle.api.file.ConfigurableFileCollection;
ConfigurableFileTree:org.gradle.api.file.ConfigurableFileTree;
ContentFilterable:org.gradle.api.file.ContentFilterable;
CopyProcessingSpec:org.gradle.api.file.CopyProcessingSpec;
CopySourceSpec:org.gradle.api.file.CopySourceSpec;
CopySpec:org.gradle.api.file.CopySpec;
DeleteSpec:org.gradle.api.file.DeleteSpec;
Directory:org.gradle.api.file.Directory;
DirectoryProperty:org.gradle.api.file.DirectoryProperty;
DirectoryTree:org.gradle.api.file.DirectoryTree;
DirectoryVar:org.gradle.api.file.DirectoryVar;
DuplicateFileCopyingException:org.gradle.api.file.DuplicateFileCopyingException;
DuplicatesStrategy:org.gradle.api.file.DuplicatesStrategy;
EmptyFileVisitor:org.gradle.api.file.EmptyFileVisitor;
FileCollection:org.gradle.api.file.FileCollection;
FileCopyDetails:org.gradle.api.file.FileCopyDetails;
FileSystemLocation:org.gradle.api.file.FileSystemLocation;
FileTree:org.gradle.api.file.FileTree;
FileTreeElement:org.gradle.api.file.FileTreeElement;
FileVisitDetails:org.gradle.api.file.FileVisitDetails;
FileVisitor:org.gradle.api.file.FileVisitor;
ProjectLayout:org.gradle.api.file.ProjectLayout;
RegularFile:org.gradle.api.file.RegularFile;
RegularFileProperty:org.gradle.api.file.RegularFileProperty;
RegularFileVar:org.gradle.api.file.RegularFileVar;
RelativePath:org.gradle.api.file.RelativePath;
ReproducibleFileVisitor:org.gradle.api.file.ReproducibleFileVisitor;
SourceDirectorySet:org.gradle.api.file.SourceDirectorySet;
UnableToDeleteFileException:org.gradle.api.file.UnableToDeleteFileException;
ConfigurableIncludedBuild:org.gradle.api.initialization.ConfigurableIncludedBuild;
IncludedBuild:org.gradle.api.initialization.IncludedBuild;
ProjectDescriptor:org.gradle.api.initialization.ProjectDescriptor;
Settings:org.gradle.api.initialization.Settings;
ScriptHandler:org.gradle.api.initialization.dsl.ScriptHandler;
Gradle:org.gradle.api.invocation.Gradle;
Attributes:org.gradle.api.java.archives.Attributes;
Manifest:org.gradle.api.java.archives.Manifest;
ManifestException:org.gradle.api.java.archives.ManifestException;
ManifestMergeDetails:org.gradle.api.java.archives.ManifestMergeDetails;
ManifestMergeSpec:org.gradle.api.java.archives.ManifestMergeSpec;
LogLevel:org.gradle.api.logging.LogLevel;
Logger:org.gradle.api.logging.Logger;
Logging:org.gradle.api.logging.Logging;
LoggingManager:org.gradle.api.logging.LoggingManager;
LoggingOutput:org.gradle.api.logging.LoggingOutput;
StandardOutputListener:org.gradle.api.logging.StandardOutputListener;
ConsoleOutput:org.gradle.api.logging.configuration.ConsoleOutput;
LoggingConfiguration:org.gradle.api.logging.configuration.LoggingConfiguration;
ShowStacktrace:org.gradle.api.logging.configuration.ShowStacktrace;
ObjectFactory:org.gradle.api.model.ObjectFactory;
ApplicationPlugin:org.gradle.api.plugins.ApplicationPlugin;
ApplicationPluginConvention:org.gradle.api.plugins.ApplicationPluginConvention;
AppliedPlugin:org.gradle.api.plugins.AppliedPlugin;
BasePlugin:org.gradle.api.plugins.BasePlugin;
BasePluginConvention:org.gradle.api.plugins.BasePluginConvention;
Convention:org.gradle.api.plugins.Convention;
DeferredConfigurable:org.gradle.api.plugins.DeferredConfigurable;
ExtensionAware:org.gradle.api.plugins.ExtensionAware;
ExtensionContainer:org.gradle.api.plugins.ExtensionContainer;
ExtraPropertiesExtension:org.gradle.api.plugins.ExtraPropertiesExtension;
GroovyBasePlugin:org.gradle.api.plugins.GroovyBasePlugin;
GroovyPlugin:org.gradle.api.plugins.GroovyPlugin;
HelpTasksPlugin:org.gradle.api.plugins.HelpTasksPlugin;
InvalidPluginException:org.gradle.api.plugins.InvalidPluginException;
JavaBasePlugin:org.gradle.api.plugins.JavaBasePlugin;
JavaLibraryDistributionPlugin:org.gradle.api.plugins.JavaLibraryDistributionPlugin;
JavaLibraryPlugin:org.gradle.api.plugins.JavaLibraryPlugin;
JavaPlugin:org.gradle.api.plugins.JavaPlugin;
JavaPluginConvention:org.gradle.api.plugins.JavaPluginConvention;
MavenPlugin:org.gradle.api.plugins.MavenPlugin;
MavenPluginConvention:org.gradle.api.plugins.MavenPluginConvention;
MavenRepositoryHandlerConvention:org.gradle.api.plugins.MavenRepositoryHandlerConvention;
ObjectConfigurationAction:org.gradle.api.plugins.ObjectConfigurationAction;
PluginAware:org.gradle.api.plugins.PluginAware;
PluginCollection:org.gradle.api.plugins.PluginCollection;
PluginContainer:org.gradle.api.plugins.PluginContainer;
PluginInstantiationException:org.gradle.api.plugins.PluginInstantiationException;
PluginManager:org.gradle.api.plugins.PluginManager;
ProjectReportsPlugin:org.gradle.api.plugins.ProjectReportsPlugin;
ProjectReportsPluginConvention:org.gradle.api.plugins.ProjectReportsPluginConvention;
ReportingBasePlugin:org.gradle.api.plugins.ReportingBasePlugin;
UnknownPluginException:org.gradle.api.plugins.UnknownPluginException;
WarPlugin:org.gradle.api.plugins.WarPlugin;
WarPluginConvention:org.gradle.api.plugins.WarPluginConvention;
AnnouncePlugin:org.gradle.api.plugins.announce.AnnouncePlugin;
AnnouncePluginExtension:org.gradle.api.plugins.announce.AnnouncePluginExtension;
Announcer:org.gradle.api.plugins.announce.Announcer;
BuildAnnouncementsPlugin:org.gradle.api.plugins.announce.BuildAnnouncementsPlugin;
AntlrPlugin:org.gradle.api.plugins.antlr.AntlrPlugin;
AntlrSourceVirtualDirectory:org.gradle.api.plugins.antlr.AntlrSourceVirtualDirectory;
AntlrTask:org.gradle.api.plugins.antlr.AntlrTask;
CompareGradleBuilds:org.gradle.api.plugins.buildcomparison.gradle.CompareGradleBuilds;
CompareGradleBuildsPlugin:org.gradle.api.plugins.buildcomparison.gradle.CompareGradleBuildsPlugin;
GradleBuildInvocationSpec:org.gradle.api.plugins.buildcomparison.gradle.GradleBuildInvocationSpec;
OsgiManifest:org.gradle.api.plugins.osgi.OsgiManifest;
OsgiPlugin:org.gradle.api.plugins.osgi.OsgiPlugin;
OsgiPluginConvention:org.gradle.api.plugins.osgi.OsgiPluginConvention;
Checkstyle:org.gradle.api.plugins.quality.Checkstyle;
CheckstyleExtension:org.gradle.api.plugins.quality.CheckstyleExtension;
CheckstylePlugin:org.gradle.api.plugins.quality.CheckstylePlugin;
CheckstyleReports:org.gradle.api.plugins.quality.CheckstyleReports;
CodeNarc:org.gradle.api.plugins.quality.CodeNarc;
CodeNarcExtension:org.gradle.api.plugins.quality.CodeNarcExtension;
CodeNarcPlugin:org.gradle.api.plugins.quality.CodeNarcPlugin;
CodeNarcReports:org.gradle.api.plugins.quality.CodeNarcReports;
CodeQualityExtension:org.gradle.api.plugins.quality.CodeQualityExtension;
FindBugs:org.gradle.api.plugins.quality.FindBugs;
FindBugsExtension:org.gradle.api.plugins.quality.FindBugsExtension;
FindBugsPlugin:org.gradle.api.plugins.quality.FindBugsPlugin;
FindBugsReports:org.gradle.api.plugins.quality.FindBugsReports;
FindBugsXmlReport:org.gradle.api.plugins.quality.FindBugsXmlReport;
JDepend:org.gradle.api.plugins.quality.JDepend;
JDependExtension:org.gradle.api.plugins.quality.JDependExtension;
JDependPlugin:org.gradle.api.plugins.quality.JDependPlugin;
JDependReports:org.gradle.api.plugins.quality.JDependReports;
Pmd:org.gradle.api.plugins.quality.Pmd;
PmdExtension:org.gradle.api.plugins.quality.PmdExtension;
PmdPlugin:org.gradle.api.plugins.quality.PmdPlugin;
PmdReports:org.gradle.api.plugins.quality.PmdReports;
TargetJdk:org.gradle.api.plugins.quality.TargetJdk;
ScalaBasePlugin:org.gradle.api.plugins.scala.ScalaBasePlugin;
ScalaPlugin:org.gradle.api.plugins.scala.ScalaPlugin;
ListProperty:org.gradle.api.provider.ListProperty;
Property:org.gradle.api.provider.Property;
PropertyState:org.gradle.api.provider.PropertyState;
Provider:org.gradle.api.provider.Provider;
ProviderFactory:org.gradle.api.provider.ProviderFactory;
Publication:org.gradle.api.publish.Publication;
PublicationContainer:org.gradle.api.publish.PublicationContainer;
PublishingExtension:org.gradle.api.publish.PublishingExtension;
InvalidIvyPublicationException:org.gradle.api.publish.ivy.InvalidIvyPublicationException;
IvyArtifact:org.gradle.api.publish.ivy.IvyArtifact;
IvyArtifactSet:org.gradle.api.publish.ivy.IvyArtifactSet;
IvyConfiguration:org.gradle.api.publish.ivy.IvyConfiguration;
IvyConfigurationContainer:org.gradle.api.publish.ivy.IvyConfigurationContainer;
IvyDependency:org.gradle.api.publish.ivy.IvyDependency;
IvyExtraInfoSpec:org.gradle.api.publish.ivy.IvyExtraInfoSpec;
IvyModuleDescriptorSpec:org.gradle.api.publish.ivy.IvyModuleDescriptorSpec;
IvyPublication:org.gradle.api.publish.ivy.IvyPublication;
IvyPublishPlugin:org.gradle.api.publish.ivy.plugins.IvyPublishPlugin;
GenerateIvyDescriptor:org.gradle.api.publish.ivy.tasks.GenerateIvyDescriptor;
PublishToIvyRepository:org.gradle.api.publish.ivy.tasks.PublishToIvyRepository;
InvalidMavenPublicationException:org.gradle.api.publish.maven.InvalidMavenPublicationException;
MavenArtifact:org.gradle.api.publish.maven.MavenArtifact;
MavenArtifactSet:org.gradle.api.publish.maven.MavenArtifactSet;
MavenDependency:org.gradle.api.publish.maven.MavenDependency;
MavenPublication:org.gradle.api.publish.maven.MavenPublication;
MavenPublishPlugin:org.gradle.api.publish.maven.plugins.MavenPublishPlugin;
AbstractPublishToMaven:org.gradle.api.publish.maven.tasks.AbstractPublishToMaven;
GenerateMavenPom:org.gradle.api.publish.maven.tasks.GenerateMavenPom;
PublishToMavenLocal:org.gradle.api.publish.maven.tasks.PublishToMavenLocal;
PublishToMavenRepository:org.gradle.api.publish.maven.tasks.PublishToMavenRepository;
PublishingPlugin:org.gradle.api.publish.plugins.PublishingPlugin;
GenerateModuleMetadata:org.gradle.api.publish.tasks.GenerateModuleMetadata;
HasPublicType:org.gradle.api.reflect.HasPublicType;
ObjectInstantiationException:org.gradle.api.reflect.ObjectInstantiationException;
TypeOf:org.gradle.api.reflect.TypeOf;
BuildDashboardReports:org.gradle.api.reporting.BuildDashboardReports;
ConfigurableReport:org.gradle.api.reporting.ConfigurableReport;
CustomizableHtmlReport:org.gradle.api.reporting.CustomizableHtmlReport;
DirectoryReport:org.gradle.api.reporting.DirectoryReport;
GenerateBuildDashboard:org.gradle.api.reporting.GenerateBuildDashboard;
Report:org.gradle.api.reporting.Report;
ReportContainer:org.gradle.api.reporting.ReportContainer;
Reporting:org.gradle.api.reporting.Reporting;
ReportingExtension:org.gradle.api.reporting.ReportingExtension;
SingleFileReport:org.gradle.api.reporting.SingleFileReport;
ComponentReport:org.gradle.api.reporting.components.ComponentReport;
DependencyReportContainer:org.gradle.api.reporting.dependencies.DependencyReportContainer;
HtmlDependencyReportTask:org.gradle.api.reporting.dependencies.HtmlDependencyReportTask;
DependentComponentsReport:org.gradle.api.reporting.dependents.DependentComponentsReport;
ModelReport:org.gradle.api.reporting.model.ModelReport;
BuildDashboardPlugin:org.gradle.api.reporting.plugins.BuildDashboardPlugin;
MissingResourceException:org.gradle.api.resources.MissingResourceException;
ReadableResource:org.gradle.api.resources.ReadableResource;
Resource:org.gradle.api.resources.Resource;
ResourceException:org.gradle.api.resources.ResourceException;
ResourceHandler:org.gradle.api.resources.ResourceHandler;
TextResource:org.gradle.api.resources.TextResource;
TextResourceFactory:org.gradle.api.resources.TextResourceFactory;
AndSpec:org.gradle.api.specs.AndSpec;
CompositeSpec:org.gradle.api.specs.CompositeSpec;
NotSpec:org.gradle.api.specs.NotSpec;
OrSpec:org.gradle.api.specs.OrSpec;
Spec:org.gradle.api.specs.Spec;
Specs:org.gradle.api.specs.Specs;
AbstractCopyTask:org.gradle.api.tasks.AbstractCopyTask;
AbstractExecTask:org.gradle.api.tasks.AbstractExecTask;
AntBuilderAware:org.gradle.api.tasks.AntBuilderAware;
CacheableTask:org.gradle.api.tasks.CacheableTask;
Classpath:org.gradle.api.tasks.Classpath;org.gradle.jvm.Classpath;
ClasspathNormalizer:org.gradle.api.tasks.ClasspathNormalizer;
CompatibilityAdapterForTaskInputs:org.gradle.api.tasks.CompatibilityAdapterForTaskInputs;
CompatibilityAdapterForTaskOutputs:org.gradle.api.tasks.CompatibilityAdapterForTaskOutputs;
CompileClasspath:org.gradle.api.tasks.CompileClasspath;
CompileClasspathNormalizer:org.gradle.api.tasks.CompileClasspathNormalizer;
Console:org.gradle.api.tasks.Console;
Copy:org.gradle.api.tasks.Copy;
Delete:org.gradle.api.tasks.Delete;
Destroys:org.gradle.api.tasks.Destroys;
Exec:org.gradle.api.tasks.Exec;
FileNormalizer:org.gradle.api.tasks.FileNormalizer;
GradleBuild:org.gradle.api.tasks.GradleBuild;
GroovyRuntime:org.gradle.api.tasks.GroovyRuntime;
GroovySourceSet:org.gradle.api.tasks.GroovySourceSet;
Input:org.gradle.api.tasks.Input;
InputDirectory:org.gradle.api.tasks.InputDirectory;
InputFile:org.gradle.api.tasks.InputFile;
InputFiles:org.gradle.api.tasks.InputFiles;
Internal:org.gradle.api.tasks.Internal;
JavaExec:org.gradle.api.tasks.JavaExec;
LocalState:org.gradle.api.tasks.LocalState;
Nested:org.gradle.api.tasks.Nested;
Optional:org.gradle.api.tasks.Optional;
OutputDirectories:org.gradle.api.tasks.OutputDirectories;
OutputDirectory:org.gradle.api.tasks.OutputDirectory;
OutputFile:org.gradle.api.tasks.OutputFile;
OutputFiles:org.gradle.api.tasks.OutputFiles;
PathSensitive:org.gradle.api.tasks.PathSensitive;
PathSensitivity:org.gradle.api.tasks.PathSensitivity;
ScalaRuntime:org.gradle.api.tasks.ScalaRuntime;
ScalaSourceSet:org.gradle.api.tasks.ScalaSourceSet;
SkipWhenEmpty:org.gradle.api.tasks.SkipWhenEmpty;
SourceSet:org.gradle.api.tasks.SourceSet;
SourceSetContainer:org.gradle.api.tasks.SourceSetContainer;
SourceSetOutput:org.gradle.api.tasks.SourceSetOutput;
SourceTask:org.gradle.api.tasks.SourceTask;
StopActionException:org.gradle.api.tasks.StopActionException;
StopExecutionException:org.gradle.api.tasks.StopExecutionException;
Sync:org.gradle.api.tasks.Sync;
TaskAction:org.gradle.api.tasks.TaskAction;
TaskCollection:org.gradle.api.tasks.TaskCollection;
TaskContainer:org.gradle.api.tasks.TaskContainer;
TaskDependency:org.gradle.api.tasks.TaskDependency;
TaskDestroyables:org.gradle.api.tasks.TaskDestroyables;
TaskExecutionException:org.gradle.api.tasks.TaskExecutionException;
TaskFilePropertyBuilder:org.gradle.api.tasks.TaskFilePropertyBuilder;
TaskInputFilePropertyBuilder:org.gradle.api.tasks.TaskInputFilePropertyBuilder;
TaskInputPropertyBuilder:org.gradle.api.tasks.TaskInputPropertyBuilder;
TaskInputs:org.gradle.api.tasks.TaskInputs;
TaskInstantiationException:org.gradle.api.tasks.TaskInstantiationException;
TaskLocalState:org.gradle.api.tasks.TaskLocalState;
TaskOutputFilePropertyBuilder:org.gradle.api.tasks.TaskOutputFilePropertyBuilder;
TaskOutputs:org.gradle.api.tasks.TaskOutputs;
TaskPropertyBuilder:org.gradle.api.tasks.TaskPropertyBuilder;
TaskReference:org.gradle.api.tasks.TaskReference;
TaskState:org.gradle.api.tasks.TaskState;
TaskValidationException:org.gradle.api.tasks.TaskValidationException;
Upload:org.gradle.api.tasks.Upload;
VerificationTask:org.gradle.api.tasks.VerificationTask;
WorkResult:org.gradle.api.tasks.WorkResult;
WorkResults:org.gradle.api.tasks.WorkResults;
WriteProperties:org.gradle.api.tasks.WriteProperties;
AntTarget:org.gradle.api.tasks.ant.AntTarget;
CreateStartScripts:org.gradle.api.tasks.application.CreateStartScripts;org.gradle.jvm.application.tasks.CreateStartScripts;
AbstractArchiveTask:org.gradle.api.tasks.bundling.AbstractArchiveTask;
Compression:org.gradle.api.tasks.bundling.Compression;
Jar:org.gradle.api.tasks.bundling.Jar;org.gradle.jvm.tasks.Jar;
Tar:org.gradle.api.tasks.bundling.Tar;
War:org.gradle.api.tasks.bundling.War;
Zip:org.gradle.api.tasks.bundling.Zip;
ZipEntryCompression:org.gradle.api.tasks.bundling.ZipEntryCompression;
AbstractCompile:org.gradle.api.tasks.compile.AbstractCompile;
AbstractOptions:org.gradle.api.tasks.compile.AbstractOptions;
BaseForkOptions:org.gradle.api.tasks.compile.BaseForkOptions;
CompileOptions:org.gradle.api.tasks.compile.CompileOptions;
DebugOptions:org.gradle.api.tasks.compile.DebugOptions;
ForkOptions:org.gradle.api.tasks.compile.ForkOptions;
GroovyCompile:org.gradle.api.tasks.compile.GroovyCompile;
GroovyCompileOptions:org.gradle.api.tasks.compile.GroovyCompileOptions;
GroovyForkOptions:org.gradle.api.tasks.compile.GroovyForkOptions;
JavaCompile:org.gradle.api.tasks.compile.JavaCompile;
AbstractDependencyReportTask:org.gradle.api.tasks.diagnostics.AbstractDependencyReportTask;
AbstractReportTask:org.gradle.api.tasks.diagnostics.AbstractReportTask;
BuildEnvironmentReportTask:org.gradle.api.tasks.diagnostics.BuildEnvironmentReportTask;
DependencyInsightReportTask:org.gradle.api.tasks.diagnostics.DependencyInsightReportTask;
DependencyReportTask:org.gradle.api.tasks.diagnostics.DependencyReportTask;
ProjectReportTask:org.gradle.api.tasks.diagnostics.ProjectReportTask;
PropertyReportTask:org.gradle.api.tasks.diagnostics.PropertyReportTask;
TaskReportTask:org.gradle.api.tasks.diagnostics.TaskReportTask;
IncrementalTaskInputs:org.gradle.api.tasks.incremental.IncrementalTaskInputs;
InputFileDetails:org.gradle.api.tasks.incremental.InputFileDetails;
Groovydoc:org.gradle.api.tasks.javadoc.Groovydoc;
Javadoc:org.gradle.api.tasks.javadoc.Javadoc;
IncrementalCompileOptions:org.gradle.api.tasks.scala.IncrementalCompileOptions;
ScalaCompile:org.gradle.api.tasks.scala.ScalaCompile;
ScalaCompileOptions:org.gradle.api.tasks.scala.ScalaCompileOptions;
ScalaDoc:org.gradle.api.tasks.scala.ScalaDoc;
ScalaDocOptions:org.gradle.api.tasks.scala.ScalaDocOptions;
ScalaForkOptions:org.gradle.api.tasks.scala.ScalaForkOptions;
AbstractTestTask:org.gradle.api.tasks.testing.AbstractTestTask;
JUnitXmlReport:org.gradle.api.tasks.testing.JUnitXmlReport;
Test:org.gradle.api.tasks.testing.Test;
TestDescriptor:org.gradle.api.tasks.testing.TestDescriptor;
TestExecutionException:org.gradle.api.tasks.testing.TestExecutionException;
TestFilter:org.gradle.api.tasks.testing.TestFilter;
TestFrameworkOptions:org.gradle.api.tasks.testing.TestFrameworkOptions;
TestListener:org.gradle.api.tasks.testing.TestListener;
TestOutputEvent:org.gradle.api.tasks.testing.TestOutputEvent;
TestOutputListener:org.gradle.api.tasks.testing.TestOutputListener;
TestReport:org.gradle.api.tasks.testing.TestReport;
TestResult:org.gradle.api.tasks.testing.TestResult;
TestTaskReports:org.gradle.api.tasks.testing.TestTaskReports;
JUnitOptions:org.gradle.api.tasks.testing.junit.JUnitOptions;
TestNGOptions:org.gradle.api.tasks.testing.testng.TestNGOptions;
PatternFilterable:org.gradle.api.tasks.util.PatternFilterable;
PatternSet:org.gradle.api.tasks.util.PatternSet;
Wrapper:org.gradle.api.tasks.wrapper.Wrapper;
Authentication:org.gradle.authentication.Authentication;
AwsImAuthentication:org.gradle.authentication.aws.AwsImAuthentication;
BasicAuthentication:org.gradle.authentication.http.BasicAuthentication;
DigestAuthentication:org.gradle.authentication.http.DigestAuthentication;
BuildInitPlugin:org.gradle.buildinit.plugins.BuildInitPlugin;
WrapperPlugin:org.gradle.buildinit.plugins.WrapperPlugin;
InitBuild:org.gradle.buildinit.tasks.InitBuild;
BuildCacheEntryReader:org.gradle.caching.BuildCacheEntryReader;
BuildCacheEntryWriter:org.gradle.caching.BuildCacheEntryWriter;
BuildCacheException:org.gradle.caching.BuildCacheException;
BuildCacheKey:org.gradle.caching.BuildCacheKey;
BuildCacheService:org.gradle.caching.BuildCacheService;
BuildCacheServiceFactory:org.gradle.caching.BuildCacheServiceFactory;
MapBasedBuildCacheService:org.gradle.caching.MapBasedBuildCacheService;
AbstractBuildCache:org.gradle.caching.configuration.AbstractBuildCache;
BuildCache:org.gradle.caching.configuration.BuildCache;
BuildCacheConfiguration:org.gradle.caching.configuration.BuildCacheConfiguration;
HttpBuildCache:org.gradle.caching.http.HttpBuildCache;
HttpBuildCacheCredentials:org.gradle.caching.http.HttpBuildCacheCredentials;
DirectoryBuildCache:org.gradle.caching.local.DirectoryBuildCache;
ParallelismConfiguration:org.gradle.concurrent.ParallelismConfiguration;
CoreJavadocOptions:org.gradle.external.javadoc.CoreJavadocOptions;
JavadocMemberLevel:org.gradle.external.javadoc.JavadocMemberLevel;
JavadocOfflineLink:org.gradle.external.javadoc.JavadocOfflineLink;
JavadocOptionFileOption:org.gradle.external.javadoc.JavadocOptionFileOption;
JavadocOutputLevel:org.gradle.external.javadoc.JavadocOutputLevel;
MinimalJavadocOptions:org.gradle.external.javadoc.MinimalJavadocOptions;
OptionLessJavadocOptionFileOption:org.gradle.external.javadoc.OptionLessJavadocOptionFileOption;
StandardJavadocDocletOptions:org.gradle.external.javadoc.StandardJavadocDocletOptions;
ConfigFile:org.gradle.ide.visualstudio.ConfigFile;
TextConfigFile:org.gradle.ide.visualstudio.TextConfigFile;
TextProvider:org.gradle.ide.visualstudio.TextProvider;
VisualStudioExtension:org.gradle.ide.visualstudio.VisualStudioExtension;
VisualStudioProject:org.gradle.ide.visualstudio.VisualStudioProject;
VisualStudioSolution:org.gradle.ide.visualstudio.VisualStudioSolution;
XmlConfigFile:org.gradle.ide.visualstudio.XmlConfigFile;
VisualStudioPlugin:org.gradle.ide.visualstudio.plugins.VisualStudioPlugin;
GenerateFiltersFileTask:org.gradle.ide.visualstudio.tasks.GenerateFiltersFileTask;
GenerateProjectFileTask:org.gradle.ide.visualstudio.tasks.GenerateProjectFileTask;
GenerateSolutionFileTask:org.gradle.ide.visualstudio.tasks.GenerateSolutionFileTask;
XcodeExtension:org.gradle.ide.xcode.XcodeExtension;
XcodeProject:org.gradle.ide.xcode.XcodeProject;
XcodePlugin:org.gradle.ide.xcode.plugins.XcodePlugin;
GenerateSchemeFileTask:org.gradle.ide.xcode.tasks.GenerateSchemeFileTask;
GenerateWorkspaceSettingsFileTask:org.gradle.ide.xcode.tasks.GenerateWorkspaceSettingsFileTask;
GenerateXcodeProjectFileTask:org.gradle.ide.xcode.tasks.GenerateXcodeProjectFileTask;
GenerateXcodeWorkspaceFileTask:org.gradle.ide.xcode.tasks.GenerateXcodeWorkspaceFileTask;
IvyDescriptorArtifact:org.gradle.ivy.IvyDescriptorArtifact;
IvyModule:org.gradle.ivy.IvyModule;
ClassDirectoryBinarySpec:org.gradle.jvm.ClassDirectoryBinarySpec;
JarBinarySpec:org.gradle.jvm.JarBinarySpec;
JvmApiSpec:org.gradle.jvm.JvmApiSpec;
JvmBinarySpec:org.gradle.jvm.JvmBinarySpec;
JvmByteCode:org.gradle.jvm.JvmByteCode;
JvmComponentSpec:org.gradle.jvm.JvmComponentSpec;
JvmLibrary:org.gradle.jvm.JvmLibrary;
JvmLibrarySpec:org.gradle.jvm.JvmLibrarySpec;
JvmResources:org.gradle.jvm.JvmResources;
JavaAppStartScriptGenerationDetails:org.gradle.jvm.application.scripts.JavaAppStartScriptGenerationDetails;
ScriptGenerator:org.gradle.jvm.application.scripts.ScriptGenerator;
TemplateBasedScriptGenerator:org.gradle.jvm.application.scripts.TemplateBasedScriptGenerator;
JavaPlatform:org.gradle.jvm.platform.JavaPlatform;
JUnitTestSuitePlugin:org.gradle.jvm.plugins.JUnitTestSuitePlugin;
JvmComponentPlugin:org.gradle.jvm.plugins.JvmComponentPlugin;
JvmTestSuiteBasePlugin:org.gradle.jvm.plugins.JvmTestSuiteBasePlugin;
ApiJar:org.gradle.jvm.tasks.api.ApiJar;
JUnitTestSuiteBinarySpec:org.gradle.jvm.test.JUnitTestSuiteBinarySpec;
JUnitTestSuiteSpec:org.gradle.jvm.test.JUnitTestSuiteSpec;
JvmTestSuiteBinarySpec:org.gradle.jvm.test.JvmTestSuiteBinarySpec;
JvmTestSuiteSpec:org.gradle.jvm.test.JvmTestSuiteSpec;
JavaToolChain:org.gradle.jvm.toolchain.JavaToolChain;
JavaToolChainRegistry:org.gradle.jvm.toolchain.JavaToolChainRegistry;
LocalJava:org.gradle.jvm.toolchain.LocalJava;
AssemblerSourceSet:org.gradle.language.assembler.AssemblerSourceSet;
AssemblerLangPlugin:org.gradle.language.assembler.plugins.AssemblerLangPlugin;
AssemblerPlugin:org.gradle.language.assembler.plugins.AssemblerPlugin;
Assemble:org.gradle.language.assembler.tasks.Assemble;
DependentSourceSet:org.gradle.language.base.DependentSourceSet;org.gradle.language.nativeplatform.DependentSourceSet;
FunctionalSourceSet:org.gradle.language.base.FunctionalSourceSet;
LanguageSourceSet:org.gradle.language.base.LanguageSourceSet;
ProjectSourceSet:org.gradle.language.base.ProjectSourceSet;
SourcesArtifact:org.gradle.language.base.artifact.SourcesArtifact;
CompilerVersion:org.gradle.language.base.compile.CompilerVersion;
ComponentModelBasePlugin:org.gradle.language.base.plugins.ComponentModelBasePlugin;
LanguageBasePlugin:org.gradle.language.base.plugins.LanguageBasePlugin;
LifecycleBasePlugin:org.gradle.language.base.plugins.LifecycleBasePlugin;
BaseLanguageSourceSet:org.gradle.language.base.sources.BaseLanguageSourceSet;
CSourceSet:org.gradle.language.c.CSourceSet;
CLangPlugin:org.gradle.language.c.plugins.CLangPlugin;
CPlugin:org.gradle.language.c.plugins.CPlugin;
CCompile:org.gradle.language.c.tasks.CCompile;
CPreCompiledHeaderCompile:org.gradle.language.c.tasks.CPreCompiledHeaderCompile;
CoffeeScriptSourceSet:org.gradle.language.coffeescript.CoffeeScriptSourceSet;
CppApplication:org.gradle.language.cpp.CppApplication;
CppBinary:org.gradle.language.cpp.CppBinary;
CppComponent:org.gradle.language.cpp.CppComponent;
CppExecutable:org.gradle.language.cpp.CppExecutable;
CppLibrary:org.gradle.language.cpp.CppLibrary;
CppSharedLibrary:org.gradle.language.cpp.CppSharedLibrary;
CppSourceSet:org.gradle.language.cpp.CppSourceSet;
CppBasePlugin:org.gradle.language.cpp.plugins.CppBasePlugin;
CppExecutablePlugin:org.gradle.language.cpp.plugins.CppExecutablePlugin;
CppLangPlugin:org.gradle.language.cpp.plugins.CppLangPlugin;
CppLibraryPlugin:org.gradle.language.cpp.plugins.CppLibraryPlugin;
CppPlugin:org.gradle.language.cpp.plugins.CppPlugin;
CppCompile:org.gradle.language.cpp.tasks.CppCompile;
CppPreCompiledHeaderCompile:org.gradle.language.cpp.tasks.CppPreCompiledHeaderCompile;
JavaSourceSet:org.gradle.language.java.JavaSourceSet;
JavadocArtifact:org.gradle.language.java.artifact.JavadocArtifact;
JavaLanguagePlugin:org.gradle.language.java.plugins.JavaLanguagePlugin;
PlatformJavaCompile:org.gradle.language.java.tasks.PlatformJavaCompile;
JavaScriptSourceSet:org.gradle.language.javascript.JavaScriptSourceSet;
JvmResourceSet:org.gradle.language.jvm.JvmResourceSet;
JvmResourcesPlugin:org.gradle.language.jvm.plugins.JvmResourcesPlugin;
ProcessResources:org.gradle.language.jvm.tasks.ProcessResources;
HeaderExportingSourceSet:org.gradle.language.nativeplatform.HeaderExportingSourceSet;
NativeResourceSet:org.gradle.language.nativeplatform.NativeResourceSet;
AbstractNativeCompileTask:org.gradle.language.nativeplatform.tasks.AbstractNativeCompileTask;
AbstractNativePCHCompileTask:org.gradle.language.nativeplatform.tasks.AbstractNativePCHCompileTask;
AbstractNativeSourceCompileTask:org.gradle.language.nativeplatform.tasks.AbstractNativeSourceCompileTask;
Depend:org.gradle.language.nativeplatform.tasks.Depend;
ObjectiveCSourceSet:org.gradle.language.objectivec.ObjectiveCSourceSet;
ObjectiveCLangPlugin:org.gradle.language.objectivec.plugins.ObjectiveCLangPlugin;
ObjectiveCPlugin:org.gradle.language.objectivec.plugins.ObjectiveCPlugin;
ObjectiveCCompile:org.gradle.language.objectivec.tasks.ObjectiveCCompile;
ObjectiveCPreCompiledHeaderCompile:org.gradle.language.objectivec.tasks.ObjectiveCPreCompiledHeaderCompile;
ObjectiveCppSourceSet:org.gradle.language.objectivecpp.ObjectiveCppSourceSet;
ObjectiveCppLangPlugin:org.gradle.language.objectivecpp.plugins.ObjectiveCppLangPlugin;
ObjectiveCppPlugin:org.gradle.language.objectivecpp.plugins.ObjectiveCppPlugin;
ObjectiveCppCompile:org.gradle.language.objectivecpp.tasks.ObjectiveCppCompile;
ObjectiveCppPreCompiledHeaderCompile:org.gradle.language.objectivecpp.tasks.ObjectiveCppPreCompiledHeaderCompile;
WindowsResourceSet:org.gradle.language.rc.WindowsResourceSet;
WindowsResourceScriptPlugin:org.gradle.language.rc.plugins.WindowsResourceScriptPlugin;
WindowsResourcesPlugin:org.gradle.language.rc.plugins.WindowsResourcesPlugin;
WindowsResourceCompile:org.gradle.language.rc.tasks.WindowsResourceCompile;
RoutesSourceSet:org.gradle.language.routes.RoutesSourceSet;
ScalaLanguageSourceSet:org.gradle.language.scala.ScalaLanguageSourceSet;
ScalaPlatform:org.gradle.language.scala.ScalaPlatform;
ScalaLanguagePlugin:org.gradle.language.scala.plugins.ScalaLanguagePlugin;
AbstractScalaCompile:org.gradle.language.scala.tasks.AbstractScalaCompile;
BaseScalaCompileOptions:org.gradle.language.scala.tasks.BaseScalaCompileOptions;
PlatformScalaCompile:org.gradle.language.scala.tasks.PlatformScalaCompile;
ScalaToolChain:org.gradle.language.scala.toolchain.ScalaToolChain;
SwiftApplication:org.gradle.language.swift.SwiftApplication;
SwiftBinary:org.gradle.language.swift.SwiftBinary;
SwiftComponent:org.gradle.language.swift.SwiftComponent;
SwiftExecutable:org.gradle.language.swift.SwiftExecutable;
SwiftLibrary:org.gradle.language.swift.SwiftLibrary;
SwiftSharedLibrary:org.gradle.language.swift.SwiftSharedLibrary;
SwiftBasePlugin:org.gradle.language.swift.plugins.SwiftBasePlugin;
SwiftExecutablePlugin:org.gradle.language.swift.plugins.SwiftExecutablePlugin;
SwiftLibraryPlugin:org.gradle.language.swift.plugins.SwiftLibraryPlugin;
SwiftCompile:org.gradle.language.swift.tasks.SwiftCompile;
UnexportMainSymbol:org.gradle.language.swift.tasks.UnexportMainSymbol;
TwirlImports:org.gradle.language.twirl.TwirlImports;
TwirlSourceSet:org.gradle.language.twirl.TwirlSourceSet;
TwirlTemplateFormat:org.gradle.language.twirl.TwirlTemplateFormat;
MavenModule:org.gradle.maven.MavenModule;
MavenPomArtifact:org.gradle.maven.MavenPomArtifact;
ConfigurationCycleException:org.gradle.model.ConfigurationCycleException;
Defaults:org.gradle.model.Defaults;
Each:org.gradle.model.Each;
Finalize:org.gradle.model.Finalize;
InvalidModelRuleDeclarationException:org.gradle.model.InvalidModelRuleDeclarationException;
InvalidModelRuleException:org.gradle.model.InvalidModelRuleException;
Managed:org.gradle.model.Managed;
Model:org.gradle.model.Model;
ModelElement:org.gradle.model.ModelElement;
ModelMap:org.gradle.model.ModelMap;
ModelRuleBindingException:org.gradle.model.ModelRuleBindingException;
ModelSet:org.gradle.model.ModelSet;
ModelViewClosedException:org.gradle.model.ModelViewClosedException;
Mutate:org.gradle.model.Mutate;
Path:org.gradle.model.Path;
ReadOnlyModelViewException:org.gradle.model.ReadOnlyModelViewException;
RuleInput:org.gradle.model.RuleInput;
RuleSource:org.gradle.model.RuleSource;
RuleTarget:org.gradle.model.RuleTarget;
Rules:org.gradle.model.Rules;
Unmanaged:org.gradle.model.Unmanaged;
Validate:org.gradle.model.Validate;
WriteOnlyModelViewException:org.gradle.model.WriteOnlyModelViewException;
BuildType:org.gradle.nativeplatform.BuildType;
BuildTypeContainer:org.gradle.nativeplatform.BuildTypeContainer;
Flavor:org.gradle.nativeplatform.Flavor;
FlavorContainer:org.gradle.nativeplatform.FlavorContainer;
NativeBinary:org.gradle.nativeplatform.NativeBinary;
NativeBinarySpec:org.gradle.nativeplatform.NativeBinarySpec;
NativeComponentExtension:org.gradle.nativeplatform.NativeComponentExtension;
NativeComponentSpec:org.gradle.nativeplatform.NativeComponentSpec;
NativeDependencySet:org.gradle.nativeplatform.NativeDependencySet;
NativeExecutable:org.gradle.nativeplatform.NativeExecutable;
NativeExecutableBinary:org.gradle.nativeplatform.NativeExecutableBinary;
NativeExecutableBinarySpec:org.gradle.nativeplatform.NativeExecutableBinarySpec;
NativeExecutableFileSpec:org.gradle.nativeplatform.NativeExecutableFileSpec;
NativeExecutableSpec:org.gradle.nativeplatform.NativeExecutableSpec;
NativeInstallationSpec:org.gradle.nativeplatform.NativeInstallationSpec;
NativeLibrary:org.gradle.nativeplatform.NativeLibrary;
NativeLibraryBinary:org.gradle.nativeplatform.NativeLibraryBinary;
NativeLibraryBinarySpec:org.gradle.nativeplatform.NativeLibraryBinarySpec;
NativeLibraryRequirement:org.gradle.nativeplatform.NativeLibraryRequirement;
NativeLibrarySpec:org.gradle.nativeplatform.NativeLibrarySpec;
ObjectFile:org.gradle.nativeplatform.ObjectFile;
PrebuiltLibraries:org.gradle.nativeplatform.PrebuiltLibraries;
PrebuiltLibrary:org.gradle.nativeplatform.PrebuiltLibrary;
PrebuiltSharedLibraryBinary:org.gradle.nativeplatform.PrebuiltSharedLibraryBinary;
PrebuiltStaticLibraryBinary:org.gradle.nativeplatform.PrebuiltStaticLibraryBinary;
PreprocessingTool:org.gradle.nativeplatform.PreprocessingTool;
Repositories:org.gradle.nativeplatform.Repositories;
SharedLibraryBinary:org.gradle.nativeplatform.SharedLibraryBinary;
SharedLibraryBinarySpec:org.gradle.nativeplatform.SharedLibraryBinarySpec;
StaticLibraryBinary:org.gradle.nativeplatform.StaticLibraryBinary;
StaticLibraryBinarySpec:org.gradle.nativeplatform.StaticLibraryBinarySpec;
TargetedNativeComponent:org.gradle.nativeplatform.TargetedNativeComponent;
Tool:org.gradle.nativeplatform.Tool;
Architecture:org.gradle.nativeplatform.platform.Architecture;
NativePlatform:org.gradle.nativeplatform.platform.NativePlatform;
OperatingSystem:org.gradle.nativeplatform.platform.OperatingSystem;
NativeComponentModelPlugin:org.gradle.nativeplatform.plugins.NativeComponentModelPlugin;
NativeComponentPlugin:org.gradle.nativeplatform.plugins.NativeComponentPlugin;
AbstractLinkTask:org.gradle.nativeplatform.tasks.AbstractLinkTask;
CreateStaticLibrary:org.gradle.nativeplatform.tasks.CreateStaticLibrary;
InstallExecutable:org.gradle.nativeplatform.tasks.InstallExecutable;
LinkExecutable:org.gradle.nativeplatform.tasks.LinkExecutable;
LinkMachOBundle:org.gradle.nativeplatform.tasks.LinkMachOBundle;
LinkSharedLibrary:org.gradle.nativeplatform.tasks.LinkSharedLibrary;
ObjectFilesToBinary:org.gradle.nativeplatform.tasks.ObjectFilesToBinary;
PrefixHeaderFileGenerateTask:org.gradle.nativeplatform.tasks.PrefixHeaderFileGenerateTask;
NativeTestSuiteBinarySpec:org.gradle.nativeplatform.test.NativeTestSuiteBinarySpec;
NativeTestSuiteSpec:org.gradle.nativeplatform.test.NativeTestSuiteSpec;
CppTestSuite:org.gradle.nativeplatform.test.cpp.CppTestSuite;
CppUnitTestPlugin:org.gradle.nativeplatform.test.cpp.plugins.CppUnitTestPlugin;
CUnitTestSuiteBinarySpec:org.gradle.nativeplatform.test.cunit.CUnitTestSuiteBinarySpec;
CUnitTestSuiteSpec:org.gradle.nativeplatform.test.cunit.CUnitTestSuiteSpec;
CUnitConventionPlugin:org.gradle.nativeplatform.test.cunit.plugins.CUnitConventionPlugin;
CUnitPlugin:org.gradle.nativeplatform.test.cunit.plugins.CUnitPlugin;
GenerateCUnitLauncher:org.gradle.nativeplatform.test.cunit.tasks.GenerateCUnitLauncher;
GoogleTestTestSuiteBinarySpec:org.gradle.nativeplatform.test.googletest.GoogleTestTestSuiteBinarySpec;
GoogleTestTestSuiteSpec:org.gradle.nativeplatform.test.googletest.GoogleTestTestSuiteSpec;
GoogleTestConventionPlugin:org.gradle.nativeplatform.test.googletest.plugins.GoogleTestConventionPlugin;
GoogleTestPlugin:org.gradle.nativeplatform.test.googletest.plugins.GoogleTestPlugin;
NativeBinariesTestPlugin:org.gradle.nativeplatform.test.plugins.NativeBinariesTestPlugin;
RunTestExecutable:org.gradle.nativeplatform.test.tasks.RunTestExecutable;
SwiftXCTestBinary:org.gradle.nativeplatform.test.xctest.SwiftXCTestBinary;
SwiftXCTestSuite:org.gradle.nativeplatform.test.xctest.SwiftXCTestSuite;
XCTestConventionPlugin:org.gradle.nativeplatform.test.xctest.plugins.XCTestConventionPlugin;
InstallXCTestBundle:org.gradle.nativeplatform.test.xctest.tasks.InstallXCTestBundle;
XcTest:org.gradle.nativeplatform.test.xctest.tasks.XcTest;
Clang:org.gradle.nativeplatform.toolchain.Clang;
CommandLineToolConfiguration:org.gradle.nativeplatform.toolchain.CommandLineToolConfiguration;
Gcc:org.gradle.nativeplatform.toolchain.Gcc;
GccCommandLineToolConfiguration:org.gradle.nativeplatform.toolchain.GccCommandLineToolConfiguration;
GccCompatibleToolChain:org.gradle.nativeplatform.toolchain.GccCompatibleToolChain;
GccPlatformToolChain:org.gradle.nativeplatform.toolchain.GccPlatformToolChain;
NativePlatformToolChain:org.gradle.nativeplatform.toolchain.NativePlatformToolChain;
NativeToolChain:org.gradle.nativeplatform.toolchain.NativeToolChain;
NativeToolChainRegistry:org.gradle.nativeplatform.toolchain.NativeToolChainRegistry;
Swiftc:org.gradle.nativeplatform.toolchain.Swiftc;
SwiftcPlatformToolChain:org.gradle.nativeplatform.toolchain.SwiftcPlatformToolChain;
VisualCpp:org.gradle.nativeplatform.toolchain.VisualCpp;
VisualCppPlatformToolChain:org.gradle.nativeplatform.toolchain.VisualCppPlatformToolChain;
ClangCompilerPlugin:org.gradle.nativeplatform.toolchain.plugins.ClangCompilerPlugin;
GccCompilerPlugin:org.gradle.nativeplatform.toolchain.plugins.GccCompilerPlugin;
MicrosoftVisualCppCompilerPlugin:org.gradle.nativeplatform.toolchain.plugins.MicrosoftVisualCppCompilerPlugin;
SwiftCompilerPlugin:org.gradle.nativeplatform.toolchain.plugins.SwiftCompilerPlugin;
InputNormalization:org.gradle.normalization.InputNormalization;
InputNormalizationHandler:org.gradle.normalization.InputNormalizationHandler;
RuntimeClasspathNormalization:org.gradle.normalization.RuntimeClasspathNormalization;
Application:org.gradle.platform.base.Application;
ApplicationBinarySpec:org.gradle.platform.base.ApplicationBinarySpec;
ApplicationSpec:org.gradle.platform.base.ApplicationSpec;
Binary:org.gradle.platform.base.Binary;
BinaryContainer:org.gradle.platform.base.BinaryContainer;
BinarySpec:org.gradle.platform.base.BinarySpec;
BinaryTasks:org.gradle.platform.base.BinaryTasks;
BinaryTasksCollection:org.gradle.platform.base.BinaryTasksCollection;
ComponentBinaries:org.gradle.platform.base.ComponentBinaries;
ComponentSpec:org.gradle.platform.base.ComponentSpec;
ComponentSpecContainer:org.gradle.platform.base.ComponentSpecContainer;
ComponentType:org.gradle.platform.base.ComponentType;
DependencySpec:org.gradle.platform.base.DependencySpec;
DependencySpecBuilder:org.gradle.platform.base.DependencySpecBuilder;
DependencySpecContainer:org.gradle.platform.base.DependencySpecContainer;
GeneralComponentSpec:org.gradle.platform.base.GeneralComponentSpec;
InvalidModelException:org.gradle.platform.base.InvalidModelException;
Library:org.gradle.platform.base.Library;
LibraryBinaryDependencySpec:org.gradle.platform.base.LibraryBinaryDependencySpec;
LibraryBinarySpec:org.gradle.platform.base.LibraryBinarySpec;
LibrarySpec:org.gradle.platform.base.LibrarySpec;
ModelInstantiationException:org.gradle.platform.base.ModelInstantiationException;
ModuleDependencySpec:org.gradle.platform.base.ModuleDependencySpec;
ModuleDependencySpecBuilder:org.gradle.platform.base.ModuleDependencySpecBuilder;
Platform:org.gradle.platform.base.Platform;
PlatformAwareComponentSpec:org.gradle.platform.base.PlatformAwareComponentSpec;
PlatformContainer:org.gradle.platform.base.PlatformContainer;
ProjectDependencySpec:org.gradle.platform.base.ProjectDependencySpec;
ProjectDependencySpecBuilder:org.gradle.platform.base.ProjectDependencySpecBuilder;
SourceComponentSpec:org.gradle.platform.base.SourceComponentSpec;
ToolChain:org.gradle.platform.base.ToolChain;
ToolChainRegistry:org.gradle.platform.base.ToolChainRegistry;
TransformationFileType:org.gradle.platform.base.TransformationFileType;
TypeBuilder:org.gradle.platform.base.TypeBuilder;
Variant:org.gradle.platform.base.Variant;
VariantComponent:org.gradle.platform.base.VariantComponent;
VariantComponentSpec:org.gradle.platform.base.VariantComponentSpec;
BaseBinarySpec:org.gradle.platform.base.binary.BaseBinarySpec;
BaseComponentSpec:org.gradle.platform.base.component.BaseComponentSpec;
BinaryBasePlugin:org.gradle.platform.base.plugins.BinaryBasePlugin;
ComponentBasePlugin:org.gradle.platform.base.plugins.ComponentBasePlugin;
JvmClasses:org.gradle.play.JvmClasses;
PlayApplicationBinarySpec:org.gradle.play.PlayApplicationBinarySpec;
PlayApplicationSpec:org.gradle.play.PlayApplicationSpec;
PlayPlatformAwareComponentSpec:org.gradle.play.PlayPlatformAwareComponentSpec;
PublicAssets:org.gradle.play.PublicAssets;
PlayDistribution:org.gradle.play.distribution.PlayDistribution;
PlayDistributionContainer:org.gradle.play.distribution.PlayDistributionContainer;
PlayPlatform:org.gradle.play.platform.PlayPlatform;
PlayApplicationPlugin:org.gradle.play.plugins.PlayApplicationPlugin;
PlayCoffeeScriptPlugin:org.gradle.play.plugins.PlayCoffeeScriptPlugin;
PlayDistributionPlugin:org.gradle.play.plugins.PlayDistributionPlugin;
PlayJavaScriptPlugin:org.gradle.play.plugins.PlayJavaScriptPlugin;
PlayPlugin:org.gradle.play.plugins.PlayPlugin;
PlayPluginConfigurations:org.gradle.play.plugins.PlayPluginConfigurations;
PlayRoutesPlugin:org.gradle.play.plugins.PlayRoutesPlugin;
PlayTestPlugin:org.gradle.play.plugins.PlayTestPlugin;
PlayTwirlPlugin:org.gradle.play.plugins.PlayTwirlPlugin;
PlayIdePlugin:org.gradle.play.plugins.ide.PlayIdePlugin;
JavaScriptMinify:org.gradle.play.tasks.JavaScriptMinify;
PlayCoffeeScriptCompile:org.gradle.play.tasks.PlayCoffeeScriptCompile;
PlayRun:org.gradle.play.tasks.PlayRun;
RoutesCompile:org.gradle.play.tasks.RoutesCompile;
TwirlCompile:org.gradle.play.tasks.TwirlCompile;
PlayToolChain:org.gradle.play.toolchain.PlayToolChain;
GradlePluginDevelopmentExtension:org.gradle.plugin.devel.GradlePluginDevelopmentExtension;
PluginDeclaration:org.gradle.plugin.devel.PluginDeclaration;
IvyPluginPublishingRules:org.gradle.plugin.devel.plugins.IvyPluginPublishingRules;
JavaGradlePluginPlugin:org.gradle.plugin.devel.plugins.JavaGradlePluginPlugin;
MavenPluginPublishingRules:org.gradle.plugin.devel.plugins.MavenPluginPublishingRules;
GeneratePluginDescriptors:org.gradle.plugin.devel.tasks.GeneratePluginDescriptors;
PluginUnderTestMetadata:org.gradle.plugin.devel.tasks.PluginUnderTestMetadata;
ValidateTaskProperties:org.gradle.plugin.devel.tasks.ValidateTaskProperties;
PluginManagementSpec:org.gradle.plugin.management.PluginManagementSpec;
PluginRequest:org.gradle.plugin.management.PluginRequest;
PluginResolutionStrategy:org.gradle.plugin.management.PluginResolutionStrategy;
PluginResolveDetails:org.gradle.plugin.management.PluginResolveDetails;
PluginDependenciesSpec:org.gradle.plugin.use.PluginDependenciesSpec;
PluginDependencySpec:org.gradle.plugin.use.PluginDependencySpec;
PluginId:org.gradle.plugin.use.PluginId;
Ear:org.gradle.plugins.ear.Ear;
EarPlugin:org.gradle.plugins.ear.EarPlugin;
EarPluginConvention:org.gradle.plugins.ear.EarPluginConvention;
DeploymentDescriptor:org.gradle.plugins.ear.descriptor.DeploymentDescriptor;
EarModule:org.gradle.plugins.ear.descriptor.EarModule;
EarSecurityRole:org.gradle.plugins.ear.descriptor.EarSecurityRole;
EarWebModule:org.gradle.plugins.ear.descriptor.EarWebModule;
FileContentMerger:org.gradle.plugins.ide.api.FileContentMerger;
GeneratorTask:org.gradle.plugins.ide.api.GeneratorTask;
PropertiesFileContentMerger:org.gradle.plugins.ide.api.PropertiesFileContentMerger;
PropertiesGeneratorTask:org.gradle.plugins.ide.api.PropertiesGeneratorTask;
PropertyListGeneratorTask:org.gradle.plugins.ide.api.PropertyListGeneratorTask;
XmlFileContentMerger:org.gradle.plugins.ide.api.XmlFileContentMerger;
XmlGeneratorTask:org.gradle.plugins.ide.api.XmlGeneratorTask;
EclipsePlugin:org.gradle.plugins.ide.eclipse.EclipsePlugin;
EclipseWtpPlugin:org.gradle.plugins.ide.eclipse.EclipseWtpPlugin;
GenerateEclipseClasspath:org.gradle.plugins.ide.eclipse.GenerateEclipseClasspath;
GenerateEclipseJdt:org.gradle.plugins.ide.eclipse.GenerateEclipseJdt;
GenerateEclipseProject:org.gradle.plugins.ide.eclipse.GenerateEclipseProject;
GenerateEclipseWtpComponent:org.gradle.plugins.ide.eclipse.GenerateEclipseWtpComponent;
GenerateEclipseWtpFacet:org.gradle.plugins.ide.eclipse.GenerateEclipseWtpFacet;
GenerateIdeaModule:org.gradle.plugins.ide.idea.GenerateIdeaModule;
GenerateIdeaProject:org.gradle.plugins.ide.idea.GenerateIdeaProject;
GenerateIdeaWorkspace:org.gradle.plugins.ide.idea.GenerateIdeaWorkspace;
IdeaPlugin:org.gradle.plugins.ide.idea.IdeaPlugin;
JavaScriptBasePlugin:org.gradle.plugins.javascript.base.JavaScriptBasePlugin;
JavaScriptExtension:org.gradle.plugins.javascript.base.JavaScriptExtension;
JavaScriptRepositoriesExtension:org.gradle.plugins.javascript.base.JavaScriptRepositoriesExtension;
SourceTransformationException:org.gradle.plugins.javascript.base.SourceTransformationException;
CoffeeScriptBasePlugin:org.gradle.plugins.javascript.coffeescript.CoffeeScriptBasePlugin;
CoffeeScriptCompile:org.gradle.plugins.javascript.coffeescript.CoffeeScriptCompile;
CoffeeScriptCompileOptions:org.gradle.plugins.javascript.coffeescript.CoffeeScriptCompileOptions;
CoffeeScriptCompileSpec:org.gradle.plugins.javascript.coffeescript.CoffeeScriptCompileSpec;
CoffeeScriptCompiler:org.gradle.plugins.javascript.coffeescript.CoffeeScriptCompiler;
CoffeeScriptExtension:org.gradle.plugins.javascript.coffeescript.CoffeeScriptExtension;
EnvJsExtension:org.gradle.plugins.javascript.envjs.EnvJsExtension;
EnvJsPlugin:org.gradle.plugins.javascript.envjs.EnvJsPlugin;
BrowserEvaluate:org.gradle.plugins.javascript.envjs.browser.BrowserEvaluate;
BrowserEvaluator:org.gradle.plugins.javascript.envjs.browser.BrowserEvaluator;
HttpFileServer:org.gradle.plugins.javascript.envjs.http.HttpFileServer;
HttpFileServerFactory:org.gradle.plugins.javascript.envjs.http.HttpFileServerFactory;
SimpleHttpFileServer:org.gradle.plugins.javascript.envjs.http.simple.SimpleHttpFileServer;
SimpleHttpFileServerFactory:org.gradle.plugins.javascript.envjs.http.simple.SimpleHttpFileServerFactory;
JsHint:org.gradle.plugins.javascript.jshint.JsHint;
JsHintExtension:org.gradle.plugins.javascript.jshint.JsHintExtension;
JsHintPlugin:org.gradle.plugins.javascript.jshint.JsHintPlugin;
RhinoExtension:org.gradle.plugins.javascript.rhino.RhinoExtension;
RhinoPlugin:org.gradle.plugins.javascript.rhino.RhinoPlugin;
RhinoShellExec:org.gradle.plugins.javascript.rhino.RhinoShellExec;
Sign:org.gradle.plugins.signing.Sign;
SignOperation:org.gradle.plugins.signing.SignOperation;
Signature:org.gradle.plugins.signing.Signature;
SignatureSpec:org.gradle.plugins.signing.SignatureSpec;
SigningExtension:org.gradle.plugins.signing.SigningExtension;
SigningPlugin:org.gradle.plugins.signing.SigningPlugin;
Signatory:org.gradle.plugins.signing.signatory.Signatory;
SignatoryProvider:org.gradle.plugins.signing.signatory.SignatoryProvider;
SignatorySupport:org.gradle.plugins.signing.signatory.SignatorySupport;
Dsl:org.gradle.plugins.signing.signatory.pgp.Dsl;
PgpKeyId:org.gradle.plugins.signing.signatory.pgp.PgpKeyId;
PgpSignatory:org.gradle.plugins.signing.signatory.pgp.PgpSignatory;
PgpSignatoryFactory:org.gradle.plugins.signing.signatory.pgp.PgpSignatoryFactory;
PgpSignatoryProvider:org.gradle.plugins.signing.signatory.pgp.PgpSignatoryProvider;
AbstractSignatureType:org.gradle.plugins.signing.type.AbstractSignatureType;
AbstractSignatureTypeProvider:org.gradle.plugins.signing.type.AbstractSignatureTypeProvider;
BinarySignatureType:org.gradle.plugins.signing.type.BinarySignatureType;
DefaultSignatureTypeProvider:org.gradle.plugins.signing.type.DefaultSignatureTypeProvider;
SignatureType:org.gradle.plugins.signing.type.SignatureType;
SignatureTypeProvider:org.gradle.plugins.signing.type.SignatureTypeProvider;
ArmoredSignatureType:org.gradle.plugins.signing.type.pgp.ArmoredSignatureType;
BaseExecSpec:org.gradle.process.BaseExecSpec;
ExecResult:org.gradle.process.ExecResult;
ExecSpec:org.gradle.process.ExecSpec;
JavaExecSpec:org.gradle.process.JavaExecSpec;
JavaForkOptions:org.gradle.process.JavaForkOptions;
ProcessForkOptions:org.gradle.process.ProcessForkOptions;
TestSuiteBinarySpec:org.gradle.testing.base.TestSuiteBinarySpec;
TestSuiteContainer:org.gradle.testing.base.TestSuiteContainer;
TestSuiteSpec:org.gradle.testing.base.TestSuiteSpec;
TestSuiteTaskCollection:org.gradle.testing.base.TestSuiteTaskCollection;
TestingBasePlugin:org.gradle.testing.base.plugins.TestingBasePlugin;
TestingModelBasePlugin:org.gradle.testing.base.plugins.TestingModelBasePlugin;
JacocoPlugin:org.gradle.testing.jacoco.plugins.JacocoPlugin;
JacocoPluginExtension:org.gradle.testing.jacoco.plugins.JacocoPluginExtension;
JacocoTaskExtension:org.gradle.testing.jacoco.plugins.JacocoTaskExtension;
JacocoBase:org.gradle.testing.jacoco.tasks.JacocoBase;
JacocoCoverageVerification:org.gradle.testing.jacoco.tasks.JacocoCoverageVerification;
JacocoMerge:org.gradle.testing.jacoco.tasks.JacocoMerge;
JacocoReport:org.gradle.testing.jacoco.tasks.JacocoReport;
JacocoReportBase:org.gradle.testing.jacoco.tasks.JacocoReportBase;
JacocoReportsContainer:org.gradle.testing.jacoco.tasks.JacocoReportsContainer;
JacocoLimit:org.gradle.testing.jacoco.tasks.rules.JacocoLimit;
JacocoViolationRule:org.gradle.testing.jacoco.tasks.rules.JacocoViolationRule;
JacocoViolationRulesContainer:org.gradle.testing.jacoco.tasks.rules.JacocoViolationRulesContainer;
BuildTask:org.gradle.testkit.runner.BuildTask;
GradleRunner:org.gradle.testkit.runner.GradleRunner;
InvalidPluginMetadataException:org.gradle.testkit.runner.InvalidPluginMetadataException;
InvalidRunnerConfigurationException:org.gradle.testkit.runner.InvalidRunnerConfigurationException;
TaskOutcome:org.gradle.testkit.runner.TaskOutcome;
UnexpectedBuildFailure:org.gradle.testkit.runner.UnexpectedBuildFailure;
UnexpectedBuildResultException:org.gradle.testkit.runner.UnexpectedBuildResultException;
UnexpectedBuildSuccess:org.gradle.testkit.runner.UnexpectedBuildSuccess;
UnsupportedFeatureException:org.gradle.testkit.runner.UnsupportedFeatureException;
SourceControl:org.gradle.vcs.SourceControl;
VcsMapping:org.gradle.vcs.VcsMapping;
VcsMappings:org.gradle.vcs.VcsMappings;
VersionControlSpec:org.gradle.vcs.VersionControlSpec;
VersionControlSystem:org.gradle.vcs.VersionControlSystem;
VersionRef:org.gradle.vcs.VersionRef;
GitVersionControlSpec:org.gradle.vcs.git.GitVersionControlSpec;
ForkMode:org.gradle.workers.ForkMode;
IsolationMode:org.gradle.workers.IsolationMode;
WorkerConfiguration:org.gradle.workers.WorkerConfiguration;
WorkerExecutionException:org.gradle.workers.WorkerExecutionException;
WorkerExecutor:org.gradle.workers.WorkerExecutor;
")))
                (invoke "ant" "jar")
                #t)))))))))

;; Gradle doesn't provide a gradle binary or script, so we provide it instead.
;; Gradle expects that all its modules and dependency jars are located in the
;; same directory. That directory must be called "lib".
;; In this package, we create a script that puts gradle-launcher in the
;; classpath (that's ok because gradle-launcher has a Class-Path declaration in
;; its MANIFEST.MF). This runs the main entry point of gradle that will look
;; for its requirements in that directory. I don't really understand how this
;; is done, since the classpath contains only jar files and not directories,
;; and it seems to look for gradle-installation-beacon, but it's definitely not
;; in the classpath...
;;
;; Currently, gradle can find its modules and start running, but it will fail
;; at reading the api-mapping.txt file from gradle-docs.
(define-public gradle
  (package
    (inherit gradle-base-services)
    (name "gradle")
    (build-system trivial-build-system)
    (arguments
     `(#:modules ((guix build utils))
       #:builder (begin
                   (use-modules (guix build utils))
                   (let* ((output (assoc-ref %outputs "out"))
                          (bindir (string-append output "/bin"))
                          (libdir (string-append output "/lib"))
                          (plugdir (string-append libdir "/plugins"))
                          (filename (string-append bindir "/gradle"))
                          (plugins
                           '("gradle-workers"
                             "gradle-version-control"
                             "gradle-testing-jvm"
                             "gradle-testing-base"
                             "gradle-resources-http"
                             "gradle-reporting"
                             "gradle-plugins"
                             "gradle-platform-native"
                             "gradle-platform-jvm"
                             "gradle-platform-base"
                             "gradle-plugin-use"
                             "gradle-language-jvm"
                             "gradle-language-java"
                             "gradle-language-groovy"
                             "gradle-diagnostics"
                             "gradle-dependency-management"
                             "java-apache-ivy"
                             "java-bouncycastle"
                             "java-bsh"
                             "java-commons-codec"
                             "java-gson"
                             "java-hamcrest-all"
                             "java-httpcomponents-httpclient"
                             "java-httpcomponents-httpcore"
                             "java-jatl"
                             "java-jcifs"
                             "java-jcommander"
                             "java-jgit"
                             "java-jsch"
                             "java-snakeyaml"
                             "java-testng"
                             "java-junit"
                             "java-nekohtml"
                             "java-xerces"))
                          ;; java-asm-6 and java-jansi are already present in groovy.
                          (dependencies 
                           '("gradle-wrapper"
                             "gradle-tooling-api"
                             "gradle-runtime-api-info"
                             "gradle-resources"
                             "gradle-process-services"
                             "gradle-persistent-cache"
                             "gradle-native"
                             "gradle-model-groovy"
                             "gradle-model-core"
                             "gradle-messaging"
                             "gradle-logging"
                             "gradle-launcher"
                             "gradle-jvm-services"
                             "gradle-installation-beacon"
                             "gradle-docs"
                             "gradle-core-api"
                             "gradle-core"
                             "gradle-cli"
                             "gradle-build-option"
                             "gradle-build-cache"
                             "gradle-base-services-groovy"
                             "gradle-base-services"
                             "groovy"
                             "java-commons-compress"
                             "java-commons-collections"
                             "java-commons-io"
                             "java-commons-lang"
                             "java-guava-for-gradle"
                             "java-jansi-native"
                             "java-javax-inject"
                             "java-jaxp"
                             "java-jcip-annotations"
                             "java-jsr305"
                             "java-jul-to-slf4j"
                             "java-kryo"
                             "java-minlog"
                             "java-native-platform"
                             "java-objenesis"
                             "java-reflectasm"
                             "java-slf4j-api"
                             "ant")))
                     (mkdir-p bindir)
                     (mkdir-p plugdir)
                     (with-output-to-file filename
                       (lambda _
                         (format #t "#!~a\n
export GRADLE_HOME=~a\n
~a -cp ~a -Dorg.gradle.appname=gradle org.gradle.launcher.GradleMain $@"
                                 (string-append (assoc-ref %build-inputs "bash")
                                                "/bin/bash")
                                 output
                                 (string-append (assoc-ref %build-inputs "icedtea-8")
                                                "/bin/java")
                                 (string-append libdir "/gradle-launcher-4.8.jar"))))
                     (chmod filename #o755)
                     ;; Create a symlink for every dependency listed above.
                     (for-each
                       (lambda (lib)
                         (symlink lib (string-append libdir "/" (basename lib))))
                       (apply append
                         (map
                           (lambda (lib)
                             (find-files (assoc-ref %build-inputs lib)
                                         ".*.jar"))
                           dependencies)))
                     (for-each
                       (lambda (lib)
                         (symlink lib (string-append plugdir "/" (basename lib))))
                       (apply append
                         (map
                           (lambda (lib)
                             (find-files (assoc-ref %build-inputs lib)
                                         ".*.jar"))
                           plugins)))
                     ;; Using a symlink for gradle-launcher doesn't seem to work.
                     (delete-file (string-append libdir "/gradle-launcher-4.8.jar"))
                     (copy-file (string-append (assoc-ref %build-inputs "gradle-launcher")
                                               "/share/java/gradle-launcher-4.8.jar")
                                (string-append libdir
                                               "/gradle-launcher-4.8.jar"))))))
    (inputs
     `(("gradle-wrapper"               ,gradle-wrapper)
       ("gradle-workers"               ,gradle-workers)
       ("gradle-version-control"       ,gradle-version-control)
       ("gradle-tooling-api"           ,gradle-tooling-api)
       ("gradle-testing-jvm"           ,gradle-testing-jvm)
       ("gradle-testing-base"          ,gradle-testing-base)
       ("gradle-runtime-api-info"      ,gradle-runtime-api-info)
       ("gradle-resources-http"        ,gradle-resources-http)
       ("gradle-resources"             ,gradle-resources)
       ("gradle-reporting"             ,gradle-reporting)
       ("gradle-process-services"      ,gradle-process-services)
       ("gradle-plugin-use"            ,gradle-plugin-use)
       ("gradle-plugins"               ,gradle-plugins)
       ("gradle-platform-native"       ,gradle-platform-native)
       ("gradle-platform-jvm"          ,gradle-platform-jvm)
       ("gradle-platform-base"         ,gradle-platform-base)
       ("gradle-persistent-cache"      ,gradle-persistent-cache)
       ("gradle-native"                ,gradle-native)
       ("gradle-model-groovy"          ,gradle-model-groovy)
       ("gradle-model-core"            ,gradle-model-core)
       ("gradle-messaging"             ,gradle-messaging)
       ("gradle-logging"               ,gradle-logging)
       ("gradle-launcher"              ,gradle-launcher)
       ("gradle-language-jvm"          ,gradle-language-jvm)
       ("gradle-language-java"         ,gradle-language-java)
       ("gradle-language-groovy"       ,gradle-language-groovy)
       ("gradle-jvm-services"          ,gradle-jvm-services)
       ("gradle-internal-testing"      ,gradle-internal-testing)
       ("gradle-installation-beacon"   ,gradle-installation-beacon)
       ("gradle-docs"                  ,gradle-docs)
       ("gradle-diagnostics"           ,gradle-diagnostics)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-core-api"              ,gradle-core-api)
       ("gradle-core"                  ,gradle-core)
       ("gradle-cli"                   ,gradle-cli)
       ("gradle-build-option"          ,gradle-build-option)
       ("gradle-build-cache"           ,gradle-build-cache)
       ("gradle-base-services-groovy"  ,gradle-base-services-groovy)
       ("gradle-base-services"         ,gradle-base-services)
       ("groovy" ,groovy)
       ("icedtea-8" ,icedtea-8)
       ("java-asm-6" ,java-asm-6)
       ("java-apache-ivy" ,java-apache-ivy)
       ("java-bouncycastle" ,java-bouncycastle)
       ("java-bsh" ,java-bsh)
       ("java-commons-codec" ,java-commons-codec)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-gson" ,java-gson)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-hamcrest-all" ,java-hamcrest-all)
       ("java-httpcomponents-httpclient" ,java-httpcomponents-httpclient)
       ("java-httpcomponents-httpcore" ,java-httpcomponents-httpcore)
       ("java-jansi" ,java-jansi)
       ("java-jansi-native" ,java-jansi-native)
       ("java-jatl" ,java-jatl)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jaxp" ,java-jaxp)
       ("java-jcifs" ,java-jcifs)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jcommander" ,java-jcommander)
       ("java-jgit" ,java-jgit)
       ("java-jsch" ,java-jsch)
       ("java-jsr305" ,java-jsr305)
       ("java-jul-to-slf4j" ,java-jul-to-slf4j)
       ("java-junit" ,java-junit)
       ("java-kryo" ,java-kryo)
       ("java-minlog" ,java-minlog)
       ("java-native-platform" ,java-native-platform)
       ("java-nekohtml" ,java-nekohtml)
       ("java-objenesis" ,java-objenesis)
       ("java-reflectasm" ,java-reflectasm)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-snakeyaml" ,java-snakeyaml)
       ("java-testng" ,java-testng)
       ("java-xerces" ,java-xerces)
       ("ant" ,ant)
       ("bash" ,bash)))
    (native-inputs '())))
