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
  #:use-module (gnu packages java)
  #:use-module (more packages groovy)
  #:use-module (more packages java)
  #:use-module (more packages maven))

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
  (package
    (name (string-append "gradle-" subproject))
    (version "4.4.0")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/gradle/gradle/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append "gradle-" version ".tar.gz"))
              (sha256
               (base32
                "026232dy578nl8gzj1nfc9r25p8alcwdbwb9b8s3bw4krxmy6saz"))
              (patches
                (search-patches
                  "gradle-match-files-witouht-version-number.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name (string-append "gradle-" ,subproject "-4.4.jar")
       #:source-dir (string-append "subprojects/" ,subproject "/src/main/java")
       #:jdk ,icedtea-8
       #:tests? #f;; Ignore tests for now
       #:test-dir (string-append "subprojects/" ,subproject "/src/test")
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'add-implementation-info
           (lambda _
             (substitute* "build.xml"
               (("message=\"")
                (string-append "message=\"Implementation-Title: Gradle"
                               "${line.separator}"
                               "Implementation-Version: 4.4"
                               "${line.separator}")))
             #t))
         (add-before 'build 'add-properties
           (lambda* (#:key inputs #:allow-other-keys)
             (mkdir-p "build/classes")
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
             (let ((resources (string-append "subprojects/" ,subproject
                                             "/src/main/resources")))
             (if (file-exists? resources)
               (copy-recursively resources "build/classes"))))))))
    (inputs '())
    (native-inputs '())
    (home-page "")
    (synopsis "Build system")
    (description "Build system")
    (license license:asl2.0)))

(define (gradle-groovy-subproject subproject projects runtime)
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
classname=\"org.codehaus.groovy.ant.Groovyc\" />"))))))))
      (native-inputs
       `(("groovy" ,groovy)
         ,@(package-inputs groovy))))))

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
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-templates" ,groovy-templates)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-japicmp" ,java-japicmp)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jboss-javassist" ,java-jboss-javassist)))
    (home-page "")
    (synopsis "")
    (description "")
    (license license:asl2.0)))

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
               (("groovyjarjarantlr") "antlr"))))
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
       ("groovy-xml" ,groovy-xml)
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
       ("groovy-xml" ,groovy-xml)
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
                     (format #t "baseVersion=4.4
commitId=cf7821a6f79f8e2a598df21780e3ff7ce8db2b82
buildTimestamp=19710101000000+0000
buildTimestampIso=1971-01-01 00\\:00\\:00 UTC
versionNumber=4.4
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
                                '("groovy-bootstrap")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("groovy-bootstrap" ,groovy-bootstrap)
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
  (package
    (inherit (gradle-subproject "cli" '() '()))))

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
               '("groovy-bootstrap" "java-slf4j-api" "java-guava-for-gradle"
                 "java-jcip-annotations" "java-commons-lang" "java-asm-6")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("groovy-bootstrap" ,groovy-bootstrap)
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
       ("groovy-ant" ,groovy-ant)
       ("groovy-bootstrap" ,groovy-bootstrap)
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
               '("groovy-bootstrap" "java-jcip-annotations" "java-guava-for-gradle")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-model-core" ,gradle-model-core)
       ("groovy-bootstrap" ,groovy-bootstrap)
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

(define-public gradle-core
  (let ((base (gradle-subproject
                "core"
                '("gradle-docs" "gradle-model-groovy" "gradle-base-services"
                  "gradle-base-services-groovy" "gradle-messaging"
                  "gradle-logging" "gradle-resources" "gradle-cli"
                  "gradle-build-option" "gradle-native" "gradle-persistent-cache"
                  "gradle-build-cache" "gradle-core-api" "gradle-process-services"
                  "gradle-jvm-services" "gradle-model-core")
                '("groovy-ant" "groovy-bootstrap" "groovy-json" "groovy-templates"
                  "groovy-xml" "ant" "java-guava-for-gradle"
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
                '("gradle-base-services" "gradle-core-api" "gradle-core")
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
         ("groovy-bootstrap" ,groovy-bootstrap)
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
                                     "gradle-base-services-4.4.jar "
                                     "gradle-core-api-4.4.jar "
                                     "gradle-core-4.4.jar"
                                     "${line.separator}")))
                   #t))))))))))

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
               '("groovy-bootstrap" "java-jatl" "java-commons-lang")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-logging" ,gradle-logging)
       ("gradle-model-core" ,gradle-model-core)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jatl" ,java-jatl)
       ("java-javax-inject" ,java-javax-inject)))))

(define-public gradle-resources-http
  (package
    (inherit (gradle-subproject
               "resources-http"
               '("gradle-resources" "gradle-base-services" "gradle-core")
               ;; TODO: jcl-over-slf4j
               '("java-httpcomponents-client" "java-httpcomponents-core"
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
       ("java-httpcomponents-client" ,java-httpcomponents-client)
       ("java-httpcomponents-core" ,java-httpcomponents-core)
       ("java-jaxp" ,java-jaxp)
       ("java-jcifs" ,java-jcifs)
       ("java-jsr305" ,java-jsr305)
       ("java-nekohtml" ,java-nekohtml)
       ("java-slf4j-api" ,java-slf4j-api)
       ("java-xerces" ,java-xerces)))))

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
                   "java-jcip-annotations" "java-bouncycastle-bcprov"
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
         ("groovy-bootstrap" ,groovy-bootstrap)
         ("java-apache-ivy" ,java-apache-ivy)
         ("java-asm-6" ,java-asm-6)
         ("java-bouncycastle-bcprov" ,java-bouncycastle-bcprov)
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
               '("java-commons-collections" "groovy-bootstrap")))
    (inputs
     `(("gradle-base-services" ,gradle-base-services)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-core" ,gradle-core)
       ("gradle-core-api" ,gradle-core-api)
       ("gradle-dependency-management" ,gradle-dependency-management)
       ("gradle-model-core" ,gradle-model-core)
       ("gradle-workers" ,gradle-workers)
       ("groovy-bootstrap" ,groovy-bootstrap)
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
         ("groovy-bootstrap" ,groovy-bootstrap)
         ("java-asm-6" ,java-asm-6)
         ("java-bouncycastle-bcprov" ,java-bouncycastle-bcprov)
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
               '("groovy-bootstrap" "groovy-json" "java-guava-for-gradle" "java-jatl"
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
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-json" ,groovy-json)
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
       ("groovy-bootstrap" ,groovy-bootstrap)
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
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-xml" ,groovy-xml)
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
               '("groovy-bootstrap" "java-commons-io")))
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
       ("groovy-bootstrap" ,groovy-bootstrap)
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
       ("groovy-bootstrap" ,groovy-bootstrap)
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
       ("groovy-bootstrap" ,groovy-bootstrap)
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
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-lang" ,java-commons-lang)
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
       ("groovy-bootstrap" ,groovy-bootstrap)
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
               '("groovy-bootstrap" "ant" "java-asm-6" "java-commons-io"
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
       ("groovy-ant" ,groovy-ant)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-templates" ,groovy-templates)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-cli" ,java-commons-cli)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jsr305" ,java-jsr305)
       ("java-slf4j-api" ,java-slf4j-api)))))

;; This package doesn't work. I need to understand how api-mapping.txt and
;; default-imports.txt are generated. Currently they are generated by a custom
;; task that is run by gradle, but we don't have enough of gradle to run that.
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
                    (format #t "")))
                (with-output-to-file "build/classes/api-mapping.txt"
                  (lambda _
                    (format #t "")))
                (zero? (system* "ant" "jar")))))))))))

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
                          (filename (string-append bindir "/gradle"))
                          (dependencies 
                           '("gradle-wrapper"
                             "gradle-tooling-api"
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
                             "gradle-internal-testing"
                             "gradle-installation-beacon"
                             "gradle-docs"
                             "gradle-core-api"
                             "gradle-core"
                             "gradle-cli"
                             "gradle-build-option"
                             "gradle-build-cache"
                             "gradle-base-services-groovy"
                             "gradle-base-services"
                             "groovy-ant"
                             "groovy-bootstrap"
                             "groovy-json"
                             "groovy-templates"
                             "groovy-xml"
                             "java-asm-6"
                             "java-commons-compress"
                             "java-commons-collections"
                             "java-commons-io"
                             "java-commons-lang"
                             "java-guava-for-gradle"
                             "java-jansi"
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
                     (mkdir-p (string-append libdir "/plugins"))
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
                                 (string-append libdir "/gradle-launcher-4.4.jar"))))
                     (chmod filename #o755)
                     (for-each
                       (lambda (lib)
                         (symlink lib (string-append libdir "/" (basename lib))))
                       (apply append
                         (map
                           (lambda (lib)
                             (find-files (assoc-ref %build-inputs lib)
                                         ".*.jar"))
                           dependencies)))
                     (delete-file (string-append libdir "/gradle-launcher-4.4.jar"))
                     (copy-file (string-append (assoc-ref %build-inputs "gradle-launcher")
                                               "/share/java/gradle-launcher-4.4.jar")
                                (string-append libdir
                                               "/gradle-launcher-4.4.jar"))))))
    (inputs
     `(("gradle-wrapper"              ,gradle-wrapper)
       ("gradle-tooling-api"          ,gradle-tooling-api)
       ("gradle-resources"            ,gradle-resources)
       ("gradle-process-services"     ,gradle-process-services)
       ("gradle-persistent-cache"     ,gradle-persistent-cache)
       ("gradle-native"               ,gradle-native)
       ("gradle-model-groovy"         ,gradle-model-groovy)
       ("gradle-model-core"           ,gradle-model-core)
       ("gradle-messaging"            ,gradle-messaging)
       ("gradle-logging"              ,gradle-logging)
       ("gradle-launcher"             ,gradle-launcher)
       ("gradle-jvm-services"         ,gradle-jvm-services)
       ("gradle-internal-testing"     ,gradle-internal-testing)
       ("gradle-installation-beacon"  ,gradle-installation-beacon)
       ("gradle-docs"                 ,gradle-docs)
       ("gradle-core-api"             ,gradle-core-api)
       ("gradle-core"                 ,gradle-core)
       ("gradle-cli"                  ,gradle-cli)
       ("gradle-build-option"         ,gradle-build-option)
       ("gradle-build-cache"          ,gradle-build-cache)
       ("gradle-base-services-groovy" ,gradle-base-services-groovy)
       ("gradle-base-services"        ,gradle-base-services)
       ("groovy-ant" ,groovy-ant)
       ("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-json" ,groovy-json)
       ("groovy-templates" ,groovy-templates)
       ("groovy-xml" ,groovy-xml)
       ("icedtea-8" ,icedtea-8)
       ("java-asm-6" ,java-asm-6)
       ("java-commons-compress" ,java-commons-compress)
       ("java-commons-collections" ,java-commons-collections)
       ("java-commons-io" ,java-commons-io)
       ("java-commons-lang" ,java-commons-lang)
       ("java-guava-for-gradle" ,java-guava-for-gradle)
       ("java-jansi" ,java-jansi)
       ("java-jansi-native" ,java-jansi-native)
       ("java-javax-inject" ,java-javax-inject)
       ("java-jaxp" ,java-jaxp)
       ("java-jcip-annotations" ,java-jcip-annotations)
       ("java-jsr305" ,java-jsr305)
       ("java-jul-to-slf4j" ,java-jul-to-slf4j)
       ("java-kryo" ,java-kryo)
       ("java-minlog" ,java-minlog)
       ("java-native-platform" ,java-native-platform)
       ("java-objenesis" ,java-objenesis)
       ("java-reflectasm" ,java-reflectasm)
       ("java-slf4j-api" ,java-slf4j-api)
       ("ant" ,ant)
       ("bash" ,bash)))
    (native-inputs '())))
