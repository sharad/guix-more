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

(define-module (more packages groovy)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system ant)
  #:use-module (gnu packages java)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages java))

;; This package contains the java bootstrap that is used to build groovy submodules.
(define-public groovy-java-bootstrap
  (package
    (name "groovy-java-bootstrap")
    (version "2.4.13")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/apache/groovy/archive/GROOVY_"
                                  "2_4_13.tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0qf1l029ilhnldmd194aybk3053apya3vfd33d3m80n2zh2wnbc1"))
              (patches
                (search-patches
                  "groovy-Add-exceptionutilsgenerator.patch"))))
    (build-system ant-build-system)
    (arguments
     `(#:jar-name "groovy.jar"
       #:source-dir "src/main:subprojects/groovy-test/src/main/java"
       #:test-dir "src/test"
       #:tests? #f
       ;; Reference to plus is ambiguous
       ;#:jdk ,icedtea-8
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
                             "build/classes/org/codehaus/groovy/runtime/ExceptionUtils.class")))))))
    (native-inputs
     `(("java-junit" ,java-junit)
       ("java-hamcrest-core" ,java-hamcrest-core)
       ("antlr2" ,antlr2)
       ("java-jmock-1" ,java-jmock-1)
       ("java-xmlunit-legacy" ,java-xmlunit-legacy)))
    (inputs
     `(("java-commons-cli" ,java-commons-cli)
       ("java-asm" ,java-asm)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-xstream" ,java-xstream)
       ("java-jansi" ,java-jansi)
       ("java-jline-2" ,java-jline-2)))
    (home-page "")
    (synopsis "")
    (description "")
    (license (list license:gpl2
                   license:cddl1.1))))

(define-public groovy-bootstrap
  (package
    (inherit groovy-java-bootstrap)
    (name "groovy-bootstrap")
    (arguments
     `(#:jar-name "groovy.jar"
       ;Requires groovy-xml and logback-classic which are circular dependencies
       #:tests? #f
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
                             "target/classes/org/codehaus/groovy/runtime/ExceptionUtils.class"))))
         (add-before 'build 'generate-dgminfo
           (lambda _
             (mkdir-p "target/classes/org/codehaus/groovy/runtime")
             (mkdir-p "target/classes/META-INF")
             (system* "javac" "-cp" (getenv "CLASSPATH")
                      "src/main/org/codehaus/groovy/tools/DgmConverter.java")
             (zero? (system* "java" "-cp" (string-append (getenv "CLASSPATH")
                                                         ":src/main")
                             "org.codehaus.groovy.tools.DgmConverter"))))
         (add-before 'build 'copy-resources
           (lambda _
             (with-directory-excursion "src/main"
               (for-each (lambda (file)
                           (mkdir-p (string-append "../../target/classes/"
                                                   (dirname file)))
                           (copy-file file
                                      (string-append "../../target/classes/"
                                                     file)))
                  (find-files "." ".*.(txt|properties|xml|html)")))))
         (replace 'build
           (lambda _
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "target/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy.jar"
                               "-C" "target/classes" "."))))))))
    (inputs
     `(("java-apache-ivy-bootstrap" ,java-apache-ivy-bootstrap)
       ,@(package-inputs groovy-java-bootstrap)))
    (native-inputs
     `(("groovy-java-bootstrap" ,groovy-java-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

;; Common test classes
(define-public groovy-tests-bootstrap
  (package
    (inherit groovy-bootstrap)
    (name "groovy-tests-bootstrap")
    (arguments
     `(#:jar-name "groovy-tests-bootstrap.jar"
       #:tests? #f; no tests
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (append
                                 (find-files "src/test" "TestSupport.java")
                                 (find-files "src/test" "HeadlessTestSupport.java")
                                 (find-files "src/test" "XmlAssert.java"))))
               (zero? (system* "jar" "-cf" "build/jar/groovy-tests-bootstrap.jar"
                               "-C" "build/classes" "."))))))))
    (inputs
     `(("groovy-test" ,groovy-test)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-test
  (package
    (inherit groovy-bootstrap)
    (name "groovy-test")
    (arguments
     `(#:jar-name "groovy-test.jar"
       #:test-dir "subprojects/groovy-test/src/test"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "subprojects/groovy-test/src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-test.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (zero? (apply system* "java" "-cp"
                           (string-append (getenv "CLASSPATH") ":build/classes")
                           "org.codehaus.groovy.tools.FileSystemCompiler"
                           "-d" "build/test-classes"
                           "-j"
                           (append
                             (find-files "subprojects/groovy-test/src/test"
                                         ".*\\.(groovy|java)$"))))
             (zero? (system* "ant" "check")))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-xml
  (package
    (inherit groovy-bootstrap)
    (name "groovy-xml")
    (arguments
     `(#:jar-name "groovy-xml.jar"
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-xml")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-xml.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-templates
  (package
    (inherit groovy-bootstrap)
    (name "groovy-templates")
    (arguments
     `(#:jar-name "groovy-templates.jar"
       #:test-dir "subprojects/groovy-templates/src/test"
       #:tests? #f;Requires spock-framework which is a circular dependency
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "subprojects/groovy-templates/src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-templates.jar"
                               "-C" "build/classes" "."))))))))
    (inputs
     `(("groovy-xml" ,groovy-xml)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-groovydoc
  (package
    (inherit groovy-bootstrap)
    (name "groovy-groovydoc")
    (arguments
     `(#:jar-name "groovy-groovydoc.jar"
       #:test-dir "subprojects/groovy-groovydoc/src/test"
       #:tests? #f; Requires groovy-ant which is a circular dependency
       #:phases
       (modify-phases %standard-phases
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "subprojects/groovy-groovydoc/src/main/resources"
                               "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "subprojects/groovy-groovydoc/src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-groovydoc.jar"
                               "-C" "build/classes" "."))))))))
    (inputs
     `(("groovy-templates" ,groovy-templates)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-ant
  (package
    (inherit groovy-bootstrap)
    (name "groovy-ant")
    (arguments
     `(#:jar-name "groovy-ant.jar"
       #:test-dir "src/test"
       ;; FIXME: Excluding all tests because they fail
       #:test-exclude (list
                        "**/GroovyTest.java"
                        "**/GroovycTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-ant")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-ant.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (find-files "src/test"
                                         ".*\\.(groovy|java)$")))
               (zero? (system* "ant" "check"))))))))
    (inputs
     `(("groovy-groovydoc" ,groovy-groovydoc)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-xml" ,groovy-xml)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-bsf
  (package
    (inherit groovy-bootstrap)
    (name "groovy-bsf")
    (arguments
     `(#:jar-name "groovy-bsf.jar"
       #:test-dir "src/test"
       #:test-exclude (list
;; exception from Groovy: org.codehaus.groovy.runtime.InvokerInvocationException:
;; groovy.lang.MissingMethodException: No signature of method:
;; java.util.ArrayList.each() is applicable for argument types:
;; (groovy.script.MapFromList$_doit_closure1) values:
;; [groovy.script.MapFromList$_doit_closure1@17e554d5]
                        "**/BSFTest.java")
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-bsf")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-bsf.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\""))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (find-files "src/test"
                                         ".*\\.(groovy|java)$")))
               (zero? (system* "ant" "check"))))))))
    (inputs
     `(("java-commons-bsf" ,java-commons-bsf)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-swing
  (package
    (inherit groovy-bootstrap)
    (name "groovy-swing")
    (arguments
     `(#:jar-name "groovy-swing.jar"
       ;; FIXME: tests are not run
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-swing")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-swing.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "src/test/groovy/groovy/util/GroovySwingTestCase.groovy"
               (("HeadlessTestSupport.headless") "isHeadless()"))
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (find-files "src/test"
                                         ".*\\.(groovy|java)$")))
               (zero? (system* "ant" "check"))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-console
  (package
    (inherit groovy-bootstrap)
    (name "groovy-console")
    (arguments
     `(#:jar-name "groovy-console.jar"
       ;; FIXME: tests are not run
       #:test-dir "src/test"
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-console")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-console.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (substitute*
               "../groovy-swing/src/test/groovy/groovy/util/GroovySwingTestCase.groovy"
               (("HeadlessTestSupport.headless") "isHeadless()"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "../groovy-swing/src/test"
                                           ".*\\.(groovy|java)$")
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (inputs
     `(("groovy-swing" ,groovy-swing)
       ("groovy-templates" ,groovy-templates)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ("java-commons-logging-minimal" ,java-commons-logging-minimal)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-docgenerator
  (package
    (inherit groovy-bootstrap)
    (name "groovy-docgenerator")
    (arguments
     `(#:jar-name "groovy-docgenerator.jar"
       #:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-docgenerator")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-docgenerator.jar"
                               "-C" "build/classes" "."))))))))
    (inputs
     `(("groovy-templates" ,groovy-templates)
       ("groovy-swing" ,groovy-swing)
       ("java-qdox-1.12" ,java-qdox-1.12)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-groovysh
  (package
    (inherit groovy-bootstrap)
    (name "groovy-groovysh")
    (arguments
     `(#:jar-name "groovy-groovysh.jar"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-groovysh")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-groovysh.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (inputs
     `(("groovy-xml" ,groovy-xml)
       ("groovy-console" ,groovy-console)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-jmx
  (package
    (inherit groovy-bootstrap)
    (name "groovy-jmx")
    (arguments
     `(#:jar-name "groovy-jmx.jar"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-jmx")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-jmx.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-json
  (package
    (inherit groovy-bootstrap)
    (name "groovy-json")
    (arguments
     `(#:jar-name "groovy-json.jar"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-json")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-json.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-jsr223
  (package
    (inherit groovy-bootstrap)
    (name "groovy-jsr223")
    (arguments
     `(#:jar-name "groovy-jsr223.jar"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-jsr223")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-jsr223.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-nio
  (package
    (inherit groovy-bootstrap)
    (name "groovy-nio")
    (arguments
     `(#:jar-name "groovy-nio.jar"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:tests? #f; Requires spock-framework
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-nio")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-nio.jar"
                               "-C" "build/classes" "."))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-servlet
  (package
    (inherit groovy-bootstrap)
    (name "groovy-servlet")
    (arguments
     `(#:jar-name "groovy-jsr223.jar"
       #:test-dir "src/test"
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-servlet")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-servlet.jar"
                               "-C" "build/classes" ".")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (substitute* "build.xml"
               (("depends=\"compile-tests\"") "depends=\"\"")
               (("}/java") "}/groovy"))
             (and
               (zero? (apply system* "java" "-cp"
                             (string-append (getenv "CLASSPATH") ":build/classes")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (append
                               (find-files "src/test"
                                           ".*\\.(groovy|java)$"))))
               (zero? (system* "ant" "check"))))))))
    (inputs
     `(("groovy-templates" ,groovy-templates)
       ("groovy-xml" ,groovy-xml)
       ,@(package-inputs groovy-bootstrap)))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-json" ,groovy-json)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-sql
  (package
    (inherit groovy-bootstrap)
    (name "groovy-sql")
    (arguments
     `(#:jar-name "groovy-sql.jar"
       #:test-dir "src/test"
       #:tests? #f;TODO: Requires hsqldb
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-sql")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-sql.jar"
                               "-C" "build/classes" "."))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy-testng
  (package
    (inherit groovy-bootstrap)
    (name "groovy-testng")
    (arguments
     `(#:jar-name "groovy-testng.jar"
       #:tests? #f; No tests
       #:jdk ,icedtea-8
       #:phases
       (modify-phases %standard-phases
         (add-before 'configure 'chdir
           (lambda _
             (chdir "subprojects/groovy-testng")))
         (add-before 'build 'copy-resources
           (lambda _
             (copy-recursively "src/main/resources" "build/classes")))
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (mkdir-p "build/jar")
             (and
               (zero? (apply system* "java" "-cp" (getenv "CLASSPATH")
                               "org.codehaus.groovy.tools.FileSystemCompiler"
                               "-d" "build/classes"
                               "-j"; joint compilation
                               (find-files "src/main"
                                           ".*\\.(groovy|java)$")))
               (zero? (system* "jar" "-cf" "build/jar/groovy-testng.jar"
                               "-C" "build/classes" "."))))))))
    (native-inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-test" ,groovy-test)
       ("groovy-tests-bootstrap" ,groovy-tests-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

(define-public groovy
  (package
    (inherit groovy-bootstrap)
    (name "groovy")
    (arguments
     `(#:tests? #f; No tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs inputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (out-bin (string-append out "/bin")))
               (with-directory-excursion "src/bin"
                 (substitute* "startGroovy"
                   (("\"\\\\\"") "\"")
                   (("\\\\\"\"") "\"")
                   (("\\\\\\$") "$")
                   (("STARTER_CLASSPATH=.*")
                    (string-append
                      "STARTER_CLASSPATH="
                      (string-join
                        (apply append
                          (map (lambda (input)
                                 (find-files (assoc-ref inputs input) ".*.jar"))
                               '("groovy-bootstrap" "groovy-ant" "groovy-bsf"
                                 "groovy-console" "groovy-docgenerator"
                                 "groovy-groovydoc" "groovy-groovysh"
                                 "groovy-jmx" "groovy-json" "groovy-jsr223"
                                 "groovy-nio" "groovy-servlet" "groovy-sql"
                                 "groovy-swing" "groovy-templates" "groovy-testng"
                                 "java-commons-cli" "java-asm"
                                 "java-classpathx-servletapi" "java-xstream"
                                 "java-jansi" "java-jline-2" "antlr2")))
                        ":"))))
                 (for-each
                   (lambda (tool)
                     (install-file tool out-bin)
                     (chmod (string-append out-bin "/" tool) #o755))
                   '("grape" "groovy" "groovyc" "groovyConsole" "groovydoc"
                     "groovysh" "java2groovy" "startGroovy")))
               (install-file "src/conf/groovy-starter.conf"
                             (string-append out "/conf"))))))))
    (inputs
     `(("groovy-bootstrap" ,groovy-bootstrap)
       ("groovy-ant" ,groovy-ant)
       ("groovy-bsf" ,groovy-bsf)
       ("groovy-console" ,groovy-console)
       ("groovy-docgenerator" ,groovy-docgenerator)
       ("groovy-groovydoc" ,groovy-groovydoc)
       ("groovy-groovysh" ,groovy-groovysh)
       ("groovy-jmx" ,groovy-jmx)
       ("groovy-json" ,groovy-json)
       ("groovy-jsr223" ,groovy-jsr223)
       ("groovy-nio" ,groovy-nio)
       ("groovy-servlet" ,groovy-servlet)
       ("groovy-sql" ,groovy-sql)
       ("groovy-swing" ,groovy-swing)
       ("groovy-templates" ,groovy-templates)
       ("groovy-testng" ,groovy-testng)
       ("java-commons-cli" ,java-commons-cli)
       ("java-asm" ,java-asm)
       ("java-classpathx-servletapi" ,java-classpathx-servletapi)
       ("java-xstream" ,java-xstream)
       ("java-jansi" ,java-jansi)
       ("java-jline-2" ,java-jline-2)
       ("antlr2" ,antlr2)))))
