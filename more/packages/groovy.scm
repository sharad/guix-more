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
    (name "groovy")
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
     `(("java-junit" ,java-junit)
       ("antlr2" ,antlr2)
       ("java-jmock-1" ,java-jmock-1)
       ("java-xmlunit-legacy" ,java-xmlunit-legacy)))
    (inputs
     `(("java-commons-cli" ,java-commons-cli)
       ("java-asm" ,java-asm)
       ("java-tomcat" ,java-tomcat)
       ("java-xstream" ,java-xstream)
       ("java-jansi" ,java-jansi)
       ("java-jline" ,java-jline)))
    (home-page "")
    (synopsis "")
    (description "")
    (license (list license:gpl2
                   license:cddl1.1))))

(define-public groovy-templates
  (package
    (inherit groovy-java-bootstrap)
    (name "groovy-templates")
    (arguments
     `(#:jar-name "groovy-templates.jar"
       #:phases
       (modify-phases %standard-phases
         (replace 'build
           (lambda _
             (mkdir-p "build/classes")
             (zero? (system* "java" "-cp" (getenv "CLASSPATH")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/classes"
                             "-j"; joint compilation
                             (find-files "subprojects/groovy-templates/src/main"
                                         ".*\\.(groovy|java)$")))))
         (replace 'check
           (lambda _
             (mkdir-p "build/test-classes")
             (zero? (system* "java" "-cp" (getenv "CLASSPATH")
                             "org.codehaus.groovy.tools.FileSystemCompiler"
                             "-d" "build/test-classes"
                             "-j"
                             (find-files "subprojects/groovy-templates/src/test"
                                         ".*\\.(groovy|java)$"))))))))
    (native-inputs
     `(("groovy-java-bootstrap" ,groovy-java-bootstrap)
       ,@(package-native-inputs groovy-java-bootstrap)))))

