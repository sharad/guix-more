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

(define-module (more packages kotlin)
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
  #:use-module (gnu packages maven)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages web)
  #:use-module (gnu packages xml)
  #:use-module (more packages java)
  #:use-module (more packages maven))

;; Needs maven-core
(define-public kotlin
  (package
    (name "kotlin")
    ;; last version with a build.xml file
    ;; TODO: check if version 1.2.32-1.2.39 exist. 1.2.40 doesn't have build.xml.
    (version "1.2.31")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JetBrains/kotlin/archive/v"
                                  version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1lm1rvj1vf4z8nzpffqcdwcydnlf24ls07z0r0nc4by3hjxzs3sv"))))
    (build-system ant-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-before 'build 'fix-build.xml
           (lambda _
             (substitute* "build.xml"
               (("org/jetbrains/kotlin/ant/antlib.xml")
                "ant/src/org/jetbrains/kotlin/ant/antlib.xml")))))))
    (home-page "https://kotlinlang.org/")
    (synopsis "Programming language targetting the JVM")
    (description "")
    (license license:asl2.0)))

(define-public java-javac2
  (package
    (name "java-javac2")
    ;; The release page on github is a mess
    (version "0.182.2256")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/JetBrains/intellij-community/"
                                  "archive/appcode/" (substring version 2) ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1b9csd0vs6m41q4jhlwl3rlfb596vcnjhch75pcwk8999zrvqf0f"))))
    (build-system ant-build-system)
    (arguments
     `(#:source-dir "java/compiler/javac2/src"
       ;; No test
       #:tests? #f))
    (home-page "https://github.com/JetBrains/intellij-community")
    (synopsis "")
    (description "")
    (license license:asl2.0)))
