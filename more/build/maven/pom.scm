;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2019 Julien Lepiller <julien@lepiller.eu>
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

(define-module (more build maven pom)
  #:use-module (sxml simple)
  #:use-module (ice-9 match)
  #:export (get-pom
            pom-description
            pom-name
            pom-version
            pom-artifactid
            pom-groupid
            pom-dependencies))

(define (get-pom file)
  (let ((pom-content (call-with-input-file file xml->sxml)))
    (match pom-content
      (('*TOP* _ (_ ('@ _ ...) content ...))
        content))))

(define (pom-ref content attr)
  (assoc-ref
    content
    (string->symbol
      (string-append "http://maven.apache.org/POM/4.0.0:" attr))))

(define (pom-groupid content)
  (string-join
    (string-split
      (car (pom-ref content "groupId"))
      #\.)
    "/"))

(define (pom-artifactid content)
  (car (pom-ref content "artifactId")))

(define (pom-version content)
  (car (pom-ref content "version")))

(define (pom-name content)
  (car (pom-ref content "name")))

(define (pom-description content)
  (car (pom-ref content "description")))

(define (pom-dependencies content)
  (filter
    (lambda (a) a)
    (map
      (match-lambda
        ((? string? _) #f)
        (('http://maven.apache.org/POM/4.0.0:dependency content ...)
         (let loop ((content content) (groupid #f) (artifactid #f) (version #f) (scope #f))
           (match content
             ('() 
              `(dependency
                 (groupId ,groupid)
                 (artifactId ,artifactid)
                 (version ,version)
                 ,@(if scope `((scope ,scope)) '())))
             (((? string? _) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:scope scope) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:groupId groupid) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:artifactId artifactid) content ...)
              (loop content groupid artifactid version scope))
             ((('http://maven.apache.org/POM/4.0.0:version version) content ...)
              (loop content groupid artifactid version scope))))))
      (pom-ref content "dependencies"))))
