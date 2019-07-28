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

(define-module (more build maven plugin)
  #:use-module (more build maven java)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 match)
  #:use-module (srfi srfi-9)
  #:export (generate-mojo-from-files
	    default-convert-type
	    maven-convert-type))

(define-record-type mojo
  (make-mojo package name goal description requires-direct-invocation?
             requires-project? requires-reports? aggregator?
             requires-online? inherited-by-default? instantiation-strategy
             execution-strategy since thread-safe? phase parameters components)
  mojo?
  (package mojo-package)
  (name mojo-name)
  (goal mojo-goal)
  (description mojo-description)
  (requires-direct-invocation? mojo-requires-direct-invocation?)
  (requires-project? mojo-requires-project?)
  (requires-reports? mojo-requires-reports?)
  (aggregator? mojo-aggregator?)
  (requires-online? mojo-requires-online?)
  (inherited-by-default? mojo-inherited-by-default?)
  (instantiation-strategy mojo-instantiation-strategy)
  (execution-strategy mojo-execution-strategy)
  (since mojo-since)
  (thread-safe? mojo-thread-safe?)
  (phase mojo-phase)
  (parameters mojo-parameters)
  (components mojo-components))

(define* (update-mojo mojo
           #:key
           (package (mojo-package mojo))
           (name (mojo-name mojo))
           (goal (mojo-goal mojo))
           (description (mojo-description mojo))
           (requires-direct-invocation? (mojo-requires-direct-invocation? mojo))
           (requires-project? (mojo-requires-project? mojo))
           (requires-reports? (mojo-requires-reports? mojo))
           (aggregator? (mojo-aggregator? mojo))
           (requires-online? (mojo-requires-online? mojo))
           (inherited-by-default? (mojo-inherited-by-default? mojo))
           (instantiation-strategy (mojo-instantiation-strategy mojo))
           (execution-strategy (mojo-execution-strategy mojo))
           (since (mojo-since mojo))
           (thread-safe? (mojo-thread-safe? mojo))
           (phase (mojo-phase mojo))
           (parameters (mojo-parameters mojo))
           (components (mojo-components mojo)))
  (make-mojo package name goal description requires-direct-invocation?
             requires-project? requires-reports? aggregator?
             requires-online? inherited-by-default? instantiation-strategy
             execution-strategy since thread-safe? phase parameters components))

(define-record-type mojo-parameter
  (make-mojo-parameter name type since required editable description
                       configuration)
  mojo-parameter?
  (name          mojo-parameter-name)
  (type          mojo-parameter-type)
  (since         mojo-parameter-since)
  (required      mojo-parameter-required)
  (editable      mojo-parameter-editable)
  (description   mojo-parameter-description)
  (configuration mojo-parameter-configuration))

(define* (update-mojo-parameter mojo-parameter
           #:key (name (mojo-parameter-name mojo-parameter))
                 (type (mojo-parameter-type mojo-parameter))
                 (since (mojo-parameter-since mojo-parameter))
                 (required (mojo-parameter-required mojo-parameter))
                 (editable (mojo-parameter-editable mojo-parameter))
                 (description (mojo-parameter-description mojo-parameter))
                 (configuration (mojo-parameter-configuration mojo-parameter)))
  (make-mojo-parameter name type since required editable description
                       configuration))

(define-record-type mojo-component
  (make-mojo-component field role hint)
  mojo-component?
  (field mojo-component-field)
  (role  mojo-component-role)
  (hint  mojo-component-hint))

(define* (update-mojo-component mojo-component
           #:key (field (mojo-component-field mojo-component))
                 (role (mojo-component-role mojo-component))
                 (hint (mojo-component-hint mojo-component)))
  (make-mojo-component field role hint))

(define (generate-mojo-parameter mojo-parameter)
  `(parameter (name ,(mojo-parameter-name mojo-parameter))
              (type ,(mojo-parameter-type mojo-parameter))
              ,@(if (mojo-parameter-since mojo-parameter)
                    `(since (mojo-parameter-since mojo-parameter))
                    '())
              (required ,(if (mojo-parameter-required mojo-parameter) "true" "false"))
              (editable ,(if (mojo-parameter-editable mojo-parameter) "true" "false"))
              (description ,(mojo-parameter-description mojo-parameter))))

(define (generate-mojo-configuration mojo-parameter)
  (let ((config (mojo-parameter-configuration mojo-parameter)))
    (if config
        `(,(string->symbol (mojo-parameter-name mojo-parameter))
           (@ ,@config))
        #f)))

(define (generate-mojo-component mojo-component)
  `(requirement
     (role ,(mojo-component-role mojo-component))
     (role-hint ,(mojo-component-hint mojo-component))
     (field-name ,(mojo-component-field mojo-component))))

(define (generate-mojo mojo)
  `(mojo
     (goal ,(mojo-goal mojo))
     (description ,(mojo-description mojo))
     ,@(let ((val (mojo-requires-direct-invocation? mojo)))
         (if val
             `((requiresDirectInvocation ,val))
             '()))
     ,@(let ((val (mojo-requires-project? mojo)))
         (if val
             `((requiresProject ,val))
             '()))
     ,@(let ((val (mojo-requires-reports? mojo)))
         (if val
             `((requiresReports ,val))
             '()))
     ,@(let ((val (mojo-aggregator? mojo)))
         (if val
             `((aggregator ,val))
             '()))
     ,@(let ((val (mojo-requires-online? mojo)))
         (if val
             `((requiresOnline ,val))
             '()))
     ,@(let ((val (mojo-inherited-by-default? mojo)))
         (if val
             `((inheritedByDefault ,val))
             '()))
     ,@(let ((phase (mojo-phase mojo)))
             (if phase
                 `((phase ,phase))
                 '()))
     (implementation ,(string-append (mojo-package mojo) "." (mojo-name mojo)))
     (language "java")
     (instantiationStrategy ,(mojo-instantiation-strategy mojo))
     (executionStrategy ,(mojo-execution-strategy mojo))
     ,@(let ((since (mojo-since mojo)))
             (if since
                 `((since ,since))
                 '()))
     ,@(let ((val (mojo-thread-safe? mojo)))
         (if val
             `((threadSafe ,val))
             '()))
     (parameters
       ,(map generate-mojo-parameter (mojo-parameters mojo)))
     (configuration
       ,@(filter (lambda (a) a) (map generate-mojo-configuration (mojo-parameters mojo))))
     (requirements
       ,@(map generate-mojo-component (mojo-components mojo)))))


(define (default-convert-type type)
  (cond
    ((equal? type "String") "java.lang.String")
    ((equal? type "File") "java.io.File")
    ((and (> (string-length type) 6)
          (equal? (substring type 0 5) "List<"))
     "java.util.List")
    ((and (> (string-length type) 15)
          (equal? (substring type 0 14) "LinkedHashSet<"))
     "java.util.LinkedHashSet")
    (else type)))

(define (maven-convert-type type)
  (cond
    ((equal? type "MavenProject")
     "org.apache.maven.project.MavenProject")
    (else (default-convert-type type))))

(define (update-mojo-from-file mojo file convert-type)
  (define parse-tree (parse-java-file file))

  (define (update-mojo-from-attrs mojo attrs)
    (let loop ((mojo mojo) (attrs attrs))
      (match attrs
        ('() mojo)
        ((attr attrs ...)
         (match attr
           (('annotation-attr ('attr-name name) ('attr-value value))
            (cond
              ((equal? name "name")
               (loop (update-mojo mojo #:goal value) attrs))
              ((equal? name "defaultPhase")
               (loop (update-mojo mojo #:phase value) attrs))
              ((equal? name "requiresProject")
               (loop (update-mojo mojo #:requires-project? value) attrs))
              ((equal? name "threadSafe")
               (loop (update-mojo mojo #:thread-safe? value) attrs))
              (else
                (raise `(not-found-attr ,name)))))
           (_ (loop mojo attrs)))))))

  (define (update-mojo-parameter-from-attrs mojo-parameter attrs)
    (match attrs
      ('() mojo-parameter)
      (('annotation-attr ('attr-name name) ('attr-value value))
       (cond
         ((equal? name "editable")
          (update-mojo-parameter mojo-parameter #:editable value))
         ((equal? name "required")
          (update-mojo-parameter mojo-parameter #:required value))
         (else
           (update-mojo-parameter mojo-parameter
                                  #:configuration
                                  (cons
                                    (list (string->symbol name) value)
                                    (or
				      (mojo-parameter-configuration mojo-parameter)
				      '()))))))
      ((attr attrs ...)
       (match attr
         (('annotation-attr ('attr-name name) ('attr-value value))
          (cond
            ((equal? name "editable")
             (update-mojo-parameter-from-attrs
               (update-mojo-parameter mojo-parameter #:editable value)
               attrs))
            ((equal? name "required")
             (update-mojo-parameter-from-attrs
               (update-mojo-parameter mojo-parameter #:required value)
               attrs))
            (else (update-mojo-parameter-from-attrs
                    (update-mojo-parameter mojo-parameter
                                           #:configuration
                                           (cons
                                             (list (string->symbol name) value)
                                             (or
					       (mojo-parameter-configuration mojo-parameter)
					       '())))
                    attrs))))
         ((attrss ...)
          (update-mojo-parameter-from-attrs mojo-parameter (append attrss attrs)))))))

  (define (update-mojo-component-from-attrs mojo-component attrs)
    (match attrs
      ('() mojo-component)
      ((attr attrs ...)
       (match attr
         (('annotation-attr ('attr-name name) ('attr-value value))
          (cond
            ((equal? name "role")
             (update-mojo-component-from-attrs
               (update-mojo-component mojo-component #:role value)
               attrs))
            ((equal? name "hint")
             (update-mojo-component-from-attrs
               (update-mojo-component mojo-component #:hint value)
               attrs))
            (else (raise `(not-found-attr ,name)))))
         ((attrss ...)
          (update-mojo-component-from-attrs mojo-component (append attrss attrs)))))))

  (define (update-mojo-from-class-content mojo content)
    (let loop ((content content)
               (mojo mojo)
               (last-comment #f))
      (match content
        ('() mojo)
        ((('comment last-comment) content ...)
         (loop content mojo last-comment))
        ((('param-pat ('annotation-pat annot-name attrs ...) ('type-name type)
           ('param-name name)) content ...)
         (cond
           ((equal? annot-name "Parameter")
            (loop content
                  (update-mojo mojo
                               #:parameters
                               (cons (update-mojo-parameter-from-attrs
                                       (make-mojo-parameter
                                         name (convert-type type) #f #f #f last-comment #f)
                                       attrs)
                                     (mojo-parameters mojo)))
                  #f))
           ((equal? annot-name "Component")
            (loop content
                  (update-mojo mojo
                               #:components
                               (cons (update-mojo-component-from-attrs
                                       (make-mojo-component name #f #f)
                                       attrs)
                                     (mojo-components mojo)))
                  #f))
           (else (raise `(not-found-annot ,annot-name)))))
        ((('param-pat _ ...) content ...)
         (loop content mojo #f))
        ((('method-pat _ ...) content ...)
         (loop content mojo #f)))))

  (let loop ((content parse-tree)
             (mojo mojo)
             (last-comment #f))
    (if (null? content)
        mojo
        (match content
          ((tls content ...)
           (match tls
             (('package package)
              (loop content (update-mojo mojo #:package package) last-comment))
             (('comment last-comment)
              (loop content mojo last-comment))
             (('class-pat ('annotation-pat annot-name (attrs ...)) name ('extends extend)
               ('class-body class-content ...))
              (loop content
                    (update-mojo-from-class-content
                      (update-mojo-from-attrs
                        (update-mojo mojo
                                     #:name name
                                     #:description last-comment)
                        attrs)
                      class-content)
                    #f))
             (('class-pat ('annotation-pat annot-name (attrs ...)) name ('extends extend)
               ('implements implement) ('class-body class-content ...))
              (loop content
                    (update-mojo-from-class-content
                      (update-mojo-from-attrs
                        (update-mojo mojo
                                     #:name name
                                     #:description last-comment)
                        attrs)
                      class-content)
                    #f))
             (_
              (loop content mojo last-comment))))))))

(define (generate-mojo-from-files convert-type . files)
  (let ((mojo (make-mojo #f #f #f #f #f #f #f #f #f #f "per-lookup"
                         "once-per-session" #f #f #f '() '())))
    (let loop ((files files) (mojo mojo))
      (if (null? files)
          (generate-mojo mojo)
          (loop
            (cdr files)
            (update-mojo-from-file
              (update-mojo mojo
                #:package #f
                #:name #f
                #:goal #f
                #:description #f
                #:requires-direct-invocation? #f
                #:requires-project? #f
                #:requires-reports? #f
                #:aggregator? #f
                #:requires-online? #f
                #:inherited-by-default? #f
                #:instantiation-strategy "per-lookup"
                #:execution-strategy "once-per-session"
                #:since #f
                #:thread-safe? #f
                #:phase #f)
              (car files)
	      convert-type))))))
