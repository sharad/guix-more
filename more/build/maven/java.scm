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

(define-module (more build maven java)
  #:use-module (ice-9 peg)
  #:use-module (ice-9 textual-ports)
  #:export (parse-java-file))

(define-peg-pattern java-file body (and (* WS) (* (and top-level-statement
                                                       (* WS)))))
(define-peg-pattern WS none (or " " "\n" "\t" "\r"))
(define-peg-pattern top-level-statement body (or package import-pat class-pat comment))
(define-peg-pattern package all (and (ignore "package") (* WS) package-name
                                     (* WS) (ignore ";")))
(define-peg-pattern import-pat all (and (ignore "import") (* WS) package-name
                                     (* WS) (ignore ";")))
(define-peg-pattern comment all (and (ignore "/*")
                                     comment-part))
(define-peg-pattern comment-part body (or (ignore (and (* "*") "/"))
                                          (and (* "*") (+ comment-chr) comment-part)))
(define-peg-pattern comment-chr body (or "\t" "\n" (range #\ #\)) (range #\+ #\xffff)))
(define-peg-pattern package-name body (* (or (range #\a #\z) (range #\A #\Z)
                                             (range #\0 #\9) "_" ".")))
(define-peg-pattern class-pat all (and (? (and annotation-pat (* WS)))
                                       (? (and (ignore "public") (* WS))) (ignore "class")
                                       (* WS) package-name (* WS)
                                       (? extends)
                                       (? implements)
                                       (ignore "{") class-body (ignore "}")))
(define-peg-pattern extends all (? (and (ignore "extends") (* WS)
                                        package-name (* WS))))
(define-peg-pattern implements all (? (and (ignore "implements") (* WS)
                                           package-name (* WS))))
(define-peg-pattern annotation-pat all (and (ignore "@") package-name
                                            (? (and
                                                 (ignore "(") (* WS)
                                                 annotation-attr
                                                 (* (and (ignore ",") (* WS) annotation-attr))
                                                 (ignore ")")))))
(define-peg-pattern annotation-attr all (and attr-name (* WS) (ignore "=")
                                             (* WS) attr-value (* WS)))
(define-peg-pattern attr-name all (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)
                                         "_")))
(define-peg-pattern attr-value all (or "true" "false"
                                       (+ (or (range #\0 #\9) (range #\a #\z)
                                              (range #\A #\Z) "." "_"))
                                       string-pat))
(define-peg-pattern string-pat body (and (ignore "\"") (* string-chr) (ignore "\"")))
(define-peg-pattern string-chr body (or " " "!" (and (ignore "\\") "\"")
                                        (and (ignore "\\") "\\") (range #\# #\xffff)))

(define-peg-pattern class-body all (and (* WS) (* (and class-statement (* WS)))))
(define-peg-pattern class-statement body (or comment param-pat method-pat))
(define-peg-pattern param-pat all (and (? (and annotation-pat (* WS)))
                                       (? (and (ignore (or "private" "public" "protected"))
                                               (* WS)))
                                       type-name (* WS) param-name
                                       (? (and (* WS) (ignore "=") (* WS) (+ valuechr)))
                                       (ignore ";")))
(define-peg-pattern valuechr none (or string-pat comment "\n" "\t" "\r"
                                      (range #\  #\:) (range #\< #\xffff)))
(define-peg-pattern param-name all (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)
                                          "_")))
(define-peg-pattern type-name all (* (or (range #\a #\z) (range #\A #\Z) (range #\0 #\9)
                                         "_" "<" ">")))
(define-peg-pattern method-pat all (and (? (and annotation-pat (* WS)))
                                        (? (and (ignore (or "private" "public" "protected"))
                                                (* WS)))
                                        type-name (* WS) param-name (* WS)
                                        (ignore "(")
                                        param-list (ignore ")") (* WS)
                                        (? (and (ignore "throws") (* WS) package-name (* WS)
                                                (* (and (ignore ",") (* WS) package-name
                                                        (* WS)))))
                                        (ignore "{") (* WS) (? (and method-statements (* WS)))
                                        (ignore "}")))
(define-peg-pattern param-list all (and (* WS) (* (and type-name (* WS) param-name (* WS)
                                                       (? (ignore ",")) (* WS)))))
(define-peg-pattern method-statements none (and (or (+ method-chr)
                                                    (and "{" method-statements "}")
                                                    string-pat)
                                                (? method-statements)))
(define-peg-pattern method-chr none (or "\t" "\n" "\r" " " "!" (range #\# #\z) "|"
                                        (range #\~ #\xffff)))


(define (parse-java-file file)
  (peg:tree (match-pattern java-file (call-with-input-file file get-string-all))))
