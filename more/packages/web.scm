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

(define-module (more packages web)
  #:use-module ((guix licenses) #:prefix l:)
  #:use-module (gnu packages)
  #:use-module (gnu packages python)
  #:use-module (more packages python)
  #:use-module (more packages django)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix utils)
  #:use-module (guix build-system python)
  #:use-module (srfi srfi-1))

(define-public pootle
  (package
    (name "pootle")
    (version "2.8.0rc5")
    (source
      (origin
        (method url-fetch)
        (uri (pypi-uri "Pootle" version ".tar.bz2"))
        (sha256
         (base32
          "0m6qcpkcy22dk3ad5y2k8851kqg2w6vrkywgy4vabwbacd7r1mvn"))
        (modules '((guix build utils)))
        (snippet
         '(begin
            (substitute* "Pootle.egg-info/requires.txt"
              (("1.7.3") "1.8.0"))
            (substitute* "requirements/base.txt"
              (("1.7.3") "1.8.0"))))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2))
    (propagated-inputs
     `(("django-allauth" ,python2-django-allauth)
       ("django-assets" ,python2-django-assets)
       ("django-bulk-update" ,python2-django-bulk-update)
       ("django-contact-form" ,python2-django-contact-form)
       ("django-contrib-comments" ,python2-django-contrib-comments)
       ("django-overextends" ,python2-django-overextends)
       ("django-redis" ,python2-django-redis)
       ("django-rq" ,python2-django-rq)
       ("django-sortedm2m" ,python2-django-sortedm2m)
       ("django-statici18n" ,python2-django-statici18n)
       ("babel" ,python2-babel)
       ("cssmin" ,python2-cssmin)
       ("diff-match-patch" ,python2-diff-match-patch)
       ("dirsync" ,python2-dirsync)
       ("elasticsearch" ,python2-elasticsearch)
       ("jsonfield" ,python2-django-jsonfield)
       ("lxml" ,python2-lxml)
       ("dateutil" ,python2-dateutil-2)
       ("levenshtein" ,python2-levenshtein)
       ("mysqlclient" ,python2-mysqlclient)
       ("psycopg2" ,python2-psycopg2)
       ("pytz" ,python2-pytz)
       ("rq" ,python2-rq)
       ("scandir" ,python2-scandir)
       ("stemming" ,python2-stemming)
       ("translate-toolkit" ,python2-translate-toolkit)))
    (home-page "http://pootle.translatehouse.org/")
    (synopsis "Community localization server")
    (description "Community localization server.")
    (license l:gpl3+)))
