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

(define-module (more packages django)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (gnu packages)
  #:use-module (gnu packages django)
  #:use-module (gnu packages python)
  #:use-module (more packages python))


(define-public python-django-allauth
  (package
    (name "python-django-allauth")
    (version "0.30.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-allauth" version))
              (sha256
               (base32
                "1fslqc5qqb0b66yscvkyjwfv8cnbfx5nlkpnwimyb3pf1nc1w7r3"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-mock" ,python-mock)
       ("python-openid" ,python-openid)
       ("python-requests" ,python-requests)
       ("python-requests-oauthlib" ,python-requests-oauthlib)))
    (home-page "http://www.intenct.nl/projects/django-allauth")
    (synopsis "Reusable Django app for local and social authentication")
    (description
      "django-allauth is a reusable Django app that allows for both local and
social authentication, with flows that just work.")
    (license license:expat)))

(define-public python2-django-allauth
  (package
    (inherit (package-with-python2 python-django-allauth))
    (propagated-inputs
     `(("python-django" ,python2-django)
       ("python-mock" ,python2-mock)
       ("python-openid" ,python2-openid)
       ("python-requests" ,python2-requests)
       ("python-requests-oauthlib" ,python2-requests-oauthlib)))))

(define-public python-django-assets
  (package
    (name "python-django-assets")
    (version "0.12")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-assets" version))
              (sha256
               (base32
                "0y0007fvkn1rdlj2g0y6k1cnkx53kxab3g8i85i0rd58k335p365"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (propagated-inputs
     `(("python-django" ,python-django)
       ("python-webassets" ,python-webassets)))
    (home-page "http://github.com/miracle2k/django-assets")
    (synopsis "Asset management for Django")
    (description
      "Asset management for Django, to compress and merge CSS and Javascript
files.  Integrates the webassets library with Django, adding support for
merging, minifying and compiling CSS and Javascript files.")
    (license license:bsd-2)))

(define-public python2-django-assets
  (package-with-python2 python-django-assets))

(define-public python-django-jsonfield
  (package
    (name "python-django-jsonfield")
    (version "1.0.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "jsonfield" version))
              (sha256
               (base32
                "19x4lak0hg9c20r7mvf27w7i8r6i4sg2g0ypmlmp2665fnk76zvy"))
            (modules '((guix build utils)))
            (snippet
              '(substitute* "jsonfield/tests.py"
                 (("django.forms.util") "django.forms.utils")))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/bradjasper/django-jsonfield")
    (synopsis "Store validated JSON in your model")
    (description
      "django-jsonfield is a reusable Django field that allows you to store
validated JSON in your model.  It silently takes care of serialization.  To
use, simply add the field to one of your models.")
    (license license:expat)))

(define-public python2-django-jsonfield
  (package-with-python2 python-django-jsonfield))

(define-public python-django-bulk-update
  (package
    (name "python-django-bulk-update")
    (version "1.1.10")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-bulk-update" version))
              (sha256
               (base32
                "0mbng9m7swfc0dnidipbzlxfhlfjrv755dlnha5s4m9mgdxb1fhc"))))
    (build-system python-build-system)
    (native-inputs
     `(("six" ,python-six)
       ("jsonfield" ,python-django-jsonfield)))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/aykut/django-bulk-update")
    (synopsis "Simple bulk update over Django ORM or with helper function")
    (description
      "Simple bulk update over Django ORM or with helper function.  This
project aims to bulk update given objects using one query over Django ORM.")
    (license license:expat)))

(define-public python2-django-bulk-update
  (package-with-python2 python-django-bulk-update))

(define-public python-django-contact-form
  (package
    (name "python-django-contact-form")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-contact-form" version))
              (sha256
               (base32
                "0az590y56k5ahv4sixrkn54d3a8ig2q2z9pl6s3m4f533mx2gj17"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/ubernostrum/django-contact-form")
    (synopsis "Contact form for Django")
    (description
      "This application provides simple, extensible contact-form functionality
for Django sites.")
    (license license:bsd-3)))

(define-public python2-django-contact-form
  (package-with-python2 python-django-contact-form))

(define-public python-django-contrib-comments
  (package
    (name "python-django-contrib-comments")
    (version "1.8.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-contrib-comments" version))
              (sha256
               (base32
                "0bxsgw8jrkhg6r5s0z6ksfi4w8yknaqb1s9acmxd9pm3pnsnp5kx"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/django/django-contrib-comments")
    (synopsis "Comments framework")
    (description
      "Django used to include a comments framework; since Django 1.6 it's been
separated to a separate project.  This is that project.  This framework can be
used to attach comments to any model, so you can use it for comments on blog
entries, photos, book chapters, or anything else.")
    (license license:bsd-3)))

(define-public python2-django-contrib-comments
  (package-with-python2 python-django-contrib-comments))

(define-public python-django-overextends
  (package
    (name "python-django-overextends")
    (version "0.4.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-overextends" version))
              (sha256
               (base32
                "05rxfjwkwi354qpwjacv1ak77ksgj5fql9yz8i3f1a20b97l8196"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)
       ("sphinx-me" ,python-sphinx-me)))
    (home-page "https://github.com/stephenmcd/django-overextends")
    (synopsis "Circular template inheritance")
    (description
      "A Django reusable app providing the overextends template tag, a drop-in
replacement for Django's extends tag, which allows you to use circular template
inheritance.  The primary use-case for overextends is to simultaneously
override and extend templates from other reusable apps, in your own Django
project.")
    (license license:bsd-2)))

(define-public python2-django-overextends
  (package-with-python2 python-django-overextends))

(define-public python-django-redis
  (package
    (name "python-django-redis")
    (version "4.7.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-redis" version))
              (sha256
               (base32
                "0yyyxv8n9l9dhs893jsqwg2cxqkkc79g719n9dzzzqgkzialv1c1"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-redis" ,python-redis)))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/niwibe/django-redis")
    (synopsis "Full featured redis cache backend for Django")
    (description
      "Full featured redis cache backend for Django.")
    (license license:bsd-3)))

(define-public python2-django-redis
  (package-with-python2 python-django-redis))

(define-public python-django-rq
  (package
    (name "python-django-rq")
    (version "0.9.4")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-rq" version))
              (sha256
               (base32
                "04v8ilfdp10bk31fxgh4cn083gsn5m06342cnpm5d10nd8hc0vky"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-rq" ,python-rq)))
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/ui/django-rq")
    (synopsis "Django integration with RQ")
    (description
      "Django integration with RQ, a Redis based Python queuing library.
Django-RQ is a simple app that allows you to configure your queues in django's
settings.py and easily use them in your project.")
    (license license:expat)))

(define-public python2-django-rq
  (package-with-python2 python-django-rq))

(define-public python-django-sortedm2m
  (package
    (name "python-django-sortedm2m")
    (version "1.3.3")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-sortedm2m" version))
              (sha256
               (base32
                "0axf765i7b3c2s83nlph47asi8s071dhq8l7y382v1pw785s22vi"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/gregmuellegger/django-sortedm2m")
    (synopsis "drop-in replacement for django's own ManyToManyField")
    (description
      "Sortedm2m is a drop-in replacement for django's own ManyToManyField.
The provided SortedManyToManyField behaves like the original one but remembers
the order of added relations.")
    (license license:bsd-3)))

(define-public python2-django-sortedm2m
  (package-with-python2 python-django-sortedm2m))

(define-public python-django-appconf
  (package
    (name "python-django-appconf")
    (version "1.0.2")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-appconf" version))
              (sha256
               (base32
                "0qdjdx35g66xjsc50v0c5h3kg6njs8df33mbjx6j4k1vd3m9lkba"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)))
    (home-page "https://github.com/django-compressor/django-appconf")
    (synopsis "Handle configuration defaults of packaged Django apps")
    (description
      "This app precedes Django's own AppConfig classes that act as \"objects
[to] store metadata for an application\" inside Django's app loading mechanism.
In other words, they solve a related but different use case than
django-appconf and can't easily be used as a replacement.  The similarity in
name is purely coincidental.")
    (license license:bsd-3)))

(define-public python2-django-appconf
  (package-with-python2 python-django-appconf))

(define-public python-django-statici18n
  (package
    (name "python-django-statici18n")
    (version "1.3.0")
    (source (origin
              (method url-fetch)
              (uri (pypi-uri "django-statici18n" version))
              (sha256
               (base32
                "0alcf4g1nv69njhq5k3qw4mfl2k6dc18bik5nk0g1mnp3m8zyz7k"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-django" ,python-django)
       ("django-appconf" ,python-django-appconf)))
    (home-page "https://github.com/zyegfryed/django-statici18n")
    (synopsis "Generate JavaScript catalog to static files")
    (description
      "A Django app that provides helper for generating JavaScript catalog to
static files.")
    (license license:bsd-3)))

(define-public python2-django-statici18n
  (package-with-python2 python-django-statici18n))
