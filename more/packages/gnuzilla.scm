;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2013, 2015 Andreas Enge <andreas@enge.fr>
;;; Copyright © 2013, 2014, 2015, 2016, 2017 Ludovic Courtès <ludo@gnu.org>
;;; Copyright © 2014, 2015, 2016, 2017 Mark H Weaver <mhw@netris.org>
;;; Copyright © 2015 Sou Bunnbu <iyzsong@gmail.com>
;;; Copyright © 2016 Efraim Flashner <efraim@flashner.co.il>
;;; Copyright © 2016 Alex Griffin <a@ajgrf.com>
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

(define-module (more packages gnuzilla)
  #:use-module ((srfi srfi-1) #:hide (zip))
  #:use-module (gnu packages)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (more packages google)
  #:use-module (gnu packages base)
  #:use-module (gnu packages databases)
  #:use-module (gnu packages glib)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages gtk)
  #:use-module (gnu packages gnome)
  #:use-module (gnu packages gnuzilla)
  #:use-module (gnu packages libcanberra)
  #:use-module (gnu packages cups)
  #:use-module (gnu packages kerberos)
  #:use-module (gnu packages linux)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages fontutils)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages libreoffice)  ;for hunspell
  #:use-module (gnu packages image)
  #:use-module (gnu packages libffi)
  #:use-module (gnu packages pulseaudio)
  #:use-module (gnu packages python)
  #:use-module (gnu packages xorg)
  #:use-module (gnu packages gl)
  #:use-module (gnu packages assembly)
  #:use-module (gnu packages icu4c)
  #:use-module (gnu packages video)
  #:use-module (gnu packages xdisorg)
  #:use-module (gnu packages zip))

(define (mozilla-patch file-name changeset hash)
  "Return an origin for CHANGESET from the mozilla-esr45 repository."
  (origin
    (method url-fetch)
    (uri (string-append "https://hg.mozilla.org/releases/mozilla-esr45/raw-rev/"
                        changeset))
    (sha256 (base32 hash))
    (file-name file-name)))

(define-public icecat-skia
  (package
    (name "icecat-skia")
    (version "45.5.1-gnu1")
    (source
     (origin
      (method url-fetch)
      (uri (string-append "mirror://gnu/gnuzilla/"
                          (first (string-split version #\-))
                          "/" "icecat" "-" version ".tar.bz2"))
      (sha256
       (base32
        "1sbfgsibmb8wfmb2g40gmqwq3nikmrgzksf51ydrz7gnafyfaqw1"))
      (patches
       (list
        (search-patch "icecat-avoid-bundled-libraries.patch")
        (search-patch "icecat-binutils.patch")
        (mozilla-patch "icecat-CVE-2016-9897.patch"      "2e5438a92617" "0pyjbzyy04759ldpcar8q8cccv67j1jkxsg46rkq7a3rbhmwmw4p")
        (mozilla-patch "icecat-CVE-2016-9899.patch"      "7391f60fb790" "1hydggpmmm2cs9lb15micnkxn4wl4cda9g74hkn3zmks805vjz3h")
        (mozilla-patch "icecat-CVE-2016-9893-pt1.patch"  "744e01001e6e" "0z2fq765kap3ll9as5rvjpnbj3pw26074alw7df0zi215qz47nxr")
        (mozilla-patch "icecat-CVE-2016-9904-pt1.patch"  "8ae673f34a5b" "1rlbihckl9afa0y91lqs7gpnv6a7zxzrmxjv95y3yrl03kibqp76")
        (mozilla-patch "icecat-CVE-2016-9904-pt2.patch"  "409c23c144fe" "05kgs16y8s5pxmg2dxp93247zagnj6zgj3209qpm5hz3an7gr13h")
        (mozilla-patch "icecat-CVE-2016-9900.patch"      "994d9bd0e28d" "007wifyx3b2ln4fwv1i8n24yz5ngjf4mkzd7sqr5bpd3q88ff293")
        (mozilla-patch "icecat-CVE-2016-9893-pt2.patch"  "0fc43af8982e" "0pc8q9knzq2knj723mwkay1lnzbzysb07ygxnc16mcb6f7vl2mw8")
        (mozilla-patch "icecat-CVE-2016-9893-pt3.patch"  "19f9a4643d77" "0w5yxj1l0hvs66q9agjp4m5sfby7fj05lx33gaqf899bw4hn4vcf")
        (mozilla-patch "icecat-CVE-2016-9893-pt4.patch"  "c58442c414f5" "1z1w1v8xagkhrwgp51ij1k2gx0ripslia09vm78812n7gcwddaas")
        (mozilla-patch "icecat-CVE-2016-9898.patch"      "5054047b7328" "0xlw8irymfp3bcaa5jpf7clf7bq6qxp3i8zapp8jya8lzr1nf868")
        (mozilla-patch "icecat-bug-881832-pt1.patch"     "1123263318a3" "1qkxwva3zrcs1zhga8ncmndq03988dx75i896g53gbvpskj06915")
        (mozilla-patch "icecat-bug-881832-pt2.patch"     "dc87c0a39adf" "01rapf14f3r2wk0cjd16dn1rll4ipgs33cnjmjck48nvk67ikz6h")
        (mozilla-patch "icecat-bug-881832-pt3.patch"     "f20e5f488368" "15ql9ywifb3gm2g1057k63f821dbs3wqsh3zhndprzf3dn6aha4i")
        (mozilla-patch "icecat-bug-881832-pt4.patch"     "7950c4d5bd7c" "0jhkg5hq5yfy7rh21k1mpbsbr81ql85aazym30zy3n2cf28xxhd7")
        (mozilla-patch "icecat-bug-881832-pt5.patch"     "972734ec21b6" "073i4v1f1ydy49i57pvzscz95sjr5bbk9s5sajxvmmcsmwhpjdfy")
        (mozilla-patch "icecat-CVE-2016-9905-pt1.patch"  "aebd3687e05e" "1qz6hdgflcrqyg7fv66cbg23v4b7q5bc2yxzrgjxs4j1d7jy1s0s")
        (mozilla-patch "icecat-CVE-2016-9905-pt2.patch"  "63d8e5cd27cb" "11fsgyngy7v59ma30xdbmycwf4izwikzvaljngm3ks4534inpl4a")
        (mozilla-patch "icecat-bug-1279202.patch"        "e560997291af" "1hn35slasfcj3ryka4fsarx4l9r99z0iwj67fmbv6zxz4z133kks")
        (mozilla-patch "icecat-CVE-2016-9902.patch"      "21c615b65048" "0ibgsxa36x9ajn2jqbhxxvrfvj6x6iyspsmzzn4brdz11n93skhr")
        (mozilla-patch "icecat-CVE-2016-9901.patch"      "c15e5afc0430" "17gj32agqs94548z8lvz0l6zz3kbwajn8as0y4iw5nb6jsll4c66")
        (mozilla-patch "icecat-CVE-2016-9893-pt5.patch"  "46163fb1cb34" "1yikayczfgfla3aka0159apq3149d52sgvlca0sivx4myd0lvjm7")
        (mozilla-patch "icecat-CVE-2016-9893-pt6.patch"  "cde2a37100f5" "100abggnhwyw84almxrkxqfpyfkd4pqkcrh5y9g4d3jd2h16asvl")
        (mozilla-patch "icecat-CVE-2016-9893-pt7.patch"  "9b78ab1e6d07" "19ib6bp96xk000ll40b8qxvizkncyzclz2rsb9w5fa42qs9978ff")
        (mozilla-patch "icecat-CVE-2016-9893-pt8.patch"  "78ebf9c9dfb0" "1shgr4rk6r2zxr1qqk1j3qnnqzqxnbi093qhlrfh8q5q1ivqf6k1")
        (mozilla-patch "icecat-CVE-2016-9893-pt9.patch"  "a46a9f16823c" "0dwkyz3kcqnfcbhbfh2lss7s0yh87rgzb871qxx3x4ynyqph9mnz")
        (mozilla-patch "icecat-CVE-2017-5376.patch"      "6bc7cc7a33a6" "1wggcqv84n8mp7xps7hy4rwy61fkh45imfqzc0b46s3w5hyhypn2")
        (mozilla-patch "icecat-CVE-2016-9893-pt10.patch" "778f65148b40" "0j2a153sk0654vv2lnxjib4lwml3mlqn6vs46c2pp82iba8nyfrm")
        (mozilla-patch "icecat-CVE-2016-9895.patch"      "94bd2b43c766" "10h0qpr6m9cqyqxxnkbb6mzb3cagavzlynkxgd7a4izyq1bv28rk")
        (mozilla-patch "icecat-CVE-2016-9893-pt11.patch" "893de7431d51" "11gyik8mwipl6ipypkvdq519pw7ccbg0g0bnvxb7271n44cqqcq5")
        (mozilla-patch "icecat-CVE-2017-5383-pt1.patch"  "b21dee058b2c" "005khpimffqzas7slajid4dd2c15nyk7rjk6fsps87bgnx0gx0s4")
        (mozilla-patch "icecat-bug-1258410-pt1.patch"    "cd23c5d74be8" "1myzkpzg6pmjacjz8az06m9kz84in1mwsiaw5sgx47pm02598wm5")
        (mozilla-patch "icecat-bug-1258410-pt2.patch"    "b83594617d15" "171cqflc9jh47az96im1whwhyq4ayicspqsczsmn2x5ll9lqw5fq")
        (mozilla-patch "icecat-CVE-2017-5380.patch"      "d6c6f5e4e641" "10x9f46ylm2q3i669hj1csd6agdm8w7xa0iln691z8ayg2hcrxdz")
        (mozilla-patch "icecat-CVE-2017-5373-pt1.patch"  "122f5fbfc563" "05pp2f4pg4j8a8pdgjhfrc8g2chhkhsarn9n8sbyag0fy3ig1cvd")
        (mozilla-patch "icecat-CVE-2017-5390.patch"      "297c675ddadc" "1jc1b5i69vq1fvz3qfnnv52c9cj17bjbmfyzmqlw5ywna0wfvabz")
        (mozilla-patch "icecat-CVE-2017-5373-pt2.patch"  "3cff736e3bb6" "1nxqwnl9zksvkfkmis7zica4xrhwfndjyy2sxc1dvrh9rshk1swq")
        (mozilla-patch "icecat-CVE-2017-5373-pt3.patch"  "2732280adabc" "0zrpq3aybaw2yy38vs6883a4nw01x4kxn3lfqn9yhcgjvngmmyia")
        (mozilla-patch "icecat-CVE-2017-5373-pt4.patch"  "81c9fdbd96e8" "0scv1zyi4vbsjdsyj4w70n5jd50baq0dzw3qpxqf1n69nfb9k214")
        (mozilla-patch "icecat-CVE-2017-5373-pt5.patch"  "a386ca6a3013" "1m1scz2pxzmg9wya8is5dcr3mgvkx3g1xlykgigmw2mqs5zcdg9s")
        (mozilla-patch "icecat-CVE-2017-5373-pt6.patch"  "0521b0e4707c" "1mv057p4hcvapibpbd9apryag19aiqdzafc6df2angl97m4mcbjx")
        (mozilla-patch "icecat-bug-1290037.patch"        "bf0dd9ae6807" "02iw5ngsvvij95arnn69a744d6si27g1x41ixg16l51dbn900b3r")
        (mozilla-patch "icecat-bug-1322666.patch"        "576f03e362c5" "0m88xs0jwhzx2lg12cvimxjknp7rpsvvhxxblhiqqjwnqip0pyc0")
        (mozilla-patch "icecat-bug-1304266.patch"        "4d82e7314a72" "1rrrw4rw0xv7c2myiypcqh1fk47rk3fvic79zh6m04bl3knclr1r")
        (mozilla-patch "icecat-CVE-2017-5373-pt7.patch"  "0617dd4b444d" "1ipags2cl2p521pm0qx110h5di2mgif6h1r3g8l9b0rc5m9b1y2j")
        (mozilla-patch "icecat-CVE-2017-5375.patch"      "ead08c2a6c57" "1nnnwdr7411xpz6n9j869g6sz447cq6xsmds9cw6d24iprcinp5m")
        (mozilla-patch "icecat-CVE-2017-5378-pt1.patch"  "c5e67d41bdd0" "05kwn5zv381lsiw9vbzm8fh6s1lddx47l8f4pwg487h9dj7vbdfq")
        (mozilla-patch "icecat-CVE-2017-5373-pt8.patch"  "2ce94f2ea797" "1yrnjqpafjns68z99s1m6jins3agid7c1z3v9qgk5xzfcddl31pn")
        (mozilla-patch "icecat-CVE-2017-5373-pt9.patch"  "e4ac8a66d3db" "1ggbh6nk005plm7kvp7qznbnlpidqkipxc5hlaqbjc3m0z9mypzl")
        (mozilla-patch "icecat-CVE-2017-5386.patch"      "8c171b75a0c6" "1cplkgwkhqvb62q7s8cya2drjdswnlicv5d9a60fxvnp6m7a3z02")
        (mozilla-patch "icecat-CVE-2017-5383-pt2.patch"  "79a31d8153bf" "0dzwk2hc741c0jr8xya4y8q59lk8mcgnvfdrdk0vzq1gby16b9zi")
        (mozilla-patch "icecat-CVE-2017-5378-pt2.patch"  "747085a1cd93" "0rpc8hl0jka4akdjnlwya4x9v4pqa1r5vc4pkmxsgvqpp2vkq4m2")
        (mozilla-patch "icecat-bug-1331606.patch"        "f0329536734b" "1br5shyfwp009y623wgjq60vhcp5k5zpzkgy4r3s5yrm5xyfcprf")
        (mozilla-patch "icecat-bug-1328642.patch"        "237c1ca6e035" "0808g5j17var6wv6ivj08i43fwkga9abmmgmz5w3hb8mhajkjw58")
        (mozilla-patch "icecat-CVE-2017-5396.patch"      "910ad552cbf5" "1qss3gc6n4myzwvwanc09xvyiq3hwhixn2xvpbk9xkfz2apdlfa6")
        (mozilla-patch "icecat-bug-1312827.patch"        "5c894801eb1e" "03xyjwx9rfx5w7q6wq9if83fcfgrlvvbyxpsawzxxgybnizyy7g8")
        (mozilla-patch "icecat-bug-1191841.patch"        "83ee87b301bd" "1kd9z4py372a2f1jg7jwvwri43vipf0k7lcvsg7b0cqr5p70zwcg")
        (mozilla-patch "icecat-bug-1234317.patch"        "eb6f18dbfe8d" "1dh3263fvg3dxav09g6fn41ax4q1jmwp5fk6pl6shn16mrfha1qq")))
      (modules '((guix build utils)))
      (snippet
       '(begin
          (use-modules (ice-9 ftw))
          ;; Remove bundled libraries that we don't use, since they may
          ;; contain unpatched security flaws, they waste disk space and
          ;; network bandwidth, and may cause confusion.
          (for-each delete-file-recursively
                    '(;; FIXME: Removing the bundled icu breaks configure.
                      ;;   * The bundled icu headers are used in some places.
                      ;;   * The version number is taken from the bundled copy.
                      ;;"intl/icu"
                      ;;
                      ;; FIXME: A script from the bundled nspr is used.
                      ;;"nsprpub"
                      ;;
                      ;; TODO: Use system media libraries.  Waiting for:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=517422>
                      ;;   * libogg
                      ;;   * libtheora
                      ;;   * libvorbis
                      ;;   * libtremor (not yet in guix)
                      ;;   * libopus
                      ;;   * speex
                      ;;   * soundtouch (not yet in guix)
                      ;;
                      ;; TODO: Use system harfbuzz.  Waiting for:
                      ;; <https://bugzilla.mozilla.org/show_bug.cgi?id=847568>
                      ;;
                      ;; TODO: Use system graphite2.
                      ;;
                      "modules/freetype2"
                      "modules/zlib"
                      "modules/libbz2"
                      "ipc/chromium/src/third_party/libevent"
                      "media/libjpeg"
                      "media/libvpx"
                      "security/nss"
                      "gfx/cairo"
                      "js/src/ctypes/libffi"
                      "db/sqlite3"))
          ;; Delete .pyc files, typically present in icecat source tarballs
          (for-each delete-file (find-files "." "\\.pyc$"))
          ;; Delete obj-* directories, sometimes present in icecat tarballs
          (for-each delete-file-recursively
                    (scandir "." (lambda (name)
                                   (string-prefix? "obj-" name))))
          #t))))
    (build-system gnu-build-system)
    (inputs
     `(("alsa-lib" ,alsa-lib)
       ("bzip2" ,bzip2)
       ("cairo" ,cairo)
       ("cups" ,cups)
       ("dbus-glib" ,dbus-glib)
       ("gdk-pixbuf" ,gdk-pixbuf)
       ("glib" ,glib)
       ("gstreamer" ,gstreamer)
       ("gst-plugins-base" ,gst-plugins-base)
       ("gtk+" ,gtk+-2)
       ("pango" ,pango)
       ("freetype" ,freetype)
       ("hunspell" ,hunspell)
       ("libcanberra" ,libcanberra)
       ("libgnome" ,libgnome)
       ("libjpeg-turbo" ,libjpeg-turbo)
       ("libxft" ,libxft)
       ("libevent" ,libevent-2.0)
       ("libxinerama" ,libxinerama)
       ("libxscrnsaver" ,libxscrnsaver)
       ("libxcomposite" ,libxcomposite)
       ("libxt" ,libxt)
       ("libffi" ,libffi)
       ("libvpx" ,libvpx)
       ("icu4c" ,icu4c)
       ("pixman" ,pixman)
       ("pulseaudio" ,pulseaudio)
       ("mesa" ,mesa)
       ("mit-krb5" ,mit-krb5)
       ("nspr" ,nspr)
       ("nss" ,nss)
       ("skia" ,skia)
       ("sqlite" ,sqlite)
       ("startup-notification" ,startup-notification)
       ("unzip" ,unzip)
       ("yasm" ,yasm)
       ("zip" ,zip)
       ("zlib" ,zlib)))
    (native-inputs
     `(("perl" ,perl)
       ("python" ,python-2) ; Python 3 not supported
       ("python2-pysqlite" ,python2-pysqlite)
       ("pkg-config" ,pkg-config)
       ("which" ,which)))
    (arguments
     `(#:tests? #f          ; no check target
       #:out-of-source? #t  ; must be built outside of the source directory
       #:parallel-build? #f

       ;; XXX: There are RUNPATH issues such as
       ;; $prefix/lib/icecat-31.6.0/plugin-container NEEDing libmozalloc.so,
       ;; which is not in its RUNPATH, but they appear to be harmless in
       ;; practice somehow.  See <http://hydra.gnu.org/build/378133>.
       #:validate-runpath? #f

       #:configure-flags '("--enable-default-toolkit=cairo-gtk2"
                           "--enable-pango"
                           "--enable-gio"
                           "--enable-svg"
                           "--enable-canvas"
                           "--enable-mathml"
                           "--enable-startup-notification"
                           "--enable-pulseaudio"
                           "--enable-gstreamer=1.0"
                           "--enable-skia"

                           "--disable-gnomevfs"
                           "--disable-gconf"
                           "--disable-gnomeui"

                           ;; Building with debugging symbols takes ~5GiB, so
                           ;; disable it.
                           "--disable-debug"
                           "--disable-debug-symbols"

                           ;; Hack to work around missing
                           ;; "unofficial" branding in icecat.
                           "--enable-official-branding"

                           ;; Avoid bundled libraries.
                           "--with-system-zlib"
                           "--with-system-bz2"
                           "--with-system-jpeg"        ; must be libjpeg-turbo
                           "--with-system-libevent"
                           "--with-system-libvpx"
                           "--with-system-icu"
                           "--with-system-nspr"
                           "--with-system-nss"
                           "--enable-system-pixman"
                           "--enable-system-cairo"
                           "--enable-system-ffi"
                           "--enable-system-hunspell"
                           "--enable-system-sqlite"

                           ;; Fails with "--with-system-png won't work because
                           ;; the system's libpng doesn't have APNG support".
                           ;; According to
                           ;; http://sourceforge.net/projects/libpng-apng/ ,
                           ;; "the Animated Portable Network Graphics (APNG)
                           ;; is an unofficial extension of the Portable
                           ;; Network Graphics (PNG) format";
                           ;; we probably do not wish to support it.
                           ;; "--with-system-png"
                           )

       #:modules ((ice-9 ftw)
                  (ice-9 rdelim)
                  (ice-9 match)
                  ,@%gnu-build-system-modules)
       #:phases
       (modify-phases %standard-phases
         (add-after
          'unpack 'ensure-no-mtimes-pre-1980
          (lambda _
            ;; Without this, the 'source/test/addons/packed.xpi' and
            ;; 'source/test/addons/simple-prefs.xpi' targets fail while trying
            ;; to create zip archives.
            (let ((early-1980 315619200)) ; 1980-01-02 UTC
              (ftw "." (lambda (file stat flag)
                         (unless (<= early-1980 (stat:mtime stat))
                           (utime file early-1980 early-1980))
                         #t))
              #t)))
         (add-after
          'unpack 'remove-h264parse-from-blacklist
          (lambda _
            ;; Remove h264parse from gstreamer format helper blacklist.  It
            ;; was put there to work around a bug in a pre-1.0 version of
            ;; gstreamer.  See:
            ;; https://www.mozilla.org/en-US/security/advisories/mfsa2015-47/
            (substitute* "dom/media/gstreamer/GStreamerFormatHelper.cpp"
              (("^  \"h264parse\",\n") ""))
            #t))
         (add-after
          'unpack 'arrange-to-link-libxul-with-libraries-it-might-dlopen
          (lambda _
            ;; libxul.so dynamically opens libraries, so here we explicitly
            ;; link them into libxul.so instead.
            ;;
            ;; TODO: It might be preferable to patch in absolute file names in
            ;; calls to dlopen or PR_LoadLibrary, but that didn't seem to
            ;; work.  More investigation is needed.
            (substitute* "toolkit/library/moz.build"
              (("^# This needs to be last")
               "OS_LIBS += [
    'GL', 'gnome-2', 'canberra', 'Xss', 'cups', 'gssapi_krb5',
    'gstreamer-1.0', 'gstapp-1.0', 'gstvideo-1.0' ]\n\n"))
            #t))
         (replace
          'configure
          ;; configure does not work followed by both "SHELL=..." and
          ;; "CONFIG_SHELL=..."; set environment variables instead
          (lambda* (#:key outputs configure-flags #:allow-other-keys)
            (let* ((out (assoc-ref outputs "out"))
                   (bash (which "bash"))
                   (abs-srcdir (getcwd))
                   (srcdir (string-append "../" (basename abs-srcdir)))
                   (flags `(,(string-append "--prefix=" out)
                            ,(string-append "--with-l10n-base="
                                            abs-srcdir "/l10n")
                            ,@configure-flags)))
              (setenv "SHELL" bash)
              (setenv "CONFIG_SHELL" bash)
              (mkdir "../build")
              (chdir "../build")
              (format #t "build directory: ~s~%" (getcwd))
              (format #t "configure flags: ~s~%" flags)
              (zero? (apply system* bash
                            (string-append srcdir "/configure")
                            flags)))))
         (add-before 'configure 'install-desktop-entry
           (lambda* (#:key outputs #:allow-other-keys)
             ;; Install the '.desktop' file.
             (define (swallow-%%-directives input output)
               ;; Interpret '%%ifdef' directives found in the '.desktop' file.
               (let loop ((state 'top))
                 (match (read-line input 'concat)
                   ((? eof-object?)
                    #t)
                   ((? string? line)
                    (cond ((string-prefix? "%%ifdef" line)
                           (loop 'ifdef))
                          ((string-prefix? "%%else" line)
                           (loop 'else))
                          ((string-prefix? "%%endif" line)
                           (loop 'top))
                          (else
                           (case state
                             ((top else)
                              (display line output)
                              (loop state))
                             (else
                              (loop state)))))))))

             (let* ((out (assoc-ref outputs "out"))
                    (applications (string-append out "/share/applications")))
               (call-with-input-file "debian/icecat.desktop.in"
                 (lambda (input)
                   (call-with-output-file "debian/icecat.desktop"
                     (lambda (output)
                       (swallow-%%-directives input output)))))

               (substitute* "debian/icecat.desktop"
                 (("@MOZ_DISPLAY_NAME@")
                  "GNU IceCat")
                 (("^Exec=@MOZ_APP_NAME@")
                  (string-append "Exec=" out "/bin/icecat"))
                 (("@MOZ_APP_NAME@")
                  "icecat"))
               (install-file "debian/icecat.desktop" applications)
               #t)))
         (add-after 'install-desktop-entry 'install-icons
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (with-directory-excursion "browser/branding/official"
                 (for-each
                  (lambda (file)
                    (let* ((size (string-filter char-numeric? file))
                           (icons (string-append out "/share/icons/hicolor/"
                                                 size "x" size "/apps")))
                      (mkdir-p icons)
                      (copy-file file (string-append icons "/icecat.png"))))
                  '("default16.png" "default22.png" "default24.png"
                    "default32.png" "default48.png" "content/icon64.png"
                    "mozicon128.png" "default256.png")))))))))
    (home-page "http://www.gnu.org/software/gnuzilla/")
    (synopsis "Entirely free browser derived from Mozilla Firefox")
    (description
     "IceCat is the GNU version of the Firefox browser.  It is entirely free
software, which does not recommend non-free plugins and addons.  It also
features built-in privacy-protecting features.")
    (license license:mpl2.0)     ;and others, see toolkit/content/license.html
    (properties
     `((ftp-directory . "/gnu/gnuzilla")
       (cpe-name . "firefox_esr")
       (cpe-version . ,(first (string-split version #\-)))))))
