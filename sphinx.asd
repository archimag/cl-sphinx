;;;; sphinx.asd
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem sphinx
  :depends-on (#:docutils #:closure-template #:cl-fad #:colorize)
  :components ((:module "src"
                        :components ((:file "packages")
                                     (:file "document" :depends-on ("packages"))
                                     (:file "toctree" :depends-on ("document"))
                                     (:file "ref" :depends-on ("document"))
                                     (:file "resolve" :depends-on ("document"))
                                     (:file "cl" :depends-on ("packages"))
                                     (:file "reader" :depends-on ("toctree"))
                                     (:file "writer" :depends-on ("reader" "resolve" "ref" "cl"))))))