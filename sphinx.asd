;;;; sphinx.asd
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defsystem sphinx
  :depends-on (#:docutils #:closure-template #:cl-fad #:colorize)
  :components ((:file "sphinx")))