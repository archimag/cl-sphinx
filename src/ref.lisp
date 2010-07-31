;;;; ref.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defclass inner-reference (docutils.nodes:raw)
  ((href :initarg :href :initform nil :reader inner-reference-href)
   (title :initarg :title :initform nil :reader inner-reference-title)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node inner-reference))
  (docutils:part-append (docutils.writer.html::start-tag node "a"
                                                         (list :href (format nil "~A.html" (inner-reference-href node)))))
  (docutils:part-append (inner-reference-title node))
  (docutils:part-append "</a>"))

(docutils.parser.rst:def-role ref (text)
  (ppcre:register-groups-bind (href title) ("^([^<]+)\\s+<([^>]+)>$" text)
    (make-instance 'inner-reference
                   :href title
                   :title href)))