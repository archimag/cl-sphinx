;;;; toctree.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)


(defclass toctree (docutils.nodes:paragraph)
  ((maxdepth :initarg :maxdepth :initform 1)))

(docutils.parser.rst:def-directive toctree (parent &option maxdepth &content content)
  (unless (typep docutils:*document* 'document)
    (change-class docutils:*document* 'document))
  (setf (document-childs docutils:*document*)
        (concatenate 'list
                     (document-childs docutils:*document*)
                     content))
  (docutils:add-child parent
                      (make-instance 'toctree
                                     :maxdepth maxdepth)))
                              
