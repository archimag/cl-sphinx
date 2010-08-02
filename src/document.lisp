;;;; document.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defvar *root* nil)

(defvar *root-path* nil)

(defvar *current-document* nil)

(defclass document (docutils.nodes:document)
  ((prev :initform nil)
   (next :initform nil)
   (childs :initform nil :accessor document-childs)))

(defmethod docutils::do-transforms ((transforms list) (document document))
  (let ((*current-document* document))
    (call-next-method)))

(defun document-childs-recursively (doc)
  (alexandria:flatten (iter (for child in (document-childs doc))
                            (collect child)
                            (collect (document-childs-recursively child)))))


(defun document-name (doc)
  (docutils:attribute doc :name))

(defun document-path (doc)
  (docutils:setting :source-path doc))