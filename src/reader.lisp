;;;; reader.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defvar *verbose* nil)

(defclass reader (docutils.parser.rst:rst-reader) ())

(defmethod docutils:transforms ((reader reader))
  (concatenate 'list
               '(inner-reference-map api-reference-map-transform)
               (remove 'docutils.transform:resolve-media
                       (call-next-method))
               '(resolve-static-files)))

(defmethod docutils:read-document (source (reader reader))
  (when *verbose*
    (format *trace-output* "Read ~A~&" source))
  (let ((doc (change-class (call-next-method) 'document)))
    (setf (document-childs doc)
          (iter (for child in (document-childs doc))
                (unless (string= child "")
                  (collect (docutils:read-document (merge-pathnames child
                                                                    (document-path doc))
                                                   (make-instance 'reader))))))
    doc))
