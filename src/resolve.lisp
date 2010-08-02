;;;; resolve.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defun make-relavive-href (href base)
  (iter (for i from 0)
        (for h initially (cdr (pathname-directory href)) then (cdr h))
        (for b initially (cdr (pathname-directory base)) then (cdr b))
        (finding (namestring (make-pathname :directory (concatenate 'list
                                                                    (list :relative)
                                                                    (make-list (length b) :initial-element :up)
                                                                    h)
                                            :name (pathname-name href)
                                            :type (pathname-type href)))
                 such-that (not (and (stringp (car h))
                                     (stringp (car b))
                                     (string= (car h) (car b)))))))

(defun resolve-doc (doc base)
  (make-pathname :type "html"
                 :defaults (make-relavive-href (document-path doc)
                                               (document-path base))))

(defun static-href (name)
  (make-relavive-href (format nil "_static/~A" name)
                      (enough-namestring (document-path *current-document*)
                                         *root-path*)))


(defclass resolve-static-files (docutils:transform)
  ()
  (:default-initargs :priority 850)
  (:documentation "Resolve static dependancies"))

(defmethod docutils:transform ((transform resolve-static-files))
  (let ((document (docutils:document (docutils:node transform))))
    (when (docutils:setting :resolve-media document)
      (docutils:with-nodes (node document)
        (typecase node
          (docutils.nodes:image
           (setf (docutils:attribute node :uri)
                 (static-href (docutils:attribute node :uri)))))))))

