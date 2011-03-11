;;;; ref.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defvar *inner-reference-map* nil)

(defclass inner-reference (docutils.nodes:raw)
  ((id :initarg :id :initform nil :reader inner-reference-id)
   (title :initarg :title :initform nil :reader inner-reference-title)))

(defun inner-reference-href (node)
  (if *inner-reference-map*
      (let ((info (gethash (inner-reference-id node) *inner-reference-map*)))
        (if info
            (format nil
                    "~A#~A"
                    (resolve-doc (getf info :doc)
                                 *current-document*)
                    (docutils::make-id (docutils::normalise-name (getf info :name))))))))



(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node inner-reference))
  (when *inner-reference-map*
    (let ((info (gethash (inner-reference-id node) *inner-reference-map*)))
      (cond
        (info (let ((href (format nil
                                  "~A#~A"
                                  (resolve-doc (getf info :doc)
                                               *current-document*)
                                  (docutils::make-id (docutils::normalise-name (getf info :name))))))
                (docutils:part-append
                 (docutils.writer.html::start-tag node
                                                  "a"
                                                  (list :href href)))
         (docutils:part-append (or (inner-reference-title node)
                                   (getf info :name)))
         (docutils:part-append "</a>")))
        (t (docutils:part-append "Bad section id: "
                                 (inner-reference-id node)))))))

(with-sphinx-markup
  (docutils.parser.rst:def-role ref (text)
    (or (ppcre:register-groups-bind (title id) ("^([^<]+)\\s+<([^>]+)>$" text)
          (make-instance 'inner-reference
                         :title title
                         :id id))
        (make-instance 'inner-reference
                       :id text))))

(defclass inner-reference-map (docutils:transform)
  ()
  (:default-initargs :priority 300)
  (:documentation "Fill *inner-reference-map*"))


(defmethod docutils:transform ((transform inner-reference-map))
  (when *inner-reference-map*
    (let ((document (docutils:document (docutils:node transform)))
          (target-name nil))
      (docutils:with-nodes (node document)
        (typecase node
          (docutils.nodes:target
           (setf target-name
                 (if (docutils:attribute node :refuri)
                     nil
                     (docutils:attribute node :name))))
          (docutils.nodes:section
           (when target-name
             (setf (gethash target-name *inner-reference-map*)
                   (list :doc document
                         :name (docutils:attribute node :name)))))
          (otherwise (setf target-name nil)))))))

