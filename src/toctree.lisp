;;;; toctree.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)


(defclass toctree (docutils.nodes:paragraph)
  ((maxdepth :initarg :maxdepth :initform 1 :reader toctree-maxdepth)))

(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node toctree))
  (labels ((impl (doc depth)
             (when (and (> depth 0)
                        (document-childs doc))
               (docutils:part-append
                (docutils.writer.html::start-tag node
                                                 "ul"
                                                 '(:class "toctree")))
               (iter (for child in (document-childs doc))
                     (docutils:part-append
                      (docutils.writer.html::start-tag node "li")
                      (docutils.writer.html::start-tag node
                                                       "a"
                                                       (list :href (resolve-doc child
                                                                                *current-document*)))
                      (document-name child)
                      "</a>")
                     (impl child (1- depth))
                     (docutils:part-append "</li>"))
               (docutils:part-append "</ul>"))))
    (impl *current-document*
          (toctree-maxdepth node))))

(with-sphinx-markup                                             
  (docutils.parser.rst:def-directive toctree (parent &option maxdepth &content content)
    (unless (typep docutils:*document* 'document)
      (change-class docutils:*document* 'document))
    (setf (document-childs docutils:*document*)
          (concatenate 'list
                       (document-childs docutils:*document*)
                       content))
    (docutils:add-child parent
                        (make-instance 'toctree
                                       :maxdepth (parse-integer maxdepth)))))
                              
