;;;; cl.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)


;;;; code-block

(defclass code-block (docutils.nodes:raw)
  ((lang :initarg :lang :initform nil :reader code-block-lang)
   (code :initarg :code :initform nil :reader code-block-code)))

(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node code-block))
  (docutils:part-append (docutils.writer.html::start-tag node
                                                         "div"
                                                         '(:class "code")))
  (docutils:part-append (colorize::html-colorization :common-lisp
                                                     (code-block-code node)))
  (docutils:part-append "</div>"))

(docutils.parser.rst:def-directive code-block (parent lang &content content)
  (let ((node (docutils:make-node 'docutils.nodes:paragraph)))
    (docutils:add-child node
                        (make-instance 'code-block
                                       :lang lang
                                       :code (docutils::join-strings content #\Newline)))
    (docutils:add-child parent node)))


;;;; common-lisp-entity

(defclass common-lisp-entity (docutils.nodes:raw)
  ((package :initarg :package :initform nil :reader common-lisp-entity-package)
   (name :initarg :name :reader common-lisp-entity-name)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node common-lisp-entity))
  (docutils:part-append
   (docutils.writer.html::start-tag node
                                    "span"
                                    '(:class "common-lisp-entity")))
  (when (common-lisp-entity-package node)
    (docutils:part-append (common-lisp-entity-package node)
                          ":"))
  (docutils:part-append (common-lisp-entity-name node)
                        "</span>"))

(defun parse-common-lisp-entity (str)
  (ppcre:register-groups-bind (package name) ("^(?:([^:]+):{1,2})?(.+)$" str)
    (values package name)))
  
(defclass common-lisp-variable (common-lisp-entity) ())
(defclass common-lisp-function (common-lisp-entity) ())
(defclass common-lisp-macros (common-lisp-entity) ())
(defclass common-lisp-constant (common-lisp-entity) ())
(defclass common-lisp-class (common-lisp-entity) ())

(defmacro def-common-lisp-entity-role (name entity-class)
  (alexandria:with-gensyms (package symbol-name text)
    `(docutils.parser.rst:def-role ,name (,text)
       (multiple-value-bind (,package ,symbol-name) (parse-common-lisp-entity ,text)
         (make-instance ',entity-class
                        :package ,package
                        :name ,symbol-name)))))

(def-common-lisp-entity-role var common-lisp-variable)
(def-common-lisp-entity-role fun common-lisp-function)
(def-common-lisp-entity-role macro common-lisp-macros)
(def-common-lisp-entity-role const common-lisp-constant)
(def-common-lisp-entity-role class common-lisp-class)

;; hypespec-ref

(defclass hyperspec-ref (docutils.nodes:raw)
  ((spec :initarg :spec :reader hyperspec-ref-spec)))

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node hyperspec-ref))
  (docutils:part-append
   (docutils.writer.html::start-tag node
                                    "a"
                                    (list :href (clhs-lookup:spec-lookup (hyperspec-ref-spec node))
                                          :class "common-lisp-entity"))
   (hyperspec-ref-spec node)
   "</a>"))

(docutils.parser.rst:def-role hs (spec)
  (make-instance 'hyperspec-ref
                 :spec spec))
