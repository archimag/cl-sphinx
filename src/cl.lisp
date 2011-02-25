;;;; cl.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defun parse-common-lisp-entity (str)
  (ppcre:register-groups-bind (package name) ("^(?:([^:]+):{1,2})?(.+)$" str)
    (values package name)))

(defmacro with-tag ((tag &optional class) &body body)
    (let ((attrs (if class (list :class class))))
    `(progn
       (docutils:part-append (docutils.writer.html::start-tag nil
                                                              ,tag
                                                              ',attrs))
       ,@body
       (docutils:part-append "</" ,tag ">"))))

(defun permalink (id &optional (title "Permalink to this definition"))
  (docutils:part-append 
   (docutils.writer.html::start-tag nil
                                    "a"
                                    (list :href (format nil "#~A" id)
                                          :class "headerlink"
                                          :title title))
   "¶</a>"))

(defvar *api-reference-map* nil)

;;;; reference

(defclass description (docutils.nodes:raw) 
  ((package :initarg :package :initform nil :reader entity-package)
   (name :initarg :name :reader entity-name)
   (arglist :initarg :arglist :reader entity-arglist)))

(defmethod docutils:allowed-child-p ((parent description) node &optional index)
  (declare (ignore index))
  t)

(defun entity-full-name (entity)
  (format nil "~A:~A" (entity-package entity) (entity-name entity)))

(defun entity-id (entity)
  (docutils::make-id (entity-full-name entity)))

(defgeneric show-description-title (entity)
  (:method (entity)
    (with-tag ("span" "common-lisp-entity")
      (docutils:part-append (typecase entity
                              (variable-description "Variable")
                              (function-description "Function")
                              (macro-description "Macro"))
                            " "
                            (entity-full-name entity)))
    (docutils:part-append " ("
                          (entity-arglist entity)
                          ")")
    (permalink (entity-id entity))))

(defmethod show-description-title :around (entity)
  (docutils:part-append
   (docutils.writer.html::start-tag nil
                                    "dl"
                                    (list :id (entity-id entity))))
  (with-tag ("dt")
    (call-next-method))
  (docutils:part-append "</dl>"))
                                    
                                          
      

(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node description))
  (docutils:part-append (docutils.writer.html::start-tag node
                                                         "dl"
                                                         '(:class "entity-description")))
 (show-description-title node)
 (with-tag ("dd")
   (docutils:with-children (ch node)
     (docutils:visit-node write ch)))
  (docutils:part-append "</dl>"))


(defclass variable-description (description) ())

(defmethod show-description-title ((entity variable-description))
  (with-tag ("span" "common-lisp-entity")
    (docutils:part-append "Variable "
                          (entity-full-name entity)))
  (permalink (entity-id entity)))

(defmacro def-entity-description-directive (directive class)
  `(docutils.parser.rst:def-directive ,directive (parent name &option args &content-parser parser)
     (let ((paragraph (docutils:make-node 'docutils.nodes:paragraph))
           (node (multiple-value-bind (package symbol-name) (parse-common-lisp-entity name)
                   (make-instance ',class
                                  :package package
                                  :name symbol-name
                                  :arglist args))))
       (docutils:add-child paragraph node)
       (docutils:add-child parent paragraph)
       (funcall parser node))))

(defclass function-description (description) ())
(defclass macro-description (description) ())

(def-entity-description-directive defvar variable-description)
(def-entity-description-directive defun function-description)
(def-entity-description-directive defmacro macro-description)


(defclass api-reference-map-transform (docutils:transform) ())

(defmethod docutils:transform ((transform api-reference-map-transform))
  (when *api-reference-map*
    (let ((document (docutils:document (docutils:node transform))))
      (docutils:with-nodes (node document)
        (when (typep node 'description)
          (setf (gethash (list (type-of node)
                               (entity-package node)
                               (entity-name node))
                         *api-reference-map*)
                (list :doc document
                      :name (entity-full-name node))))))))
  
;;;; common-lisp-entity

(defclass common-lisp-entity (docutils.nodes:raw)
  ((package :initarg :package :initform nil :reader entity-package)
   (name :initarg :name :reader entity-name)
   (description-class :initarg :description-class :initform nil :reader entity-description-class)))

(defun api-reference-href (node &aux (dclass (entity-description-class node)))
  (if (and *api-reference-map* dclass)
      (let ((info (gethash (list dclass
                                 (entity-package node)
                                 (entity-name node))
                           *api-reference-map*)))
        (if info
            (format nil
                    "~A#~A"
                    (resolve-doc (getf info :doc)
                                 *current-document*)
                    (docutils::make-id (docutils::normalise-name (getf info :name))))))))


(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node common-lisp-entity))
  (let ((href (api-reference-href node)))
    (cond
      (href (docutils:part-append
             (docutils.writer.html::start-tag node
                                              "a"
                                              `(:class "common-lisp-entity" :href ,href)))
            (when (entity-package node)
              (docutils:part-append (entity-package node)
                                    ":"))
            (docutils:part-append (entity-name node)
                                  "</a>"))
       (t (docutils:part-append
           (docutils.writer.html::start-tag node
                                            "span"
                                            '(:class "common-lisp-entity")))
          (when (entity-package node)
            (docutils:part-append (entity-package node)
                                  ":"))
          (docutils:part-append (entity-name node)
                                "</span>")))))

(defmacro def-common-lisp-entity-role (name &optional description-class)
  (alexandria:with-gensyms (package symbol-name text)
    `(docutils.parser.rst:def-role ,name (,text)
         (multiple-value-bind (,package ,symbol-name) (parse-common-lisp-entity ,text)
           (make-instance 'common-lisp-entity
                          :package ,package
                          :name ,symbol-name
                          :description-class ',description-class)))))

(def-common-lisp-entity-role var variable-description)
(def-common-lisp-entity-role fun function-description)
(def-common-lisp-entity-role macro macro-description)
(def-common-lisp-entity-role const)
(def-common-lisp-entity-role class)

;;;; hypespec-ref

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
