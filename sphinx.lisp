;;;; sphinx.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>


(defpackage #:sphinx
  (:use #:cl #:iter)
  (:export #:read-document-tree))

(in-package #:sphinx)

(defclass tree-document (docutils.nodes:document)
  ((prev :initform nil)
   (next :initform nil)
   (childs :initform nil :accessor tree-document-childs)))

(defun tree-documment-all-childs (doc)
  (alexandria:flatten (iter (for child in (tree-document-childs doc))
                            (collect (tree-documment-all-childs child)))))

(defun static-href (name)
  (format nil "_static/~A" name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; transformations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass resolve-static-files (docutils:transform)
  ()
  (:default-initargs :priority 850)
  (:documentation "Resolve image dependancies"))

(defmethod docutils:transform ((transform resolve-static-files))
  (let ((document (docutils:document (docutils:node transform))))
    (when (docutils:setting :resolve-media document)
      (docutils:with-nodes (node document)
        (typecase node
          (docutils.nodes:image
           (setf (docutils:attribute node :uri)
                 (static-href (docutils:attribute node :uri)))))))))
           
           ;; (let ((uri (docutils:attribute node :uri)))
           ;;   (if (fad:file-exists-p (merge-pathnames uri *acliki-image-dir*))
           ;;       (setf (docutils:attribute node :uri)
           ;;             (resource-href uri))
           ;;       (docutils:report :warning
           ;;                        (list "Media uri ~S is either relative or the media file was not found." uri)
           ;;                        :node node)))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; toctree
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass toctree (docutils.nodes:paragraph)
  ((maxdepth :initarg :maxdepth :initform 1)))

(docutils.parser.rst:def-directive toctree (parent docutils.parser.rst:&option maxdepth docutils.parser.rst:&content content)
  (unless (typep docutils:*document* 'tree-document)
    (change-class docutils:*document* 'tree-document))
  (setf (tree-document-childs docutils:*document*)
        (concatenate 'list
                     (tree-document-childs docutils:*document*)
                     content))
  (docutils:add-child parent
                      (make-instance 'toctree
                                     :maxdepth maxdepth)))
                              

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; code-block
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass code-block (docutils.nodes:raw)
  ((lang :initarg :lang :initform nil :reader code-block-lang)
   (code :initarg :code :initform nil :reader code-block-code)))


(docutils.parser.rst:def-directive code-block (parent lang docutils.parser.rst:&content content)
  (let ((node (docutils:make-node 'docutils.nodes:paragraph)))
    (docutils:add-child node
                        (make-instance 'code-block
                                       :lang lang
                                       :code (format nil
                                                     "~{~A~&~}"
                                                     (coerce content 'list))))
    (docutils:add-child parent node)))

(defmethod docutils:visit-node ((writer docutils.writer.html:html-writer) (node code-block))
  (docutils:part-append "<div class=\"code\">")
  (docutils:part-append (colorize::html-colorization :common-lisp
                                                     (code-block-code node)))
  (docutils:part-append "</div>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; ref
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass inner-reference (docutils.nodes:raw)
  ((href :initarg :href :initform nil :reader inner-reference-href)
   (title :initarg :title :initform nil :reader inner-reference-title)))

(docutils.parser.rst:def-role ref (text)
  (ppcre:register-groups-bind (href title) ("^([^<]+)\\s+<([^>]+)>$" text)
    (make-instance 'inner-reference
                   :href title
                   :title href)))


(defmethod docutils:visit-node ((write docutils.writer.html:html-writer) (node inner-reference))
  (docutils:part-append (docutils.writer.html::start-tag node "a"
                                                         (list :href (inner-reference-href node))))
  (docutils:part-append (inner-reference-title node))
  (docutils:part-append "</a>"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; reader
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass tree-reader (docutils.parser.rst:rst-reader) ())

(defmethod docutils:transforms ((reader tree-reader))
  (append (remove 'docutils.transform:resolve-media
                  (call-next-method))
          '(resolve-static-files)))

(defmethod docutils:read-document (source (reader tree-reader))
  (let ((doc (change-class (call-next-method) 'tree-document)))
    (setf (tree-document-childs doc)
          (iter (for child in (tree-document-childs doc))
                (unless (string= child "")
                  (collect (docutils:read-document (merge-pathnames child
                                                                    (docutils:setting :source-path doc))
                                                   (make-instance 'tree-reader))))))
    doc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; make-documentation
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar *root*)

(defun make-contents-plist (doc)
  (iter (for child in (tree-document-childs doc))
        (collect (list :href (format nil 
                                     "~A.html" 
                                     (pathname-name (docutils:setting :source-path child)))
                       :title (docutils:attribute child :name)
                       :childs (make-contents-plist child)))))


(defun compile-template (path)
  (let ((closure-template:*default-translate-package* (closure-template:make-template-package (gensym)))
        (closure-template.parser.expression::*possible-functions* (cons "staticHref"
                                                                        closure-template.parser.expression::*possible-functions*)))
    (import 'static-href closure-template:*default-translate-package*)
    (unwind-protect
         (eval (cons 'lambda
                     (cddr (closure-template:translate-template
                            :common-lisp-backend
                            (format nil 
                                    "{template finalizePage}~A{/template}"
                                    (alexandria:read-file-into-string (merge-pathnames "_static/template.tmpl"
                                                                                       path)))))))
      (delete-package closure-template:*default-translate-package*))))

(defun write-html (doc path template contents)
  (let ((content (funcall template
                          (list :title (docutils:attribute doc :name)
                                :contents contents
                                :content (let ((writer (make-instance 'docutils.writer.html:html-writer)))
                                           (docutils:visit-node writer doc)
                                           (with-output-to-string (out)
                                             (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                                                                   docutils.writer.html:docinfo
                                                                   docutils.writer.html:body))
                                                   (docutils:write-part writer part out))))))))
  (alexandria:write-string-into-file content
                                     path
                                     :if-exists :supersede
                                     :if-does-not-exist :create)))


(defun make-documentation (contents target-dir)  
  (let* ((root (docutils:read-document contents (make-instance 'tree-reader)))
         (root-path (docutils:setting :source-path root))
         (target (ensure-directories-exist (fad:pathname-as-directory target-dir)))
         (contents (make-contents-plist root))
         (template (compile-template root-path)))
    (flet ((target-pahtname (orig)
             (ensure-directories-exist (merge-pathnames (enough-namestring orig root-path)
                                                        target))))
      (iter (for doc in (cons root (tree-document-childs root))) 
            (write-html doc
                        (make-pathname :type "html"
                                                :defaults (target-pahtname (docutils:setting :source-path doc)))
                        template
                        contents))
      (fad:walk-directory (make-pathname :directory (pathname-directory (merge-pathnames "_static/" root-path)))
                          #'(lambda (f)
                              (unless (string= "template.tmpl"
                                               (format nil
                                                       "~A.~A"
                                                       (pathname-name f)
                                                       (pathname-type f)))
                                (fad:copy-file f (target-pahtname f) :overwrite t)))))))
