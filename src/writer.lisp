;;;; writer.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sphinx-writer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass sphinx-html-writer (docutils.writer.html:html-writer) () )

(defun headline (node h)
  (docutils:part-append
   (docutils.writer.html::start-tag node h)
   (docutils.writer.html::encode (docutils:as-text node)))

  (permalink (or (docutils:attribute node :id)
                 (docutils:attribute (docutils:parent node) :id))
             "Permalink to this headline")

  (docutils:part-append "</" h ">"))
  

(defmethod docutils:visit-node ((writer sphinx-html-writer) (node docutils.nodes:title))
  (cond
    ((typep (docutils:parent node) 'docutils:document)
     (docutils:with-part (docutils.writer.html:head)
       (docutils:part-append (format nil
                                     "<title>~A</title>~%"
                                     (docutils.writer.html::encode (docutils:as-text node)))))
     (docutils:with-part (docutils.writer.html:body-pre-docinfo)
       (headline node "h1")))
    (t
     (headline node
               (format nil
                       "h~D"
                       (+ docutils.writer.html::*section-level*
                          (docutils:setting :initial-header-level writer)))))))

(defmethod docutils:visit-node ((writer sphinx-html-writer) (node docutils.nodes:subtitle))
  (cond
    ((typep (docutils:parent node) 'docutils:document)
     (headline node "h2"))
    (t (call-next-method))))
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun compile-template (path)
  (let ((closure-template:*default-closure-template-package* (closure-template:ensure-ttable-package (gensym)))
        (closure-template.parser::*possible-functions* (cons "staticHref" closure-template.parser::*possible-functions*))
        (closure-template::*user-functions* '(("STATIC-HREF" . static-href))))
    (unwind-protect
         (progn
           (closure-template:compile-template
            :common-lisp-backend
            (format nil 
                    "{template finalizePage}~A{/template}"
                    (alexandria:read-file-into-string (merge-pathnames "_static/template.tmpl"
                                                                       path))))
            (closure-template:ttable-find-template             
             (closure-template:package-ttable  closure-template:*default-closure-template-package*)
             "FINALIZE-PAGE"))
      (delete-package closure-template:*default-closure-template-package*))))

(defun make-contents-plist (doc)
  (iter (for child in (document-childs doc))
        (collect (list :href (if (not (eql child *current-document*))
                                 (namestring
                                  (make-pathname
                                   :defaults (make-relavive-href (enough-namestring (document-path child)
                                                                                    (document-path *root*))
                                                                 (enough-namestring (document-path *current-document*)
                                                                                    (document-path *root*)))
                                   :type "html")))
                       :title (document-name child)
                       :childs (make-contents-plist child)))))




(defun write-html (doc path template)
  (when *verbose*
    (format *trace-output* "Write ~A~&" path))
  (let ((*current-document* doc))
    (let ((content (with-output-to-string (out)
                     (funcall template
                              (list :title (document-name doc)
                                    :contents (make-contents-plist *root*)
                                    :content (let ((writer (make-instance 'sphinx-html-writer)))
                                               (docutils:visit-node writer doc)
                                               (with-output-to-string (out)
                                                 (iter (for part in  '(docutils.writer.html:body-pre-docinfo 
                                                                       docutils.writer.html:docinfo
                                                                       docutils.writer.html:body))
                                                       (docutils:write-part writer part out))
                                                 (format out "</div>"))))
                              out))))
      (alexandria:write-string-into-file content
                                         path
                                         :if-exists :supersede
                                         :if-does-not-exist :create))))


(defun make-documentation (contents target-dir &key verbose)
  (let* ((*verbose* verbose)
         (*inner-reference-map* (make-hash-table :test 'equal))
         (*api-reference-map* (make-hash-table :test 'equal))
         (*root-path* contents)
         (*root* (docutils:read-document contents (make-instance 'reader)))
         (root-path (document-path *root*))
         (target (ensure-directories-exist (fad:pathname-as-directory target-dir)))
         (template (compile-template root-path)))
    (flet ((target-pahtname (orig)
             (ensure-directories-exist (merge-pathnames (enough-namestring orig root-path)
                                                        target))))
      (iter (for doc in (cons *root* (document-childs-recursively *root*))) 
            (write-html doc
                        (make-pathname :type "html"
                                       :defaults (target-pahtname (document-path doc)))
                        template))
      (fad:walk-directory (make-pathname :directory (pathname-directory (merge-pathnames "_static/" root-path)))
                          #'(lambda (f)
                              (unless (string= "template.tmpl"
                                               (format nil
                                                       "~A.~A"
                                                       (pathname-name f)
                                                       (pathname-type f)))
                                (fad:copy-file f (target-pahtname f) :overwrite t)))))
    *api-reference-map*))
