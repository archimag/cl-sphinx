;;;; writer.lisp
;;;;
;;;; This file is part of the cl-sphinx library, released under the GNU General Public License (GPL)
;;;; See <http://www.gnu.org/copyleft/gpl.html> for license details
;;;;
;;;; Author: Moskvitin Andrey <archimag@gmail.com>

(in-package #:sphinx)

(defun make-contents-plist (doc)
  (iter (for child in (document-childs doc))
        (collect (list :href (if (not (eql child *current-document*))
                                 (namestring
                                  (make-pathname
                                   :defaults (make-relavive-href (enough-namestring (docutils:setting :source-path child)
                                                                                    (docutils:setting :source-path *root*))
                                                                 (enough-namestring (docutils:setting :source-path *current-document*)
                                                                                    (docutils:setting :source-path *root*)))
                                   :type "html")))
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

(defun write-html (doc path template)
  (when *verbose*
    (format *trace-output* "Write ~A~&" path))
  (let ((*current-document* doc))
    (let ((content (funcall template
                            (list :title (docutils:attribute doc :name)
                                  :contents (make-contents-plist *root*)
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
                                         :if-does-not-exist :create))))


(defun make-documentation (contents target-dir &key verbose)
  (let ((*verbose* verbose))
    (let* ((*root* (docutils:read-document contents (make-instance 'reader)))
           (root-path (docutils:setting :source-path *root*))
           (target (ensure-directories-exist (fad:pathname-as-directory target-dir)))
           (template (compile-template root-path)))
      (flet ((target-pahtname (orig)
               (ensure-directories-exist (merge-pathnames (enough-namestring orig root-path)
                                                          target))))
        (iter (for doc in (cons *root* (document-childs-recursively *root*))) 
              (write-html doc
                          (make-pathname :type "html"
                                         :defaults (target-pahtname (docutils:setting :source-path doc)))
                          template))
        (fad:walk-directory (make-pathname :directory (pathname-directory (merge-pathnames "_static/" root-path)))
                            #'(lambda (f)
                                (unless (string= "template.tmpl"
                                                 (format nil
                                                         "~A.~A"
                                                         (pathname-name f)
                                                         (pathname-type f)))
                                  (fad:copy-file f (target-pahtname f) :overwrite t))))))))
