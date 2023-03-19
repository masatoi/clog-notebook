(defpackage clog-notebook
  (:use #:cl #:clog)
  (:import-from #:clog-user
                #:*body*)
  (:export #:start-notebook))

(in-package :clog-notebook)

;;; Eval lisp code

(defun eval-form (form)
  (handler-bind ((error (lambda (e)
                          (return-from eval-form
                            (with-output-to-string (s)
                              (format s "~A~%~%" e)
                              #+sbcl(sb-debug:print-backtrace :stream s))))))
    (with-output-to-string (s)
      (prin1 (eval form) s))))

(defun str->progn-form (str)
  (with-input-from-string (s str)
    (labels ((iter (data result)
                   (if (eq data :eof)
                       (nreverse result)
                       (iter
                         (read s nil :eof)
                         (cons data result)))))
      (cons 'progn
            (handler-case
                (iter (read s nil :eof) nil)
              (end-of-file (e)
                (declare (ignore e))
                nil
                ))))))

(defun eval-form-str (str)
  (eval-form (str->progn-form str)))

;;; Define cells

(defun setup-lisp-ace (editor status &key (package "CLOG-USER"))
  (set-on-change editor
                 (lambda (obj)
                   (let* ((ace-obj (clog-ace::js-ace obj))
                          (token-str
                            (js-query obj (format nil "var row = ~A.selection.getCursor().row; ~
                            var column = ~A.selection.getCursor().column; ~
                            var o = column;
                            var c; var charRange; var b=0; ~
                            while (column > 0) {
                              column--;
                              charRange = new ace.Range(row, column-1, row, column); ~
                              c = ~A.session.getTextRange(charRange); ~
                              if (c==')') { b++ } ~
                              if (c=='(' && b==0) { ~
                                charRange = new ace.Range(row, column, row, o); column=0;~
                                c = ~A.session.getTextRange(charRange);} ~
                              if (c=='(' && b > 0) { b-- } }~
                            c"
                                                  ace-obj ace-obj ace-obj ace-obj))))
                     (unless (equal token-str "")
                       (with-input-from-string (stream token-str)
                         (ignore-errors
                          (let* ((token-symbol (read stream))
                                 (*PACKAGE* (find-package package))
                                 (SWANK::*buffer-package* (find-package package))
                                 (SWANK::*buffer-readtable* *readtable*)
                                 (token-symbol-str (format nil "~A" token-symbol))
                                 args-doc)
                            (ignore-errors
                             (setf args-doc
                                   (swank::autodoc `(,token-symbol-str swank::%CURSOR-MARKER%))))
                            (if args-doc
                                (setf args-doc (car args-doc))
                                (setf args-doc (swank:operator-arglist token-symbol-str package)))
                            (setf (advisory-title status)
                                  (documentation (find-symbol token-symbol-str) 'function))
                            (when args-doc
                              (setf (text status) (string-downcase args-doc))))))))))
  ;; auto-complete
  (clog-ace:set-auto-completion editor t)
  (clog-ace:set-on-auto-complete editor
                                 (lambda (obj data)
                                   (declare (ignore obj))
                                   (mapcar (lambda (symbol-name)
                                             (list :caption symbol-name
                                                   :value symbol-name
                                                   :meta ""))
                                           (swank::all-completions data *package*))))
  (setf (clog-ace:theme editor) "ace/theme/xcode")
  (setf (clog-ace:mode editor) "ace/mode/lisp")
  (setf (clog-ace:tab-size editor) 2))

(defun create-code-cell (obj)
  (let* ((cell (create-div obj :class "w3-border"))
         (code (clog-ace:create-clog-ace-element cell :class "ace-code"))
         ;; $('.ace-code').css('resize', 'both');
         (status (create-div cell :class "w3-tiny w3-border"))
         (result (create-div cell))
         (eval-button (create-button cell :content "eval"))
         (remove-this-cell-button (create-button cell :content "remove this cell"))
         (add-code-cell-button (create-button cell :content "add code cell"))
         (add-text-cell-button (create-button cell :content "add text cell")))
    (setup-lisp-ace code status :package "CLOG-NOTEBOOK")
    (set-on-click eval-button
                  (lambda (b)
                    (declare (ignore b))
                    (setf (text result)
                          ;; (inner-html result)
                          ;; (format nil "<code>code1<br>code2</code>")
                          ;; #>(format nil "<code>~A</code>" (eval-form-str (text-value code)))
                          (eval-form-str (text-value code))
                          )))
    (set-on-click remove-this-cell-button
                  (lambda (b)
                    (declare (ignore b))
                    (setf (hiddenp cell) t)))
    (set-on-click add-code-cell-button
                  (lambda (b)
                    (declare (ignore b))
                    (create-code-cell obj)))
    (set-on-click add-text-cell-button
                  (lambda (b)
                    (declare (ignore b))
                    (create-text-cell obj)))
    cell))

(defun create-text-cell (obj)
  (let* ((cell (create-div obj :class "w3-border"))
         (markdown (create-text-area cell))
         (result (create-div cell))
         (render-button (create-button cell :content "render"))
         (edit-button (create-button cell :content "edit"))
         (remove-this-cell-button (create-button cell :content "remove this cell"))
         (add-code-cell-button (create-button cell :content "add code cell"))
         (add-text-cell-button (create-button cell :content "add text cell")))
    (setf (hiddenp edit-button) t)
    (set-on-click render-button
                  (lambda (b)
                    (declare (ignore b))
                    (setf (hiddenp edit-button) nil)
                    (setf (hiddenp render-button) t)
                    (setf (hiddenp result) nil)
                    (setf (inner-html result) (clcm:cm->html (value markdown)))
                    (setf (hiddenp markdown) t)))
    (set-on-click edit-button
                  (lambda (b)
                    (declare (ignore b))
                    (setf (hiddenp edit-button) t)
                    (setf (hiddenp render-button) nil)
                    (setf (hiddenp result) t)
                    (setf (hiddenp markdown) nil)))
    (set-on-click remove-this-cell-button
                  (lambda (b)
                    (declare (ignore b))
                    (setf (hiddenp cell) t)))
    (set-on-click add-code-cell-button
                  (lambda (b)
                    (declare (ignore b))
                    (create-code-cell obj)))
    (set-on-click add-text-cell-button
                  (lambda (b)
                    (declare (ignore b))
                    (create-text-cell obj)))
    cell))

(defun create-menu-bar (obj)
  (let* ((menu-bar (clog-gui:create-gui-menu-bar obj))
         (file-menu (clog-gui:create-gui-menu-drop-down menu-bar :content "File"))
         (file-open (clog-gui:create-gui-menu-item file-menu :content "Open"))
         (file-close (clog-gui:create-gui-menu-item file-menu :content "Close"))
         (edit-menu (clog-gui:create-gui-menu-drop-down menu-bar :content "Edit"))
         (view-menu (clog-gui:create-gui-menu-drop-down menu-bar :content "View"))
         (insert-menu (clog-gui:create-gui-menu-drop-down menu-bar :content "Insert"))
         (cell-menu (clog-gui:create-gui-menu-drop-down menu-bar :content "Cell")))
    (declare (ignorable file-open file-close edit-menu view-menu insert-menu cell-menu))
    menu-bar))

;;; Bootstrap

(defun on-new-window (body)
  (set-html-on-close body "Connection Lost")
  (load-css (html-document body) "/css/w3.css")
  (setf (title (html-document body)) "CLOG Notebook")

  (clog-gui:clog-gui-initialize body)

  (create-menu-bar body)

  (let ((notebook-body (create-div body)))
    (create-code-cell notebook-body)))

(create-menu-bar *body*)

(defparameter bar (clog-gui:create-gui-menu-bar *body*))
(clog-gui:create-gui-menu-drop-down bar :content "File")

(create-menu-bar *body*)


(create-div *body* :content "hoge")

(defun start-notebook ()
  "Start notebook."
  (initialize #'on-new-window)
  (open-browser))

;; w3-dropdown-content
;; $('.w3-dropdown-content').css('z-index', 100);
