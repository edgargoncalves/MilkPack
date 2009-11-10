;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; tasklist.lisp
;;;   - implementation of NSTableView to show tasks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;TODO: cell view for task

;;TODO: refactor this code, it's ugly...

;; Interface for the datasource of the tableview:
(objc:defmethod #/tableView:objectValueForTableColumn:row:
    ((self rtm-controller) table-view column (row :<NSI>nteger))
  (flet ((compute-column-value (columns-selectors-alist table-contents &optional selected-task)
	   (let* ((column-id (make-lisp-string (#/identifier column)))
		  (selector-entry (cdr (assoc column-id columns-selectors-alist :test #'string=)))
		  (selector (if (listp selector-entry) (first selector-entry) selector-entry))
		  (transformer (if (listp selector-entry) (second selector-entry) #'make-nsstring)))
	     (awhen (or selected-task (nth row table-contents))
	       (funcall transformer (funcall selector it))))))
    	(cond ((or (null (rtm-instance self))
		   (%null-ptr-p (rtm-instance self))
		   (null (rtm-user-info (rtm-instance self))))
	       	 ;; (format t "woooo, table is none")
	       #@"Nothing")
	      ((eql table-view (contacts-table-view self))
	       (compute-column-value '(("fullname" . rtm-lisp-api::get-fullname)
				       ("user"     . rtm-lisp-api::get-username)
				       ("id"       . rtm-lisp-api::get-id))
				     (rtm-lisp-api::get-contacts (rtm-user-info (rtm-instance self)))))
	      ((eql table-view (tasks-table-view self))
	       (let* ((task-list (filter (lambda (x) (string= (rtm-lisp-api::get-completed x) ""))
				   (get-current-tasks (rtm-instance self))) )
		      (selected-task (nth row task-list)))
	       (compute-column-value
		`(("description" . rtm-lisp-api::get-name)
		  ("due"         . (rtm-lisp-api::get-due
				    ,(lambda (x) (make-nsstring (car (split-sequence:split-sequence #\T x))))))
		  ("priority"    . rtm-lisp-api::get-priority)
		  ("estimate"    . rtm-lisp-api::get-estimate)
		  ("tags"        . (rtm-lisp-api::get-tags
				    ,(lambda (x) (make-nsstring (format nil "~{~a~^, ~}" (if (stringp (car x)) x (car x)))))))
		  ("complete"    . (rtm-lisp-api::get-completed
				    ,(lambda (x) (#/numberWithInteger: ns:ns-number (if (and x (not (string= x ""))) #$YES #$NO))))))
		task-list
		selected-task)))
	      (t #@"Nothing"))))

(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
    ((self rtm-controller) table-view)
  (if (or (null (rtm-instance self))
	  (%null-ptr-p (rtm-instance self))
	  (null (rtm-user-info (rtm-instance self))))
      0
      (cond 
	((eql table-view (contacts-table-view self))
	 (length (rtm-lisp-api::get-contacts (rtm-user-info (rtm-instance self)))))
	((eql table-view (tasks-table-view self))
	 (length (filter (lambda (x) (string= (rtm-lisp-api::get-completed x) ""))
		   (get-current-tasks (rtm-instance self)))))
	(t
	 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; to click on a table use the following methods

(defvar *currently-selected-task*)

(defun get-table-view-selected-item (table-view container &optional selected-row)
  (let ((row (or selected-row (#/selectedRow table-view))))
    (and (> row -1)
	 (nth row container))))

(objc:defmethod (#/tableViewSelectionDidChange: :void)
    ((self rtm-controller) notification)
  (declare (special *currently-selected-task* *rtm-controller*))
  ;; Note, this method works for single selections only, for now.
  (let* ((changed-table (#/object notification)))
    (cond ((eql changed-table (tasks-table-view self))
	   (aif (get-table-view-selected-item changed-table (get-current-tasks-filtered-and-sorted))
		(progn
		  (setf *currently-selected-task* it)
		  ;; if details hud is visible, then update it:
		  (let ((hud-controller (task-details-hud *rtm-controller*)))
		    (when (#/isVisible (#/window hud-controller))
		      (update-details-hud hud-controller *currently-selected-task*))))
		(setf *currently-selected-task* nil))))))



;; double click action:
(objc:defmethod (#/doubleClick: :void) ((self rtm-controller) sender)
  (declare (ignore sender)
	   (special *rtm-controller*))
  (#/showWindow: (task-details-hud *rtm-controller*) self))


#|
Copyright 2009 Edgar Gonçalves

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

     http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.
|#
