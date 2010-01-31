;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; taskdetails.lisp
;;;   - implementation of an NSPanel controller, a HUD. controller class.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defclass task-details-controller (ns:ns-window-controller)
  ((name-text-view     :foreign-type :id :accessor name-text-view)
   (priority-text-view :foreign-type :id :accessor priority-text-view)
   (tags-token-view    :foreign-type :id :accessor tags-token-view)
   (link-text-view     :foreign-type :id :accessor link-text-view)
   ;; (location-text-view :foreign-type :id :accessor location-text-view)
   (location-selected      :foreign-type :id :accessor location-selected)
   (created-date-view  :foreign-type :id :accessor created-date-view)
   (modified-date-view :foreign-type :id :accessor modified-date-view)
   (due-date-view      :foreign-type :id :accessor due-date-view)
   (is-due-view        :foreign-type :id :accessor is-due-view)
   (estimate-text-view :foreign-type :id :accessor estimate-text-view)
   (list-text-view     :foreign-type :id :accessor list-text-view)
   (list-selected      :foreign-type :id :accessor list-selected)
   (map-view           :foreign-type :id :accessor map-view)
   (notes-table-view   :foreign-type :id :accessor notes-table-view)
   (note-text-view     :foreign-type :id :accessor note-text-view)
   (rtm-instance       :foreign-type :id :accessor rtm-instance :initarg :rtm))
  (:metaclass ns:+ns-object))


(make-kvc-entity task-details-controller #/listSelected     #/setListSelected:     list-selected)
(make-kvc-entity task-details-controller #/locationSelected #/setLocationSelected: location-selected)


;; (objc:defmethod #/init ((self task-details-controller))
;;   (call-next-method)
;;   )

(defmethod update-details-hud ((self task-details-controller) task)
  (declare (special *rtm-controller*))
  (macrolet ((handle-value (rtm-selector
			    iboutlet-selector &key
			    (handler-fn 'make-nsstring)
			    (default-value nil default-value-setp)
			    (value-checker '(lambda (x) x))
			    (view-changer '#/setStringValue:))
	       `(let ((x (funcall #',rtm-selector task)))
		  (funcall #',view-changer
			   (funcall #',iboutlet-selector self)
			   (if (funcall #',value-checker x) (funcall #',handler-fn x) (if ,default-value-setp ,default-value #@""))))))
    (handle-value rtm:get-name     name-text-view)
    (handle-value rtm:get-priority priority-text-view)
    (handle-value rtm:get-estimate estimate-text-view)
    (handle-value rtm:get-url      link-text-view)
    (#/setDateValue: (due-date-view self) (#/date ns:ns-date))

    (macrolet ((handle-date-picker (rtm-selector view-selector)
		 `(handle-value ,rtm-selector
				,view-selector
				:view-changer #/setDateValue:
				:default-value (#/date ns:ns-date)
				:value-checker (lambda (x) (and x (not (string= "" x))))
				:handler-fn (lambda (x) (#/dateFromString: (date-formatter *rtm-controller*)
								      (make-nsstring
								       (car (split-sequence:split-sequence #\Z x))))))))
      (fixDueDateEnabling self (not (string= "" (rtm:get-due task))))
      (handle-date-picker rtm:get-due      due-date-view)
      (handle-date-picker rtm:get-created  created-date-view)
      (handle-date-picker rtm:get-modified modified-date-view))
    
    (handle-value rtm:get-tags
		  tags-token-view
		  :handler-fn (lambda (x) (make-nsstring (format nil "~{~a~^, ~}" (if (stringp (car x)) x (car x))))))

    ;; TODO review if this is working:
    ;;TODO: try to save a list change, check website
    ;; TODO: remove the textbox from both the code and the IB
    (let* ((combo-items (non-smart-task-lists (rtm-instance *rtm-controller*)))
	   (rtmlist (rtm:get-list task))
	   (right-list (find-in-nsarray combo-items (make-nsstring (rtm:get-name rtmlist))
					:test #'#/isEqualToString:
					:key #'name)))
      (#/setListSelected: self right-list))

    (let ((combo-items (locations (rtm-instance *rtm-controller*)))
	   (rtmlocation (rtm:get-location task)))
      (if rtmlocation
	(let ((loc (find-in-nsarray combo-items (make-nsstring (rtm:get-name rtmlocation))
					 :test #'#/isEqualToString:
					 :key #'#/name)))
	  (#/setLocationSelected: self loc))
	(progn
	  ;; set popup list to no value, and set the image to a default image:
	  (#/setLocationSelected: self +null-ptr+)
	  (#/setImage: (map-view self) (#/imageNamed: (find-class 'ns:ns-image) #@"globe.png")))))    
    (#/reloadData (notes-table-view self))))


(defmethod fixDueDateEnabling ((self task-details-controller) desired-state)
  ;; (un)check checkbox
  (#/setState: (is-due-view self) (if desired-state #$NSOnState #$NSOffState))
  ;; (dis/en)able due date box
  (#/setEnabled: (due-date-view self) (if desired-state #$YES #$NO)))

(def-ibaction #/isDueViewStatus: task-details-controller
  (fixDueDateEnabling self (< 0 (#/state (is-due-view self)))))


(def-ibaction #/showWindow: task-details-controller
  (declare (special *currently-selected-task*))
  (when *currently-selected-task*
    (call-next-method sender)
    ;; populate all fields with task details:
    (update-details-hud self *currently-selected-task*)))

(def-ibaction #/viewMap:   task-details-controller
  ;; Open small pane with webview on google maps, correctly placed.
  (declare (special *currently-selected-task*))
  (let* ((location (rtm:get-location *currently-selected-task*)))
    (ccl::run-program "open" (list (rtm:get-location-url location)))))

(def-ibaction #/goToLink:  task-details-controller
  ;; Open browser window on url page
  (declare (special *currently-selected-task*))
  (ccl::run-program "open" (list (rtm:get-url *currently-selected-task*))))

(def-ibaction #/completeTask:  task-details-controller
  (declare (special *rtm-controller*))
  (#/completeTask: *rtm-controller* self)
  ;; hide window.
  (hide-window self))

(def-ibaction #/saveTask: task-details-controller
  ;; this method adds a task, then closes itself.
  (declare (special *currently-selected-task*))
  (macrolet ((save-field (field-selector rtm-selector &key
					 (value-extractor '(lambda (x) (make-lisp-string (#/stringValue x)))))
	       `(let ((x (funcall #',value-extractor (funcall #',field-selector self))))
		  (funcall #',rtm-selector *currently-selected-task* x))))
    ;; loop for the properties and do the rtm change operation on them.
    (save-field name-text-view     rtm:rtm-change-task-name)
    (save-field estimate-text-view rtm:rtm-change-task-estimate)
    (save-field link-text-view     rtm:rtm-change-task-url)

    (unless (%null-ptr-p (#/locationSelected self))
      (save-field #/locationSelected
		  rtm:rtm-change-task-location
		  :value-extractor rtm-location))

    (unless (%null-ptr-p (#/listSelected self))
      (save-field #/listSelected
		  rtm:rtm-move-task-to-list
		  :value-extractor rtm-task-list))
    

    (let ((has-due-p (< 0 (#/state (is-due-view self)))))
      (save-field due-date-view
		  (lambda (task x)
		    (rtm:rtm-change-task-due-date task (if has-due-p x "")))
		  :value-extractor (lambda (x)
				     (let ((out-formatter (make-instance 'ns:ns-date-formatter)))
				       (#/setDateFormat: out-formatter #@"YYYY-MM-dd'T'HH:mm:ss")
				       (make-lisp-string (#/stringFromDate: out-formatter (#/dateValue x)))))))
    (save-field priority-text-view rtm:rtm-change-task-priority
		:value-extractor (lambda (x) (make-lisp-string (#/labelForSegment: x (#/selectedSegment x)))))
    (save-field tags-token-view    rtm:rtm-change-task-tags))
  (hide-window self))


(defmethod hide-window ((self task-details-controller))
  (declare (special *rtm-controller*))
  ;; redraw current task list again:
  (let ((rtmi (rtm-instance *rtm-controller*)))
    (update-current-tasklist rtmi)
    (let ((tableview (tasks-table-view (tasklist-controller *rtm-controller*))))
      (#/deselectAll: tableview nil)
      (#/reloadData tableview))
    (redraw-sidepanel-counts)
    ;; hide nib
    (#/orderOut: (#/window self) +null-ptr+)
    ;; let the user see his tasks, and save our internal state.
    (save-app-data rtmi)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Notes list.

;; Interface for the datasource of the tableview:
(objc:defmethod #/tableView:objectValueForTableColumn:row:
    ((self task-details-controller) table-view column (row :<NSI>nteger))
  (declare (special *currently-selected-task*))
  (declare (ignore column))
  (when *currently-selected-task*
    (if (eql table-view (notes-table-view self))
	(let* ((notes-list (rtm:get-notes *currently-selected-task*))
	       (sel-note (nth row notes-list)))
	  (let* ((note-title (rtm:get-title sel-note))
		 (title (if (string= "" note-title) "<untitled>" note-title)))
	    (make-nsstring title)))
	#@"Nothing")))


(objc:defmethod (#/numberOfRowsInTableView: :<NSI>nteger)
    ((self task-details-controller) table-view)
  (declare (special *currently-selected-task*))
  (if (and (not (or (null (rtm-instance self))
		    (%null-ptr-p (rtm-instance self))
		    (null (rtm-user-info (rtm-instance self)))
		    (null *currently-selected-task*)))
	   (eql table-view (notes-table-view self)))
      (length (rtm:get-notes *currently-selected-task*))
      0))

;;; to rename a note:
(objc:defmethod (#/tableView:setObjectValue:forTableColumn:row: :void)
    ((self task-details-controller) table-view object column row)
  (declare (ignore column row))
  (declare (special *currently-selected-task*))
  (let ((note (get-table-view-selected-item table-view (rtm:get-notes *currently-selected-task*)))
	(new-title (make-lisp-string object)))
    (rtm:rtm-edit-note note new-title (get-contents note))))

;;; to select a note:
(objc:defmethod (#/tableViewSelectionDidChange: :void)
    ((self task-details-controller) notification)
  (declare (special *currently-selected-task*))
  ;; Note, this method works for single selections only, for now.
  (let* ((changed-table (#/object notification)))
    (cond ((eql changed-table (notes-table-view self))
	   (awhen (get-table-view-selected-item changed-table (rtm:get-notes *currently-selected-task*))
	     ;; update note contents view:
	     (let* ((view (note-text-view self))
		    (note-contents (rtm:get-contents it))
		    (contents (make-nsstring (if (string= "" note-contents) "<empty note>" note-contents))))
	       (#/setString: view contents)))))))




(defmethod reload-notes ((self task-details-controller))
  (declare (special *currently-selected-task*))
  (#/reloadData (notes-table-view self)))

(def-ibaction #/addNote: task-details-controller
  (declare (special *currently-selected-task*))
  (when *currently-selected-task*
    (rtm:rtm-add-note-to-task *currently-selected-task* "<untitled>" "<empty note>")
    (reload-notes self)))

(def-ibaction #/deleteNote: task-details-controller
  (declare (special *currently-selected-task*))
  (when *currently-selected-task*
    (awhen (get-table-view-selected-item (notes-table-view self) (rtm:get-notes *currently-selected-task*))
      (rtm:rtm-delete-note it)
      (#/setString: (note-text-view self) #@"")
      (reload-notes self))))

(def-ibaction #/saveNote: task-details-controller
  (declare (special *currently-selected-task*))
  (when *currently-selected-task*
    (awhen (get-table-view-selected-item (notes-table-view self) (rtm:get-notes *currently-selected-task*))
      (rtm:rtm-edit-note it (rtm:get-title it) (make-lisp-string (#/string (#/textStorage (note-text-view self))))))))

;;TODO: place edited tasks on bold/italique
;;TODO: disable save/- buttons when a note isn't selected. enable when they are.
;;TODO: enable save button only when text is changed.


#|
Copyright 2009, 2010 Edgar Gonçalves

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
