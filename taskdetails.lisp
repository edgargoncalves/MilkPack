;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; taskdetails.lisp
;;;   - implementation of an NSPanel controller, a HUD. controller class.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



(defclass task-details-controller (ns:ns-window-controller)
  ((name-text-view     :foreign-type :id :accessor name-text-view)
   (priority-text-view :foreign-type :id :accessor priority-text-view)
   (tags-token-view    :foreign-type :id :accessor tags-token-view)
   (link-text-view     :foreign-type :id :accessor link-text-view)
   (location-text-view :foreign-type :id :accessor location-text-view)
   (created-date-view  :foreign-type :id :accessor created-date-view)
   (modified-date-view :foreign-type :id :accessor modified-date-view)
   (due-date-view      :foreign-type :id :accessor due-date-view)
   (is-due-view        :foreign-type :id :accessor is-due-view)
   (estimate-text-view :foreign-type :id :accessor estimate-text-view)
   (list-text-view     :foreign-type :id :accessor list-text-view)
   (rtm-instance       :foreign-type :id :accessor rtm-instance :initarg :rtm))
  (:metaclass ns:+ns-object))

;; (objc:defmethod #/init ((self task-details-controller))
;;   (call-next-method)
;;   )

(defmethod update-details-hud ((self task-details-controller) task)
  (declare (special *rtm-controller*))
  (macrolet ((handle-value (rtm-selector
			    iboutlet-selector &key
			    (handler-fn 'make-nsstring)
			    default-value
			    (value-checker '(lambda (x) x))
			    (view-changer '#/setStringValue:))
	       `(let ((x (funcall #',rtm-selector task)))
		  (funcall #',view-changer
			   (funcall #',iboutlet-selector self)
			   (if (funcall #',value-checker x) (funcall #',handler-fn x) (or ,default-value #@""))))))
    (handle-value rtm::get-name     name-text-view)
    (handle-value rtm::get-priority priority-text-view)
    (handle-value rtm::get-estimate estimate-text-view)
    (handle-value rtm::get-url      link-text-view)
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
      (fixDueDateEnabling self (not (string= "" (rtm::get-due task))))
      (handle-date-picker rtm::get-due      due-date-view)
      (handle-date-picker rtm::get-created  created-date-view)
      (handle-date-picker rtm::get-modified modified-date-view))
    
    (handle-value rtm::get-tags
		  tags-token-view
		  :handler-fn (lambda (x) (make-nsstring (format nil "~{~a~^, ~}" (if (stringp (car x)) x (car x))))))
    (handle-value rtm::get-list
		  list-text-view
		  :handler-fn (lambda (x) (make-nsstring (rtm::get-name x))))
    (handle-value rtm::get-location
		  location-text-view
		  :handler-fn (lambda (x) (make-nsstring  (rtm::get-name x))))))


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

;; TODO: make view map work.
(def-ibaction #/viewMap:   task-details-controller
  ;; Open small pane with webview on google maps, correctly placed.
  (declare (special *currently-selected-task*))
  ;; (format t "TODO - viewMap: ~s~%" (rtm::get-location *currently-selected-task*))
  (ccl::run-program "open" (list "http://maps.google.com")))

(def-ibaction #/goToLink:  task-details-controller
  ;; Open browser window on url page
  (declare (special *currently-selected-task*))
  (ccl::run-program "open" (list (rtm::get-url *currently-selected-task*))))

(def-ibaction #/completeTask:  task-details-controller
  (declare (special *rtm-controller*))
  (#/completeTask: *rtm-controller* self)
  ;; hide window.
  (hide-window self))

(def-ibaction #/saveTask: task-details-controller
  ;; this method adds a task, then closes itself.
  (declare (special *currently-selected-task*))
  (macrolet ((save-field (field-selector rtm-selector &key (when t)
					 (value-extractor '(lambda (x) (make-lisp-string (#/stringValue x)))))
	       `(let ((x (funcall #',value-extractor (funcall #',field-selector self))))
		  (when ,when
		    (funcall #',rtm-selector *currently-selected-task* x)))))
    ;; loop for the properties and do the rtm change operation on them.
    ;; TODO: combo boxes.
    (save-field name-text-view     rtm:rtm-change-task-name)
    (save-field estimate-text-view rtm:rtm-change-task-estimate)
    (save-field location-text-view rtm:rtm-change-task-location)
    (save-field link-text-view     rtm:rtm-change-task-url)
    (let ((has-due-p (< 0 (#/state (is-due-view self)))))
    (save-field due-date-view
		(lambda (task x)
		   (rtm:rtm-change-task-due-date task (if has-due-p x "")))
		:value-extractor (lambda (x)
				   (let ((out-formatter (make-instance 'ns:ns-date-formatter)))
				     (#/setDateFormat: out-formatter #@"YYYY-MM-dd'T'HH:mm:ss")
				     (make-lisp-string (#/stringFromDate: out-formatter (#/dateValue x)))))))
    (save-field priority-text-view rtm::rtm-change-task-priority
		:value-extractor (lambda (x) (make-lisp-string (#/labelForSegment: x (#/selectedSegment x)))))
    (save-field tags-token-view    rtm::rtm-change-task-tags))
  (hide-window self))


(defmethod hide-window ((self task-details-controller))
  (declare (special *rtm-controller*))
  ;; hide nib
  (#/orderOut: (#/window self) +null-ptr+)
  ;; redraw current task list again:
  (let ((rtmi (rtm-instance *rtm-controller*)))
    (update-current-tasklist)
    (let ((tableview (tasks-table-view (tasklist-controller *rtm-controller*))))
      (#/deselectAll: tableview nil)
      (#/reloadData tableview))
    (save-app-data rtmi)))


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
