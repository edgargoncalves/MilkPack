;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; rtmcontroller.lisp
;;;   - implementation of rtm-controller
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Controller class: RtmController
(defclass rtm-controller (ns:ns-object)
  ((authorize-on-web-button     :foreign-type :id :accessor authorize-on-web-button)
   (finish-authorization-button :foreign-type :id :accessor finish-authorization-button)
   (fetch-data-button           :foreign-type :id :accessor fetch-data-button)
   (load-data-button            :foreign-type :id :accessor load-data-button)
   (contacts-table-view         :foreign-type :id :accessor contacts-table-view)
   (lists-table-view            :foreign-type :id :accessor lists-table-view)
   (new-task-text-view          :foreign-type :id :accessor new-task-text-view)
   (add-task-hud                :foreign-type :id :accessor add-task-hud)
   (task-details-hud            :foreign-type :id :accessor task-details-hud)
   (tasklist-controller         :foreign-type :id :accessor tasklist-controller)
   (sidepanel-controller        :foreign-type :id :accessor sidepanel-controller)
   (date-formatter              :foreign-type :id :accessor date-formatter)
   (statusbar-menu              :foreign-type :id :accessor statusbar-menu)
   (statusbar-menu-view         :foreign-type :id :accessor statusbar-menu-view)
   (rtm-instance                :foreign-type :id :accessor rtm-instance))
  (:metaclass ns:+ns-object))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; bindings: 
(defmethod #/offlineTaskNumber ((self rtm-controller))
  (format t "i wanna know how many tasks: ~s~%" offline::offline-buffer)
  (make-nsstring (format nil "~a" 3))
  (length offline::offline-buffer))

(objc:defmethod (#/automaticallyNotifiesObserversForKey: :<BOOL>)
    ((self rtm-controller) theKey)
  (if (#/isEqualToString: #@"offlineTaskNumber" theKey)
      #$NO
      (call-next-method theKey)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; load ui parts controller functions.

(load-module "tasklist.lisp")
(load-module "sidebar-custom-cell.lisp")
(load-module "sidepanel.lisp")
(load-module "addtask.lisp")
(load-module "addlist.lisp")
(load-module "taskdetails.lisp")


(defvar *frob-key* #@"FrobKey")

;; Controller class initializer:
(objc:defmethod (#/initialize :void) ((self +rtm-controller))
  (declare (ignore self))  
  (let* ((domain (#/standardUserDefaults ns:ns-user-defaults))
	 (app-defaults (#/dictionaryWithObject:forKey: ns:ns-dictionary
						       #@"invalid-frob"
						       #@"FrobKey")))
    ;; (swank:create-swank-server 4005)
    (#/registerDefaults: domain app-defaults)
    ;; Let the current frob be taken from the defaults:
    (proceed-with-authorization)))

(defvar *rtm-controller* nil)

(objc:defmethod #/init ((self rtm-controller))
  (declare (special *rtm-controller*))
  ;; loads the rtm-instance and populates it with default values:
  (call-next-method)
  (setf *rtm-controller* self))



(objc:defmethod (#/applicationDidFinishLaunching: :void)
    ((self gui::lisp-application-delegate) notification)
  (declare (ignore notification)
	   (special *rtm-controller*))
  (#/loadDataFromDefaults: *rtm-controller* nil)
  ;; Assign a double-click action:
  (#/setDoubleAction: (tasks-table-view (tasklist-controller *rtm-controller*))
		      (GUI::@selector "doubleClick:")))


;; double click action:
(objc:defmethod (#/doubleClick: :void) ((self rtm-controller) sender)
  (declare (ignore sender)
	   (special *rtm-controller*))
  (#/showWindow: (task-details-hud *rtm-controller*) *rtm-controller*))


;; Controller action: authorize on web
(def-ibaction #/authorizeOnWeb: rtm-controller
  (#/requestAuth (rtm-instance self)))


;; Controller action: finish authorization
(def-ibaction #/finishAuthorizing: rtm-controller
  (#/proceed (rtm-instance self)))


(defmethod reload-contents ((self rtm-controller))
  (let ((contacts-table (contacts-table-view self))
	(sidepanel (sidepanel-controller self))
	(tasks-table-c (tasklist-controller self)))
    (unless (%null-ptr-p contacts-table)
      (#/reloadData contacts-table))
    (unless (%null-ptr-p tasks-table-c)
      (#/reloadData (tasks-table-view tasks-table-c)))
    (unless (%null-ptr-p sidepanel)
      (reload-sidepanel sidepanel))))

;; Controller action: fetch data from RTM
(def-ibaction #/fetchData: rtm-controller
  (let ((rtm-instance  (rtm-instance self)))
    (#/fetchData rtm-instance)
    (reload-contents self)))

;; Controller action: fetch data from disk
(def-ibaction #/loadDataFromDefaults: rtm-controller
  (let ((rtm-instance  (rtm-instance self)))
    (#/loadDataFromDefaults rtm-instance)
    (reload-contents self)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Task menu actions

(defmacro make-task-ibaction (name rtm-operation)
`(def-ibaction ,name rtm-controller
   (declare (special *currently-selected-task* *currently-selected-task-list*))
   (when (and *currently-selected-task-list* *currently-selected-task*)
     (let* ((rtmi (rtm-instance self))
	    (taskstableview (tasks-table-view (tasklist-controller self))))
       ;; operate on the task
       (,rtm-operation *currently-selected-task*)
       ;; redraw current task list again, with no selected task:
       (update-current-tasklist)
       (update-taskview-for-list *currently-selected-task-list*)
       (#/reloadData taskstableview)
       (save-app-data rtmi)))))

(make-task-ibaction #/deleteTask:   rtm:rtm-delete-task)
(make-task-ibaction #/completeTask: rtm:rtm-complete-task)
(make-task-ibaction #/postponeTask: rtm:rtm-postpone-task)

(def-ibaction #/addTask: rtm-controller
  (declare (special *currently-selected-task-list*))
  (let ((rtmi (rtm-instance self))
	(name (#/stringValue (new-task-text-view self))))
    ;; add the task
    (rtm:rtm-add-task *currently-selected-task-list* (make-lisp-string name) t)
    ;; cleanup field
    (#/setStringValue: (new-task-text-view self) #@"")
    ;; redraw current task list to include the new one.
    (update-current-tasklist)
    (#/reloadData (tasks-table-view (tasklist-controller self)))
    (save-app-data rtmi)))

(def-ibaction #/deleteList: rtm-controller
  (declare (special *currently-selected-task-list*))
  (when *currently-selected-task-list*
    (let ((rtmi (rtm-instance *rtm-controller*)))
      (rtm::rtm-delete-task-list *currently-selected-task-list*)
      (redraw-sidepanel) ;; fast operation
      (setf (rtm-user-info rtmi) rtm::*rtm-user-info*)
      (save-app-data rtmi))))

;; (make-list-ibaction #/editList: rtm::rtm-postpone-task)


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
