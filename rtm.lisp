;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Model layer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Configurable variables:


;;; Personal settings - do not include file in your published project:
;; (note that even though we don't include the file, any user that can access
;; the repl window will be able to see the var. be warned.)
(load RTM-API-KEY-PATH)


#|
;;; Instructions:
;;; 1. Clean the frob, ask RTM for a new one, and then get the token.

;; 2. Open the url in a browser window to gain authorization (example for
;; OpenMCL):
; (ccl::run-program "open" (list (rtm:rtm-api-build-auth-url)))

;; Then call the rtm:rtm-api-get-token function to set our token in `*RTM-API-TOKEN*'.
;(rtm:rtm-api-get-token)

|#

#|
1. check defaults for frob.
2. if there's one, get a token.
3. else, auth page. then store in defaults the frob.
4. with frob, call get-token.

|#

(defun request-rtm-authorization ()
  (ccl::run-program "open" (list (rtm:rtm-api-build-auth-url)))
  "ok")

(defun proceed-with-authorization (&optional second-time-p)
  (let ((frob-default-value (get-default-string #@"FrobKey")))
    ;; If we have non-empty frob and token, check the token.
    (if (string= frob-default-value "")
	;; Produces a new frob and requests a new token.
	(progn
	  (gui::alert-window :title "MilkPack System Settings"
			     :message "You must authorize Milkpack to access your Remember The Milk account. Pressing Okay will take you to a browser to do that. Come back to the MilkPack window afterwards.")
	  (handler-case
	      (request-rtm-authorization) ;; sets a new frob!
	    (error () ;; frob is invalid!
	      (set-default-value #@"FrobKey" (make-nsstring ""))
	      ;; TODO: quit everything... using #/terminate: on the nsapplication.
	      (gui::alert-window :title "MilkPack Authentication Error"
			     :message "Remember The Milk didn't give MilkPack permission. Are you offline? Press OK to quit this app, try again later.")
	      ;; (proceed-with-authorization)
	      (ccl:quit)))
	  (gui::alert-window :title "MilkPack System Settings"
			     :message "Your browser should have asked you for permission to use your online data. Press OK when that's done.")
	  ;;Set default frob.
	  (set-default-value #@"FrobKey" (make-nsstring rtm:*rtm-api-current-frob*))
	  (set-default-value #@"TokenKey" (make-nsstring ""))
	  (setf rtm:*rtm-api-token* "")
	  (proceed-with-authorization))
	;; We have a frob. Let's see about the token:
	(let ((token-default-value (get-default-string #@"TokenKey")))
	  (setf rtm:*rtm-api-current-frob* frob-default-value)
	  (handler-case
	      ;; If we have a token, check it. else, get a new one.
	      (if (string= token-default-value "")
		  (progn
		    ;; We got a new token. Proceed with it safely.
		    (rtm:rtm-api-get-token)
		    (set-default-value #@"TokenKey" (make-nsstring rtm:*rtm-api-token*)))
		  (progn
		    ;; We already have a valid token. Proceed using it.
		    (setf rtm:*rtm-api-token* token-default-value)
		    (rtm:rtm-api-check-token)
		    (set-default-value #@"TokenKey" (make-nsstring rtm:*rtm-api-token*))))
	    (error  (condition)
	      ;; The frob isn't valid, so let's get us a brand new token.
	      (set-default-value #@"FrobKey" (make-nsstring ""))
	      (if second-time-p
		  (gui::alert-window :title "MilkPack RTM Error"
				     :message (format nil "RTM returned an error message: ~a" condition))
		  (proceed-with-authorization t))))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Model entity: Rtm

(defclass rtm (ns:ns-object)
  ((user-info      :initform nil :accessor rtm-user-info)
   (active-list-id :initform ""  :accessor get-active-list-id)
   (task-lists     :foreign-type :id :accessor task-lists) ;; for use within combo boxes, for now.
   (locations     :foreign-type :id :accessor locations) ;; for use within combo boxes, for now.
   (non-smart-task-lists     :foreign-type :id :accessor non-smart-task-lists)) ;; for use within combo boxes, for now.
  (:metaclass ns:+ns-object))

(defclass nstasklist (ns:ns-object)
  ((name :foreign-type :id :accessor name)
   (is-smart :foreign-type :id :accessor is-smart)
   (rtm-task-list :initform nil :accessor rtm-task-list))
  (:metaclass ns:+ns-object))

(make-kvc-entity nstasklist #/name              #/setName:              name)
(make-kvc-entity nstasklist #/isSmart           #/setIsSmart:           is-smart :<BOOL>)

(make-kvc-array  rtm        #/taskLists         #/setTaskLists:         task-lists)
(make-kvc-array  rtm        #/nonSmartTaskLists #/setNonSmartTaskLists: non-smart-task-lists)

(defclass nslocation (ns:ns-object)
  ((name         :foreign-type :id :accessor name)
   (image        :foreign-type :id :accessor image)
   (image-url    :foreign-type :id :accessor image-url)
   (rtm-location :initform nil     :accessor rtm-location))
  (:metaclass ns:+ns-object))

(make-kvc-entity nslocation #/name      #/setName:      name)
(make-kvc-entity nslocation #/image     #/setImage:     image)
(make-kvc-entity nslocation #/imageUrl  #/setImageUrl:  image-url)
(make-kvc-array  rtm        #/locations #/setLocations: locations)



(defmethod get-active-list ((self rtm))
  "Retrieves from the RTM user info the list with id equal to active-listid."
  (rtm::find-by-id (get-active-list-id self) (rtm:get-task-lists-list)))

(defmethod set-active-list ((self rtm) (list rtm:task-list))
  (setf (get-active-list-id self) (rtm:get-id list)))

(objc:defmethod #/requestAuth ((self rtm))
  (declare (ignore self))
  (set-default-value #@"FrobKey" (make-nsstring ""))
  ;; TODO: disable the button. enable the proceed button.
  (make-nsstring (proceed-with-authorization)))


(defmethod update-rtm ((self rtm) contents)
  "updates the internal state of the application. can be used to update other state ramifications."
  (setf (rtm-user-info self) contents)
  ;; updates tasklists:
  (flet ((tasklists () (mapcar (lambda (rtm-tasklist)
				 (let ((list (make-instance 'nstasklist)))
				   (#/setName:    list  (make-nsstring (rtm:get-name rtm-tasklist)))
				   (#/setIsSmart: list  (if (rtm:is-smart rtm-tasklist) #$YES #$NO))
				   (setf (rtm-task-list list) rtm-tasklist)
				   list))
			       (rtm:get-task-lists contents)))
	 (locations () (mapcar (lambda (rtm-location)
				 (let* ((location (make-instance 'nslocation))
					(url (url-from-string (rtm:get-location-image-url rtm-location
											  :zoom 15
											  :maptype "hybrid"
											  :width 203
											  :height 119)))
					(image (#/initWithContentsOfURL: (#/alloc ns:ns-image) url)))
				   (#/setName:     location (make-nsstring (rtm:get-name rtm-location)))
				   (#/setImageUrl: location url)
				   (#/setImage: location image);;TODO: support no graphics locations.		   
				   (setf (rtm-location location) rtm-location)
				   location))
			       (rtm:get-locations contents))))
    ;;create the nsmutablearrays to be controlled by an nsarraycontroller (each with unique content instances)
    (#/setTaskLists: self
		    (convert-list-to-nsarray (tasklists) #'identity))
    (#/setNonSmartTaskLists: self 
			   (convert-list-to-nsarray
			    (filter (lambda (x) (not (rtm:is-smart (rtm-task-list x))))
				    (tasklists))
			    #'identity))
    (#/setLocations: self
		     (convert-list-to-nsarray (locations) #'identity))))

(objc:defmethod #/proceed ((self rtm))
  (let ((output (make-nsstring (proceed-with-authorization))))
    (update-rtm self (rtm:init-rtm))
    (with-connectivity
	(rtm:refresh-rtm :exclude-completed t))
    output))

(defconstant SETTINGS-FILE-NAME "rtm-data.txt")

(defun get-settings-file-name ()
  (declare (special SETTINGS-FILE-NAME))
  (let* ((preferences-path (make-lisp-string
			    (#/objectAtIndex:
			     (#_NSSearchPathForDirectoriesInDomains #$NSApplicationSupportDirectory
								    #$NSLocalDomainMask
								    #$YES)
			     0))))
    (format nil "~a/~a/~a" preferences-path PROJECT-NAME SETTINGS-FILE-NAME)))

(defun get-bundle-path ()
  (let* ((main-bundle (#/mainBundle ns:ns-bundle)))
    (make-lisp-string (#/resourcePath main-bundle))))

(defmethod save-app-data ((rtm-instance rtm))
  (declare (special rtm:*rtm-user-info*))
  (let ((rtm-data rtm:*rtm-user-info*)
	(data-pathname (get-settings-file-name)))
    ;; compile lisp file with data object contents
    (cl-store:store rtm-data data-pathname)
    ;;set internal object
    (update-rtm rtm-instance rtm-data)))

(objc:defmethod (#/fetchData :void) ((self rtm))
  ;; initialize rtm instance
  (rtm:init-rtm)
  (handler-case (rtm:refresh-rtm :exclude-completed t)
    (error (condition)
      (gui::alert-window :title "MilkPack Network Error"
			 :message (format nil "You are not online, please reconnect and relaunch MilkPack. Error: ~a" condition))))
  (save-app-data self))

(objc:defmethod (#/loadDataFromDefaults :void) ((self rtm))
  (declare (special rtm:*rtm-user-info*))
  (let* ((data-pathname (get-settings-file-name)))

    ;;load byte-compiled file and restore the variable contents into rtm-data:
    (handler-case (let ((rtm-data (cl-store:restore data-pathname)))
		    (update-rtm self rtm-data)
		    (setf rtm:*rtm-user-info* rtm-data))
      (cl-store:restore-error ()
	(#/fetchData self)))))


(defmethod get-current-tasks ((self rtm) &key refresh-from-server)
  (when (get-active-list self)
    (when (or refresh-from-server ;; TODO: check this for true offline work
	      (null (rtm:get-tasks (get-active-list self)))) ;; otherwise work faster, from memory
      (rtm:rtm-refresh-list (get-active-list self) :exclude-completed t))
    (rtm:get-tasks (get-active-list self))))


(defun sort-tasklist (tasklist)
  (setf (rtm:get-tasks tasklist)
	(sort-tasks (rtm:get-tasks tasklist))))

(defmethod update-current-tasklist ((self rtm) &key refresh-from-server)
  (when refresh-from-server
    (setf (rtm:get-tasks (get-active-list self))
	  (get-current-tasks self :refresh-from-server t)))
  (sort-tasklist (get-active-list self))
  (save-app-data self))

(defun sort-tasks (tasks)
  (let (pri1 pri2 pri3 priN)
      (dolist (task (sort tasks
			   #'string>
			   :key #'(lambda (x) (string-downcase (rtm:get-name x)))))
	(when (string= "" (rtm:get-completed task))
	  ;; task isn't complete, so include them in the appropriate bucket:
	  (cond ((string= (rtm:get-priority task) "1") (push task pri1))
		((string= (rtm:get-priority task) "2") (push task pri2))
		((string= (rtm:get-priority task) "3") (push task pri3))
		(t (push task priN)))))
      ;;append the bucckets ordingly
      (flet ((sort-dates (seq)
	       (sort seq #'(lambda (x y) (cond ((and (string= x "") (string> y "")) nil)
					  ((and (string= y "") (string> x "")) t)
					  (t (string< x y))))
		     :key #'rtm:get-due)))
	(append (sort-dates pri1)
		(sort-dates pri2)
		(sort-dates pri3)
		(sort-dates priN)))))


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
