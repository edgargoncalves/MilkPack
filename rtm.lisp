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
			     :message "You need to authorize RTM to give access to MilkPack. Continue to go to a browser and do that, then come back here.")
	  (handler-case
	      (request-rtm-authorization) ;; sets a new frob!
	    (error (condition) ;; frob is invalid!
	      (set-default-value #@"FrobKey" (make-nsstring ""))
	      ;; TODO: quit everything...
	      (proceed-with-authorization)))
	  (gui::alert-window :title "MilkPack System Settings"
			     :message "Your browser should be asking you now for authorization to use RTM. Press OK when that's done.")
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
  ((user-info     :initform nil :accessor rtm-user-info)
   (current-tasks :initform nil :accessor get-current-tasks :initarg  :current-tasks))
  (:metaclass ns:+ns-object))

(objc:defmethod #/requestAuth ((self rtm))
  (declare (ignore self))
  (set-default-value #@"FrobKey" (make-nsstring ""))
  ;; TODO: disable the button. enable the proceed button.
  (make-nsstring (proceed-with-authorization)))

(objc:defmethod #/proceed ((self rtm))
  (let ((output (make-nsstring (proceed-with-authorization))))
    (setf (rtm-user-info self) (rtm::init-rtm))
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

(defun save-app-data (rtm-instance)
  (let ((rtm-data rtm::*rtm-user-info*)
	(data-pathname (get-settings-file-name)))
    ;; compile lisp file with data object contents
    (cl-store:store rtm-data data-pathname)
    ;;set internal object
    (setf (rtm-user-info rtm-instance) rtm-data)))

(objc:defmethod (#/fetchData :void) ((self rtm))
  (declare (special *currently-selected-task-list*))
  ;; initialize rtm instance
  (rtm::init-rtm)
  (when *currently-selected-task-list*
    (setf (get-current-tasks self)
	  (rtm-lisp-api::get-tasks *currently-selected-task-list*)))
  (save-app-data self))

(objc:defmethod (#/loadDataFromDefaults :void) ((self rtm))
  (declare (special rtm::*rtm-user-info*))
  (let* ((data-pathname (get-settings-file-name)))

    ;;load byte-compiled file and restore the variable contents into rtm-data:
    (handler-case (let ((rtm-data (cl-store:restore data-pathname)))
		      (setf (rtm-user-info self) rtm-data
			    rtm::*rtm-user-info* rtm-data))
      (cl-store:restore-error ()
	(#/fetchData self)))))


(defun get-current-tasks-filtered-and-sorted ()
  (declare (special *currently-selected-task-list*))
  (when *currently-selected-task-list*
    (rtm::rtm-refresh-list *currently-selected-task-list*)
    (filter (lambda (x) (string= (rtm-lisp-api::get-completed x) ""))
      (rtm-lisp-api::get-tasks *currently-selected-task-list*))))



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
