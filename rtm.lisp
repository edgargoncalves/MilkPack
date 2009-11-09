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

(defun proceed-with-authorization ()
  (let ((frob-default-value (get-default-string #@"FrobKey")))
    ;; If we have non-empty frob and token, check the token.
    (if (string= frob-default-value "")
	;; Request a new frob and token.
	(progn
	  ;; (format t "Opening an authorization webpage... and getting a new FROB!~%")
	  (request-rtm-authorization) ;; sets a new frob!
	  ;;Set default frob.
	  (set-default-value #@"FrobKey" (make-nsstring rtm:*rtm-api-current-frob*))
	  (set-default-value #@"TokenKey" (make-nsstring ""))
	  (setf rtm:*rtm-api-token* "")
	  "Please click on proceed to finish authorization.")
	(let ((token-default-value (get-default-string #@"TokenKey")))
	  (setf rtm:*rtm-api-current-frob* frob-default-value)
	  ;; If we have a token, check it. else, get a new one.
	  (if (string= token-default-value "")
	      (handler-case
		  (progn
		    ;; (format t "PROCEED-WITH-AUTHORIZATION: getting token using frob: ~s~%" rtm:*rtm-api-current-frob*)
		    (rtm:rtm-api-get-token)
		    (set-default-value #@"TokenKey" (make-nsstring rtm:*rtm-api-token*))
		    "You may proceed, got a new token.")
		(error  (condition)
		  (set-default-value #@"FrobKey" (make-nsstring ""))
		  (format t "RTM returned an error when getting token: ~s~%" condition)
		  (proceed-with-authorization)))
	      (handler-case
		  (progn
		    ;; (format t "PROCEED-WITH-AUTHORIZATION: checking token using frob: ~s~%" frob-default-value)
		    (setf rtm:*rtm-api-token* token-default-value)
		    (rtm:rtm-api-check-token)
		    (set-default-value #@"TokenKey" (make-nsstring rtm:*rtm-api-token*))
		    "You may proceed, using the old token.")
		(error  (condition)
		  (set-default-value #@"FrobKey" (make-nsstring ""))
		  (format t "RTM returned an error when checking token: ~s~%" condition)
		  (proceed-with-authorization))))))))


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

(defvar *settings-file-name* "rtm-data.txt")

(defun save-app-data (rtm-instance)
  (declare (special *settings-file-name*))
  (let* ((rtm-data rtm::*rtm-user-info*)
	 (main-bundle (#/mainBundle ns:ns-bundle))
	 (resource-path (make-lisp-string (#/resourcePath main-bundle)))
	 (data-pathname (concatenate 'string resource-path "/" *settings-file-name*)))
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
  (declare (special rtm::*rtm-user-info* *settings-file-name*))
  (let* ((main-bundle (#/mainBundle ns:ns-bundle))
	 (resource-path (make-lisp-string (#/resourcePath main-bundle)))
	 (data-pathname (concatenate 'string resource-path "/" *settings-file-name*)))

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
