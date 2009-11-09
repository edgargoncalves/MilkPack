;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HUD window to add lists. controller specification.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass add-list-controller (ns:ns-window-controller)
  ((name-text-view     :foreign-type :id :accessor name-text-view)
   (search-text-view   :foreign-type :id :accessor search-text-view)
   (rtm-instance       :foreign-type :id :accessor rtm-instance :initarg :rtm))
  (:metaclass ns:+ns-object))

;; this method adds a task, then closes itself.
(def-ibaction #/addList: add-list-controller
  (declare (special *rtm-controller*))
  (let ((rtmi (rtm-instance self))
	(name   (make-lisp-string (#/stringValue (name-text-view self))))
	(filter (make-lisp-string (#/stringValue (search-text-view self)))))
    ;; add the list
    (rtm::rtm-add-task-list name (unless (string= "" filter) filter))
    ;; hide nib
    (#/orderOut: (#/window self) +null-ptr+)
    ;; cleanup fields
    (#/setStringValue: (name-text-view self)   #@"")
    (#/setStringValue: (search-text-view self) #@"")
    ;; redraw sidebar to include the new one.
    (redraw-sidepanel)
    (save-app-data rtmi)))
