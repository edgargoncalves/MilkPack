;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HUD window to add tasks. controller specification.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass add-task-controller (ns:ns-window-controller)
  ((name-text-view     :foreign-type :id :accessor name-text-view)
   (rtm-instance       :foreign-type :id :accessor rtm-instance :initarg :rtm))
  (:metaclass ns:+ns-object))

;; this method adds a task, then closes itself.
(def-ibaction #/addTask: add-task-controller
  (declare (special *rtm-controller* *currently-selected-task-list*))
  (let ((rtmi (rtm-instance self))
	(name (#/stringValue (name-text-view self))))
    ;; add the task
    (rtm::rtm-add-task *currently-selected-task-list* (make-lisp-string name) t)
    ;; hide nib
    (#/orderOut: (#/window self) +null-ptr+)
    ;; cleanup fields
    (#/setStringValue: (name-text-view self) #@"")
    ;; redraw current task list to include the new one.
    (setf (get-current-tasks rtmi) (get-current-tasks-filtered-and-sorted))
    (#/reloadData (tasks-table-view *rtm-controller*))
    (save-app-data rtmi)))
