;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; HUD window to add tasks. controller specification.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass add-task-controller (ns:ns-window-controller)
  ((name-text-view     :foreign-type :id :accessor name-text-view)
   (rtm-instance       :foreign-type :id :accessor rtm-instance :initarg :rtm))
  (:metaclass ns:+ns-object))

;; this method adds a task, then closes itself.
(def-ibaction #/addTask: add-task-controller
  (declare (special *rtm-controller*))
  (let ((rtmi (rtm-instance self))
	(name (#/stringValue (name-text-view self))))
    ;; add the task
    (rtm::rtm-add-task (get-active-list rtmi) (make-lisp-string name) t)
    ;; hide nib
    (#/orderOut: (#/window self) +null-ptr+)
    ;; cleanup fields
    (#/setStringValue: (name-text-view self) #@"")
    ;; redraw current task list to include the new one.
    (update-current-tasklist rtmi)
    (#/reloadData (tasks-table-view (tasklist-controller *rtm-controller*)))
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
