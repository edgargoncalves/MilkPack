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
    (rtm::rtm-add-task-list name (unless (string= "" filter) filter) :offline nil) ;; for now, don't run it offline. TODO - fix this, order lists properly.
    ;; hide nib
    (#/orderOut: (#/window self) +null-ptr+)
    ;; cleanup fields
    (#/setStringValue: (name-text-view self)   #@"")
    (#/setStringValue: (search-text-view self) #@"")
    ;; redraw sidebar to include the new one.
    (redraw-sidepanel)
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
