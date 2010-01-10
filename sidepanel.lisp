;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sidepanel.lisp
;;;   - implementation of an NSOutlineView with groups like in Finder.app
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass sidepanel-controller (ns:ns-object)
   ((lists-outline-view           :foreign-type :id :accessor lists-outline-view)
    (rtm-instance                 :foreign-type :id :accessor rtm-instance))
  (:metaclass ns:+ns-object))

(defvar *currently-selected-task-list* nil)
(defvar *sidetree-lists* nil)
(defvar *sidetree-root* nil)

;;; To implement an outline view data source for the side panel (source list):
(defclass task-list-sidebar-group ()
  ((name         :accessor name         :initarg :name)
   (lists        :accessor lists        :initarg :lists :initform nil)
   (icon         :accessor icon         :initarg :icon)
   (iconimg      :accessor iconimg      :initform nil)
   (list-nsnames :accessor list-nsnames :initform nil)))

(defun find-list-group (list-nsname)
  (declare (special *sidetree-lists*))
  ;;find group with proper list
  (or (dolist (group *sidetree-lists*)
	(when (member list-nsname (list-nsnames group))
	  (return group)))
      (error "FIND-LIST-GROUP: list not found on any group, check it: ~s" (make-lisp-string list-nsname))))

(defun create-task-list-groups (rtm-info &key needs-server-refresh)
  (let ((groups nil)
	(task-list-types
	 ;; (name-nsstring
	 ;;  icon-string
	 ;;  (exclude-list-of-prefixes exclude-task-function)
	 ;;  (include-list-of-prefixes include-task-function)
	 `((,#@"Main"        "main.icns"  (("p." "*" "@") rtm:is-smart) nil)
	   (,#@"Smart Lists" "smart.icns" (("p." "*" "@") nil) (nil rtm:is-smart))
	   (,#@"Projects"     "project.icns" nil (("p.") nil))
	   (,#@"Buckets"      "bucket.icns" nil (("*") nil))
	   (,#@"Next Actions" "context.icns"    nil (("@") nil)))))
    
    (when (or needs-server-refresh
	      (null (rtm:get-task-lists rtm-info)))
      ;; Update lists from server:
      (rtm:refresh-task-lists-list))
    
    (dolist (tasklist (reverse (rtm:get-task-lists rtm-info)))
      (let ((list-name (rtm:get-name tasklist)))
	(awhen (dolist (type task-list-types)
		 (let ((exclude-fn (second (third type)))
		       (include-fn (second (fourth type))))
		   (when (and (not (dolist (prefix (first (third type)))
				     (when (string= (subseq list-name 0 (length prefix)) prefix)
				       (return t))))
			      (not (when exclude-fn (funcall exclude-fn tasklist)))
			      (not (dolist (prefix (first (fourth type)))
				     (unless (string= (subseq list-name 0 (length prefix)) prefix)
				       (return t))))
			      (if include-fn (funcall include-fn tasklist) t))
		     ;; we got a match on this type:
		     (return type))))
	  ;; we found a type, so use it:
	  (let ((group (find (first it) groups :key #'name :test #'#/isEqualToString:)))
	    (unless group
	      ;;unless there's a group with this name, create one
	      (setf group (make-instance 'task-list-sidebar-group
					 :name (first it)
					 :icon (second it)))
	      (push group groups))
	    ;; add tasklist to its lists;
	    (push tasklist (lists group))))))
    groups))


(defmethod refresh-sidetree-contents ((self sidepanel-controller) &key needs-server-refresh)
  "sets the nsmutabledictionary to hold all the relevant content. dynamically refreshes contents."
  (declare (special *sidetree-lists* *sidetree-root*))
  (let ((rtm-info (rtm-user-info (rtm-instance self))))
    ;; Prepare sidetree-lists in case they are empty
    ;; hint: set them to nil before, for a refresh.
    (unless *sidetree-lists*
      (setf *sidetree-lists* (create-task-list-groups rtm-info :needs-server-refresh needs-server-refresh))))
  ;; create ns-object with sidebar contents. dic(string<->array(string)).
  (let ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 2)))
    (dolist (group *sidetree-lists*) ;; group is of type task-list-sidebar-group.
      (let ((group-name (name group))
	    (lists (lists group)))
	(#/setObject:forKey: dict
			     (convert-list-to-nsarray
			      lists
			      (lambda (task-list)
				(let ((nsname (make-nsstring (rtm:get-name task-list))))
				  (push nsname (list-nsnames group))
				  nsname)))
			     group-name)))
    (setf *sidetree-root* dict)))

(defun redraw-sidepanel (&key (needs-server-refresh-p nil))
  (declare (special *rtm-controller* rtm:*rtm-user-info* *sidetree-lists*))
  (let* ((sidepanel (sidepanel-controller *rtm-controller*))
	   (lists-outline (lists-outline-view sidepanel)))
    (setf *sidetree-lists* nil)
    (setf (rtm-user-info (rtm-instance *rtm-controller*))
	  rtm:*rtm-user-info*)
    (refresh-sidetree-contents sidepanel :needs-server-refresh needs-server-refresh-p)
    (#/reloadData lists-outline)
    (#/expandItem:expandChildren: lists-outline nil t)
    (let ((indexes (make-instance 'ns:ns-index-set)))
      (#/initWithIndex: indexes 1)
      (#/selectRowIndexes:byExtendingSelection: lists-outline indexes nil))))


(defmethod reload-sidepanel ((self sidepanel-controller) &key needs-server-refresh-p)
  (let* ((lists-outline (lists-outline-view self))
	 (table-column (#/tableColumnWithIdentifier: lists-outline #@"list")))
    (unless (typep (#/dataCell table-column) 'sidebar-custom-cell)
      (let ((cell (make-instance 'sidebar-custom-cell)))
	(#/setEditable: cell #$YES)
	(#/setDataCell: table-column cell)))
    (redraw-sidepanel :needs-server-refresh-p needs-server-refresh-p)))

;; Controller action: fetch lists from RTM and redraw sidepanel
(def-ibaction #/refreshLists: sidepanel-controller
  (declare (special *currently-selected-task-list*))
  ;;TODO: change name to refreshCurrentList
  ;; Since fetchData and loadDataFromDefaults already reload all lists,
  ;;    this button only syncs the current one:
  (update-current-tasklist :refresh-from-server t)
  (update-taskview-for-list *currently-selected-task-list*))


(defun get-sidetree-item-key (item)
  (declare (special *sidetree-root*))
  "Gets the key used to store a given item (assumes a 1-1 relationship)"
  (let ((key-array (#/allKeysForObject: *sidetree-root* item)))
    (#/objectAtIndex: key-array 0)))

;; Data Source methods

(objc:defmethod (#/outlineView:numberOfChildrenOfItem: :<NSI>nteger)
    ((self sidepanel-controller)
     (outline-view (:* (:struct :<NSO>utline<V>iew)))
     item)
  (declare (special *sidetree-root*))
  (cond ((or (null item)
	     (%null-ptr-p item))
	 (if *sidetree-root* (#/count *sidetree-root*) 0))
	((or (typep item 'ns:ns-mutable-dictionary)
	     (typep item 'ns:ns-mutable-array))
	 (#/count item))
	(t
	 0)))

(objc:defmethod (#/outlineView:isItemExpandable: :<BOOL>)
    ((self sidepanel-controller) outline-view item)
  (declare (ignore outline-view))
  (and (or (typep item 'ns:ns-mutable-dictionary)
	   (typep item 'ns:ns-mutable-array))
       (> (#/count item) 0)))

(objc:defmethod #/outlineView:child:ofItem:
    ((self sidepanel-controller)
     outline-view
     (index :<NSI>nteger)
     item)
  (declare (ignore outline-view))
  (declare (special *sidetree-root*))
  (cond ((%null-ptr-p item) ;; root
	 (#/objectForKey: *sidetree-root*
			  (#/objectAtIndex: (#/allKeys *sidetree-root*)
					    index)))
	((typep item 'ns:ns-mutable-array) ;;group item
	 (#/objectAtIndex: item index))
	(t
	 nil)))

(objc:defmethod #/outlineView:objectValueForTableColumn:byItem:
    ((self sidepanel-controller)
     (outline-view (:* (:struct :<NSO>utline<V>iew)))
     (table-column (:* (:struct :<NST>able<C>olumn)))
     item)
  (cond ((and (typep item 'ns:ns-mutable-array)  ;; for now, we don't want anything here.
	      (#/isEqualToString: (#/identifier table-column) #@"count"))
	 #@"")
	((typep item 'ns:ns-mutable-array) ;;get parent's (root) used key
	 (get-sidetree-item-key item))
	((and (typep item 'ns:ns-string)  ;; we want the badge. number of tasks on this list.
	      (#/isEqualToString: (#/identifier table-column) #@"count"))
	 (make-nsstring (get-count-for-list-named (make-lisp-string item) :filter-complete t)))
	((typep item 'ns:ns-string) ;; we want the list name. (we've got it!)
	 item)
	((typep item 'ns:ns-dictionary) ;; root, shouldn't appear
	 #@"nsdictionary")
	(t
	 nil)))

;; Identify groups on the sidebar:
(objc:defmethod (#/outlineView:isGroupItem: :<BOOL>)
    ((self sidepanel-controller) outline-view item)
  (declare (ignore outline-view))
  (or (typep item 'ns:ns-dictionary)
      (typep item 'ns:ns-mutable-array)))


(defmethod get-ns-image ((group task-list-sidebar-group))
  "Loads an image, or uses an already loaded image. Also sets its size."
  (unless (iconimg group)
    (setf (iconimg group) (#/imageNamed: (find-class 'ns:ns-image)
					 (make-nsstring (icon group))))
    (#/setSize: (iconimg group) (ns:make-ns-size 24 24)))
  (iconimg group))

;; Make group letters be uppercased/ places images+badges on lists:
(objc:defmethod (#/outlineView:willDisplayCell:forTableColumn:item: :void)
    ((self sidepanel-controller) outline-view cell table-column item)
  (declare (ignore outline-view))
  (unless (#/isEqualToString: (#/identifier table-column) #@"count")
    (when (and (not (%null-ptr-p cell))
	       (typep item 'ns:ns-string)) ;; it's a list name
      ;; fetch images
      (setf (image cell) (get-ns-image (find-list-group item))))
    (when (typep item 'ns:ns-mutable-array) ;; It's a group name
      ;; put in uppercase with no image
      (let ((new-title (#/mutableCopy (#/attributedStringValue cell))))
	(#/replaceCharactersInRange:withString: new-title
						(ns:make-ns-range 0 (#/length new-title))
						(#/uppercaseString (#/string new-title)))
	(#/setAttributedStringValue: cell new-title)
	(setf (image cell) +null-ptr+)
	(#/release new-title)))))

;; Make groups unselectable:
(objc:defmethod (#/outlineView:shouldSelectItem: :<BOOL>)
    ((self sidepanel-controller) outline-view item)
  (declare (ignore outline-view))
  (if (typep item 'ns:ns-mutable-array) nil t))

;; delegate message to change selection:
(objc:defmethod (#/outlineViewSelectionDidChange: :void)
    ((self sidepanel-controller) notification)
  ;; Note, this method works for single selections only, for now.
  (let* ((changed-table (#/object notification))
	 (selection (#/selectedRowIndexes changed-table)))
    (when (= 1 (#/count selection))
      (let* ((1st-index (#/firstIndex selection))
	     (item  (#/itemAtRow: changed-table 1st-index)))
	(when (typep item 'ns:ns-string)
	  (let* ((parent (#/parentForItem: changed-table item))
		 (parent-idx (#/rowForItem: changed-table parent)))
	    ;; find the item at 1st-index on the table indexed by parent-idx
	    ;; by substracting the parent-idx we identify indexes on all groups properly.
	    (awhen (nth (- (- 1st-index parent-idx) 1)
			(lists (find-list-group item)))
	      ;; Change the list:
	      (update-taskview-for-list it))))))))

(defun update-taskview-for-list (list)
  (declare (special *currently-selected-task-list* *rtm-controller*))
  (setf *currently-selected-task-list* list)
  (let ((view (tasks-table-view (tasklist-controller *rtm-controller*))))
    ;; remove the selection:
    (#/deselectAll: view nil)
    ;; reload task list view
    (#/reloadData view)))

;;; End of side panel implementation


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
