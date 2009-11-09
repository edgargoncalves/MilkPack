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

(defun get-regular-lists (rtm-info)
  (filter (lambda (task)
	    (let ((str (rtm-lisp-api::get-name task)))
	      (not (or (string= (subseq str 0 2) "p.")
		       (string= (subseq str 0 1) "*")
		       (string= (subseq str 0 1) "@")
		       (rtm-lisp-api::is-smart task)))))
    (rtm-lisp-api::get-task-lists rtm-info)))

(defun get-smart-lists (rtm-info)
  (filter (lambda (task)
	    (let ((str (rtm-lisp-api::get-name task)))
	      (not (or (string= (subseq str 0 2) "p.")
		       (string= (subseq str 0 1) "*")
		       (string= (subseq str 0 1) "@")
		       (not (rtm-lisp-api::is-smart task))))))
    (rtm-lisp-api::get-task-lists rtm-info)))

(defun get-project-lists (rtm-info)
  (filter (lambda (task) (string= (subseq (rtm-lisp-api::get-name task) 0 2) "p."))
    (rtm-lisp-api::get-task-lists rtm-info)))

(defun get-bucket-lists (rtm-info)
  (filter (lambda (task) (string= (subseq (rtm-lisp-api::get-name task) 0 1) "*"))
    (rtm-lisp-api::get-task-lists rtm-info)))

(defun get-context-lists (rtm-info)
  (filter (lambda (task) (string= (subseq (rtm-lisp-api::get-name task) 0 1) "@"))
    (rtm-lisp-api::get-task-lists rtm-info)))


(defmethod refresh-sidetree-contents ((self sidepanel-controller))
  "sets the nsmutabledictionary to hold all the relevant content. dynamically refreshes contents."
  (declare (special *sidetree-lists* *sidetree-root*))
  (flet ((convert-list-to-nsarray (list)
	   (let ((array (#/array ns:ns-mutable-array)))
	     (dolist (elt list)
	       (#/addObject: array (make-nsstring elt)))
	     array)))
    (let ((rtm-info (rtm-user-info (rtm-instance self))))
      (unless *sidetree-lists*
	(setf *sidetree-lists*
	      `((,(make-nsstring "Smart Lists")  . ,(get-smart-lists rtm-info))
		(,(make-nsstring "Buckets")      . ,(get-bucket-lists rtm-info))
		(,(make-nsstring "Projects")     . ,(get-project-lists rtm-info))
		(,(make-nsstring "Next Actions") . ,(get-context-lists rtm-info))
		(,(make-nsstring "Main")         . ,(get-regular-lists rtm-info)))))
      (let ((dict (make-instance 'ns:ns-mutable-dictionary :with-capacity 2)))
	(dolist (l *sidetree-lists*)
	  (let ((name (car l))
		(lists (cdr l)))
	    (#/setObject:forKey: dict
				 (convert-list-to-nsarray
				  (mapcar #'rtm-lisp-api::get-name lists))
				 name)))
	(setf *sidetree-root* dict)))))

(defun redraw-sidepanel ()
  (declare (special *rtm-controller* rtm::*rtm-user-info* *sidetree-lists*))
  (let* ((sidepanel (sidepanel-controller *rtm-controller*))
	   (lists-outline (lists-outline-view sidepanel)))
    (rtm::rtm-list-task-lists)
    (setf *sidetree-lists* nil)
    (setf (rtm-user-info (rtm-instance *rtm-controller*)) rtm::*rtm-user-info*)
    (refresh-sidetree-contents sidepanel)
    (#/reloadData lists-outline)
    (#/expandItem:expandChildren: lists-outline nil t)
    (let ((indexes (make-instance 'ns:ns-index-set)))
      (#/initWithIndex: indexes 1)
      (#/selectRowIndexes:byExtendingSelection: lists-outline indexes nil))))

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
	 ;; (format t "numberOfChildren: item ~s, type ~s~%" item (type-of item))
	 (#/count item))
	(t
	 ;; (format t "numberOfChildren: 0 item ~s, type ~s~%" item (type-of item))
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
  (cond ((%null-ptr-p item)
	 (#/objectForKey: *sidetree-root*
			  (#/objectAtIndex: (#/allKeys *sidetree-root*)
					    index)))
	((typep item 'ns:ns-mutable-array)
	 ;; (format t "chilcdOfItem: @array item ~s~%" item)
	 (#/objectAtIndex: item index))
	(t
	 nil)))

(objc:defmethod #/outlineView:objectValueForTableColumn:byItem:
    ((self sidepanel-controller)
     (outline-view (:* (:struct :<NSO>utline<V>iew)))
     (table-column (:* (:struct :<NST>able<C>olumn)))
     item)
  (cond ((typep item 'ns:ns-mutable-array)
	 ;;get parent's (root) used key
	 (get-sidetree-item-key item))
	((typep item 'ns:ns-string)
	 item)
	((typep item 'ns:ns-dictionary)
	 #@"nsdictionary")
	(t
	 (format t "objectValueFor...: @t item ~s~%" item)
	 nil)))

;; Identify groups on the sidebar:
(objc:defmethod (#/outlineView:isGroupItem: :<BOOL>)
    ((self sidepanel-controller) outline-view item)
  (declare (ignore outline-view))
  (or (typep item 'ns:ns-dictionary)
	  (typep item 'ns:ns-mutable-array)))

;; Make group letters be uppercased:
(objc:defmethod (#/outlineView:willDisplayCell:forTableColumn:item: :void)
    ((self sidepanel-controller) outline-view cell table-column item)
  (declare (ignore table-column outline-view))
  (when (typep item 'ns:ns-mutable-array)
    (let ((new-title (#/mutableCopy (#/attributedStringValue cell))))
      (#/replaceCharactersInRange:withString: new-title
					      (ns:make-ns-range 0 (#/length new-title))
					      (#/uppercaseString (#/string new-title)))
      (#/setAttributedStringValue: cell new-title)
      (#/release new-title))))

;; Make groups unselectable:
(objc:defmethod (#/outlineView:shouldSelectItem: :<BOOL>)
    ((self sidepanel-controller) outline-view item)
  (declare (ignore outline-view))
  (if (typep item 'ns:ns-mutable-array) nil t))

;; delegate message to change selection:
(objc:defmethod (#/outlineViewSelectionDidChange: :void)
    ((self sidepanel-controller) notification)
  (declare (special *currently-selected-task-list* *sidetree-lists* *rtm-controller*))
  ;; Note, this method works for single selections only, for now.
  (let* ((changed-table (#/object notification))
	 (selection (#/selectedRowIndexes changed-table))
	 (rtm-instance (rtm-instance self)))
    (when (= 1 (#/count selection))
      (let* ((1st-index (#/firstIndex selection))
	     (item  (#/itemAtRow: changed-table 1st-index)))
	(when (typep item 'ns:ns-string)
	  (let* ((parent (#/parentForItem: changed-table item))
		 (parent-idx (#/rowForItem: changed-table parent)))
	    ;; find the item at 1st-index on the table indexed by parent-idx
	    ;; by substracting the parent-idx we identify indexes on all groups properly.
	    (awhen (nth (- (- 1st-index parent-idx) 1)
			(let ((key (get-sidetree-item-key parent)))
			  (cdr (assoc key *sidetree-lists* :test #'equal))))
	      (setf *currently-selected-task-list* it)
	      (setf (get-current-tasks rtm-instance) (get-current-tasks-filtered-and-sorted))))))
      (#/reloadData (tasks-table-view *rtm-controller*)))))

;;; End of side panel implementation
