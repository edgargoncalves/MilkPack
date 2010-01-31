;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Generic Utilities
(defmacro awhen (it &body action)
  `(let ((it ,it))
     (when it
       ,@action)))

(defmacro aif (it ant conseq)
  `(let ((it ,it))
     (if it
	 ,ant
	 ,conseq)))

(defmacro filter (fn &body lst)
  `(mapcan (lambda (x) (and (funcall ,fn x) (list x)))
	  ,(first lst)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Cocoa-Lisp Utilities

(defun make-nsstring (str)
  (unless (typep str 'ns:ns-mutable-string)
    (#/autorelease (ccl::%make-nsstring (if (stringp str) str (format nil "~a" str))))))

(defun make-lisp-string (nsstr)
  (ccl::lisp-string-from-nsstring nsstr))


(defun set-default-value (key value)
  (let ((defaults (#/standardUserDefaults ns:ns-user-defaults)))
    (#/setObject:forKey: defaults value key)))

(defun get-default-string (key)
  (let ((domain (#/standardUserDefaults ns:ns-user-defaults)))
    (make-lisp-string (#/stringForKey: domain key))))

;; KVC syntactic sugar ftw!! :d 
(defmacro make-kvc-entity (class getter setter slot &optional (type :id))
  "Use `MAKE-KVC-ENTITY' to create the right objective c methods for single entity bindings to work well."
  `(progn
     (objc:defmethod (,setter :void) ((self ,class) (a ,type))
       (unless (eq (slot-value self ',slot) a)
	 (setf (slot-value self ',slot) a)))
     
     (objc:defmethod (,getter ,type) ((self ,class))
       (slot-value self ',slot))))

(defmacro make-kvc-array (class getter setter slot &optional (type :id))
  "Use `MAKE-KVC-ARRAY' to create the right objective c methods for array bindings to work well."
  `(progn
     (objc:defmethod (,setter :void) ((self ,class) (a ,type))
       (unless (eq (slot-value self ',slot) a)
	 (#/retain a)
	 (#/release (slot-value self ',slot))
	 (setf (slot-value self ',slot) a)))
     (objc:defmethod (,getter ,type) ((self ,class))
       (slot-value self ',slot))))


(defun convert-list-to-nsarray (list &optional (elt-handler #'make-nsstring))
  (let ((array (#/array ns:ns-mutable-array)))
    (dolist (elt list)
      (#/addObject: array (funcall elt-handler elt)))
    array))

(defmethod find-in-nsarray ((array ns:ns-array) value &key (test #'equal) (key #'identity))
  (dotimes (i (#/count array))
    (let ((obj (#/objectAtIndex: array i)))
      (when (funcall test value (funcall key obj))
	(return obj)))))



(defmacro def-ibaction (name controller-name &body body)
  `(objc:defmethod (,name :void) ((self ,controller-name) sender)
     (declare (ignorable self sender))
    ,@body))

(defmacro url-from-string (s)
  `(ccl::with-autorelease-pool
    (#/retain (#/URLWithString: ns:ns-url (make-nsstring (string ,s))))))


(defmacro with-connectivity (&body body)
  `(handler-case (progn ,@body)
     (error (condition)
       (gui::alert-window :title "MilkPack Network Error"
			  :message (format nil "You are not online, please reconnect and relaunch MilkPack. Error: ~a" condition)))))


#|
Copyright 2009, 2010 Edgar Gonçalves

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
