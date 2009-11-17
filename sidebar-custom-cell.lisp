;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sidebar-custom-cell.lisp
;;;   - implementation of a custom NSTextFieldCell, adding images.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass sidebar-custom-cell (ns:ns-text-field-cell)
  ((image      :foreign-type :id :accessor image)
   (text       :foreign-type :id :accessor text)
   (task-count :foreign-type :id :accessor task-count))
  (:metaclass ns:+ns-object))


(objc:defmethod (#/imageFrameForCellFrame: :<NSR>ect) ((self sidebar-custom-cell) (cell-frame :<NSR>ect))
  (if (%null-ptr-p (image self))
      #$NSZeroRect
      (let ((image-size (#/size (image self))))
	(ns:make-ns-rect (+ 5 (ns:ns-rect-x cell-frame))
			 (+ (ceiling (/ (- (ns:ns-rect-height cell-frame) (ns:ns-size-height image-size)) 2))
			    (ns:ns-rect-y cell-frame))
			 (ns:ns-size-width image-size)
			 (ns:ns-size-height image-size)))))

(objc:defmethod (#/drawWithFrame:inView: :void) ((self sidebar-custom-cell) (cell-frame :<NSR>ect) control-view)
  (if (%null-ptr-p (image self))
      (call-next-method cell-frame control-view)
      (let* ((image-size (#/size (image self)))
	     (image-frame (ns:make-ns-rect (ns:ns-rect-x cell-frame)
					   (ns:ns-rect-y cell-frame)
					   (+ 5 (ns:ns-size-width image-size))
					   (ns:ns-rect-height cell-frame))))
	
	(decf (ns:ns-rect-width cell-frame) (ns:ns-rect-width image-frame))
	(incf (ns:ns-rect-x cell-frame) (ns:ns-rect-width image-frame))

	(when (#/drawsBackground self)
	  (#/set (#/backgroundColor self))
	  (#_NSRectFill image-frame))

	(incf (ns:ns-rect-x image-frame) 5)
	(setf (ns:ns-rect-width image-frame) (ns:ns-size-width image-size))
	(setf (ns:ns-rect-height image-frame) (ns:ns-size-height image-size))
	(incf (ns:ns-rect-y image-frame)
	      (ceiling (/ (funcall (if (#/isFlipped control-view) #'- #'+)
			   (ns:ns-rect-height cell-frame)
			   (ns:ns-rect-height image-frame)) 2)))
	(#/setFlipped: (image self) #$YES)
	(#/drawAtPoint:fromRect:operation:fraction: (image self)
						    (ns:make-ns-point (ns:ns-rect-x image-frame)
								      (ns:ns-rect-y image-frame))
						    #$NSZeroRect
						    #$NSCompositeSourceOver
						    1)
	(call-next-method cell-frame control-view))))

(objc:defmethod (#/cellSize :<NSS>ize) ((self sidebar-custom-cell))
  (let ((cell-size (call-next-method)))
    (incf (ns:ns-size-width cell-size) (+ (if (%null-ptr-p (image self))
					      0
					      (ns:ns-size-width (image self)))
					  5))
    cell-size))
