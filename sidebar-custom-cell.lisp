;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; sidebar-custom-cell.lisp
;;;   - implementation of a custom NSTextFieldCell, adding images.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass sidebar-custom-cell (ns:ns-text-field-cell)
  ((image      :foreign-type :id :accessor image)
   (badge      :foreign-type :id :accessor badge)
   (badge-count :accessor badge-count :initform :badge-count))
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
  (unless (%null-ptr-p (image self))
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
	(#/drawAtPoint:fromRect:operation:fraction:
	 (image self)
	 (ns:make-ns-point (ns:ns-rect-x image-frame)
			   (ns:ns-rect-y image-frame))
	 #$NSZeroRect
	 #$NSCompositeSourceOver
	 1)))
  (call-next-method cell-frame control-view))

(objc:defmethod (#/cellSize :<NSS>ize) ((self sidebar-custom-cell))
  (let ((cell-size (call-next-method)))
    (incf (ns:ns-size-width cell-size) (+ (if (%null-ptr-p (image self))
					      0
					      (ns:ns-size-width (image self)))
					  5))
    cell-size))


;; ;; Badge:
;; (objc:defmethod (#/drawInteriorWithFrame:inView: :void) ((self sidebar-custom-cell) (cell-frame :<NSR>ect) control-view)

;;   ;; Draw NSBrowserCell.
;;   (call-next-method cell-frame control-view)
    
;;   ;; Set up badge string and size.
;;   (let* ((badge (make-nsstring (format nil "~d" (badge-count self)))) ;; NSString
;; 	 (badge-num-size (#/sizeWithAttributes badge nil))             ;; NSSize
;; 	 (badge-width (+ (ns:ns-size-width badge-num-size)
;; 			 (* 2 BADGE_BUFFER_LEFT))))
;;     (when (< badge-width BADGE_TEXT_SMALL)
;;       ;; The text is too short. Decrease the badge's size.
;;       (setf badge-width BADGE_TEXT_SMALL))

;;     (let* ((badge-x (- (+ (ns:ns-rect-x cell-frame) (ns:ns-rect-width cell-frame))
;; 		       BADGE_CIRCLE_BUFFER_RIGHT
;; 		       badge-width))
;; 	   (badge-y (+ (ns:ns-rect-y cell-frame) BADGE_BUFFER_TOP))
;; 	   (badge-num-x (+ badge-x BADGE_BUFFER_LEFT (if (= badge-width BADGE_TEXT_SMALL) BADGE_BUFFER_LEFT_SMALL 0)))
;; 	   (badge-rect (ns:make-ns-rect badge-x badge-y badge-width BADGE_TEXT_HEIGHT))
;; 	   ;; Draw the badge and number.
;; 	   (badge-path (#/bezierPathWithRoundedRect:xRadius:yRadius ns:ns-bezier-path badge-rect BADGE_X_RADIUS BADGE_Y_RADIUS)))

;;       (cond ((and (#/isVisible (#/mainWindow ns:ns-app))
;; 		  (not (#/isHighligted self)))
;; 	     ;; The row is not selected and the window is in focus.
;; 	     (#/set (#/colorWithCalibratedRed:green:blue:alpha: ns:ns-color .53 .6 .74 1.0))
;; 	     (#/fill badge-path)
;; 	     (let ((dict (#/init (#/alloc ns:ns-mutable-dictionary))))
;; 	       (#/setValue:forKey dict (#/boldSystemFontOfSize: ns:ns-font 11) #$NSFontAttributeName)
;; 	       (#/setValue:forKey dict (#/numberWithFloat: ns:ns-number -0.25) #$NSKernAttributeName)
;; 	       (#/setValue:forKey dict (#/whiteColor ns:ns-color)              #$NSForegroundColorAttributeName)
;; 	       (#/drawAtPoint:withAttributes: badge (ns:make-ns-point badge-num-x badge-y) dict)))
;; 	    ((#/isVisible (#/mainWindow ns:ns-app))
;; 	     ;; The row is selected and the window is in focus.
;; 	     (#/set (#/whiteColor ns:ns-color))
;; 	     (#/fill badge-path)
;; 	     (let ((dict (#/init (#/alloc ns:ns-mutable-dictionary))))
;; 	       (#/setValue:forKey dict (#/boldSystemFontOfSize: ns:ns-font 11) #$NSFontAttributeName)
;; 	       (#/setValue:forKey dict (#/numberWithFloat: ns:ns-number -0.25) #$NSKernAttributeName)
;; 	       (#/setValue:forKey dict (#/whiteColor ns:ns-color)              #$NSForegroundColorAttributeName)
;; 	       (#/drawAtPoint:withAttributes: badge (ns:make-ns-point badge-num-x badge-y) dict)))

;; 	    )

;;     else if ([[NSApp mainWindow] isVisible])
;;     {
;;         ;; The row is selected and the window is in focus.
;;         [[NSColor whiteColor] set];
;;         [badgePath fill];
;;         NSDictionary *dict = [[NSMutableDictionary alloc] init];
;;         [dict setValue:[NSFont boldSystemFontOfSize:11] forKey:NSFontAttributeName];
;;         [dict setValue:[NSNumber numberWithFloat:-.25] forKey:NSKernAttributeName];
;;         [dict setValue:[NSColor alternateSelectedControlColor] forKey:NSForegroundColorAttributeName];
;;         [badge drawAtPoint:NSMakePoint(badgeNumX,badgeY) withAttributes:dict];
;;     }
;;     else if (![[NSApp mainWindow] isVisible] && ![self isHighlighted])
;;     {
;;         ;; The row is not selected and the window is not in focus.
;;         [[NSColor disabledControlTextColor] set];
;;         [badgePath fill];
;;         NSDictionary *dict = [[NSMutableDictionary alloc] init];
;;         [dict setValue:[NSFont boldSystemFontOfSize:11] forKey:NSFontAttributeName];
;;         [dict setValue:[NSNumber numberWithFloat:-.25] forKey:NSKernAttributeName];
;;         [dict setValue:[NSColor whiteColor] forKey:NSForegroundColorAttributeName];
;;         [badge drawAtPoint:NSMakePoint(badgeNumX,badgeY) withAttributes:dict];
;;     }
;;     else
;;     {
;;         ;; The row is selected and the window is not in focus.
;;         [[NSColor whiteColor] set];
;;         [badgePath fill];
;;         NSDictionary *dict = [[NSMutableDictionary alloc] init];
;;         [dict setValue:[NSFont boldSystemFontOfSize:11] forKey:NSFontAttributeName];
;;         [dict setValue:[NSNumber numberWithFloat:-.25] forKey:NSKernAttributeName];
;;         [dict setValue:[NSColor disabledControlTextColor] forKey:NSForegroundColorAttributeName];
;;         [badge drawAtPoint:NSMakePoint(badgeNumX,badgeY) withAttributes:dict];
;;     }


	
;; 	))
  

;; )



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
