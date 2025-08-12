(in-package #:cl-yag)

;;;; container-mixin ==========================================================
;;;; Controls area for contained objects

(defclass container-mixin (area-mixin
                           content-mixin)
  ())

(defmethod print-mixin ((o container-mixin) s)
  ;; Base mixins get printed via call-next-method, so nothing to do here
  (my-next-method))

(defmethod initialize-instance :after ((obj container-mixin) &key)
  ;; Take on the area of our (eventual) parent if exists
  (let ((pam (find-parent-area-mixin obj)))
    (if (not (eq pam nil))
        (container-calc-area pam obj)))
  
  (my-next-method))

;; Determine left, top, width, and height from parent area
(defun container-calc-area (source container)
  (setf (left container) (1+ (left source))
        (top container) (1+ (top source))
        (height container) (- (height source) 2)
        (width container) (- (width source) 2))

  (assert (> (height container) 0))
  (assert (> (width container) 0))
  
  ;; Adjust for padding if needed
  (if (typep container 'padding-mixin)
      (progn
        (incf (left container) (padding-left container))
        (incf (top container) (padding-top container))
        (decf (height container) (+ (padding-top container) (padding-bottom container)))
        (decf (width container) (+ (padding-left container) (padding-right container)))))

  (assert (> (height container) 0))
  (assert (> (width container) 0))

  ;; Adjust for border if needed
  (if (typep source 'border-mixin)
      (progn
        (let (bo w)
          (setf bo (border-left source))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (width container) w)
            (incf (left container) w))
          (setf bo (border-right source))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (width container) w))
          (setf bo (border-top source))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (height container) w)
            (incf (top container) w))
          (setf bo (border-bottom source))
          (unless (eql nil bo)
            (setf w (width bo))
            (decf (height container) w)))))

  (assert (> (height container) 0))
  (assert (> (width container) 0)))

