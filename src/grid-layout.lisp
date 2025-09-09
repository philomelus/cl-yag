(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; column-layout ============================================================

(defclass grid-layout (layout-base
                       padding-mixin
                       spacing-mixin)
  ((columns :initarg :columns :initform 1 :type integer :accessor columns)
   (rows :initarg :rows :initform 1 :type integer :accessor rows)))

(defmacro defgrid-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'grid-layout ,@rest))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent grid-layout) &key)
  (v:debug :layout "[calc-area] {grid-layout} called with child ~a" (print-raw-object child))
  
  ;; Calculate parent area if needed
  (calc-layout-area parent)

  ;; Calculate our children areas if needed
  (when (eql (slot-value parent 'child-area) nil)
    (calc-layout-child-areas parent))

  (with-slots (child-area content) parent
    ;; Locate child object position (this is key to rest)
    (let ((cp (position child content :key #'(lambda (o) (foro o)))))
      (assert (not (eql cp nil)))
      (let* ((ca (aref child-area cp))
             (oa (make-instance '%rect :left (left ca) :top (top ca) :width (width ca) :height (height ca))))
      
        (with-slots ((cl left) (ct top) (cw width) (ch height)) child

          (v:debug :layout "[calc-area] {grid-layout} child ~d calculating (~a ~a) @ (~a ~a) ~a"
                   cp cw ch cl ct (print-raw-object child))

          ;; Let object calculate its area, tracking who was calculated
          (macrolet ((do-calc (var func)
                       ;; If keyword
                       `(if (typep ,var 'keyword)
                            ;; Call calculation function and save result
                            (progn
                              (setf ,var (,func ,var oa child))
                              ;; Return that we calculated
                              t)
                            ;; Otherwise we did no calculation
                            nil)))
            (let ((clp (do-calc ch calc-height))
                  (ctp (do-calc cw calc-width))
                  (cwp (do-calc cl calc-left))
                  (chp (do-calc ct calc-top)))

              ;; When there are child options or any area fields were constant
              ;; call update to make sure all is correct
              (let ((co (find-if #'(lambda (o) (eql (foro o) child)) content))
                    options)
                (when (consp co)
                  (setq options (rest co)))
                (when (or (not (or clp ctp cwp chp)) ; not calculated?
                          (> (length options) 0))    ; has options?
                  (with-local-slots ((lcl left) (lct top) (lcw width) (lch height)) (aref child-area cp)
                    (v:debug :layout "[calc-area] {grid-layout} child ~d internal area (~d ~d) @ (~d ~d) ~a"
                             cp lcw lch lcl lct (print-raw-object child)))
                  ;; Let update validate child
                  (update-layout-child-areas cp parent)))

              ;; Log updated/changed area
              (v:debug :layout "[calc-area] {grid-layout} child ~d area (~d ~d) @ (~d ~d) ~a"
                       cp cw ch cl ct (print-raw-object child))))))))
  (my-next-method))

;;;; functions ================================================================

(defmethod calc-layout-child-areas ((object grid-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."

  (with-slots (child-area) object
    (with-local-accessors (columns rows) object
      (setf child-area (make-array (* columns rows) :adjustable nil))

      ;; Determine base sizes and amount to distribute
      (with-local-accessors (left top width height) object
        (let ((base-width (truncate (/ width columns)))
              (width-adjust (truncate (mod width columns)))
              (base-height (truncate (/ height rows)))
              (height-adjust (truncate (mod height rows))))
          (assert (< width-adjust columns))
          (assert (>= width-adjust 0))
          (assert (< height-adjust rows))
          (assert (>= height-adjust 0))
          
          ;; Set initial child areas
          (loop :for v :from 0 :below rows :do
            (loop :for h :from 0 :below columns :do
              (let ((array-offset (+ (* v columns) h)))
                (setf (aref child-area array-offset)
                      (make-instance '%rect :left (+ left (* h base-width))
                                            :top (+ top (* v base-height))
                                            :width base-width
                                            :height base-height)))))
          ;; Distribute extra horizontal space
          ;; Odd to middlest column
          ;; Then rest to first and last columns moving inside until all extra used up
          (when (and (> width-adjust 0)
                     (oddp width-adjust))
            (let ((col (truncate (/ columns 2))))
              (loop :for v :from 0 :below rows :do
                (incf (width (aref child-area (+ (* v columns) col)))))
              (decf width-adjust)))
          (assert (evenp width-adjust))
          (when (> width-adjust 0)
            (do ((first 0 (incf first))
                 (last (1- columns) (decf last)))
                ((= width-adjust 0))
              (loop :for v :from 0 :below rows :do
                (let ((first-offset (+ (* v columns) first))
                      (last-offset (+ (* v columns) last)))
                  (incf (width (aref child-area first-offset)))
                  (incf (width (aref child-area last-offset)))))
              (decf width-adjust 2)))
          
          ;; Distribute extra vertical space
          ;; Odd to middlest row
          ;; Then rest to top and bottom rows moving inside until all extra used up
          (when (and (> height-adjust 0)
                     (oddp height-adjust))
            (let ((row-offset (* (truncate (/ rows 2)) columns)))
              (loop :for h :from 0 :below columns :do
                (incf (height (aref child-area (+ row-offset h)))))
              (decf height-adjust)))
          (assert (evenp height-adjust))
          (when (> height-adjust 0)
            (do ((first 0 (incf first))
                 (last (1- rows) (decf last)))
                ((> height-adjust 0))
              (loop :for h :from 0 :below columns :do
                (incf (height (aref child-area (+ (* first columns) h))))
                (incf (height (aref child-area (+ (* last columns) h)))))
              (decf height-adjust 2)))

          ;; Log the children internal areas
          (loop :for v :from 0 :below rows :do
            (loop :for h :from 0 :below columns :do
              (let ((array-offset (+ (* v columns) h)))
                (v:debug :layout "[calc-layout-child-areas] {grid-layout} child ~d,~d (~d) internal area (~d ~d) @ (~d ~d)"
                         h v array-offset
                         (width (elt child-area array-offset)) (height (elt child-area array-offset))
                         (left (elt child-area array-offset)) (top (elt child-area array-offset)))))))))))

(defmethod update-layout-child-areas (index (object grid-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."
  (declare (ignorable index))
  
  (v:info :grid "[update-layout-child-area] {~a} updating child ~d"
           (print-raw-object (nth index (content object))) index)

  ;; Figure out what fields to take from object
  ;; (let ((upd-ch-l-p nil) (upd-ch-t-p nil) (upd-ch-w-p nil) (upd-ch-h-p nil))
    
  ;;   ;; Update internal area if field wasn't calculated
  ;;   (macrolet ((update-p (field field-calc)
  ;;                `(if (not (slot-value (foro (nth index (content object))) ',field-calc))
  ;;                     (progn
  ;;                       (v:debug :layout "[update-layout-child-areas] child ~d internal ~a needs update"
  ;;                                index (symbol-name ',field))
  ;;                       t)
  ;;                     nil)))
  ;;     (setq upd-ch-l-p (update-p left left-calc))
  ;;     (setq upd-ch-t-p (update-p top top-calc))
  ;;     (setq upd-ch-w-p (update-p width width-calc))
  ;;     (setq upd-ch-h-p (update-p height height-calc)))
    
  ;;   ;; Update internal area from control based on options
  ;;   (let ((options)
  ;;         (co (nth index (content object))))
  ;;     (when (consp co)
  ;;       (setq options (rest co)))
  ;;     (dolist (opt options)
  ;;       (case opt
  ;;         (:bottom)
  ;;         (:center)
  ;;         (:left)
  ;;         (:middle)
  ;;         (:min-height
  ;;          (setq upd-ch-h-p t)
  ;;          (v:debug :layout "[update-layout-child-areas] child ~d has option :min-height" index))
  ;;         (:min-width
  ;;          (setq upd-ch-w-p t)
  ;;          (v:debug :layout "[update-layout-child-areas] child ~d has option :min-width" index ))
  ;;         (:max-height
  ;;          (setq upd-ch-h-p t))
  ;;         (:max-width
  ;;          (setq upd-ch-w-p t))
  ;;         (:right)
  ;;         (:top))))
    
  ;;   (let* ((num-children (length (content object)))
  ;;          (affected (- num-children index 1)))

  ;;     ;; Nothing to do if it was the only or last child
  ;;     (unless (> affected 0)
  ;;       (return-from update-layout-child-areas))

  ;;     ;; Update affected children
  ;;     (with-slots (child-area) object

  ;;       (v:debug :layout "[update-layout-child-areas] child ~d updating internal:" index)
  ;;       (v:debug :layout "[update-layout-child-areas]         {~a}" (print-raw-object (foro (nth index (content object)))))
  ;;       (v:debug :layout "[update-layout-child-areas]         from (~d ~d) @ (~d ~d)"
  ;;                (width (aref child-area index)) (height (aref child-area index))
  ;;                (left (aref child-area index)) (top (aref child-area index)))
  ;;       (let ((co (foro (nth index (content object)))))
  ;;         (v:debug :layout "[update-layout-child-areas]         to (~d ~d) @ (~d ~d)"
  ;;                  (slot-value co 'width) (slot-value co 'height)
  ;;                  (slot-value co 'left) (slot-value co 'top)))

  ;;       ;; Calculate newly available area
  ;;       (let ((new-height (- (height (aref child-area index))
  ;;                            (height (foro (nth index (content object)))))))
  ;;         ;; Add height of all prior siblings
  ;;         (loop :for i :from (1+ index) :to (1- num-children) :do
  ;;           (incf new-height (height (aref child-area i))))

  ;;         (v:debug :layout "[update-layout-child-areas] siblings available new height ~d" new-height)
          
  ;;         ;; Recalculate rest of the children
  ;;         (let ((new-child-height (truncate (/ new-height affected)))
  ;;               (height-to-spread (mod new-height affected)))

  ;;           ;; Update child area to objects actual area
  ;;           (with-slots (content) object
  ;;             (when upd-ch-h-p
  ;;               (setf (height (aref child-area index)) (slot-value (foro (nth index content)) 'height))
  ;;               (v:debug :layout "[update-layout-child-areas] child ~d internal height updated (~d)"
  ;;                        index (height (aref child-area index))))
  ;;             (when upd-ch-w-p
  ;;               (setf (width (aref child-area index)) (slot-value (foro (nth index content)) 'width))
  ;;               (v:debug :layout "[update-layout-child-areas] child ~d internal width updated (~d)"
  ;;                        index (width (aref child-area index))))
  ;;             (when upd-ch-l-p
  ;;               (setf (left (aref child-area index)) (slot-value (foro (nth index content)) 'left))
  ;;               (v:debug :layout "[update-layout-child-areas] child ~d internal left updated (~d)"
  ;;                        index (left (aref child-area index))))
  ;;             (when upd-ch-t-p
  ;;               (setf (top (aref child-area index)) (slot-value (foro (nth index content)) 'top))
  ;;               (v:debug :layout "[update-layout-child-areas] child ~d internal top updated (~d)"
  ;;                        index (top (aref child-area index)))))
            
  ;;           ;; Update child areas
  ;;           (loop :for i :from (1+ index) :to (1- num-children) :do
  ;;             (let ((new-top (+ (bottom (aref child-area (1- i))) 1))
  ;;                   (new-height new-child-height))

  ;;               (let ((child (foro (nth i (content object))))
  ;;                     (child-a (aref child-area i)))

  ;;                 ;; Save new internal height
  ;;                 (setf (height child-a) new-height)
                  
  ;;                 ;; Force recalc of child height if originally calculated
  ;;                 (when (slot-value child 'height-calc)
  ;;                   (setf (slot-value child 'height) (slot-value child 'height-calc)))

  ;;                 ;; Save new internal top
  ;;                 (setf (top child-a) new-top)

  ;;                 ;; Force recalc of child top if originally calculated
  ;;                 (when (slot-value child 'top-calc)
  ;;                   (setf (slot-value child 'top) (slot-value child 'top-calc)))

  ;;                 ;; For layouts that changed, force full recalculation of area
  ;;                 (when (typep child 'layout-base)
  ;;                   (v:debug :layout "[update-layout-child-areas] {~a} child ~d layout forced recalc"
  ;;                            (print-raw-object child) i)
  ;;                   (setf (slot-value child 'child-area) nil)
  ;;                   (dolist (gc (content child))
  ;;                     (when (typep gc 'area-mixin)
  ;;                       (when (slot-value gc 'width-calc)
  ;;                         (setf (slot-value gc 'width) (slot-value gc 'width-calc)))
  ;;                       (when (slot-value gc 'height-calc)
  ;;                         (setf (slot-value gc 'height) (slot-value gc 'height-calc)))
  ;;                       (when (slot-value gc 'left-calc)
  ;;                         (setf (slot-value gc 'left) (slot-value gc 'left-calc)))
  ;;                       (when (slot-value gc 'top-calc)
  ;;                         (setf (slot-value gc 'top) (slot-value gc 'top-calc)))))))))
            
  ;;           ;; Allocate left over space
  ;;           (when (> height-to-spread 0)
  ;;             (do ((next (1+ index) (+ next 1)))
  ;;                 ((= height-to-spread 0))
  ;;               (incf (height (aref child-area next)))
  ;;               (decf height-to-spread)))

  ;;           ;; Dump updates
  ;;           (dotimes (n affected)
  ;;             (let* ((i (+ index n 1))
  ;;                    (r (aref child-area i))
  ;;                    (cna (foro (nth i (content object)))))
  ;;               (v:debug :layout "[update-layout-child-areas] child ~d new internal area (~d ~d) @ (~d ~d)"
  ;;                        i (width r) (height r) (left r) (top r))
  ;;               (v:debug :layout "[update-layout-child-areas] child ~d       actual area (~d ~d) @ (~d ~d) {~a}" i
  ;;                        (slot-value cna 'width) (slot-value cna 'height) (slot-value cna 'left) (slot-value cna 'top)
  ;;                        (print-raw-object cna)))))))))
  )

