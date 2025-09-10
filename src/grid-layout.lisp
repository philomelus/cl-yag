(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; column-layout ============================================================

(defclass grid-layout (layout-base
                       padding-mixin
                       spacing-mixin)
  ((columns :initarg :columns :initform 1 :type integer :accessor columns)
   (rows :initarg :rows :initform 1 :type integer :accessor rows)
   ;; Internal
   (cells :initform nil)))

(defmacro defgrid-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'grid-layout ,@rest))

(defmethod initialize-instance :after ((object grid-layout) &key)
  ;; Create cells
  (assert (eql (slot-value object 'cells) nil))
  (with-slots (cells columns rows) object
    (when (and (> columns 0) (> rows 0))
      (loop :for v :from 0 :below rows :do
        (loop :for h :from 0 :below columns :do
              (push (make-instance 'layout-cell :parent object) cells)))
      (assert (= (length cells) (* columns rows))))))

(defmethod (setf columns) :after (value (object grid-layout))
  (grid-layout-reset-cells object))

(defmethod (setf rows) :after (value (object grid-layout))
  (grid-layout-reset-cells object))

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

(defmethod calc-layout-child-areas ((object grid-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."
  (declare (type grid-layout object))
  
  (with-slots (child-area) object
    (with-local-accessors (columns rows) object
      (setf child-area (make-array (* columns rows) :adjustable nil))

      ;; Determine base sizes and amount to distribute
      (with-local-accessors (left top width height) object
        (let ((base-width (truncate (/ width columns)))
              (width-adjust (truncate (mod width columns)))
              (base-height (truncate (/ height rows)))
              (height-adjust (truncate (mod height rows))))
          (declare (type integer base-width width-adjust base-height height-adjust))
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
  (declare (type integer index))
  (declare (type grid-layout object))
  (declare (ignorable index))
  
  (v:error :grid "[update-layout-child-area] {~a} updating child ~d"
           (print-raw-object (nth index (content object))) index))

;;;; functions ================================================================

(declaim (ftype (function ((or integer float) integer integer grid-layout &key (:recalc boolean)) null) cell-height))
(defun cell-height (height column row object &key (recalc t))
  "Set the height of a specific cell within grid-layout."

  (let ((cell (grid-layout-cell column row object)))
    (with-slots ((h hwight)) cell
      (when (/= h height)
        (setf h height)
        (with-slots ((ca child-areas)) object
          (when (and recalc ca)
            (setf ca nil)
            (calc-layout-child-areas object)))))))

(declaim (ftype (function ((or integer float) (or integer float) integer integer grid-layout &key (:recalc boolean)) null) cell-size))
(defun cell-size (width height column row object &key (recalc t))
  "Set the width and height of a specific cell within grid-layot."

  (let ((cell (grid-layout-cell column row object))
        (updated nil))
    (with-slots ((h height) (w width)) cell
      (when (/= w width)
        (setf w width updated t))
      (when (/= h height)
        (setf h height updated t)))
    (when (and recalc updated (slot-value object 'child-areas))
      (setf (slot-value object 'child-areas) nil)
      (calc-layout-child-areas object))))

(declaim (ftype (function ((or integer float) integer integer grid-layout &key (:recalc boolean)) null) cell-width))
(defun cell-width (width column row object &key (recalc t))
  "Set the width of a specific cell within grid-layout."
  
  (let ((cell (grid-layout-cell column row object)))
    (with-slots ((w width)) cell
      (when (/= w width)
        (setf w width)
        (with-slots ((ca child-areas)) object
          (when (and recalc ca)
            (setf ca nil)
            (calc-layout-child-areas object)))))))

(declaim (ftype (function ((or integer float) integer grid-layout &key (:recalc boolean)) null) column-height))
(defun column-height (height column object &key (recalc t))
  "Set height for entire column within grid-layout."

  (update-column column object #'(lambda (cell)
                                   (with-slots ((h height)) cell
                                     (if (/= h height)
                                         (progn
                                           (setf h height)
                                           t)
                                         nil)))
                 :recalc recalc)
  nil)

(declaim (ftype (function ((or integer float keyword) (or integer float keyword) integer grid-layout &key (:recalc boolean)) null) column-size))
(defun column-size (width height column object &key (recalc t))
  "Set the width and height of an entire column within grid-layout."
  (update-column column object #'(lambda (cell)
                                   (let ((updated nil))
                                     (with-slots ((h height) (w width)) cell
                                       (when (/= w width)
                                         (setf w width updated t))
                                       (when (/= h height)
                                         (setf h height updated t)))
                                     updated))
                 :recalc recalc)
  nil)

(declaim (ftype (function ((or integer float) integer grid-layout &key (:recalc boolean)) null) column-width))
(defun column-width (width column object &key (recalc t))
  "Set width of an entire column within grid-layout."
  (update-column column object #'(lambda (cell)
                                   (with-slots ((w width)) cell
                                     (if (/= w width)
                                         (progn
                                           (setf w width)
                                           t)
                                         nil)))
                 :recalc recalc)
  nil)

(defun grid-layout-cell (column row object)
  "Return LAYOUT-CELL for child of GRID-LAYOUT at COLUMN,ROW"
  
  (with-slots (columns cells) object
    (nth (+ (* row columns) column) cells)))

(defun grid-layout-child (column row object)
  "Return child object of GRID-LAYOUT at COLUMN,ROW"

  (with-slots (columns content) object
    (nth (+ (* row columns) column) content)))

(defun grid-layout-reset-cells (object)
  "Update CELLS of GRID-LAYOUT according to current COLUMNS and ROWS."
  
  (with-slots (cells columns rows) object
    (let ((num-cells (length cells))
          (needed-cells (* columns rows)))
      (if (/= needed-cells num-cells)
          (if (< needed-cells num-cells)
              ;; Delte no longer needed cells
              (progn
                ;; Remove excess cells
                (do ((count (- num-cells needed-cells) (decf count)))
                    ((= count 0))
                  (pop cells))
                  
                ;; Reset current cells
                (mapc #'layout-cell-reset cells))
                
              ;; Allocate new cells
              (progn
                ;; Reset current cells
                (mapc #'layout-cell-reset cells)
                
                ;; Make up difference
                (loop :for c :from 0 :below (- needed-cells num-cells) :do
                  (push (make-instance 'layout-cell :parent object) cells))))
          
          ;; Rest current cells
          (mapc #'layout-cell-reset cells)))))

(declaim (ftype (function ((or integer float) integer grid-layout &key (:recalc boolean)) null) row-height))
(defun row-height (height row object &key (recalc t))
  "Set the height of entire row within grid-layout."
  
  (update-row row object #'(lambda (cell)
                             (with-slots ((h height)) cell
                               (if (/= h height)
                                   (progn
                                     (setf h height)
                                     t)
                                   nil)))
              :recalc recalc)
  nil)

(declaim (ftype (function ((or integer float keyword) (or integer float keyword) integer grid-layout &key (:recalc boolean)) null) row-size))
(defun row-size (width height row object &key (recalc t))
  "Set the width and height of entire row within grid-layout."

  (update-row row object #'(lambda (cell)
                             (let ((updated nil))
                               (with-slots ((h height) (w width)) cell
                                 (when (/= w width)
                                   (setf w width updated t))
                                 (when (/= h height)
                                   (setf h height updated t)))
                               updated))
              :recalc recalc)
  nil)

(declaim (ftype (function ((or integer float keyword) integer grid-layout &key (:recalc boolean)) null) row-width))
(defun row-width (width row object &key (recalc t))
  "Set the width of entire row within grid-layout."
  
  (update-row row object #'(lambda (cell)
                             (let ((updated nil))
                               (when (/= (slot-value cell 'width) width)
                                 (setf (slot-value cell 'width) width)
                                 (setq updated t))
                               updated))
              :recalc recalc))

(declaim (ftype (function (integer grid-layout (function (layout-cell) boolean) &key (:recalc boolean)) null) update-column))
(defun update-column (column object func &key recalc)
  (with-local-slots (rows) object
    ;; Make list of affected cells
    (let* ((cells (loop :for v :from 0 :below rows
                        :collect (grid-layout-cell column v object)))
           ;; Update them if needed
           (updated (or (values-list (mapcar func cells)))))
      ;; Force relayout if anything changed, was requested, and layout
      ;; had already been done
      (when (and recalc updated (slot-value object 'child-areas))
        (setf (slot-value object 'child-areas) nil)
        (calc-layout-child-areas object))))
  nil)

(declaim (ftype (function (integer grid-layout (function (layout-cell) boolean) &key (:recalc boolean)) null) update-row))
(defun update-row (row object func &key recalc)
  (with-local-slots (columns) object
    ;; Make list of affected cells
    (let* ((cells (loop :for h :from 0 :below columns
                        :collect (grid-layout-cell h row object)))
           ;; Update them if needed
           (updated (or (values-list (mapcar func cells)))))
      ;; Force relayout if anything changed, was requested, and layout
      ;; had already been done
      (when (and recalc updated (slot-value object 'child-areas))
        (setf (slot-value object 'child-areas) nil)
        (calc-layout-child-areas object))))
  nil)
