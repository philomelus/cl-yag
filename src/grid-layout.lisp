(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

(declaim (inline grid-layout-cell
                 grid-layout-child
                 grid-layout-column
                 grid-layout-column-cells
                 grid-layout-row
                 grid-layout-row-cells))

;;;; column-layout ============================================================

;;; grid-layout-cell ------------------------------------------------

(defclass %grid-layout-cell (border-mixin
                            h-align-mixin ; when area is < col/row size, how to position horizontally
                            padding-mixin ; border to content
                            spacing-mixin ; edge to border
                            v-align-mixin) ; When area is < col/row size, how to position vertically
  ())

;;; %grid-layout-column ----------------------------------------------

(defparameter +%GRID-LAYOUT-COLUMN-WIDTH-TYPE-OPTIONS+ '(:absolue :precent :percent-all))

(defclass %grid-layout-column (h-align-mixin
                              v-align-mixin)
  ((extra :initform t :accessor extra)
   (width :initform :auto :accessor width)
   (width-type :initform :percent :accessor width-type)
   (h-align :initform :center)
   (v-align :initform :middle)))

;; methods ------------------------------------------------

(defmethod (setf width) :after (value (object %grid-layout-column))
  (if (typep value 'keyword)
      (progn
        (assert (equal value :auto))
        (when (not (slot-value object 'extra))
          (setf (slot-value object 'extra) t)
          (v:info :layout "[SETF WIDTH] {%grid-layout-column} auto-set EXTRA to T")))
      (progn
        (assert (typep value (values 'integer 'float)))
        ;; Set to absolute or percent?
        (if (<= value 1)
            (progn
              (setf (slot-value object 'width-type) :percent)
              (v:info :layout "[SETF WIDTH] {%grid-layout-column} auto-set WIDTH-TYPE to PERCENT")
              ;; Enable extra?
              (when (not (slot-value object 'extra))
                (setf (slot-value object 'extra) t)
                (v:info :layout "[SETF WIDTH] {%grid-layout-column} auto-set EXTRA to T")))
            (progn
              (setf (slot-value object 'width-type) :absolute)
              (v:info :layout "[SETF WIDTH] {%grid-layout-column} auto-set WIDTH-TYPE to ABSOLUTE")
              ;; Disable extra?
              (when (slot-value object 'extra)
                (setf (slot-value object 'extra) nil)
                (v:info :layout "[SETF WIDTH] {%grid-layout-column} auto-set EXTRA to NIL"))))))
  (my-next-method))

;;; %grid-layout-row -------------------------------------------------

(defparameter +%GRID-LAYOUT-ROW-HEIGHT-TYPE-OPTIONS+ '(:absolue :precent :percent-all))

(defclass %grid-layout-row (h-align-mixin
                           v-align-mixin)
  ((extra :initarg :extra :initform t :accessor extra)
   (height :initarg :height :initform :auto :accessor height)
   (height-type :initform :percent :accessor height-type)
   (h-align :initform :center)
   (v-align :initform :middle)))

;; methods ------------------------------------------------

(defmethod (setf height) :after (value (object %grid-layout-row))
(if (typep value 'keyword)
    (progn
      (assert (equal value :auto))
      (when (not (slot-value object 'extra))
        (setf (slot-value object 'extra) t)
        (v:info :layout "[SETF HEIGHT] {%grid-layout-row} auto-set EXTRA to NIL")))
    (progn
      (assert (typep value (values 'integer 'float)))
      ;; Set to absolute or percent?
      (if (<= value 1)
          (progn
            (setf (slot-value object 'height-type) :percent)
            (v:info :layout "[SETF HEIGHT] {%grid-layout-row} auto-set HEIGHT-TYPE to PERCENT")
            ;; Enable extra?
            (when (not (slot-value object 'extra))
              (setf (slot-value object 'extra) t)
              (v:info :layout "[SETF HEIGHT] {%grid-layout-row} auto-set EXTRA to NIL")))
          (progn
            (setf (slot-value object 'height-type) :absolute)
            (v:info :layout "[SETF HEIGHT] {%grid-layout-row} auto-set HEIGHT-TYPE to ABSOLUTE")
            ;; Disable extra?
            (when (slot-value object 'extra)
              (setf (slot-value object 'extra) nil)
              (v:info :layout "[SETF HEIGHT] {%grid-layout-row} auto-set EXTRA to T"))))))
  (my-next-method))

;;; grid-layout -----------------------------------------------------

(defclass grid-layout (layout-base
                       padding-mixin
                       spacing-mixin)
  ((columns :initarg :columns :initform 1 :type integer :accessor columns)
   (rows :initarg :rows :initform 1 :type integer :accessor rows)
   ;; Internal (accessable through functions below)
   (cell-data :initform nil)
   (column-data :initform nil)
   (row-data :initform nil)))

(defmacro defgrid-layout (&rest rest &key &allow-other-keys)
  `(make-instance 'grid-layout ,@rest))

;; TODO: Allow :columns to specify %GRID-LAYOUT-COLUMN data
;; TODO: Allow :rows to specify %GRID-LAYOUT-ROW data
;; TODO: Allow :cells to specify %GRID-LAYOUT-CELL data
;; For all of the above, there must be exactly the number items in argument
;; list as as there are columns, rows, and (* columns rows).

(defmethod initialize-instance :after ((object grid-layout) &key)
  ;; Create cells
  (assert (eql (slot-value object 'cell-data) nil))
  (assert (eql (slot-value object 'column-data) nil))
  (assert (eql (slot-value object 'row-data) nil))
  (with-slots (cell-data column-data row-data columns rows) object
    (when (and (> columns 0) (> rows 0))
      ;; Allocate cells
      (loop :for v :from 0 :below rows :do
        (loop :for h :from 0 :below columns :do
          (push (make-instance '%grid-layout-cell) cell-data)))
      (assert (= (length cell-data) (* columns rows)))
      
      ;; Allocate colums
      (setf column-data (make-array columns :element-type (or '%grid-layout-column nil)))
      (loop :for h :from 0 :below columns :do
        (setf (aref column-data h) (make-instance '%grid-layout-column)))
      
      ;; Allocate rows
      (setf row-data (make-array rows :element-type (or '%grid-layout-row nil)))
      (loop :for v :from 0 :below rows :do
        (setf (aref row-data v) (make-instance '%grid-layout-row))))))

(defmethod (setf columns) :after (value (object grid-layout))
  (grid-layout-reset-cells object))

(defmethod (setf rows) :after (value (object grid-layout))
  (grid-layout-reset-cells object))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent grid-layout) &key)
  (v:info :layout "[calc-area] {grid-layout} called with child ~a" (print-raw-object child))
  
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

          (v:info :layout "[calc-area] {grid-layout} child ~d calculating (~a ~a) @ (~a ~a) ~a"
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
                    (v:info :layout "[calc-area] {grid-layout} child ~d internal area (~d ~d) @ (~d ~d) ~a"
                             cp lcw lch lcl lct (print-raw-object child)))
                  ;; Let update validate child
                  (break)
                  (update-layout-child-areas cp parent)))

              ;; Log updated/changed area
              (v:info :layout "[calc-area] {grid-layout} child ~d area (~d ~d) @ (~d ~d) ~a"
                      cp cw ch cl ct (print-raw-object child))))))))
  (my-next-method))

(defmethod calc-layout-child-areas ((object grid-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."
  (declare (type grid-layout object))
  
  (flet ((calc-col-width (width width-type avail-width total-width max-width col cols)
           (if (typep width 'keyword)
               (progn
                 (assert (equal width :auto))
                 (max (truncate (/ avail-width (- cols col)))
                      max-width))
               (progn
                 (case width-type
                   (:absolute
                    width)
                   (:percent
                    (assert (<= width 1))
                    (* avail-width width))
                   (:percent-all
                    (assert (<= width 1))
                    (* total-width width))))))
         
         (calc-row-height (height height-type avail-height total-height max-height row rows)
           (if (typep height 'keyword)
               (progn
                 (assert (equal height :auto))
                 (max (truncate (/ avail-height (- rows row))) max-height))
               (progn
                 (case height-type
                   (:absolute
                    height)
                   (:percent
                    (* avail-height height))
                   (:percent-all
                    (* total-height height)))))))
    
    (with-slots (child-area column-data row-data) object
      (with-local-slots (columns rows) object
        ;; Allocate internal child area
        (setf child-area (make-array (* columns rows) :initial-element (make-instance '%rect) :adjustable nil :element-type (or '%rect nil)))
        (loop :for row :from 0 :below rows :do
          (loop :for col :from 0 :below columns :do
            (setf (aref child-area (+ (* row columns) col)) (make-instance '%rect))))
        ;; Determine initial layout of child objects
        (with-local-slots ((object-left left) (object-top top)
                           (object-width width) (object-height height))
                          object

          (let ((available-width object-width)
                (available-height object-height)
                (current-left object-left)
                (current-top object-top)
                (cols-max-width (make-array columns :element-type 'float :initial-element 0.0))
                (rows-max-height (make-array rows :element-type 'float :initial-element 0.0))
                (cols-avail-width (make-array columns :element-type 'float :initial-element 0.0))
                (rows-avail-height (make-array rows :element-type 'float :initial-element 0.0)))

            ;; For each row
            (loop :for v :from 0 :below rows :do

              (v:info :layout "[c-l-c-a] {g-l} starting row ~d" v)
              
              ;; Make row data available
              (with-slots ((row-height height) (row-height-type height-type)
                           (row-h-align h-align) (row-v-align v-align))
                  (aref (slot-value object 'row-data) v)

                ;; Reset column data
                (setq available-width object-width)
                (setq current-left object-left)

                ;; For each column
                (loop :for h :from 0 :below columns :do

                  (v:info :layout "[c-l-c-a] {g-l} starting column ~d" h)
                  
                  ;; Make column data available
                  (with-slots ((column-width width) (column-width-type width-type)
                               (column-h-align h-align) (column-v-align v-align)
                               (column-extra extra))
                      (aref (slot-value object 'column-data) h)

                    (v:debug :layout "[c-l-c-a] {g-l} column-data: width:~a width-type:~a extra:~a h-align:~a v-align:~a"
                             column-width column-width-type column-extra column-h-align column-v-align)
                      
                    ;; Make internal child area available
                    (with-slots ((child-area-left left) (child-area-top top)
                                 (child-area-width width) (child-area-height height))
                        (aref child-area (+ (* v columns) h))

                      (v:debug :layout "[c-l-c-a] {g-l} child ~d,~d (~d) starting (~d ~d) @ (~d ~d)"
                               h v (+ (* v columns) h) child-area-width child-area-height
                               child-area-left child-area-top)
                      
                      ;; Calculate internal child area width
                      (setf child-area-width (calc-col-width column-width column-width-type available-width
                                                             object-width (aref cols-max-width h) h columns))
                      (assert (>= child-area-width 0))
                      (v:debug :layout "[c-l-c-a] {g-l} column ~d calculated width:~d" h child-area-width)

                      ;; Update columns max width if needed
                      (when (> child-area-width (aref cols-max-width h))
                        (v:debug :layout "[c-l-c-a] {g-l} column ~d new max width:~d" h child-area-width)
                        (setf (aref cols-max-width h) child-area-width)
                        
                        ;; Update column available width
                        (assert (>= (- available-width child-area-width) (aref cols-avail-width h)))
                        (v:debug :layout "[c-l-c-a] {g-l} column ~d available width:~d"
                                 h (- available-width child-area-width))
                        (setf (aref cols-avail-width h) (- available-width child-area-width)))
                      
                      ;; Calculate internal child area height
                      (setf child-area-height (calc-row-height row-height row-height-type available-height
                                                               object-height (aref rows-max-height v) v rows))
                      (assert (>= child-area-height 0))
                      (v:debug :layout "[c-l-c-a] {g-l} column ~d calculated height:~d" h child-area-height)

                      ;; Update rows max height if needed
                      (when (> child-area-height (aref rows-max-height v))
                        (v:info :layout "[c-l-c-a] {g-l} row ~d new max height:~d" v child-area-height)
                        (setf (aref rows-max-height v) child-area-height)

                        ;; Update row available height
                        (assert (>= (- available-height child-area-height) (aref rows-avail-height v)))
                        (v:info :layout "[c-l-c-a] {g-l} row ~d available height:~d"
                                v (- available-height child-area-height))
                        (setf (aref rows-avail-height v) (- available-height child-area-height)))
                      
                      ;; Calculate internal child area left
                      (setf child-area-left current-left)
                      (v:debug :layout "[c-l-c-a] {g-l} column ~d calculated left:~d" h child-area-left)
                      
                      ;; Calculate internal child area top
                      (setf child-area-top current-top)
                      (v:info :layout "[c-l-c-a] {g-l} column ~d calculated top:~d" h child-area-top)

                      (v:info :layout "[c-l-c-a] {g-l} child ~d,~d (~d) calculated (~d ~d) @ (~d ~d)"
                              h v (+ (* v columns) h) child-area-width child-area-height
                              child-area-left child-area-top)

                      ;; Update horizontal position in this row
                      (decf available-width child-area-width)
                      (incf current-left child-area-width))))

                ;; Is there any left over horizontal?
                (let ((row-width 0))
                  (loop :for val-col :from 0 :below columns :do
                    (incf row-width (width (aref child-area val-col))))

                  ;; Yes, so distribute it evenly as possible
                  (when (< row-width object-width)
                    (v:debug :layout "[c-l-c-a] {g-l} row ~d has ~d left over" v (- object-width row-width))

                    ;; Make sure there is at least on column that can accept extra
                    (let ((has-extra nil))
                      (loop :for col :from 0 :below columns :do
                        (when (extra (aref column-data col))
                          (setf has-extra t)
                          (return)))
                      (unless has-extra
                        (error "not all width used, but no columns accept extra")))
                    
                    ;; Distribute extra from both sides until used up
                    (let ((row-offset (* v columns))
                          (amount-to-distribute (- object-width row-width))
                          (middle-offset (round (/ columns 2)))
                          (edge1 -1)
                          (edge2 columns))

                      (loop :while (> amount-to-distribute 0) :do
                        ;; Update left edge
                        (incf edge1)
                        (when (> edge1 middle-offset)
                          (setq edge1 0))

                        ;; Update right edge
                        (decf edge2)
                        (when (< edge2 middle-offset)
                          (setq edge2 (1- columns)))
                       
                        ;; Give one to a column on the left if possible
                        (loop :for edge1-offset :from edge1 :below middle-offset :do
                          (when (extra (aref column-data edge1-offset))
                            (let ((col-offset (+ row-offset edge1-offset)))
                              (v:debug :layout "[c-l-c-a] {g-l} column ~d adjusted ~d to ~d" edge1-offset
                                       (width (aref child-area col-offset))
                                       (1+ (width (aref child-area col-offset))))
                              (incf (width (aref child-area col-offset)))
                              (decf amount-to-distribute)
                              (return))))
                        
                        ;; If still have some to give away
                        (when (> amount-to-distribute 0)
                          ;; Give one away to the column on the right
                          (loop :for edge2-offset :from edge2 :downto middle-offset :do
                            (when (extra (aref column-data edge2-offset))
                              (let ((col-offset (+ row-offset edge2-offset)))
                                (v:debug :layout "[c-l-c-a] {g-l} column ~d adjusted ~d to ~d" edge2-offset
                                         (width (aref child-area col-offset))
                                         (1+ (width (aref child-area col-offset))))                                
                                (incf (width (aref child-area (+ row-offset edge2-offset))))
                                (decf amount-to-distribute)
                                (return))))))

                      ;; Recalculate the lefts
                      (let ((new-left object-left)
                            (new-total 0))
                        (loop :for col :from 0 :below columns :do
                          (with-local-slots (left width) (aref child-area (+ row-offset col))
                            (unless (= left new-left)
                              (v:debug :layout "[c-l-c-a] {g-l} column ~d updated from ~d to ~d" col left new-left)
                              (setf (left (aref child-area (+ row-offset col))) new-left))
                            (incf new-left width)
                            (incf new-total width)))
                        (assert (= new-total object-width)))
                      
                      ;; Update max column widths and available
                      (let ((avail-width object-width))
                        (loop :for col :from 0 :below columns :do
                          (with-slots (width) (aref child-area (+ row-offset col))
                            (when (> width (aref cols-max-width col))
                              (setf (aref cols-max-width col) width)
                              (v:debug :layout "[c-l-c-a] {g-l} column ~d new max width:~d" col width)
                              (when (< (aref cols-avail-width col) (- avail-width width))
                                (setf (aref cols-avail-width col) (- avail-width width))
                                (v:debug :layout "[c-l-c-a] {g-l} column ~d available width:~d"
                                         col (aref cols-avail-width col))))
                            (decf avail-width (width (aref child-area (+ row-offset col))))))))))

                ;; Update vertical position
                (let ((this-row-height (aref rows-max-height v)))
                  (decf available-height this-row-height)
                  (incf current-top this-row-height)))
                  )

            ;; Is there any left over vertical space?
            

            ;; Sanity check: All columns on all rows should have same width,
            ;; all rows should have same height
            (let ((row-offset 0))
              (loop :for row :from 0 :below rows :do
                (setq row-offset (* row columns))
                (loop :for col :from 0 :below columns :do
                  (when (/= (aref cols-max-width col) (width (aref child-area (+ row-offset col))))
                    (error "mismatched column width: ~d,~d (~d) expected ~d, got ~d"
                           col row (+ row-offset col) (width (aref child-area (+ row-offset col)))
                           (aref cols-max-width col)))
                  (when (/= (aref rows-max-height row) (height (aref child-area (+ row-offset col))))
                    (error "mismatched row height: ~d,~d (~d) expected ~d, got ~d"
                           col row (+ row-offset col) (height (aref child-area (+ row-offset col)))
                           (aref rows-max-height row))))))
            )
          )
      
        ;; ;; Determine base sizes and amount to distribute
        ;; (with-local-accessors (left top width height) object
        ;;   (let ((base-width (truncate (/ width columns)))
        ;;         (width-adjust (truncate (mod width columns)))
        ;;         (base-height (truncate (/ height rows)))
        ;;         (height-adjust (truncate (mod height rows))))
        ;;     (declare (type integer base-width width-adjust base-height height-adjust))
        ;;     (assert (< width-adjust columns))
        ;;     (assert (>= width-adjust 0))
        ;;     (assert (< height-adjust rows))
        ;;     (assert (>= height-adjust 0))
          
        ;;     ;; Set initial child areas
        ;;     (loop :for v :from 0 :below rows :do
        ;;       (loop :for h :from 0 :below columns :do
        ;;         (let ((array-offset (+ (* v columns) h)))
        ;;           (setf (aref child-area array-offset)
        ;;                 (make-instance '%rect :left (+ left (* h base-width))
        ;;                                       :top (+ top (* v base-height))
        ;;                                       :width base-width
        ;;                                       :height base-height)))))
          
        ;;     ;; Distribute extra horizontal space
        ;;     ;; Odd to middlest column
        ;;     ;; Then rest to first and last columns moving inside until all extra used up
        ;;     (when (and (> width-adjust 0)
        ;;                (oddp width-adjust))
        ;;       (let ((col (truncate (/ columns 2))))
        ;;         (loop :for v :from 0 :below rows :do
        ;;           (incf (width (aref child-area (+ (* v columns) col)))))
        ;;         (decf width-adjust)))
        ;;     (assert (evenp width-adjust))
        ;;     (when (> width-adjust 0)
        ;;       (do ((first 0 (incf first))
        ;;            (last (1- columns) (decf last)))
        ;;           ((= width-adjust 0))
        ;;         (loop :for v :from 0 :below rows :do
        ;;           (let ((first-offset (+ (* v columns) first))
        ;;                 (last-offset (+ (* v columns) last)))
        ;;             (incf (width (aref child-area first-offset)))
        ;;             (incf (width (aref child-area last-offset)))))
        ;;         (decf width-adjust 2)))
          
        ;;     ;; Distribute extra vertical space
        ;;     ;; Odd to middlest row
        ;;     ;; Then rest to top and bottom rows moving inside until all extra used up
        ;;     (when (and (> height-adjust 0)
        ;;                (oddp height-adjust))
        ;;       (let ((row-offset (* (truncate (/ rows 2)) columns)))
        ;;         (loop :for h :from 0 :below columns :do
        ;;           (incf (height (aref child-area (+ row-offset h)))))
        ;;         (decf height-adjust)))
        ;;     (assert (evenp height-adjust))
        ;;     (when (> height-adjust 0)
        ;;       (do ((first 0 (incf first))
        ;;            (last (1- rows) (decf last)))
        ;;           ((> height-adjust 0))
        ;;         (loop :for h :from 0 :below columns :do
        ;;           (incf (height (aref child-area (+ (* first columns) h))))
        ;;           (incf (height (aref child-area (+ (* last columns) h)))))
        ;;         (decf height-adjust 2)))))
        )))

  ;; Log the children internal areas
  (with-slots (child-area) object
    (with-local-slots (columns rows) object
      (loop :for row :from 0 :below rows :do
        (loop :for col :from 0 :below columns :do
          (let ((array-offset (+ (* row columns) col)))
            (v:info :layout "[c-l-c-a] {g-l} child ~d,~d (~d) internal area (~d ~d) @ (~d ~d)"
                    col row array-offset
                    (width (aref child-area array-offset)) (height (aref child-area array-offset))
                    (left (aref child-area array-offset)) (top (aref child-area array-offset))))))))
  )

(defmethod on-paint ((object grid-layout) &key &allow-other-keys)
  ;; Make sure we have layout complete
  (when (eql (slot-value object 'child-area) nil)
    (v:info :layout "[on-paint] {grid-layut} Forcing layout ~a" (print-raw-object object))
    (calc-layout-area object)
    (calc-layout-child-areas object))

  ;; Paint borders
  (with-local-slots (columns rows) object
    (loop :for v :from 0 :below rows :do
      (loop :for h :from 0 :below columns :do
        (with-borders (bl br bt bb) (grid-layout-cell h v object)
          (when (or (eql bl nil) (eql br nil) (eql bt nil) (eql bb nil))
            (let ((theme (find-theme object)))
              (unless (eql bl nil)
                (paint-border-left bl (aref (slot-value object 'child-area) (+ (* v columns) h)) theme))
              (unless (eql br nil)
                (paint-border-right br (aref (slot-value object 'child-area) (+ (* v columns) h)) theme))
              (unless (eql bt nil)
                (paint-border-top bt (aref (slot-value object 'child-area) (+ (* v columns) h)) theme))
              (unless (eql bb nil)
                (paint-border-bottom bb (aref (slot-value object 'child-area) (+ (* v columns) h)) theme))))))))

  ;; Let base take care of children
  (my-next-method))

(defmethod update-layout-child-areas (index (object grid-layout))
  "When a child has options changing the area used within the layout, this
recalculates the sizes of the children that are affected by it."
  (declare (type integer index))
  (declare (type grid-layout object))
  (declare (ignorable index))
  
  (v:error :grid "[update-layout-child-area] {~a} updating child ~d"
           (print-raw-object (nth index (content object))) index))

;;;; functions ================================================================

(declaim (ftype (function (integer integer grid-layout) %grid-layout-cell) grid-layout-cell))
(defun grid-layout-cell (column row object)
  "Return %GRID-LAYOUT-CELL for child of GRID-LAYOUT at COLUMN,ROW"
  
  (with-slots (columns cell-data) object
    (nth (+ (* row columns) column) cell-data)))

(declaim (ftype (function (T (or symbol list) integer integer grid-layout &key (:recalc boolean)) null) grid-layout-cell-set))
(defun grid-layout-cell-set (value field-or-fields column row object &key recalc)
  "Update the slots named in field-or-fields to value for the %GRID-LAYOUT-CELL at
column,row within the GRID-LAYOUT in object."
  (if recalc
      (let ((cell (grid-layout-cell column row object))
            (updated nil))
        (if (atom field-or-fields)
            (unless (equal (slot-value cell field-or-fields) value)
              (setf (slot-value cell field-or-fields) value
                    updated t))
            (mapc #'(lambda (field)
                      (unless (equal (slot-value cell field) value)
                        (setf (slot-value cell field) value
                              updated t)))
                  field-or-fields))
        (when (and updated (slot-value object 'child-area))
          (setf (slot-value object 'child-area) nil)
          (calc-layout-child-areas object)))
      (if (atom field-or-fields)
          (setf (slot-value (grid-layout-cell column row object) field-or-fields) value)
          (let ((cell (grid-layout-cell column row object))) 
            (mapc #'(lambda (field)
                      (unless (equal (slot-value cell field) value)
                        (setf (slot-value cell field) value)))
                  field-or-fields))))
  nil)

(declaim (ftype (function (integer integer grid-layout) t) grid-layout-child))
(defun grid-layout-child (column row object)
  "Return child object of GRID-LAYOUT at COLUMN,ROW"

  (with-slots (columns content) object
    (nth (+ (* row columns) column) content)))

(declaim (ftype (function (integer grid-layout) %grid-layout-column) grid-layout-column))
(defun grid-layout-column (column object)
  "Return %GRID-LAYOUT-COLUMN for specified column of specified GRID-LAYOUT object."
  
  (aref (slot-value object 'column-data) column))

(declaim (ftype (function (integer grid-layout) list) grid-layout-column-cells))
(defun grid-layout-column-cells (column object)
  "Return all %GRID-LAYOUT-CELL's for a column withn GRID-LAYOUT."

  (with-local-slots (rows) object
    (loop :for v :from 0 :below rows
          :collect (grid-layout-cell column v object))))

(declaim (ftype (function (T (or symbol list) integer grid-layout &key (:recalc boolean)) null) grid-layout-column-cells-set))
(defun grid-layout-column-cells-set (value field-or-fields column object &key recalc)
  "Update the slot(s) named in field-or-fields to value for all
%GRID-LAYOUT-CELL's in the specified row within the GRID-LAYOUT in object."

  (let ((cells (grid-layout-column-cells column object)))
    (if recalc
        (let ((updated nil))
          (mapc #'(lambda (cell)
                    (if (atom field-or-fields)
                        (unless (equal (slot-value cell field-or-fields) value)
                          (setf (slot-value cell field-or-fields) value
                                updated t))
                        (mapc #'(lambda (field)
                                  (unless (eql (slot-value cell field) value)
                                    (setf (slot-value cell field) value
                                          updated t)))
                              field-or-fields)))
                cells)
          (when (and updated (slot-value object 'child-area))
            (setf (slot-value object 'child-area) nil)
            (calc-layout-child-areas object)))
        
        ;; Update the field(s)
        (mapc #'(lambda (cell)
                  ;; field or list of fields?
                  (if (atom field-or-fields)
                      ;; If the field is different
                      (unless (equal (slot-value cell field-or-fields) value)
                        (setf (slot-value cell field-or-fields) value))
                      ;; Update all the fields
                      (mapc #'(lambda (field)
                                ;; If the field is different
                                (unless (equal (slot-value cell field) value)
                                  (setf (slot-value cell field) value)))
                            field-or-fields)))
              cells)))
  nil)

(defun grid-layout-reset-cell (object)
  "Return %GRID-LAYOUT-CELL slots to default state."
  (declare (type %grid-layout-cell object))
  
  (with-slots (border-left border-right border-top border-bottom
               h-align
               padding-left padding-right padding-top padding-bottom
               spacing-left spacing-right spacing-top spacing-bottom
               v-align)
      object
    (setf border-left nil border-right nil border-top nil border-bottom nil)
    (setf h-align :none)
    (setf padding-left 0 padding-right 0 padding-top 0 padding-bottom 0)
    (setf spacing-left 0 spacing-right 0 spacing-top 0 spacing-bottom 0)
    (setf v-align :none)))

(declaim (ftype (function (grid-layout) null) grid-layout-reset-cells))
(defun grid-layout-reset-cells (object)
  "Update CELLS of GRID-LAYOUT according to current COLUMNS and ROWS."

  ;; Reset/update cells
  (with-slots (cell-data column-data columns row-data rows) object
    (let ((num-cells (length cell-data))
          (needed-cells (* columns rows)))
      (if (/= needed-cells num-cells)
          (if (< needed-cells num-cells)
              ;; Delte no longer needed cells
              (progn
                ;; Remove excess cells
                (do ((count (- num-cells needed-cells) (decf count)))
                    ((= count 0))
                  (pop cell-data))
                  
                ;; Reset current cells
                (mapc #'grid-layout-reset-cell cell-data))
                
              ;; Allocate new cells
              (progn
                ;; Reset current cells
                (mapc #'grid-layout-reset-cell cell-data)
                
                ;; Make up difference
                (loop :for c :from 0 :below (- needed-cells num-cells) :do
                  (push (make-instance '%grid-layout-cell) cell-data))))
          
          ;; Rest current cells
          (mapc #'grid-layout-reset-cell cell-data)))

    ;; Reset/update columns
    (let ((num-columns (length column-data)))
      (if (/= num-columns columns)
          (progn
            (setf column-data (make-array columns :element-type (or '%grid-layout-column nil)))
            (loop :for h :from 0 :below columns :do
              (setf (aref column-data h) (make-instance '%grid-layout-column))))
          (mapc #'grid-layout-reset-column column-data)))

    ;; Reset/update rows
    (let ((num-rows (length row-data)))
      (if (/= num-rows rows)
          (progn
            (setf row-data (make-array rows :element-type (or '%grid-layout-row nil)))
            (loop :for v :from 0 :below rows :do
              (setf (aref row-data v) (make-instance '%grid-layout-row))))
          (mapc #'grid-layout-reset-row row-data))))
  nil)

(defun grid-layout-reset-column (object)
  "Return %GRID-LAYOUT-COLUMN slots to default state."
  (declare (type %grid-layout-column object))

  (with-slots (h-align width width-type v-align) object
    (setf width :auto)
    (setf width-type :percent)
    (setf h-align :center)
    (setf v-align :middle)))

(defun grid-layout-reset-row (object)
  "Return %GRID-LAYOUT-ROW slots to default state."
  (declare (type %grid-layout-row object))

  (with-slots (h-align height height-type v-align) object
    (setf height :auto)
    (setf height-type :percent)
    (setf h-align :center)
    (setf v-align :middle)))

(declaim (ftype (function (integer grid-layout) %grid-layout-row) grid-layout-row))
(defun grid-layout-row (row object)
  "Return %GRID-LAYOUT-ROW for specified row of specified GRID-LAYOUT object."
  (aref (slot-value object 'row-data) row))

(declaim (ftype (function (integer grid-layout) list) grid-layout-row-cells))
(defun grid-layout-row-cells (row object)
  "Return all %GRID-LAYOUT-CELL's for a row within GRID-LAYOUT."

  (with-local-slots (columns) object
    (loop :for h :from 0 :below columns
          :collect (grid-layout-cell h row object))))

(declaim (ftype (function (T (or symbol list) integer grid-layout &key (:recalc boolean)) null) grid-layout-row-cells-set))
(defun grid-layout-row-cells-set (value field-or-fields row object &key recalc)
  "Update the slots named in field-or-fields to value for all %GRID-LAYOUT-CELL's
in the specified column within the GRID-LAYOUT in object."
  
  (let ((cells (grid-layout-row-cells row object)))
    (if recalc
        (let ((updated nil))
          (if (atom field-or-fields)
              (mapc #'(lambda (cell)
                        (unless (equal (slot-value cell field-or-fields) value)
                          (setf (slot-value cell field-or-fields) value
                                updated t)))
                    cells)
              (mapc #'(lambda (cell)
                        (mapc #'(lambda (field)
                                  (unless (equal (slot-value cell field) value)
                                    (setf (slot-value cell field) value
                                          updated t)))
                              field-or-fields))
                    cells))
          (when (and updated (slot-value object 'child-area))
            (setf (slot-value object 'child-area) nil)
            (calc-layout-child-areas object)))
        (if (atom field-or-fields)
            (mapc #'(lambda (cell)
                      (unless (equal (slot-value cell field-or-fields) value)
                        (setf (slot-value cell field-or-fields) value)))
                  cells)
            (mapc #'(lambda (cell)
                      (mapc #'(lambda (field)
                                (unless (equal (slot-value cell field) value)
                                  (setf (slot-value cell field) value)))
                            field-or-fields))
                  cells))))
  nil)

