(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; column-layout ============================================================

;;; grid-layout-cell ------------------------------------------------

(defclass %grid-layout-cell (border-mixin
                            h-align-mixin ; when area is < col/row size, how to position horizontally
                            padding-mixin ; border to content
                            spacing-mixin ; edge to border
                            v-align-mixin) ; When area is < col/row size, how to position vertically
  ())

;;; %grid-layout-column ----------------------------------------------

(deftype grid-layout-column-width-type () '(member :absolute :percent :percent-all))

(defclass %grid-layout-column (h-align-mixin
                              v-align-mixin)
  ((extra :initform t :accessor extra)
   (width :initform :auto :accessor width)
   (width-type :initform :percent :type grid-layout-column-width-type :accessor width-type)
   (h-align :initform :center)
   (v-align :initform :middle)))

;; methods ------------------------------------------------

(defmethod (setf width) :after (value (object %grid-layout-column))
  (if (keywordp value)
      (progn
        (assert (equal value :auto))
        (when (not (slot-value object 'extra))
          (setf (slot-value object 'extra) t)
          (v:debug :layout "[SETF WIDTH] {%grid-layout-column} auto-set EXTRA to T")))
      (progn
        (assert (typep value (values 'integer 'float)))
        ;; Set to absolute or percent?
        (if (<= value 1)
            (progn
              (setf (slot-value object 'width-type) :percent)
              (v:debug :layout "[SETF WIDTH] {%grid-layout-column} auto-set WIDTH-TYPE to PERCENT")
              ;; Enable extra?
              (when (not (slot-value object 'extra))
                (setf (slot-value object 'extra) t)
                (v:debug :layout "[SETF WIDTH] {%grid-layout-column} auto-set EXTRA to T")))
            (progn
              (setf (slot-value object 'width-type) :absolute)
              (v:debug :layout "[SETF WIDTH] {%grid-layout-column} auto-set WIDTH-TYPE to ABSOLUTE")
              ;; Disable extra?
              (when (slot-value object 'extra)
                (setf (slot-value object 'extra) nil)
                (v:debug :layout "[SETF WIDTH] {%grid-layout-column} auto-set EXTRA to NIL"))))))
  (my-next-method))

;;; %grid-layout-row -------------------------------------------------

(deftype grid-layout-row-height-type () '(member :absolute :percent :percent-all))

(defclass %grid-layout-row (h-align-mixin
                           v-align-mixin)
  ((extra :initarg :extra :initform t :accessor extra)
   (height :initarg :height :initform :auto :accessor height)
   (height-type :initform :percent :type grid-layout-row-height-type :accessor height-type)
   (h-align :initform :center)
   (v-align :initform :middle)))

;; methods ------------------------------------------------

(defmethod (setf height) :after (value (object %grid-layout-row))
  (if (keywordp value)
      (progn
        (assert (equal value :auto))
        (when (not (slot-value object 'extra))
          (setf (slot-value object 'extra) t)
          (v:debug :layout "[SETF HEIGHT] {%grid-layout-row} auto-set EXTRA to T")))
      (progn
        (assert (typep value (values 'integer 'float)))
        ;; Set to absolute or percent?
        (if (<= value 1)
            (progn
              (setf (slot-value object 'height-type) :percent)
              (v:debug :layout "[SETF HEIGHT] {%grid-layout-row} auto-set HEIGHT-TYPE to PERCENT")
              ;; Enable extra?
              (when (not (slot-value object 'extra))
                (setf (slot-value object 'extra) t)
                (v:debug :layout "[SETF HEIGHT] {%grid-layout-row} auto-set EXTRA to T")))
            (progn
              (setf (slot-value object 'height-type) :absolute)
              (v:debug :layout "[SETF HEIGHT] {%grid-layout-row} auto-set HEIGHT-TYPE to ABSOLUTE")
              ;; Disable extra?
              (when (slot-value object 'extra)
                (setf (slot-value object 'extra) nil)
                (v:debug :layout "[SETF HEIGHT] {%grid-layout-row} auto-set EXTRA to NIL"))))))
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

(defmacro defgrid-layout (columns rows &rest rest &key &allow-other-keys)
  `(make-instance 'grid-layout :columns ,columns :rows ,rows ,@rest))

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

(defmethod calc-area ((parent grid-layout) &key)
  )

(defmethod calc-layout-child-areas ((object grid-layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the area
allocated to them, whether they choose to use it or not. This does default
layout calculations, most of the option interpretation is done in CALC-AREA."
  
  (declare (type grid-layout object))
  
  (flet ((calc-col-width (data avail-width total-width max-width col cols)
           (with-slots (width width-type) data
             (if (keywordp width)
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
                      (* total-width width)))))))
         
         (calc-row-height (data avail-height total-height max-height row rows)
           (with-slots (height height-type) data
             (if (keywordp height)
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
         
         (distribute (fields allowed amount count)
           (assert (= (length fields) count))
           (assert (= (length allowed) count))
           (let ((amount-to-distribute amount)
                 (edge1 -1)
                 (results fields))

             (loop :while (> amount-to-distribute 0) :do
               ;; Update offset
               (incf edge1)
               (when (> edge1 count)
                 (setq edge1 0))

               ;; Give one away
               (loop :for edge1-offset :from edge1 :below count :do
                 (when (aref allowed edge1-offset)
                   (when (> edge1-offset edge1)
                     (setq edge1 edge1-offset))
                   (v:debug :layout "[C-A-C-A] {distribute} edge 1 gave to ~d for ~d"
                            edge1-offset (aref results edge1-offset))
                   (incf (aref results edge1-offset))
                   (decf amount-to-distribute)
                   (return))))
             results)))
    
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
                (col-widths (make-array columns :element-type 'float :initial-element 0.0))
                (row-heights (make-array rows :element-type 'float :initial-element 0.0))
                )

            ;; Determine the max width and height of rows
            (loop :for row :from 0 :below rows :do
              (v:debug :layout "[c-l-c-a] {g-l} starting row ~d" row)
              
              ;; Make row data available
              (with-slots ((row-height height) (row-height-type height-type)
                           (row-h-align h-align) (row-v-align v-align))
                  (aref (slot-value object 'row-data) row)

                ;; Reset column data
                (setq available-width object-width)
                (setq current-left object-left)

                ;; For each column
                (loop :for col :from 0 :below columns :do
                  (v:debug :layout "[c-l-c-a] {g-l} starting column ~d" col)
                  
                  ;; Make column data available
                  (with-slots ((column-width width) (column-width-type width-type)
                               (column-h-align h-align) (column-v-align v-align)
                               (column-extra extra))
                      (aref (slot-value object 'column-data) col)
                    (v:debug :layout "[c-l-c-a] {g-l} column-data: width:~a width-type:~a extra:~a h-align:~a v-align:~a"
                            column-width column-width-type column-extra column-h-align column-v-align)
                      
                    ;; Calculate width and height
                    (let ((width (calc-col-width (aref column-data col) available-width object-width
                                                 (aref col-widths col) col columns))
                          (height (calc-row-height (aref row-data row) available-height object-height
                                                   (aref row-heights row) row rows)))
                      
                      ;; Calculate internal child area width
                      (assert (>= width 0))
                      (assert (>= height 0))
                      (v:debug :layout "[c-l-c-a] {g-l} column ~d calculated width:~d height:" col width height)
                      
                      ;; Update max column width if needed
                      (when (> width (aref col-widths col))
                        (v:debug :layout "[c-l-c-a] {g-l} column ~d new max width:~d" col width)
                        (setf (aref col-widths col) width))

                      ;; Update max row height if needed
                      (when (> height (aref row-heights row))
                        (v:debug :layout "[c-l-c-a] {g-l} row ~d new max height:~d" row height)
                        (setf (aref row-heights row) height))

                      ;; Update horizontal position
                      (decf available-width width)
                      (incf current-left width))))

                ;; Update vertical position
                (let ((this-row-height (aref row-heights row)))
                  (decf available-height this-row-height)
                  (incf current-top this-row-height))))
            
            ;; Calculate total width used
            (let ((row-width 0))
              (loop :for col :from 0 :below columns :do
                (incf row-width (aref col-widths col)))

              ;; Is it less than available?
              (when (< row-width object-width)
                (v:debug :layout "[c-l-c-a] {g-l} left over horizontal ~d" (- object-width row-width))

                ;; Yes, make sure there is at least on column that can accept extra
                (let ((has-extra nil))
                  (loop :for col :from 0 :below columns :do
                    (when (extra (aref column-data col))
                      (setf has-extra t)
                      (return)))
                  (unless has-extra
                    (error "not all width used, but no columns accept extra")))
                    
                ;; Distribute extra from both sides until used up
                (let ((allowed-list (loop :for col :from 0 :below columns
                                          :collect (extra (aref column-data col)))))
                  (let ((allowed (make-array columns :initial-contents allowed-list)))
                    (setf col-widths (distribute col-widths allowed (- object-width row-width) columns))))))
            (v:debug :layout "[C-L-C-A] {G-L} columns widths: ~a" col-widths)

            ;; Calculate total height used
            (let ((row-height 0))
              (loop :for row :from 0 :below rows :do
                (incf row-height (aref row-heights row)))
              
              ;; Is there any left over vertical space?
              (when (< row-height object-height)
                (v:debug :layout "[C-L-C-A] {G-L} left over vertical ~d" (- object-height row-height))

                ;; Yes, make sure there is a least one row that accepts extra
                (let ((has-extra nil))
                  (loop :for row :from 0 :below rows :do
                    (when (extra (aref row-data row))
                      (setf has-extra t)
                      (return)))
                  (unless has-extra
                    (error "not all height used, but no rows accept extra")))

                ;; Distribute extra from both ends until used up
                (let ((allowed-list (loop :for row :from 0 :below rows
                                          :collect (extra (aref row-data row)))))
                  (let ((allowed (make-array rows :initial-contents allowed-list)))
                    (setf row-heights (distribute row-heights allowed (- object-height row-height) rows))))))
            (v:debug :layout "[C-L-C-A] {G-L} row heights: ~a" row-heights)
            
            (let ((col-avail-widths (make-array columns :element-type 'float :initial-element 0.0))
                  (row-avail-heights (make-array rows :element-type 'float :initial-element 0.0))
                  (avail-width object-width)
                  (avail-height object-height)
                  (row-width 0)
                  (row-height 0))
              ;; Predetermine available widths
              (loop :for col :from 0 :below columns :do
                (incf row-width (aref col-widths col))
                (decf avail-width (aref col-widths col))
                (setf (aref col-avail-widths col) avail-width))
              ;; Predetermine available heights
              (loop :for row :from 0 :below rows :do
                (incf row-height (aref row-heights row))
                (decf avail-height (aref row-heights row))
                (setf (aref row-avail-heights row) avail-height))
              
              ;; Sanity check
              (assert (and (< row-width (1+ object-width))
                           (>= row-width object-width)))
              (assert (and (< row-height (1+ row-height))
                           (>= row-height object-height)))

              ;; Pass 2, assign locations to all children
              (loop :for row :from 0 :below rows :do
                (loop :for col :from 0 :below columns :do
                  (with-slots (left top width height) (aref child-area (+ (* row columns) col))
                    (setf width (aref col-widths col))
                    (setf height (aref row-heights row))
                    (if (> col 0)
                        (setf left (+ (- object-width (aref col-avail-widths (1- col))) object-left))
                        (setf left object-left))
                    (if (> row 0)
                        (setf top (+ (- object-height (aref row-avail-heights (1- row))) object-left))
                        (setf top object-top)))))))))))

  ;; Log the children internal areas
  (with-slots (child-area) object
    (with-local-slots (columns rows) object
      (loop :for row :from 0 :below rows :do
        (loop :for col :from 0 :below columns :do
          (let ((array-offset (+ (* row columns) col)))
            (v:debug :layout "[c-l-c-a] {g-l} child ~d,~d (~d) internal area (~d ~d) @ (~d ~d)"
                    col row array-offset
                    (width (aref child-area array-offset)) (height (aref child-area array-offset))
                    (left (aref child-area array-offset)) (top (aref child-area array-offset)))))))))

(defmethod on-paint ((object grid-layout) &key &allow-other-keys)
  ;; Make sure we have layout complete
  (when (eql (slot-value object 'child-area) nil)
    (v:debug :layout "[on-paint] {grid-layut} Forcing layout ~a" (print-raw-object object))
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

;;;; functions ================================================================

;; (declaim (ftype (function (integer integer grid-layout) %grid-layout-cell) grid-layout-cell))
(defun grid-layout-cell (column row object)
  "Return %GRID-LAYOUT-CELL for child of GRID-LAYOUT at COLUMN,ROW"
  
  (with-slots (columns cell-data) object
    (nth (+ (* row columns) column) cell-data)))

(defclass %grid-layout-cell (border-mixin
                            h-align-mixin ; when area is < col/row size, how to position horizontally
                            padding-mixin ; border to content
                            spacing-mixin ; edge to border
                            v-align-mixin) ; When area is < col/row size, how to position vertically
  ())
(defun grid-layout-cell-options (column row object &key (border nil borderp)
                                                     (border-h nil border-hp)
                                                     (border-v nil border-bp)
                                                     (border-left nil border-leftp)
                                                     (border-right nil border-rightp)
                                                     (border-top nil border-topp)
                                                     (border-bottom nil border-bottomp)
                                                     (h-align nil h-alignp)
                                                     (padding nil paddingp)
                                                     (padding-h nil padding-hp)
                                                     (padding-v nil padding-vp)
                                                     (padding-left nil padding-leftp)
                                                     (padding-right nil padding-rightp)
                                                     (padding-top nil padding-topp)
                                                     (padding-bottom nil padding-bottomp)
                                                     (spacing nil spacingp)
                                                     (spacing-h nil spacing-hp)
                                                     (spacing-v nil spacing-vp)
                                                     (spacing-left nil spacing-leftp)
                                                     (spacing-right nil spacing-rightp)
                                                     (spacing-top nil spacing-topp)
                                                     (spacing-bottom nil spacing-bottomp)
                                                     (v-align nil v-alignp))
  "Set options for a specific %GRID-LAYOUT-CELL.

For BORDER, PADDING, and SPACING, you can use the names directly to affect all
4 sides, or with -H to affect -LEFT and -LEFT, or -V to affect -TOP and
-BOTTOM. Note that its perfectly acceptable to use both the base name the
other options in the same call; In that case, the are setin the order BASE,
-H, -V, rest.

For example, you could use BORDER and BORDER-LEFT to set the BORDER-RIGHT,
BORDER-TOP, and BORDER-BOTTOM to the same BORDER while setting BORDER-LEFT to
a different BORDER:

    (grid-layout-cell-options 0 0 object :BORDER other-border :BORDER-LEFT
                              left-border)

Also note that in the above case, BORDER-LEFT would get set twice: Once for
the original BORDER, then again for the BORDER-LEFT.

This is the function used internally to initialize a new GRID-LAYOUT from
keyword options when provided to DEFGRID-LAYOUT."

  (macrolet ((set-field (field value)
               `(setf (slot-value cell ',field) ,value))
             (set-area-fields (name value)
               `(dolist (field (list (a:symbolicate ,name "-LEFT")
                                     (a:symbolicate ,name "-RIGHT")
                                     (a:symbolicate ,name "-TOP")
                                     (a:symbolicate ,name "-BOTTOM")))
                  (set-field field ,value))))
    
    ;; TODO:  Should be able to make a macro for the big ones ...
    ;;        I failed to get one to work...
    
    (let ((cell (grid-layout-cell column row object)))
      ;; borders
      (when borderp
        (set-area-fields border border))
      (when border-hp
        (set-field border-left border-h)
        (set-field border-right border-h))
      (when border-bp
        (set-field border-top border-v)
        (set-field border-bottom border-v))
      (when border-leftp
        (set-field border-left border-left))
      (when border-rightp
        (set-field border-right border-right))
      (when border-topp
        (set-field border-top border-top))
      (when border-bottomp
        (set-field border-bottom border-bottom))

      ;; h-align
      (when h-alignp
        (set-field h-align h-align))
      
      ;; padding
      (when paddingp
        (set-area-fields padding padding))
      (when padding-hp
        (set-field padding-left padding-h)
        (set-field padding-right padding-h))
      (when padding-vp
        (set-field padding-top padding-v)
        (set-field paddingbottom padding-v))
      (when padding-leftp
        (set-field padding-left padding-left))
      (when padding-rightp
        (set-field padding-right padding-right))
      (when padding-topp
        (set-field padding-top padding-top))
      (when padding-bottomp
        (set-field padding-bottom padding-bottom))

      ;; v-align
      (when v-alignp
        (set-field v-align v-align))

      ;; padding
      (when spacingp
        (set-area-fields spacing spacing))
      (when spacing-hp
        (set-field spacing-left spacing-h)
        (set-field spacing-right spacing-h))
      (when spacing-vp
        (set-field spacing-top spacing-v)
        (set-field spacingbottom spacing-v))
      (when spacing-leftp
        (set-field spacing-left spacing-left))
      (when spacing-rightp
        (set-field spacing-right spacing-right))
      (when spacing-topp
        (set-field spacing-top spacing-top))
      (when spacing-bottomp
        (set-field spacing-bottom spacing-bottom)))))

;; (declaim (ftype (function (T (or symbol list) integer integer grid-layout &key (:recalc boolean)) null) grid-layout-cell-set))
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

;; (declaim (ftype (function (integer integer grid-layout) t) grid-layout-child))
(defun grid-layout-child (column row object)
  "Return child object of GRID-LAYOUT at COLUMN,ROW"

  (with-slots (columns content) object
    (nth (+ (* row columns) column) content)))

;; (declaim (ftype (function (integer grid-layout) %grid-layout-column) grid-layout-column))
(defun grid-layout-column (column object)
  "Return %GRID-LAYOUT-COLUMN for specified column of specified GRID-LAYOUT object."
  
  (aref (slot-value object 'column-data) column))

;; (declaim (ftype (function (integer grid-layout) list) grid-layout-column-cells))
(defun grid-layout-column-cells (column object)
  "Return all %GRID-LAYOUT-CELL's for a column withn GRID-LAYOUT."

  (with-local-slots (rows) object
    (loop :for v :from 0 :below rows
          :collect (grid-layout-cell column v object))))

;; (declaim (ftype (function (T (or symbol list) integer grid-layout &key (:recalc boolean)) null) grid-layout-column-cells-set))
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

(defun grid-layout-column-options (column object &key (width nil widthp) (type nil typep)
                                                   (extra nil extrap) (h-align nil h-alignp)
                                                   (v-align nil v-alignp))
  "Set options for a specific %GRID-LAYOUT-COLUMN.

If you specify WIDTH but do not specify TYPE, the WIDTH-TYPE will be set to
:PERCENT if WIDTH is <= 1.0, and :PERCENT-ALL otherwise. If you specify both
WIDTH and TYPE, your option will be used.

This is the function used internally to initialize a new GRID-LAYOUT from
keyword options when provided to DEFGRID-LAYOUT."
  
  (let ((col-obj (grid-layout-column column object)))
    (when widthp
      (setf (slot-value col-obj 'width) width))
    (if typep
        (setf (slot-value col-obj 'width-type) type)
        (when widthp
          (if (> width 1.0)
              (setf (slot-value col-obj 'width-type) :absolute)
              (setf (slot-value col-obj 'width-type) :percent-all))))
    (when extrap
      (setf (slot-value col-obj 'extra) extra))
    (when h-alignp
      (setf (slot-value col-obj 'h-align) h-align))
    (when v-alignp
      (setf (slot-value col-obj 'v-align) v-align))))

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

;; (declaim (ftype (function (grid-layout) null) grid-layout-reset-cells))
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

;; (declaim (ftype (function (integer grid-layout) %grid-layout-row) grid-layout-row))
(defun grid-layout-row (row object)
  "Return %GRID-LAYOUT-ROW for specified row of specified GRID-LAYOUT object."
  (aref (slot-value object 'row-data) row))

;; (declaim (ftype (function (integer grid-layout) list) grid-layout-row-cells))
(defun grid-layout-row-cells (row object)
  "Return all %GRID-LAYOUT-CELL's for a row within GRID-LAYOUT."

  (with-local-slots (columns) object
    (loop :for h :from 0 :below columns
          :collect (grid-layout-cell h row object))))

;; (declaim (ftype (function (T (or symbol list) integer grid-layout &key (:recalc boolean)) null) grid-layout-row-cells-set))
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

(defun grid-layout-row-options (row object &key (height nil heightp) (type nil typep)
                                             (extra nil extrap) (h-align nil h-alignp)
                                             (v-align nil v-alignp))
  "Set options for a specific %GRID-LAYOUT-ROW.

If you specify HEIGHT but do not specify TYPE, the HEIGHT-TYPE will be set to
:PERCENT if HEIGHT is <= 1.0, and :PERCENT-ALL otherwise. If you specify both
HEIGHT and TYPE, your option will be used.

This is the function used internally to initialize a new GRID-LAYOUT from
keyword options when provided to DEFGRID-LAYOUT."
  
  (let ((row-obj (grid-layout-row row object)))
    (when heightp
      (setf (slot-value row-obj 'height) height))
    (if typep
        (setf (slot-value row-obj 'height-type) type)
        (when heightp
          (if (> height 1.0)
              (setf (slot-value row-obj 'height-type) :absolute)
              (setf (slot-value row-obj 'height-type) :percent-all))))
    (when extrap
      (setf (slot-value row-obj 'extra) extra))
    (when h-alignp
      (setf (slot-value row-obj 'h-align) h-align))
    (when v-alignp
      (setf (slot-value row-obj 'v-align) v-align))))

