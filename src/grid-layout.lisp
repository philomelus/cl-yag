(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

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

;; TODO: Allow :columns to specify LAYOUT-COLUMN-DATA
;; TODO: Allow :rows to specify LAYOUT-ROW-DATA
;; TODO: Allow :cells to specify LAYOUT-CELL-DATA
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
          (push (make-instance 'layout-cell-data) cell-data)))
      (assert (= (length cell-data) (* columns rows)))
      
      ;; Allocate colums
      (setf column-data (make-array columns :element-type (or 'layout-column-data nil)))
      (loop :for h :from 0 :below columns :do
        (setf (aref column-data h) (make-instance 'layout-column-data)))
      
      ;; Allocate rows
      (setf row-data (make-array rows :element-type (or 'layout-row-data nil)))
      (loop :for v :from 0 :below rows :do
        (setf (aref row-data v) (make-instance 'layout-row-data))))))

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

(defmethod layout-cell ((object grid-layout) &key (column nil columnp) (row nil rowp))
  "Return LAYOUT-CELL-DATA for child of GRID-LAYOUT at COLUMN,ROW"

  (assert (and columnp rowp))
  (assert (typep column 'coordinate))
  (assert (typep row 'coordinate))
  
  (with-slots (columns cell-data) object
    (nth (+ (* row columns) column) cell-data)))

(defmethod layout-cell-options ((object grid-layout)
                                &key (column nil columnp)
                                  (row nil rowp)
                                  (border nil borderp)
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
  "Set options for a specific LAYOUT-CELL-DATA within a GRID-LAYOUT.

For BORDER, PADDING, and SPACING, you can use the names directly to affect all
4 sides, or with -H to affect -LEFT and -LEFT, or -V to affect -TOP and
-BOTTOM. Note that its perfectly acceptable to use both the base name the
other options in the same call; In that case, the are setin the order BASE,
-H, -V, rest.

For example, you could use BORDER and BORDER-LEFT to set the BORDER-RIGHT,
BORDER-TOP, and BORDER-BOTTOM to the same BORDER while setting BORDER-LEFT to
a different BORDER:

    (layout-cell-options :column 0 :row 0 object :BORDER other-border :BORDER-LEFT
                         left-border)

Also note that in the above case, BORDER-LEFT would get set twice: Once for
the original BORDER, then again for the BORDER-LEFT.

This is the function used internally to initialize a new GRID-LAYOUT from
keyword options when provided to DEFGRID-LAYOUT."

  (assert (and columnp rowp))
  (assert (typep column 'coordinate))
  (assert (typep row 'coordinate))
  
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
    
    (let ((cell (layout-cell object :column column :row row)))
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

(defmethod layout-child ((object grid-layout) &key (column nil columnp) (row nil rowp))
  "Return child object of GRID-LAYOUT at COLUMN,ROW"

  (assert (and columnp rowp))
  (assert (typep column 'coordinate))
  (assert (typep row 'coordinate))
  
  (with-slots (columns content) object
    (nth (+ (* row columns) column) content)))

(defmethod layout-column ((object grid-layout) &key (column nil columnp))
  "Return LAYOUT-COLUMN-DATA for specified column of specified GRID-LAYOUT object."

  (assert columnp)
  (assert (typep column 'coordinate))
  (aref (slot-value object 'column-data) column))

(defmethod layout-column-cells ((object grid-layout) &key (column nil columnp))
  "Return all LAYOUT-CELL-DATA's for a column within GRID-LAYOUT."

  (assert columnp)
  (assert (typep column 'coordinate))
  
  (with-local-slots (rows) object
    (loop :for v :from 0 :below rows
          :collect (layout-cell object :column column :row v))))

(defmethod layout-column-options ((object grid-layout) &key (column nil columnp)
                                                         (width nil widthp) (type nil typep)
                                                         (extra nil extrap) (h-align nil h-alignp)
                                                         (v-align nil v-alignp))
  "Set options for a specific LAYOUT-COLUMN-DATA.

If you specify WIDTH but do not specify TYPE, the WIDTH-TYPE will be set to
:PERCENT if WIDTH is <= 1.0, and :PERCENT-ALL otherwise. If you specify both
WIDTH and TYPE, your option will be used.

This is the function used internally to initialize a new GRID-LAYOUT from
keyword options when provided to DEFGRID-LAYOUT."

  (assert columnp)
  (assert (typep column 'coordinate))
  
  (let ((col-obj (layout-column object :column column)))
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
        (with-borders (bl br bt bb) (layout-cell object :column h :row v)
          (unless (and (eql bl nil) (eql br nil) (eql bt nil) (eql bb nil))
            (let ((style (get-theme-style object)))
              (unless (eql bl nil)
                (paint-border-left bl style (aref (slot-value object 'child-area) (+ (* v columns) h))))
              (unless (eql br nil)
                (paint-border-right br style (aref (slot-value object 'child-area) (+ (* v columns) h))))
              (unless (eql bt nil)
                (paint-border-top bt style (aref (slot-value object 'child-area) (+ (* v columns) h))))
              (unless (eql bb nil)
                (paint-border-bottom bb style (aref (slot-value object 'child-area) (+ (* v columns) h))))))))))

  ;; Let base take care of children
  (my-next-method))

(defmethod layout-row ((object grid-layout) &key (row nil rowp))
  "Return LAYOUT-ROW-DATA for specified row of specified GRID-LAYOUT object."

  (assert rowp)
  (assert (typep row 'coordinate))
  
  (aref (slot-value object 'row-data) row))

(defmethod layout-row-cells ((object grid-layout) &key (row nil rowp))
  "Return all LAYOUT-CELL-DATA's for a row within GRID-LAYOUT."

  (assert rowp)
  (assert (typep row 'coordinate))
  
  (with-local-slots (columns) object
    (loop :for h :from 0 :below columns
          :collect (layout-cell object :column h :row row))))


(defmethod layout-row-options ((object grid-layout) &key (row nil rowp)
                                                      (height nil heightp) (type nil typep)
                                                      (extra nil extrap) (h-align nil h-alignp)
                                                      (v-align nil v-alignp))
  "Set options for a specific LAYOUT-ROW-DATA.

If you specify HEIGHT but do not specify TYPE, the HEIGHT-TYPE will be set to
:PERCENT if HEIGHT is <= 1.0, and :PERCENT-ALL otherwise. If you specify both
HEIGHT and TYPE, your option will be used.

This is the function used internally to initialize a new GRID-LAYOUT from
keyword options when provided to DEFGRID-LAYOUT."

  (assert rowp)
  (assert (typep row 'coordinate))
  
  (let ((row-obj (layout-row object :row row)))
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

;;;; FUNCTIONS ================================================================

(declaim (ftype (function (grid-layout) null) grid-layout-reset-cells))
(defun grid-layout-reset-cells (object)
  "Update LAYOUT-CELL-DATA's of GRID-LAYOUT according to current COLUMNS and
ROWS."

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
                (mapc #'layout-reset-cell cell-data))
                
              ;; Allocate new cells
              (progn
                ;; Reset current cells
                (mapc #'layout-reset-cell cell-data)
                
                ;; Make up difference
                (loop :for c :from 0 :below (- needed-cells num-cells) :do
                  (push (make-instance 'layout-cell-data) cell-data))))
          
          ;; Rest current cells
          (mapc #'layout-reset-cell cell-data)))

    ;; Reset/update columns
    (let ((num-columns (length column-data)))
      (if (/= num-columns columns)
          (progn
            (setf column-data (make-array columns :element-type (or 'layout-column-data nil)))
            (loop :for h :from 0 :below columns :do
              (setf (aref column-data h) (make-instance 'layout-column-data))))
          (mapc #'layout-reset-column column-data)))

    ;; Reset/update rows
    (let ((num-rows (length row-data)))
      (if (/= num-rows rows)
          (progn
            (setf row-data (make-array rows :element-type (or 'layout-row-data nil)))
            (loop :for v :from 0 :below rows :do
              (setf (aref row-data v) (make-instance 'layout-row-data))))
          (mapc #'layout-reset-row row-data))))
  nil)

