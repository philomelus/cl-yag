(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; LAYOUT-BASE ==============================================================

;; These can be combined with widgets in the content list to change the way
;; the widget is layed out (for example
;;
;; :content (list (list (defwindow ...) :max-width))
;;
;; would make sure the window used the maximum available width.
(deftype layout-child-options () '(member :min-height :min-width :max-height :max-width
                                   :left :center :right
                                   :top :middle :bottom))
(deftype layout-coordinate-option-type () '(member :auto :auto-min :auto-max))
(deftype layout-left-type () '(or coordinate layout-coordinate-option-type (member :left :center :right)))
(deftype layout-top-type () '(or coordinate layout-coordinate-option-type (member :top :middle :bottom)))
(deftype layout-width-type () '(or coordinate layout-coordinate-option-type))
(deftype layout-height-type () '(or coordinate layout-coordinate-option-type))

(deftype layout-original-left-type () '(or null layout-left-type))
(deftype layout-original-top-type () '(or null layout-top-type))
(deftype layout-original-width-type () '(or null layout-width-type))
(deftype layout-original-height-type () '(or null layout-height-type))

(defclass layout-base (area-mixin-base
                       content-mixin
                       parent-mixin)
  ((height :type layout-left-type :initform :auto :initarg nil :documentation "HEIGHT coordinate of allocated area. Type shouldn't be changed after creation.")
   (left :type layout-top-type :initform :auto :initarg nil :documentation "LEFT coordinate of allocated area. Type shouldn't be changed after creation.")
   (top :type layout-top-type :initform :auto :initarg nil :documentation "TOP coordinate of allocated area. Type shouldn't be changed after creation.")
   (width :type layout-height-type :initform :auto :initarg nil :documentation "WIDTH of allocated area. Type shouldn't be changed after creation.")

   ;; Internal
   (changing :type boolean :initform nil :documentation "When T, causes LAYOUT-CHANGED calls to be ignored.")
   (child-area :initform nil :documentation "Holds actual area allocated to each child, even if they don't use it.")
   
   (original-height :type layout-original-left-type :initform nil :documentation "Holds original HEIGHT slot value. Used when re-layout is required.")
   (original-left :type layout-original-top-type :initform nil :documentation "Holds original LEFT slot value. Used when re-layout is required.")
   (original-top :type layout-original-width-type :initform nil :documentation "Holds original TOP slot value. Used when re-layout is required.")
   (original-width :type layout-original-height-type :initform nil :documentation "Holds original WIDTH slot value. Used when re-layout is required.")))

;;; METHODS ---------------------------------------------------------

(defmethod calc-area :around ((object layout-base) &key)
  "Initialize our size, setup internal CHILD-AREA, call derived object's
CALC-AREA to layout children, then allow all child LAYOUT-BASE derived objects
to calculate their layout."
  
  ;; Calculate our area if needed
  (calc-layout-area object)

  ;; Calculate our internal child area if needed
  (when (eql (slot-value object 'child-area) nil)
    (calc-layout-child-areas object))

  ;; Let derived object handler deal with its children
  (call-next-method)

  ;; Allow any child layouts to layout as well
  (dolist (child (content object))
    (let ((child-object (foro child)))
      (unless (eql child-object nil)
        (when (typep child-object 'layout-base)
          (calc-area child-object))))))

(defmethod calc-height (type area (object layout-base))
  "Called to return height of layout. In particular, this specific method is
called when we are a child of another layout."

  (slot-value area 'height))

(defmethod calc-left (type area width height (object layout-base))
  "Called to return left coordinate. In particular, this specific method is
called when we are a child of another layout."

  (slot-value area 'left))

(defmethod calc-top (type area width height (object layout-base))
  "Called to return top coordinate. In particular, this specific method is
called when we are a child of another layout."

  (slot-value area 'top))

(defmethod calc-width (type area (object layout-base))
  "Called to return width of layout. In particular, this specific method is
called when we are a child of another layout."

  (slot-value area 'width))

(defmethod (setf content) :after (value (object layout-base))
  "After content update, make sure options area valid."
  
  (validate-layout-options object))

(defmethod (setf content) :around (value (object layout-base))
  "When updating content, allow reuse of previous contents by tracking objects
that exist both before and after update. Reset persistant objects. Update
internal calc-* slots."
  
  (let ((old (content object)))
    (let ((areas (make-array (length old) :adjustable nil :fill-pointer nil)))
      ;; Save old areas
      (dotimes (n (length old))
        (setf (aref areas n) (area-rect (foro (nth n old)))))

      ;; Update the content
      (call-next-method)

      (let ((new (content object)))
        (let ((common (intersection old new :key #'(lambda (o) (foro o)))))
          ;; Locate objects in both old and new
          
          (dolist (child common)
            (assert (not (eql child nil)))
            ;; Reset area
            (let ((op (position (foro child) old :key #'(lambda (o) (foro o)) :test #'eql))
                  (np (position (foro child) new :key #'(lambda (o) (foro o)) :test #'eql)))
              (assert (not (eql op nil)))
              (assert (not (eql np nil)))
              (reset-area (foro (nth op old)) (foro (nth np new)))))))))

  ;; Save area options
  (layout-save-original object)

  ;; Force relayout
  (unless (eql (slot-value object 'child-area) nil)
    (setf (slot-value object 'child-area) nil)))

(defmethod initialize-instance :after ((object layout-base) &key)
  (validate-layout-options object)
  (layout-save-original object))

(defmethod on-char (key mods (obj layout-base) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
     (when (on-char key mods (foro child))
       (return-from on-char t))))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj layout-base) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
     (if (on-mouse-down x y b (foro child))
         (return-from on-mouse-down t))))
  nil)

(defmethod on-mouse-move (x y dx dy (obj layout-base) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
      (on-mouse-move x y dx dy (foro child)))))

(defmethod on-mouse-up (x y b (obj layout-base) &key)
  (dolist (child (content obj))
    (unless (eql child nil)
      (on-mouse-up x y b (foro child)))))

(defmethod on-paint ((object layout-base) &key)
  ;; Paint children
  (dolist (c (content object))
    (unless (eql c nil)
      (let ((co (foro c)))
        (on-paint co))))
  (my-next-method))

(defmethod width ((object layout-base))
  (with-slots ((local-width width)) object
    (when (keywordp local-width)
      (assert (typep local-width 'layout-width-type))
      (calc-area (find-parent-layout object))
      (assert (typep (slot-value object 'width) 'coordinate))
      (setf local-width (slot-value object 'width)))
    local-width))

;;;; LAYOUT-CELL-DATA =========================================================

(defclass layout-cell-data (border-mixin
                            h-align-mixin ; when area is < col/row size, how to position horizontally
                            padding-mixin ; border to content
                            spacing-mixin ; edge to border
                            v-align-mixin) ; When area is < col/row size, how to position vertically
  ()
  (:documentation "Data used for layout of a specific CELL within a layout."))

;;;; LAYOUT-COLUMN-DATA =======================================================

(deftype layout-column-width-type () '(member :absolute :percent :percent-all))

(defclass layout-column-data (extra-mixin
                              h-align-mixin
                              v-align-mixin)
  ((width :type layout-width-type :initarg :width :initform :auto :accessor width)
   (width-type :type layout-column-width-type :initform :percent :accessor width-type)
   (h-align :initform :center)  ; Applies to column, unless specified in cell
   (v-align :initform :middle)) ;  Applies to column, unless specified in cell
  (:documentation "Data used for layout calculations of a column in a LAYOUT with columns. CELL
options override options here."))

;; METHODS ------------------------------------------------

(defmethod (setf width) :after (value (object layout-column-data))
  (if (keywordp value)
      (progn
        (assert (equal value :auto))
        (when (not (slot-value object 'extra))
          (setf (slot-value object 'extra) t)
          (v:debug :layout "[SETF WIDTH] {LAYOUT-COLUMN-DATA} automatically set EXTRA to T")))
      (progn
        (assert (typep value 'coordinate))
        ;; Set to absolute or percent?
        (if (<= value 1)
            (progn
              (setf (slot-value object 'width-type) :percent)
              (v:debug :layout "[SETF WIDTH] {LAYOUT-COLUMN-DATA} automatically set WIDTH-TYPE to PERCENT")
              ;; Enable extra?
              (when (not (slot-value object 'extra))
                (setf (slot-value object 'extra) t)
                (v:debug :layout "[SETF WIDTH] {LAYOUT-COLUMN-DATA} automatically set EXTRA to T")))
            (progn
              (setf (slot-value object 'width-type) :absolute)
              (v:debug :layout "[SETF WIDTH] {LAYOUT-COLUMN-DATA} automatically set WIDTH-TYPE to ABSOLUTE")
              ;; Disable extra?
              (when (slot-value object 'extra)
                (setf (slot-value object 'extra) nil)
                (v:debug :layout "[SETF WIDTH] {LAYOUT-COLUMN-DATA} automatically set EXTRA to NIL"))))))
  (my-next-method))

;;;; LAYOUT-ROW-DATA ==========================================================

(deftype layout-row-height-type () '(member :absolute :percent :percent-all))

(defclass layout-row-data (extra-mixin
                           h-align-mixin
                           v-align-mixin)
  ((height :type layout-height-type :initarg :height :initform :auto :accessor height)
   (height-type :type layout-row-height-type :initform :percent :accessor height-type)
   (h-align :initform :center)
   (v-align :initform :middle))
  (:documentation "Data used for layout calculations of a row in a LAYOUT that uses rows. CELL
options override options here."))

;; METHODS ------------------------------------------------

(defmethod (setf height) :after (value (object layout-row-data))
  (if (keywordp value)
      (progn
        (assert (equal value :auto))
        (when (not (slot-value object 'extra))
          (setf (slot-value object 'extra) t)
          (v:debug :layout "[SETF HEIGHT] {LAYOUT-ROW-DATA} automatically set EXTRA to T")))
      (progn
        (assert (typep value (values 'integer 'float)))
        ;; Set to absolute or percent?
        (if (<= value 1)
            (progn
              (setf (slot-value object 'height-type) :percent)
              (v:debug :layout "[SETF HEIGHT] {LAYOUT-ROW-DATA} automatically set HEIGHT-TYPE to PERCENT")
              ;; Enable extra?
              (when (not (slot-value object 'extra))
                (setf (slot-value object 'extra) t)
                (v:debug :layout "[SETF HEIGHT] {LAYOUT-ROW-DATA} automatically set EXTRA to T")))
            (progn
              (setf (slot-value object 'height-type) :absolute)
              (v:debug :layout "[SETF HEIGHT] {LAYOUT-ROW-DATA} automatically set HEIGHT-TYPE to ABSOLUTE")
              ;; Disable extra?
              (when (slot-value object 'extra)
                (setf (slot-value object 'extra) nil)
                (v:debug :layout "[SETF HEIGHT] {LAYOUT-ROW-DATA} automatically set EXTRA to NIL"))))))
  (my-next-method))

;;;; LAYOUT ===================================================================
;; Simple layout allowing single child object that can be use auto area

(defclass layout (layout-base)
  ((cell :type layout-cell-data :initform (make-instance 'layout-cell-data))))

(defmacro deflayout (&rest rest &key &allow-other-keys)
  `(make-instance 'layout ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod calc-area ((parent layout) &key)

  ;; Calculate parent area if needed
  (calc-layout-area parent)

  ;; Calculate our children areas if needed
  (when (eql (slot-value parent 'child-area) nil)
    (calc-layout-child-areas parent))

  (with-slots (child-area content) parent
    ;; Only 1 possible child here
    (let ((child (first content))
          (cp 0))
      (let* ((child-internal (aref child-area cp))
             (oa (make-instance '%rect :left (left child-internal) :top (top child-internal) :width (width child-internal) :height (height child-internal))))
      
        (with-slots ((cl left) (ct top) (cw width) (ch height)) (foro child)
          (v:debug :layout "[calc-area] {layout} child calculating (~a ~a) @ (~a ~a)"
                   cw ch cl ct)
        
          (macrolet ((do-calc (var func)
                       `(if (keywordp ,var)
                            (progn
                              (setf ,var (,func ,var oa (foro child)))
                              t)
                            nil))
                     (do-calc-alt (w h var func)
                       `(if (keywordp ,var)
                            (progn
                              (setf ,var (,func ,var oa ,w ,h (foro child)))
                              t)
                            nil)))
            (let ((cwp (do-calc cw calc-width))
                  (chp (do-calc ch calc-height))
                  (clp (do-calc-alt cw ch cl calc-left))
                  (ctp (do-calc-alt cw ch ct calc-top)))

              ;; If any of the area was not-calculated, update internal area
              (unless cwp
                (setf (slot-value child-internal 'width) cw))
              (unless chp
                (setf (slot-value child-internal 'height) ch))
              (unless clp
                (setf (slot-value child-internal 'left) cl))
              (unless ctp
                (setf (slot-value child-internal 'top) ct))
              
              ;; Handle options
              (let* ((co (find-if #'(lambda (o) (eql (foro o) (foro child))) content))
                     options)
                (when (consp co)
                  (setq options (rest co)))
                ;; Does it have options?
                (when (> (length options) 0)
                  (dolist (option options)
                    (case option
                      (:bottom
                       (with-local-slots ((ci-top top) (ci-height height)) child-internal
                         (with-local-slots ((ch-top top) (ch-height height)) (foro child)
                           (let ((ci-bottom (+ ci-top ci-height))
                                 (ch-bottom (+ ch-top ch-height)))
                             (if (> ci-bottom ch-bottom)
                                 (setf (slot-value (foro child) 'top) (- ci-bottom ch-height)))))))
                      (:center
                       ;; Move object to horizontal center
                       (if (> (slot-value child-internal 'width) (slot-value (foro child) 'width))
                           (setf (slot-value (foro child) 'left) (+ (slot-value child-internal 'left)
                                                                    (/ (- (slot-value child-internal 'width)
                                                                          (slot-value (foro child) 'width))
                                                                       2)))))
                      (:left
                       ;; Move child to left edge
                       (if (< (slot-value child-internal 'left) (slot-value (foro child) 'left))
                           (setf (slot-value (foro child) 'left) (slot-value child-internal 'left))))
                      (:middle
                       (with-local-slots ((ci-height height)) child-internal
                         (with-local-slots ((ch-height height)) (foro child)
                           (if (> ci-height ch-height)
                               (setf (slot-value (foro child) 'top) (+ (slot-value child-internal 'top)
                                                                       (/ (- ci-height ch-height) 2)))))))
                      (:min-height) ; nothing to do when there are no siblings
                      (:min-width)  ; nothing to do when there are no siblings
                      (:max-height) ; nothing to do when there are no siblings
                      (:max-width)  ; nothing to do when there are no siblings
                      (:right
                       ;; Move child to right edge
                       (let ((ca-right (+ (slot-value child-internal 'left) (slot-value child-internal 'width)))
                             (ch-right (+ (slot-value (foro child) 'left) (slot-value (foro child) 'width))))
                         (if (> ca-right ch-right)
                             (setf (slot-value (foro child) 'left) (- ca-right (slot-value (foro child) 'width))))))
                      (:top
                       ;; Move child to top edge
                       (if (< (slot-value child-internal 'top) (slot-value (foro child) 'top))
                           (setf (slot-value (foro child) 'top) (slot-value child-internal 'top))))))))

              ;; Log updated/changed area
              (v:debug :layout "[calc-area] {layout} child area (~d ~d) @ (~d ~d) ~a"
                       cw ch cl ct (print-raw-object child)))))))))

(defmethod calc-layout-child-areas ((object layout))
  "Calculate the over-all area of each child.
Note that children may use different sizes themselves, this is just the
area allocated to them, whether they choose to use it or not."
  
  (with-slots (child-area) object
    (let ((num-children (length (content object))))
      (assert (or (= num-children 0) (= num-children 1)))
      (setf child-area (make-array 1 :adjustable nil))

      (when (= num-children 1)
        (with-local-slots (left top width height) object
          ;; Set initial area for child
          (setf (aref child-area 0) (make-instance '%rect :left left :top top
                                                          :width width :height height))

          (let ((child (aref child-area 0)))
            (v:debug :layout "[CALC-LAYOUT-CHILD-AREAS] {LAYOUT} child internal area (~d ~d) @ (~d ~d)"
                    (width child) (height child) (left child) (top child))))))))

(defmethod layout-cell ((object layout) &key)
  "Return LAYOUT-DATA-CELL for LAYOUT."
  
  (slot-value object 'cell))

(defmethod layout-changed ((object layout) &key (parent nil parentp) (child nil childp))
  (declare (ignorable parent parentp child childp))
  
  ;; If layout-changed is not currently disabled
  (unless (slot-value object 'changing)
    (if (and childp child)
        ;; Our (only) child changed, so force it to relayout
        (progn
          (assert (= (length (content object)) 1))

          (let ((child (foro (first (content object)))))
            ;; Reset child to orignal state
            (reset-area child)
          
            ;; Re-layout
            (calc-area object)))
        
        ;; The layout that owns us changed
        (progn
          (v:warn :layout "[LAYOUT-CHANGED] {LAYOUT} parent layout changed")
          (error "not implemented")
          )))
  
  (my-next-method))

(defmethod on-paint ((object layout) &key)
  "Draw LAYOUT, which means take care of LAYOUT-CELL-DATA and then pass on to LAYOUT-BASE."

  ;; Draw borders if they exist
  (with-slots (cell) (foro object)
    (with-borders (bl br bt bb) cell
      (let ((blp (eql bl nil)) (brp (eql br nil))
            (btp (eql bt nil)) (bbp (eql bb nil)))
        (unless (and blp brp btp bbp)
          (let ((theme (find-theme (foro object))))
            (unless blp
              (paint-border-left bl (foro object) theme :blend-top btp :blend-bottom bbp))
            (unless brp
              (paint-border-right br (foro object) theme :blend-top btp :blend-bottom bbp))
            (unless btp
              (paint-border-top bt (foro object) theme :blend-left blp :blend-right brp))
            (unless bbp
              (paint-border-bottom bb (foro object) theme :blend-left blp :blend-right brp)))))))

  ;; Let layout-base paint the child
  (my-next-method))

(defmethod left ((object layout))
  (slot-value object 'left))

(defmethod top ((object layout))
  (slot-value object 'top))

(defmethod right ((object layout))
  (+ (left object) (width object)))

(defmethod bottom ((object layout))
  (+ (top object) (height object)))

(defmethod height ((object layout))
  (slot-value object 'height))

(defmethod width ((object layout))
  (slot-value object 'width))

;;;; functions ================================================================

(defun area-allocated (object)
  "Returns the area allocated to a widget.
This will be the same or larger than the widgets actual area slots.
Returns nil if not found."

  (let ((owner (find-parent-content object)))
    (let ((op (position object (content owner) :key #'(lambda (o) (foro o)))))
      (unless (eql op nil)
        (return-from area-allocated (aref (slot-value owner 'child-area) op)))))
  nil)

(defun calc-layout-area (object)
  "Calculate area of a layout."
  
  ;; Start with existing area
  (with-local-slots ((local-left left) (local-top top) (local-width width) (local-height height))
                    object

    ;; Calculate our area if needed
    (when (or (keywordp local-left)
              (keywordp local-top)
              (keywordp local-width)
              (keywordp local-height))
      (v:debug :layout "[calc-layout-area] calculating layout area ~a" (print-raw-object object))
      
      ;; Locate first parent that's layout or has area
      (let ((pam (find-parent-area-or-layout object))
            pam-left pam-top pam-width pam-height)
        ;; Get area of parent.  Is it another layout?
        (if (typep pam 'layout-base)
            ;; Yes, so get our area from its child-area
            (let ((pam-offset (position object (content pam))))
              (assert (not (eql pam-offset nil)))
              (with-local-slots ((pl left) (pt top) (pw width) (ph height))
                                (aref (slot-value pam 'child-area) pam-offset)
                (setf pam-left pl pam-top pt pam-width pw pam-height ph)
                (assert (typep pam-left 'layout-left-type))
                (assert (typep pam-top 'layout-top-type))
                (assert (typep pam-width 'layout-width-type))
                (assert (typep pam-height 'layout-height-type))))
            ;; Nope, so get the actual area
            (setf pam-left (slot-value pam 'left)
                  pam-top (slot-value pam 'top)
                  pam-width (slot-value pam 'width)
                  pam-height (slot-value pam 'height)))

        (v:debug :layout "[calc-layout-area] parent area: (~d ~d) @ (~d ~d) ~a"
                 pam-width pam-height pam-left pam-top (print-raw-object object))
        
        ;; If parent has borders
        (when (typep pam 'border-mixin)

          ;; DO NOT USER with-border, as its too early in compilation to
          ;; access it without circular dependencies
          (let ((pam-border-left (border-left pam))
                (pam-border-right (border-right pam))
                (pam-border-top (border-top pam))
                (pam-border-bottom (border-bottom pam)))
           
            (let ((pam-pad-left 0)
                  (pam-pad-top 0)
                  (pam-pad-right 0)
                  (pam-pad-bottom 0))

              ;; If parent has padding
              (when (typep pam 'padding-mixin)
                (setq pam-pad-left (padding-left pam))
                (setq pam-pad-top (padding-top pam))
                (setq pam-pad-right (padding-right pam))
                (setq pam-pad-bottom (padding-bottom pam)))
              
              ;; Adjust for border sizes
              (unless (eql pam-border-left nil)
                (incf pam-left (+ (thickness pam-border-left) pam-pad-left))
                (decf pam-width (+ (thickness pam-border-left) pam-pad-left)))
              (unless (eql pam-border-top nil)
                (incf pam-top (+ (thickness pam-border-top) pam-pad-top))
                (decf pam-height (+ (thickness pam-border-top) pam-pad-top)))
              (unless (eql pam-border-right nil)
                (decf pam-width (+ (thickness pam-border-right) pam-pad-right)))
              (unless (eql pam-border-bottom nil)
                (decf pam-height (+ (thickness pam-border-bottom) pam-pad-bottom))))))

        ;; If parent has spacing
        (when (typep pam 'spacing-mixin)
          ;; Adjust for spacing
          (let ((pam-space-left (spacing-left pam))
                (pam-space-top (spacing-top pam)))
            (incf pam-left pam-space-left)
            (incf pam-top pam-space-top)
            (decf pam-width (+ pam-space-left (spacing-right pam)))
            (decf pam-height (+ pam-space-top (spacing-bottom pam)))))
        
        ;; Save our calculate area where needed
        (when (keywordp local-height)
          (setf (slot-value object 'height) pam-height))
        (when (keywordp local-width)
          (setf (slot-value object 'width) pam-width))
        (when (keywordp local-left)
          (setf (slot-value object 'left) pam-left))
        (when (keywordp local-top)
          (setf (slot-value object 'top) pam-top))
        
        (v:debug :layout "[calc-layout-area] calculated area: (~d ~d) @ (~d ~d) ~a"
                 pam-width pam-height pam-left pam-top (print-raw-object object))
        (v:debug :layout "[calc-layout-area] actual area: (~d ~d) @ (~d ~d) ~a"
                 (slot-value object 'width) (slot-value object 'height) (slot-value object 'left)
                 (slot-value object 'top) (print-raw-object object))))))

(defun dump-layout (object &optional (indent ""))
  (when (not (eql object nil))
    (v:debug :layout "~a~a:~%" indent (print-raw-object object))
    (with-slots (content) object
      (dotimes (n (length (content object)))
        (let ((child (nth n content)))
          (unless (equal child nil)
            (v:info :layout "~achild ~d area (~d ~d) @ (~d ~d)~%" (concatenate 'string indent "  ") n
                     (slot-value child 'width) (slot-value child 'height) (slot-value child 'left) (slot-value child 'top))
            (when (typep child 'layout-base)
              (dump-layout child (concatenate 'string indent "  ")))))))))

(defun find-parent-area-or-layout (object &key (failp t))
  "Locate first parent that either has area or is has layout-base as a superclass.
Generates error on failure."
  
  (assert (typep object 'parent-mixin-base))
  
  (let ((p (parent object)))
    (loop
      (when (typep p 'manager)
        (if failp
            (error "found MANAGER when looking for AREA-MIXIN-BASE or LAYOUT-BASE")
            (return-from find-parent-area-or-layout nil)))
      
      (when (eql p nil)
        (if failp
            (error "no parent when looking for AREA-MIXIN-BASE or LAYOUT-BASE")
            (return-from find-parent-area-or-layout nil)))

      ;; If its a layout
      (if (typep p 'layout-base)
          (return p))
      
      ;; If it has area
      (if (typep p 'area-mixin-base)
          (return p))

      ;; Get next parent
      (assert (typep p 'parent-mixin-base))
      (setf p (parent p)))))

(defun find-parent-content (object &key (failp t))
  "Return the first parent of object with type content-mixin.
Generates error when no parent has content-mixin as a superclass."

  (assert (typep object 'parent-mixin-base))
  
  (let ((parobj (parent object)))
    (loop
      (when (typep parobj 'content-mixin-base)
        (return-from find-parent-content parobj))
      (unless (typep parobj 'parent-mixin-base)
        (if failp
            (error "no more parents when looking for CONTENT-MIXIN")
            (return-from find-parent-content nil)))
      (setq parobj (parent parobj)))))

(defun find-parent-layout (object &key (failp nil))
  "Ascend parents until a layout-base is found, then return it.  If none are found will return NIL."

  (assert (typep object 'parent-mixin-base))
  
  (let ((mod (parent object)))
    (loop
      (when (eql mod nil)
        (if failp
            (error "no more parents while looking for LAYOUT-BASE")
            (return-from find-parent-layout nil)))
      (when (typep mod 'layout-base)
        (return-from find-parent-layout mod))
      (if (typep mod 'parent-mixin)
          (setf mod (parent mod))
          (if failp
              (error "no more parents while looking for LAYOUT-BASE")
              (return-from find-parent-layout nil))))))

(declaim (ftype (function (layout-cell-data) null) layout-reset-cell))
(defun layout-reset-cell (object)
  "Update LAYOUT-CELL-DATA slots to default state."
  
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

(declaim (ftype (function (layout-column-data) null) layout-reset-column))
(defun layout-reset-column (object)
  "Return LAYOUT-COLUMN-DATA slots to default state."

  (with-slots (h-align width width-type v-align) object
    (setf width :auto)
    (setf width-type :percent)
    (setf h-align :center)
    (setf v-align :middle)))

(declaim (ftype (function (layout-base) null) layout-reset-original))
(defun layout-reset-original (object)
  "Return layout of object to original state, allowing it to be
re-layed out."

  (macrolet ((update-value (dest src)
               `(unless (keywordp (slot-value object ,dest))
                  (let ((value (slot-value object ,src)))
                    (when value
                      (setf (slot-value object ,dest) value))))))
    
    (update-value 'width 'original-width)
    (update-value 'height 'original-height)
    (update-value 'left 'original-left)
    (update-value 'top 'original-top))
  
  nil)

(declaim (ftype (function (layout-row-data) null) layout-reset-row))
(defun layout-reset-row (object)
  "Return LAYOUT-ROW-DATA slots to default state."
  (declare (type layout-row-data object))

  (with-slots (h-align height height-type v-align) object
    (setf height :auto)
    (setf height-type :percent)
    (setf h-align :center)
    (setf v-align :middle)))

(declaim (ftype (function (layout-base) null) layout-save-original))
(defun layout-save-original (object)
  (with-slots (left top width height) object
    (flet ((is-keyword (field)
             (if (keywordp field)
                 field
                 nil)))
      (setf (slot-value object 'original-left) (is-keyword left))
      (setf (slot-value object 'original-top) (is-keyword top))
      (setf (slot-value object 'original-width) (is-keyword width))
      (setf (slot-value object 'original-height) (is-keyword height))))
  nil)

(defmethod validate-layout-options (object)
  "Validate options for slots in LAYOUT-BASE. Raises an error when invalid
settings are detected."
  
  ;; Validate layout options
  (with-slots (content) object
    (unless (eql content nil)
      (dolist (child content)
        ;; Child have layout options?
        (when (consp child)
          ;; Yes so validate them
          (let ((options (rest child)))
            (dolist (option (rest child))
              (unless (typep option 'layout-child-options)
                (error "unknown layout option: ~a" option)))
            (when (and (member :min-height options)
                       (member :max-height options))
              (error "MIN-HEIGHT and MAX-HEIGHT are mutually exclusive"))
            (when (and (member :min-width options)
                       (member :max-width options))
              (error "MIN-WIDTH and MAX-WIDTH are mutually exclusive"))
            (when (and (member :top options)
                       (member :middle options)
                       (member :bottom options))
              (error "TOP, MIDDLE, and BOTTOM are mutually exclusive"))
            (when (and (member :left options)
                       (member :center options)
                       (member :rightr options))
              (error "LEFT, CENTER, and RIGHT are mutually exclusive"))))))))

;;;; MACROS ===================================================================

(defmacro layout-change (object)
  "Call LAYOUT-CHANGED for appropriate parent or children of OBJECT."
  
  (a:with-gensyms (parlo block instance child)
    `(block ,block
       (let ((,instance ,object))
         (when (typep ,instance 'parent-mixin-base)
           (let ((,parlo (find-parent-layout ,instance)))
             (unless (eql ,parlo nil)
               (layout-changed ,parlo :child t)
               (return-from ,block))))
         (when (typep ,instance 'content-mixin-base)
           (mapc #'(lambda (,child)
                     (when (typep ,child 'layout-base)
                       (layout-changed ,child :parent t)))
                 (content ,instance))
           (return-from ,block))))))

(defmacro with-changes (object &body body)
  "Prevent LAYOUT-CHANGED calls while performing multiple slot updates that would
normally cause LAYOUT-CHANGED to be called for each change. Generates a single
LAYOUT-CHANGED after body (even if no layout-changed calls were made)."
  
  (a:with-gensyms (parent-layout block instance child parent children layout)
    `(let ((,instance ,object)
           ,parent ,children)
       (declare (ignorable ,parent ,children))
       (block ,block
         ;; Do we have a parent?
         (when (typep ,instance 'parent-mixin-base)
           ;; Yes, so see if we have a layout above us
           (let ((,parent-layout (find-parent-layout ,instance)))
             ;; Did we have a parent layout?
             (unless (eql ,parent-layout nil)
               ;; Yes.
               (push ,parent-layout ,parent)
               (return-from ,block))))
         ;; Not a parent or no parent layout, do we have content?
         (when (typep ,instance 'content-mixin-base)
           ;; Yes, so find any immediate child layouts
           (mapc #'(lambda (,child)
                     (when (typep ,child 'layout-base)
                       (push ,child ,children)))
                 (content ,instance))
           (return-from ,block)))
       ;; Disable layout-changed
       (mapc #'(lambda (,layout)
                 (setf (slot-value ,layout 'changing) t))
             (append ,parent ,children))
       ,@body
       ;; Parent or children?
       (if (null ,parent)
           ;; We are the parent, so re-enable layout-changed and generate once
           ;; per child since we possibly absorbed many (or at least one?)
           (mapc #'(lambda (,child)
                     (setf (slot-value ,child 'changing) nil)
                     (layout-changed ,child :parent t))
                 ,children)
           ;; Let our parent layout know we changed our layout
           (progn
             (setf (slot-value (first ,parent) 'changing) nil)
             (layout-changed (first ,parent) :child t))))))

