(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; layout-base ==============================================================

(defparameter +LAYOUT-CHILD-OPTIONS+ '(:min-height :min-width :max-height :max-width
                                       :left :center :right
                                       :top :middle :bottom))

(defclass layout-base (area-mixin-base
                       content-mixin
                       parent-mixin)
  ((height :initform :auto :initarg nil :documentation "HEIGHT coordinate of allocated area. Type shouldn't be changed after creation.")
   (left :initform :auto :initarg nil :documentation "LEFT coordinate of allocated area. Type shouldn't be changed after creation.")
   (top :initform :auto :initarg nil :documentation "TOP coordinate of allocated area. Type shouldn't be changed after creation.")
   (width :initform :auto :initarg nil :documentation "WIDTH of allocated area. Type shouldn't be changed after creation.")

   ;; Internal
   (changing :initform nil :documentation "When T, causes LAYOUT-CHANGED calls to be ignored.")
   (child-area :initform nil :documentation "Holds actual area allocated to each child, even if they don't use it.")
   
   (original-height :initform nil :documentation "Holds original HEIGHT slot value.  Used when re-layout is required.")
   (original-left :initform nil :documentation "Holds original LEFT slot value.  Used when re-layout is required.")
   (original-top :initform nil :documentation "Holds original TOP slot value.  Used when re-layout is required.")
   (original-width :initform nil :documentation "Holds original WIDTH slot value.  Used when re-layout is required.")
   ))

;;; methods ---------------------------------------------------------

(defmethod calc-area (child (parent layout-base) &key)

  (v:debug :layout "[calc-area] {layout-base} called with child ~a" (print-raw-object child))
  
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

          (v:debug :layout "[calc-area] {layout-base} child ~d calculating (~a ~a) @ (~a ~a) ~a"
                   cp cw ch cl ct (print-raw-object child))
        
          (macrolet ((do-calc (var func)
                       `(if (typep ,var 'keyword)
                            (progn
                              (setf ,var (,func ,var oa child))
                              t)
                            nil)))
            (let ((cwp (do-calc cw calc-width))
                  (chp (do-calc ch calc-height))
                  (clp (do-calc cl calc-left))
                  (ctp (do-calc ct calc-top)))

              ;; Call update when there are child options or any area field wasn't
              ;; calculated
              (let ((co (find-if #'(lambda (o) (eql (foro o) child)) content))
                    options)
                (when (consp co)
                  (setq options (rest co)))
                (when (or (not (or clp ctp cwp chp))
                          (> (length options) 0))
                  (with-local-slots ((lcl left) (lct top) (lcw width) (lch height)) (aref child-area cp)
                    (v:debug :layout "[calc-area] {layout-base} child ~d internal area (~d ~d) @ (~d ~d) ~a"
                             cp lcw lch lcl lct (print-raw-object child)))
                  (update-layout-child-areas cp parent)))

              ;; Log updated/changed area
              (v:debug :layout "[calc-area] {layout-base} child ~d area (~d ~d) @ (~d ~d) ~a"
                       cp cw ch cl ct (print-raw-object child))))))))
  (my-next-method))

(defmethod (setf content) :after (value (object layout-base))
  "After content update, make sure options area valid."
  
  (validate-layout-base-options object))

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
  (validate-layout-base-options object)
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

;;;; layout ===================================================================
;; Simple layout allowing single child object that can be use auto area

(defclass layout (layout-base)
  ())

(defmacro deflayout (&rest rest &key &allow-other-keys)
  `(make-instance 'layout ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod calc-area (child (parent layout) &key)

  (v:debug :layout "[calc-area] {layout} called with child ~a" (print-raw-object child))
  
  ;; Calculate parent area if needed
  (calc-layout-area parent)

  ;; Calculate our children areas if needed
  (when (eql (slot-value parent 'child-area) nil)
    (calc-layout-child-areas parent))

  (with-slots (child-area content) parent
    ;; Only 1 possible child here
    (assert (eql (foro (first content)) child))
    (let ((cp 0))
      (let* ((child-internal (aref child-area cp))
             (oa (make-instance '%rect :left (left child-internal) :top (top child-internal) :width (width child-internal) :height (height child-internal))))
      
        (with-slots ((cl left) (ct top) (cw width) (ch height)) child
          (v:debug :layout "[calc-area] {layout} child calculating (~a ~a) @ (~a ~a) ~a"
                   cw ch cl ct (print-raw-object child))
        
          (macrolet ((do-calc (var func)
                       `(if (typep ,var 'keyword)
                            (progn
                              (setf ,var (,func ,var oa child))
                              t)
                            nil)))
            (let ((cwp (do-calc cw calc-width))
                  (chp (do-calc ch calc-height))
                  (clp (do-calc cl calc-left))
                  (ctp (do-calc ct calc-top)))

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
              (let ((co (find-if #'(lambda (o) (eql (foro o) child)) content))
                    options)
                (when (consp co)
                  (setq options (rest co)))
                ;; Does it have options?
                (when (> (length options) 0)
                  (dolist (option options)
                    (case option
                      (:bottom
                       (with-local-slots ((ci-top top) (ci-height height)) child-internal
                         (with-local-slots ((ch-top top) (ch-height height)) child
                           (let ((ci-bottom (+ ci-top ci-height))
                                 (ch-bottom (+ ch-top ch-height)))
                             (if (> ci-bottom ch-bottom)
                                 (setf (slot-value child 'top) (- ci-bottom ch-height)))))))
                      (:center
                       ;; Move object to horizontal center
                       (if (> (slot-value child-internal 'width) (slot-value child 'width))
                           (setf (slot-value child 'left) (+ (slot-value child-internal 'left)
                                                             (/ (- (slot-value child-internal 'width)
                                                                   (slot-value child 'width))
                                                                2)))))
                      (:left
                       ;; Move child to left edge
                       (if (< (slot-value child-internal 'left) (slot-value child 'left))
                           (setf (slot-value child 'left) (slot-value child-internal 'left))))
                      (:middle
                       (with-local-slots ((ci-height height)) child-internal
                         (with-local-slots ((ch-height height)) child
                           (if (> ci-height ch-height)
                               (setf (slot-value child 'top) (+ (slot-value child-internal 'top)
                                                                (/ (- ci-height ch-height) 2)))))))
                      (:min-height) ; nothing to do when there are no siblings
                      (:min-width)  ; nothing to do when there are no siblings
                      (:max-height) ; nothing to do when there are no siblings
                      (:max-width)  ; nothing to do when there are no siblings
                      (:right
                       ;; Move child to right edge
                       (let ((ca-right (+ (slot-value child-internal 'left) (slot-value child-internal 'width)))
                             (ch-right (+ (slot-value child 'left) (slot-value child 'width))))
                         (if (> ca-right ch-right)
                             (setf (slot-value child 'left) (- ca-right (slot-value child 'width))))))
                      (:top
                       ;; Move child to top edge
                       (if (< (slot-value child-internal 'top) (slot-value child 'top))
                           (setf (slot-value child 'top) (slot-value child-internal 'top))))))))

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
            (v:debug :layout "[calc-layout-child-areas] {layout} child internal area (~d ~d) @ (~d ~d)"
                    (width child) (height child) (left child) (top child))))))))

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
          
            ;; Allow it to be re-layed out
            (calc-area child object)))
        
        ;; The layout that owns us changed
        (progn
          (v:warn :layout "[layout-changed] {layout} parent layout changed")
          (error "not implemented")
          )))
  
  (my-next-method))

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
  "Calculate area of a layout itself."
  
  ;; Start with existing area
  (with-local-slots ((ll left) (lt top) (lw width) (lh height)) object

    ;; Calculate our area if needed
    (when (or (typep ll 'keyword)
              (typep lt 'keyword)
              (typep lw 'keyword)
              (typep lh 'keyword))
      (v:debug :layout "[calc-layout-area] calculating layout area ~a" (print-raw-object object))
      
      ;; Locate first parent that's layout or has area
      (let ((pam (find-parent-area-or-layout object)))
        (let (pal pat paw pah)
          ;; Get area of parent.  Is it another layout?
          (if (typep pam 'layout-base)
              ;; Yeah, so get our area from its child-area
              (let ((pamo (position object (content pam))))
                (assert (not (eql pamo nil)))
                (with-local-slots ((pl left) (pt top) (pw width) (ph height))
                                  (aref (slot-value pam 'child-area) pamo)
                  (setf pal pl pat pt paw pw pah ph)
                  (assert (typep pal 'number))
                  (assert (typep pat 'number))
                  (assert (typep paw 'number))
                  (assert (typep pah 'number))))
              ;; Nope, so get the actual area
              (setf pal (slot-value pam 'left)
                    pat (slot-value pam 'top)
                    paw (slot-value pam 'width)
                    pah (slot-value pam 'height)))

          (v:debug :layout "[calc-layout-area] parent area: (~d ~d) @ (~d ~d) ~a"
                   paw pah pal pat (print-raw-object object))
          
          ;; If parent has borders
          (when (typep pam 'border-mixin)
            
            (with-borders (pam-bl pam-br pam-bt pam-bb) pam
             (let ((pam-pl 0)
                   (pam-pt 0)
                   (pam-pr 0)
                   (pam-pb 0))

               ;; If parent has padding (only applies when there are borders)
               (when (typep pam 'padding-mixin)
                 (setq pam-pl (padding-left pam))
                 (setq pam-pt (padding-top pam))
                 (setq pam-pr (padding-right pam))
                 (setq pam-pb (padding-bottom pam)))
              
               ;; Adjust for border sizes
               (unless (eql pam-bl nil)
                 (incf pal (+ (thickness pam-bl) pam-pl))
                 (decf paw (+ (thickness pam-bl) pam-pl)))
               (unless (eql pam-bt nil)
                 (incf pat (+ (thickness pam-bt) pam-pt))
                 (decf pah (+ (thickness pam-bt) pam-pt)))
               (unless (eql pam-br nil)
                 (decf paw (+ (thickness pam-br) pam-pr)))
               (unless (eql pam-bb nil)
                 (decf pah (+ (thickness pam-bb) pam-pb))))))

          ;; If parent has spacing
          (when (typep pam 'spacing-mixin)
            ;; Adjust for spacing
            (let ((pam-sl (spacing-left pam))
                  (pam-st (spacing-top pam)))
              (incf pal pam-sl)
              (incf pat pam-st)
              (decf paw (+ pam-sl (spacing-right pam)))
              (decf pah (+ pam-st (spacing-bottom pam)))))
          
          ;; Save our area where needed
          (when (member lh +AREA-HEIGHT-OPTS+)
            (setf (slot-value object 'height) pah))
          (when (member lw +AREA-WIDTH-OPTS+)
            (setf (slot-value object 'width) paw))
          (when (member ll +AREA-LEFT-OPTS+)
            (setf (slot-value object 'left) pal))
          (when (member lt +AREA-TOP-OPTS+)
            (setf (slot-value object 'top) pat))

          (v:debug :layout "[calc-layout-area] new area: (~d ~d) @ (~d ~d) ~a"
                   paw pah pal pat (print-raw-object object))
          (v:debug :layout "[calc-layout-area] actual area: (~d ~d) @ (~d ~d) ~a"
                   (slot-value object 'width) (slot-value object 'height) (slot-value object 'left)
                   (slot-value object 'top) (print-raw-object object)))))))

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

(defun find-parent-area-or-layout (object)
  "Locate first parent that either has area or is has layout-base as a superclass.
Generates error on failure."
  
  (assert (typep object 'parent-mixin))
  (let ((p (parent object)))
    (loop
      (when (typep p 'manager)
        (error "found manager when looking for area-mixin"))
      
      (when (eql p nil)
        (error "no parent when looking for area-mixin"))

      ;; If its a layout
      (if (typep p 'layout-base)
          (return p))
      
      ;; If it has area
      (if (typep p 'area-mixin)
          (return p))

      ;; Get next parent
      (assert (typep p 'area-mixin))
      (setf p (parent p)))))

;; TODO: find-if works here ...
(defun find-parent-content (object)
  "Return the first parent of object with type content-mixin.
Generates error when no parent has content-mixin as a superclass."

  (let ((parobj (parent object)))
    (loop
      (when (typep parobj 'content-mixin)
        (return-from find-parent-content parobj))
      (unless (typep parobj 'parent-mixin)
        (error "no more parents, object not owned by content??? ~a" (print-raw-object object)))
      (setq parobj (parent parobj)))))

(defun find-parent-layout (object)
  "Ascend parents until a layout-base is found, then return it.  If none are found will return NIL."

  (let ((mod (parent object)))
    (loop
      (when (eql mod nil)
        (return-from find-parent-layout nil))
      (when (typep mod 'layout-base)
        (return-from find-parent-layout mod))
      (if (typep mod 'parent-mixin)
          (setf mod (parent mod))
          (return-from find-parent-layout nil)))))

(declaim (ftype (function (layout-base) null) layout-reset-original))
(defun layout-reset-original (object)
  "Return layout of object to original state, allowing it to be
re-layed out."

  (macrolet ((update-value (dest src)
               `(unless (typep (slot-value object ,dest) 'keyword)
                  (let ((value (slot-value object ,src)))
                    (when value
                      (setf (slot-value object ,dest) value))))))
    
    (update-value 'width 'original-width)
    (update-value 'height 'original-height)
    (update-value 'left 'original-left)
    (update-value 'top 'originaltop))
  
  nil)

(declaim (ftype (function (layout-base) null) layout-save-original))
(defun layout-save-original (object)
  (with-slots (left top width height) object
    (flet ((is-keyword (field)
             (if (typep field 'keyword)
                 field
                 nil)))
      (setf (slot-value object 'original-left) (is-keyword left))
      (setf (slot-value object 'original-top) (is-keyword top))
      (setf (slot-value object 'original-width) (is-keyword width))
      (setf (slot-value object 'original-height) (is-keyword height))))
  nil)

(defmethod validate-layout-base-options (object)
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
              (unless (member option +LAYOUT-CHILD-OPTIONS+)
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

(defmacro with-changes (object &body body)
  "Prevent LAYOUT-CHANGED calls while performing multiple slot updates that would
normally cause LAYOUT-CHANGED to be called for each change. Generates a single
LAYOUT-CHANGED after body (even if none had been)."
  
  (a:with-gensyms (parlo block instance child parent children layout)
    `(let ((,instance ,object)
           ,parent ,children)
       (declare (ignorable ,parent ,children))
       (block ,block
         ;; Do we have a parent?
         (when (typep ,instance 'parent-mixin)
           ;; Yes, so see if we have a layout above us
           (let ((,parlo (find-parent-layout ,instance)))
             ;; Did we have a parent layout?
             (unless (eql ,parlo nil)
               ;; Yes.
               (push ,parlo ,parent)
               (return-from ,block))))
         ;; Not a parent or no parent layout, do we have content?
         (when (typep ,instance 'content-mixin)
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

