(in-package #:cl-yag)

;;;; layout-base ==============================================================

(defparameter +LAYOUT-CHILD-OPTIONS+ '(:min-height :min-width :max-height :max-width
                                       :left :center :right
                                       :top :middle :bottom))

(defclass layout-base (container-mixin
                       parent-mixin
                       ready-mixin)
  ((left :initform :auto :initarg nil)
   (top :initform :auto :initarg nil)
   (width :initform :auto :initarg nil)
   (height :initform :auto :initarg nil)

   ;; Internal
   (child-area :initform nil)))

(defmethod initialize-instance :after ((object layout-base) &key)
  (validate-layout-base-options object))

(defmethod (setf content) :after (value (object layout-base))
  (validate-layout-base-options object))

;;; methods ---------------------------------------------------------

(defmethod on-char (key mods (obj layout-base) &key)
  (dolist (child (content obj))
    (when (on-char key mods (foro child))
      (return-from on-char t)))
  (my-next-method))

(defmethod on-mouse-down (x y b (obj layout-base) &key)
  (dolist (child (content obj))
    (if (on-mouse-down x y b (foro child))
        (return-from on-mouse-down t)))
  nil)

(defmethod on-mouse-move (x y dx dy (obj layout-base) &key)
  (dolist (child (content obj))
    (on-mouse-move x y dx dy (foro child))))

(defmethod on-mouse-up (x y b (obj layout-base) &key)
  (dolist (child (content obj))
    (on-mouse-up x y b (foro child))))

(defmethod on-paint ((obj layout-base) &key)
  (dolist (c (content obj))
    (let ((co (foro c)))
      (on-paint co)))
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
  (with-area (ll lt lw lh) object

    ;; Calculate our area if needed
    (when (or (typep ll 'keyword)
              (typep lt 'keyword)
              (typep lw 'keyword)
              (typep lh 'keyword))
      (v:debug :layout "[calc-layout-area] {~a} calculating layout area" (print-raw-object object))
      
      ;; Locate first parent that's layout or has area
      (let ((pam (find-parent-area-mixin-or-layout object)))
        (let (pal pat paw pah)
          ;; Get area of parent.  Is it another layout?
          (if (typep pam 'layout-base)
              ;; Yeah, so get our area from its child-area
              (let ((pamo (position object (content pam))))
                (assert (not (eql pamo nil)))
                (with-area (pl pt pw ph)
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

          (v:debug :layout "[calc-layout-area] {~a} parent area: (~d ~d) @ (~d ~d)"
                  (print-raw-object object) paw pah pal pat)
          
          ;; If parent has borders
          (when (typep pam 'border-mixin)
            ;; Adjust for border sizes
            (let ((pam-bl (border-left pam))
                  (pam-bt (border-top pam))
                  (pam-br (border-right pam))
                  (pam-bb (border-bottom pam)))
              (unless (eql pam-bl nil)
                (incf pal (width pam-bl))
                (decf paw (width pam-bl)))
              (unless (eql pam-bt nil)
                (incf pat (width pam-bt))
                (decf pah (width pam-bt)))
              (unless (eql pam-br nil)
                (decf paw (width pam-br)))
              (unless (eql pam-bb nil)
                (decf pah (width pam-bb)))))

          ;; If parent has padding
          (when (typep pam 'padding-mixin)
            ;; Adjust for padding
            (let ((pam-sl (padding-left pam))
                  (pam-st (padding-top pam)))
              (incf pal pam-sl)
              (incf pat pam-st)
              (decf paw (+ pam-sl (padding-right pam)))
              (decf pah (+ pam-st (padding-bottom pam)))))

          ;; Save our area where needed
          (when (member lh *AREA-HEIGHT-OPTS*)
            (setf (slot-value object 'height) pah))
          (when (member lw *AREA-WIDTH-OPTS*)
            (setf (slot-value object 'width) paw))
          (when (member ll *AREA-LEFT-OPTS*)
            (setf (slot-value object 'left) pal))
          (when (member lt *AREA-TOP-OPTS*)
            (setf (slot-value object 'top) pat))

          (v:debug :layout "[calc-layout-area] {~a} new area: (~d ~d) @ (~d ~d)"
                  (print-raw-object object) paw pah pal pat)
          (v:debug :layout "[calc-layout-area] {~a} actual area: (~d ~d) @ (~d ~d)"
                  (print-raw-object object) (slot-value object 'width)
                  (slot-value object 'height) (slot-value object 'left)
                  (slot-value object 'top))
          )))))

(defun dump-layout (object &optional (indent ""))
  (when (not (eql object nil))
    (v:debug :layout "~a~a:~%" indent (print-raw-object object))
    (with-slots (content) object
      (dotimes (n (length (content object)))
        (let ((child (nth n content)))
          (v:debug :layout "~achild ~d area (~d ~d) @ (~d ~d)~%" (concatenate 'string indent "  ") n
                  (slot-value child 'child) (slot-value child 'height) (slot-value child 'left) (slot-value child 'top))
          (when (typep child 'layout-base)
            (dump-layout child (concatenate 'string indent "  "))))))))

(defun find-parent-area-mixin-or-layout (object &key (exclude nil excludep))
  "Locate first parent that either has area or is has layout-base as a superclass.
Generates error on failure."
  
  (assert (typep object 'parent-mixin))
  (flet ((excluded (obj)
           (when excludep
             (dolist (exc exclude)
               (when (typep obj exc)
                 (return-from excluded t))))
           nil))
    (let ((p (parent object)))
      (loop
        (when (typep p 'manager)
          (error "found manager when looking for area-mixin"))
      
        (when (eql p nil)
          (error "no parent when looking for area-mixin"))

        (unless (excluded p)
          ;; If its a layout
          (if (typep p 'layout-base)
              (return p))
      
          ;; If it has area
          (if (typep p 'area-mixin)
              (return p)))

        ;; Get next parent
        (assert (typep p 'area-mixin))
        (setf p (parent p))))))

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

(defmethod validate-layout-base-options (object)
  "Validate options for slots in LAYOUT-BASE.  Raises an error when invalid
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
              (error "TOP, MIDDLE, and RIGHT are mutually exclusive"))
            (when (and (member :left options)
                       (member :center options)
                       (member :rightr options))
              (error "LEFT, CENTER, and RIGHT are mutually exclusive"))))))))
