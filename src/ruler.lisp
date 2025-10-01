(in-package #:cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;; DIVISION --------------------------------------------------------

(deftype extent-type-type () '(member :percent :absolute))

(defclass division (theme-mixin)
  ((extent :initarg :extent :initform 0.50 :accessor extent)
   (extent-type :type extent-type-type :initarg :extent-type :initform :percent :accessor extent-type)
   (period :initarg :period :initform 1 :accessor period)
   ;; Internal use
   (parent :initform nil)
   (period-divisions :initform nil)))

(defmacro defdivision (&rest rest &key &allow-other-keys)
  `(make-instance 'division ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod default-theme-type ((object division))
  'division)

(defmethod initialize-instance :before ((object division) &key)
  (unless (theme-value-defaultp 'division nil 'color)
    (set-theme-value-default 'division nil 'color +color-red+)
    (set-theme-value-default 'division nil 'thickness -1)))

(defmethod initialize-instance :after ((object division) &key)
  (validate-division-options object))

(defmethod (setf extent-type) :after (value (object division))
  (validate-division-options object))

(defmethod (setf period) :after (value (object division))
  ;; Reset period divisions
  (setf (slot-value object 'period-divisions) nil)
  (setf (slot-value (slot-value object 'parent) 'recalc-periods) t))

;;; FUNCTIONS -------------------------------------------------------

(defun paint-ruler-division (align object start size vertical)
  (declare (type division object))
  
  (with-theme-let ((dc (division nil color)) (tn (division nil thickness))) object
    (with-local-accessors ((ex extent) (et extent-type)) object
      (case align
        (:begin
         (let ((en (+ start (case et
                              (:absolute ex)
                              (:percent (* size ex)))))
               (st start))
           (assert (= (min st en) st))
           (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
             (dolist (div (slot-value object 'period-divisions))
               (if vertical
                   (al:draw-line st div en div dc tn)
                   (al:draw-line div st div en dc tn))))))
        ((:center :middle)
         (let (en st sz)
           (setq en (+ (1- start) size))
           (setq st (1- start))
           (setq sz (case et
                      (:absolute ex)
                      (:percent (if (= ex 1.0) 0 (* size ex)))))
           (decf en (/ sz 2))
           (incf st (/ sz 2))
           (let ((sesz (/ (- en st 1) 2)))
             (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
               (dolist (div (slot-value object 'period-divisions))
                 (if vertical
                     (progn
                       (al:draw-line st div (+ st sesz) div dc tn)
                       (al:draw-line (- en sesz) div en div dc tn))
                     (progn
                       (al:draw-line div st div (+ st sesz) dc tn)
                       (al:draw-line div (- en sesz) div en dc tn))))))))
        (:end
         (let ((en (+ start size))
               (st (- (+ start size) (case et
                                       (:absolute ex)
                                       (:percent (* size ex))))))
           (assert (= (min st en) st))
           (decf en)
           (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
             (dolist (div (slot-value object 'period-divisions))
               (if vertical
                   (al:draw-line st div en div dc tn)
                   (al:draw-line div st div en dc tn))))))))))

(defun paint-ruler-division-horizontal (align area object)
  (paint-ruler-division align object (top area) (height area) nil))

(defun paint-ruler-division-vertical (align area object)
  (paint-ruler-division align object (left area) (width area) t))

(declaim (ftype (function (division) null) validate-division-options))
(defun validate-division-options (object)
  (with-local-accessors (extent-type) object
    (unless (typep extent-type 'extent-type-type)
      (error "unknown EXTENT-TYPE option: ~a" extent-type))))

;;; MACROS ----------------------------------------------------------

(defmacro division-100 (&rest rest &key &allow-other-keys)
  `(defdivision :period 100 ,@rest))

(defmacro division-25 (&rest rest &key &allow-other-keys)
  `(defdivision :period 25 ,@rest))

(defmacro division-10 (&rest rest &key &allow-other-keys)
  `(defdivision :period 10 ,@rest))

(defmacro division-5 (&rest rest &key &allow-other-keys)
  `(defdivision :period 5 ,@rest))

(defmacro division-2 (&rest rest &key &allow-other-keys)
  `(defdivision :period 2 ,@rest))

;;;; RULER ====================================================================

(deftype ruler-align-type () '(member :begin :center :middle :end))

(defclass ruler (area-mixin
                 parent-mixin
                 shortcuts-mixin
                 theme-mixin
                 visible-mixin)
  ((divisions :initarg :divisions :initform nil :accessor divisions)
   (vertical :initarg :vertical :initform nil :accessor vertical)
   (align :type ruler-align-type :initarg :align :initform :end :accessor align)
   ;; Internal use
   (recalc-periods :initform t)))

(defmacro defruler (&rest rest &key &allow-other-keys)
  `(make-instance 'ruler ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod (setf align) :after (value (object ruler))
  (validate-ruler-options object))

(defmethod (setf divisions) :after (value (object ruler))
  (mapcar #'(lambda (o) (setf (slot-value o 'parent) object)) (divisions object))
  (setf (slot-value object 'recalc-periods) t))

(defmethod default-theme-type ((object ruler))
  'ruler)

(defmethod initialize-instance :before ((object ruler) &key)
  (unless (theme-value-defaultp 'ruler nil 'line-color)
    (set-theme-value-default 'ruler nil 'line-color +color-red+)
    (set-theme-value-default 'ruler nil 'line-thickness -1)))

(defmethod initialize-instance :after ((object ruler) &key)
  (validate-ruler-options object)
  
  ;; Set divisions parent
  (mapcar #'(lambda (o) (setf (parent o) object)) (divisions object)))

(defmethod on-command ((object ruler) &key)
  (if (visible object)
      (setf (visible object) nil)
      (setf (visible object) t)))

(defmethod on-paint ((object ruler) &key)
  (with-local-slots (visible vertical) object
    (when visible
      (if vertical
          (paint-ruler-vertical object)
          (paint-ruler-horizontal object)))))

(defmethod paint-ruler-horizontal ((object ruler))
  ;; Make sure the divisions have been calculated
  (when (slot-value object 'recalc-periods)
    (calc-ruler-divisions object))

  (with-local-accessors (left top width height align) object
    (let ((y (case align
               (:begin top)
               ((:center :middle) (1- (+ top (/ height 2))))
               (:end (+ top height)))))
      ;; Paint the main line
      (with-theme-let ((lc (ruler nil line-color)) (ltn (ruler nil line-thickness))) object
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-line (1- left) y (right object) y lc ltn)))
  
      ;; Paint the divisions
      (let ((area-rect (make-instance '%rect :left left :top top :width width :height height)))
        (dolist (div (divisions object))
          (paint-ruler-division-horizontal align area-rect div))))))

(defmethod paint-ruler-vertical ((object ruler))
  ;; Make sure the divisions have been calculated
  (when (slot-value object 'recalc-periods)
    (calc-ruler-divisions object))

  (with-local-accessors (left top width height align) object
    (let ((x (case align
               (:begin left)
               ((:center :middle) (1- (+ left (/ width 2))))
               (:end (+ left width)))))
      ;; Paint the main line
      (with-theme-let ((lc (ruler nil line-color)) (ltn (ruler nil line-thickness))) object
        (with-blender (+OP-ADD+ +BLEND-ONE+ +BLEND-ZERO+)
          (al:draw-line x (1- top) x (bottom object) lc ltn)))
  
      ;; Paint the divisions
      (let ((area-rect (make-instance '%rect :left left :top top :width width :height height)))
        (dolist (div (divisions object))
          (paint-ruler-division-vertical align area-rect div))))))

;;; GLOBALS ---------------------------------------------------------

(defvar *ruler-custom-class-counter* 0 "Used to make unique class names for rulers.")

;;; FUNCTIONS -------------------------------------------------------

(declaim (ftype (function (ruler) null) calc-ruler-divisions))
(defun calc-ruler-divisions (object)
  "Make sure all division's within ruler object have their period-divisions set up."

  (flet ((unused (pos divsdivs)
           (dolist (divs divsdivs)
             (when (member pos divs)
               (return-from unused nil)))
           t))
    (with-local-accessors (divisions vertical) object
      (let ((ls (left object))
            (ts (top object))
            (rs (right object))
            (bs (bottom object))
            divs-divs)

        (dolist (div divisions)
          (with-slots ((pd period-divisions) period) div
            ;; Calculate period divisions
            (let (start end)
              (if vertical
                  (setq start (min ts bs) end (max ts bs))
                  (setq start (min ls rs) end (max ls rs)))
              (do ((np start (+ np period)))
                  ((> np end))
                ;; Only if a previous div hasn't already used it
                (let ((not-used (unused np divs-divs)))
                  (when not-used
                    (push np pd)))))

            ;; Save periods for next loop
            (push pd divs-divs))))))
  
  (setf (slot-value object 'recalc-periods) nil))

(defun make-ruler-custom-name (base-name)
  "Generates a unique ruler class name by appending a counter and suffix to the base name."
  
  (incf *ruler-custom-class-counter*)
  (intern (format nil "~a-CLASS-~8,'0d" (string-upcase base-name) *ruler-custom-class-counter*)))

(declaim (ftype (function (ruler) null) validate-ruler-options))
(defun validate-ruler-options (object)
  (with-local-slots (align) object
    (unless (typep align 'ruler-align-type)
      (error "unknown align option: ~a" align))))

;;;; macros ===================================================================

;; TODO: Deduplicate once I figure out how
(defmacro ruler-100-25-5 (&rest rest &key (div-100-color nil div-100-colorp) (div-100-extent nil div-100-extentp)
                                       (div-25-color nil div-25-colorp) (div-25-extent nil div-25-extentp)
                                       (div-5-color nil div-5-colorp) (div-5-extent nil div-5-extentp)
                          &allow-other-keys)
  (let ((name (make-ruler-custom-name "%ruler-25-5"))
        (object (gensym)))
    `(let ((,object))
       (mop:ensure-class ',name
                         :direct-superclasses (list (find-class 'cl-yag::ruler))
                         :direct-slots '((:name div-100 :readers (div-100))
                                         (:name div-25 :readers (div-25))
                                         (:name div-5 :readers (div-5))))
       (setf ,object (make-instance ',name ,@(remove-keyword-params rest '(:div-100-color :div-100-extent :div-100-extent-type
                                                                           :div-25-color :div-25-extent :div-25-extent-type
                                                                           :div-5-color :div-5-extent :div-5-extent-type))))
       (setf (slot-value ,object 'div-100) (division-100))
       ,(if div-100-colorp
            `(set-theme-value (slot-value ,object 'div-100) 'division 'color ,div-100-color))
       ,(if div-100-extentp
            `(setf (extent (slot-value ,object 'div-100)) ,div-100-extent))
       (setf (slot-value ,object 'div-25) (division-25))
       ,(if div-25-colorp
            `(set-theme-value (slot-value ,object 'div-25) 'division 'color ,div-25-color))
       ,(if div-25-extentp
            `(setf (extent (slot-value ,object 'div-25)) ,div-25-extent))
       (setf (slot-value ,object 'div-5) (division-5))
       ,(if div-5-colorp
            `(set-theme-value (slot-value ,object 'div-5) 'division 'color ,div-5-color))
       ,(if div-5-extentp
            `(setf (extent (slot-value ,object 'div-5)) ,div-5-extent))
       (setf (divisions ,object) (list (slot-value ,object 'div-100)
                                       (slot-value ,object 'div-25)
                                       (slot-value ,object 'div-5)))
       ,object)))

;; TODO: Deduplicate once I figure out how
(defmacro ruler-10-2 (&rest rest &key (div-10-color nil div-10-colorp) (div-10-extent nil div-10-extentp)
                                   (div-2-color nil div-2-colorp) (div-2-extent nil div-2-extentp)
                      &allow-other-keys)
  (let ((name (make-ruler-custom-name "%ruler-10-2"))
        (object (gensym)))
    `(let ((,object))
       (closer-mop:ensure-class ',name
                                :direct-superclasses (list (find-class 'cl-yag::ruler))
                                :direct-slots '((:name div-10 :readers (div-10))
                                                (:name div-2 :readers (div-2))))
       (setf ,object (make-instance ',name ,@(remove-keyword-params rest '(:div-10-color :div-10-extent :div-10-extent-type
                                                                           :div-2-color :div-2-extent :div-2-extent-type))))
       (setf (slot-value ,object 'div-10) (division-10))
       ,(if div-10-colorp
            `(set-theme-value (slot-value ,object 'div-10) 'division 'color ,div-10-color))
       ,(if div-10-extentp
            `(setf (extent (slot-value ,object 'div-10)) ,div-10-extent))
       (setf (slot-value ,object 'div-2) (division-2))
       ,(if div-2-colorp
            `(set-theme-value (slot-value ,object 'div-2) 'division 'color ,div-2-color))
       ,(if div-2-extentp
            `(setf (extent (slot-value ,object 'div-2)) ,div-2-extent))
       (setf (divisions ,object) (list (slot-value ,object 'div-10)
                                       (slot-value ,object 'div-2)))
       ,object)))

;; TODO: Deduplicate once I figure out how
(defmacro ruler-25-5 (&rest rest &key (div-25-color nil div-25-colorp) (div-25-extent nil div-25-extentp)
                                   (div-5-color nil div-5-colorp) (div-5-extent nil div-5-extentp)
                      &allow-other-keys)
  (let ((name (make-ruler-custom-name "%ruler-25-5"))
        (object (gensym)))
    `(let ((,object))
       (closer-mop:ensure-class ',name
                                :direct-superclasses (list (find-class 'cl-yag::ruler))
                                :direct-slots '((:name div-25 :readers (div-25))
                                                (:name div-5 :readers (div-5))))
       (setf ,object (make-instance ',name ,@(remove-keyword-params rest '(:div-25-color :div-25-extent :div-25-extent-type
                                                                           :div-5-color :div-5-extent :div-5-extent-type))))
       (setf (slot-value ,object 'div-25) (division-25))
       ,(if div-25-colorp
            `(set-theme-value (slot-value ,object 'div-25) 'division 'color ,div-25-color))
       ,(if div-25-extentp
            `(setf (extent (slot-value ,object 'div-25)) ,div-25-extent))
       (setf (slot-value ,object 'div-5) (division-5))
       ,(if div-5-colorp
            `(set-theme-value (slot-value ,object 'div-5) 'division 'color ,div-5-color))
       ,(if div-5-extentp
            `(setf (extent (slot-value ,object 'div-5)) ,div-5-extent))
       (setf (divisions ,object) (list (slot-value ,object 'div-25)
                                       (slot-value ,object 'div-5)))
       ,object)))

