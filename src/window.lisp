(in-package #:clg)

;;;; window ===================================================================

(defclass window (active-mixin
                  area-mixin
                  border-mixin
                  container-mixin
                  enable-mixin
                  visible-mixin)
  ((options :initarg :options :initform () :type list :accessor options)))

;;;------------------------------------------------------------------
;;; Make sure our children know who their parent is

(defmethod initialize-instance :after ((obj window) &key)
  (if (slot-boundp obj 'content)
      (dolist (child (content obj))
        (if (typep child 'parent-mixin)
            (setf (parent child) obj))))
  (next-method))

(defmethod (setf content) :after (val (obj window))
  (dolist (child (content obj))
    (setf (parent child) obj))
  (next-method))

;;;------------------------------------------------------------------

;; If window is visible, hide it
(defmethod hide ((obj window) &key)
  (when (visible obj)
    (setf (visible obj) nil))
  (next-method))

(defmethod on-mouse-down ((obj window) x y b &key)
  (format *standard-output* "~&on-mouse-down (window): ~d ~d ~8b" x y b)
  (next-method))

(defmethod on-mouse-moved ((obj window) x y dx dy &key)
  (format *standard-output* "~&on-mouse-moved (window): ~d ~d ~d ~d" x y dx dy)
  (next-method))

(defmethod on-mouse-up ((obj window) x y b &key)
  (format *standard-output* "~&on-mouse-up (window): ~d ~d ~8b" x y b)
  (next-method))

;; Paint all contained objects and set clean
(defmethod on-paint ((obj window) &key)
  (let ((x (area-left obj))
        (y (area-top obj)))
    (let ((r (+ x (area-width obj) -1))
          (b (+ y (area-height obj) -1)))
      ;; Left side
      (if (slot-boundp obj 'border-left)
          (let ((bo (border-left obj)))
            (case (style bo)
              (:default
               (al:draw-line x y x b (color bo) (width bo))))))
      ;; Top side
      (if (slot-boundp obj 'border-top)
          (let ((bo (border-top obj)))
            (case (style bo)
              (:default
               (al:draw-line x y r y (color bo) (width bo))))))
      ;; Right side
      (if (slot-boundp obj 'border-right)
          (let ((bo (border-right obj)))
            (case (style bo)
              (:default
               (al:draw-line r y r b (color bo) (width bo))))))
      ;; Bottom side
      (if (slot-boundp obj 'border-bottom)
          (let ((bo (border-bottom obj)))
            (case (style bo)
              (:default
               (al:draw-line x b r b (color bo) (width bo))))))))
  (let ((children (content obj)))
    (dolist (c children)
      (progn
        (on-paint c))))
  (next-method))

(defmethod ready ((obj window) &key manager)
  ;; Let contained object prepare for events
  (dolist (child (content obj))
    (ready child :manager manager :parent obj))
  (next-method))

;; If window isn't visible, make it so
(defmethod show ((obj window) &key)
  (unless (visible obj)
    (setf (visible obj) t))
  (next-method))

;; Make sure options is a list
;; TODO:  Make sure its a list of keywords ...
(defmethod (setf window-options) :after (newval (obj window))
  (with-slots (options) obj
    (when (not (typep options 'list))
      (setq newval (list newval))
      (cerror "Retry, using (list ~a)" "options must be a list, receivd ~a" options)
      (setf (window-options obj) newval)))
  (next-method))
