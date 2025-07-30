(in-package #:clg)

;;;; text =====================================================================

(defclass text (area-mixin
                color-mixin
                font-mixin
                h-align-mixin
                parent-mixin
                title-mixin
                v-align-mixin)
  ())

;;; methods ---------------------------------------------------------

(defmethod on-paint ((obj text) &key)
  (al:draw-text (font obj) (color obj) (text-calc-left obj) (area-top obj) 0 (title obj))
  (next-method))

(defmethod ready ((obj text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

(defun text-calc-left (obj)
  (let ((al (area-left obj))
        (ha (h-align obj)))
    (if (member ha '(:none :left))
        (return-from text-calc-left al))
    (assert (not (eq nil (font obj))))
    
    (let ((tw (al:get-text-width (font obj) (title obj)))
          (aw (area-width obj)))
      (case ha
        (:center
         ;; Does text fit?
         (if (> (- aw tw) 1)
             ;; Yes so center it
             (+ (truncate (/ (- aw tw) 2)) al)
             ;; No, so give up
             al))
        
        (:right
         (if (> (- aw tw) 0)
             (+ al (- aw tw))
             al))
        (otherwise
         (error "unknown horizontal align option: ~a" ha))))))

(defun text-calc-top (obj)
  (let ((at (area-top obj))
        (va (v-align obj)))
    (if (member va '(:none :top))
        (return-from text-calc-top at))
    (assert (not (eq nil (font obj))))
    (let ((th (al:get-font-line-height (font obj)))
          (ah (area-height obj)))
      (let ((off (- ah th)))
        (case va
          (:middle
           (if (> off 1)
               (+ (/ off 2) at)
               at))
          (:bottom
           (if (> off 0)
               (- (area-bottom obj) th))
           at)
          (otherwise
           (error "unknown vertical align option: ~a" va)))))))

;;;; active-text ==============================================================

(defclass active-text (text)
  ((inside :initform nil :type boolean :accessor active-text-inside)))

;;; methods ---------------------------------------------------------

(defmethod on-mouse-enter ((obj active-text) x y &key)
  ;; Repaint with hilite of some sort
  (next-method))

(defmethod on-mouse-exit ((obj active-text) x y &key)
  ;; Repaint normally
  (next-method))

(defmethod on-paint ((obj active-text) &key)
  (al:draw-text (font obj) (if (active-text-inside obj)
                               (color-inverse (color obj))
                               (color obj))
                (text-calc-left obj) (text-calc-top obj) 0 (title obj))
  (next-method))

(defmethod ready ((obj active-text) &key manager parent)
  (declare (ignore manager parent))
  (next-method))

