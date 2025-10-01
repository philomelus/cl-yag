(in-package :cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3) (space 0) (compilation-speed 0)))

;;;; THEMER ===================================================================

(defclass themer (theme-base)
  ((theme-data :initform nil)
   (theme-style :type theme-style :initarg :theme-style :initform :default :accessor theme-style)))

(defmacro defthemere (&rest rest &key &allow-other-keys)
  `(make-instance 'themer ,@rest))

;;; METHODS ---------------------------------------------------------

(defmethod get-theme-value ((object themer) type id &key (style nil style-p))
  (declare (ignorable object type id style style-p))
  (error "not implemented"))

(defmethod set-theme-value ((object themer) type id value &key (style nil style-p))
  (declare (ignorable object type id style style-p))
  (error "not implemented"))

(defmethod theme-valuep ((object themer) type id &key (style nil style-p))
  (declare (ignorable object type id style style-p))
  (error "not implemented"))

