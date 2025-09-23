(in-package :cl-yag)

(declaim (optimize (debug 3) (speed 0) (safety 3)))

;;;; MAIN =====================================================================

(defstruct main-data
  boss
  
  ruler-left ruler-top ruler-right ruler-bottom
  grid
  outline
  
  window layout1 layout2 layout3 text1 button1 button2 button3)

(defparameter +MW-W+ 960)
(defparameter +MW-H+ 720)

(defparameter +MW-WL+ 100)
(defparameter +MW-WT+ 100)
(defparameter +MW-WW+ (- +MW-W+ 200))
(defparameter +MW-WH+ (- +MW-H+ 200))

(defparameter *main-data* (make-main-data))

(defmethod must-init ((test sb-sys::system-area-pointer) desc)
  (when (null-pointer-p test)
    (if (/= (al:get-errno) 0)
        (error "Couldn't initialize ~s (~d)." desc (al:get-errno))
        (error "Couldn't initialize ~s." desc))))

(defun main-init ()
  (must-init (al:init) "allegro")
  (must-init (al:install-keyboard) "keyboard")
  (must-init (al:install-mouse) "mouse")
  (must-init (al:init-font-addon) "font addon")
  (must-init (al:init-ttf-addon) "ttf addon")
  (must-init (al:init-image-addon) "image addon")
  (must-init (al:init-primitives-addon) "primitives addon"))

(defun main-setup-display ()
  (al:set-new-display-option :sample-buffers 1 :suggest)
  (al:set-new-display-option :samples 0 :require)
  (al:set-new-display-flags '(:resizable))
  (al:set-blender +op-add+ +blend-one+ +blend-zero+)
  (al:create-display +MW-W+ +MW-H+))

(defun main ()
  (main-init)
  (let ((screen (main-setup-display))
        (timer (al:create-timer (/ 1 60.0)))
        (queue (al:create-event-queue))
        (event (cffi:foreign-alloc '(:union al:event)))
        widgets)
    
    (unwind-protect
         (with-slots (ruler-left ruler-top ruler-right ruler-bottom grid) *main-data*
           
           (let ((c1 (al:map-rgb-f 0.75 0 0))
                 (c2 (al:map-rgb-f 0.50 0 0)))

             ;; left ruler
             (setf ruler-left (ruler-10-2 :left (- +MW-WL+ 10) :top +MW-WT+ :width 10 :height +MW-WH+
                                          :line-color c1 :div-10-color c1 :div-2-color c2
                                          :div-10-extent 1 :div-2-extent 0.5
                                          :vertical t
                                          :align :end))
             (push ruler-left widgets)

             ;; top ruler
             (setf ruler-top (ruler-10-2 :left +MW-WL+ :top (- +MW-WT+ 10) :width +MW-WW+ :height 10
                                         :line-color c1 :div-10-color c1 :div-2-color c2
                                         :div-10-extent 1 :div-2-extent 0.5
                                         :align :end))
             (push ruler-top widgets)

             ;; right ruler
             (setf ruler-right (ruler-10-2 :left (+ +MW-WL+ +MW-WW+) :top +MW-WT+ :width 10 :height +MW-WH+
                                           :line-color c1 :div-10-color c1 :div-2-color c2
                                           :div-10-extent 1 :div-2-extent 0.5
                                           :vertical t
                                           :align :begin))
             (push ruler-right widgets)
           
             ;; bottom ruler
             (setf ruler-bottom (ruler-10-2 :left 200 :top 600 :width 300 :height 10
                                            :line-color c1 :div-10-color c1 :div-2-color c2
                                            :div-10-extent 1 :div-2-extent 0.5
                                            :align :begin))
             (push ruler-bottom widgets)
           
             (setf grid (defgrid :major 50 :minor 10 :left 1 :top 1 :width +MW-W+ :height +MW-H+))
             (setf (major-color grid) (al:map-rgb-f 0 0.35 0))
             (setf (minor-color grid) (al:map-rgb-f 0 0.25 0))
             (push grid widgets)))
      
      (with-slots (boss window layout1 layout2 layout3 text1 button1 button2 button3
                   outline)
          *main-data*

        (setf text1 (deftext :title "title" :h-align :center :v-align :middle
                             :height :auto-min))
           
        (setf button1 (defbutton :title "asteroids" :h-align :center :v-align :middle
                                 :shortcuts (list '(:a :shift) '(:a :none))
                                 :left :center :top :middle :width :auto :height :auto-min
                                 :padding-left 0 :padding-right 0 :padding-top 10 :padding-bottom 10))
        (setf button2 (defbutton :title "blastem" :h-align :center :v-align :middle
                                 :shortcuts (list '(:b :shift) '(:b :none))
                                 :left :center :top :middle :width :auto :height :auto-min
                                 :padding-left 10 :padding-right 10 :padding-top 10 :padding-bottom 10))
        (setf button3 (defbutton :title "quit" :h-align :center :v-align :middle
                                 :shortcuts (list '(:q :shift) '(:q :none))
                                 :left :center :top :middle :width :auto :height :auto-min
                                 :padding-left 0 :padding-right 0 :padding-top 10 :padding-bottom 10))

        (setf (font *theme-default*) (default-font -24))
           
        (setf layout3 (defcolumn-layout :content (list button1 button2 button3)))
        
        (setf layout2 (defcolumn-layout :content (list `(,text1 :min-height) layout3)))

        (setf layout1 (defgrid-layout 3 3 :content (list nil nil nil
                                                         nil layout2 nil
                                                         nil nil nil)))
        (grid-layout-row-options 0 layout1 :height 0.10 :type :percent-all :extra nil)
        (grid-layout-row-options 2 layout1 :height 0.10 :type :percent-all :extra nil)
        (grid-layout-column-options 0 layout1 :width 0.33 :type :percent-all :extra nil)
        (grid-layout-column-options 2 layout1 :width 0.33 :type :percent-all :extra nil)
        
        (setf window (defwindow +MW-WL+ +MW-WT+ +MW-WW+ +MW-WH+ :content (list layout1)))
        (push window widgets)
        (setf boss (defmanager :content widgets))
           
        ;; (setf (interior-color window) (al:map-rgb 191 63 63))
           
        (setf (padding window) 10)

        (defmethod on-command ((obj (eql button1)) &key)
          (v:info :app "item 1 clicked"))

        (defmethod on-command ((obj (eql button2)) &key)
          (v:info :app "item 2 clicked"))

        (defmethod on-command ((obj (eql button3)) &key)
          (v:info :app "item 3 clicked")
          (setf (process boss) nil))

        (defmethod on-char ((key (eql :g)) mods (object (eql boss)) &key)
          (with-slots (grid) *main-data*
            (if (visible grid)
                (setf (visible grid) nil)
                (setf (visible grid) t))))
        
        (defmethod on-char ((key (eql :o)) mods (object (eql boss)) &key)
          (if outline
              (setf outline nil)
              (setf outline t)))

        (defmethod on-char ((key (eql :r)) mods (object (eql boss)) &key)
          (with-slots (ruler-left ruler-top ruler-right ruler-bottom) *main-data*
            (dolist (ruler (list ruler-left ruler-top ruler-right ruler-bottom))
              (if (visible ruler)
                  (setf (visible ruler) nil)
                  (setf (visible ruler) t)))))

        (defmethod on-char ((key (eql :escape)) mods (object (eql boss)) &key)
          (setf (process boss) nil))

        (defmethod on-timer (source count (object (eql boss)) &key)
          (al:clear-to-color (al:map-rgb-f 0 0 0))
          (when outline
            (al:draw-line (coerce +MW-WL+ 'float) 0 200.0 719 (al:map-rgb-f 0 1 0) 1)
            (al:draw-line 500.0 0 500.0 719 (al:map-rgb-f 0 1 0) 1)
            (al:draw-line 0 200.0 959 200.0 (al:map-rgb-f 0 1 0) 1)
            (al:draw-line 0 600.0 959 600.0 (al:map-rgb-f 0 1 0) 1))
          (paint boss)
          (al:flip-display))

        (al:start-timer timer)
        (al:clear-keyboard-state screen)

        (al:register-event-source queue (al:get-keyboard-event-source))
        (al:register-event-source queue (al:get-display-event-source screen))
        (al:register-event-source queue (al:get-timer-event-source timer))
        (al:register-event-source queue (al:get-mouse-event-source))

        ;; (print *theme-3d-gray*)
           
        (process-events queue boss)

        ;; Remove methods
        (cleanup-method on-command `((eql ,button1)))
        (cleanup-method on-command `((eql ,button2)))
        (cleanup-method on-command `((eql ,button3)))
        (cleanup-method on-char `((eql :g) t (eql ,boss)))
        (cleanup-method on-char `((eql :o) t (eql ,boss)))
        (cleanup-method on-char `((eql :r) t (eql ,boss)))
        (cleanup-method on-char `((eql :escape) t (eql ,boss)))
        (cleanup-method on-timer `(t t (eql ,boss))))
      
      (progn
        (cffi:foreign-free event)
        (al:destroy-display screen)
        (al:destroy-timer timer)
        (al:destroy-event-queue queue))))
  nil)

