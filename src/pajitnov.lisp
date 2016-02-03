;;;; pajitnov.lisp

(in-package #:pajitnov)

;;; "pajitnov" goes here. Hacks and glory await!

(defun initialize ()
  (let ((text-program (make-program #p"./data/shaders/text.v.glsl"
                                    #p"./data/shaders/text.f.glsl"))
        (cube-program (make-program #p"./data/shaders/cube.v.glsl"
                                    #p"./data/shaders/cube.f.glsl"))
        (rect-program (make-program #p"./data/shaders/rect.v.glsl"
                                    #p"./data/shaders/rect.f.glsl")))
    (setf *program-manager* (make-instance 'program-manager)
          *font-manager* (make-instance 'font-manager)
          *text-drawer* (make-instance 'text-drawer :program text-program)
          *rect-drawer* (make-instance 'rect-drawer :program rect-program)
          *cube-drawer* (make-instance 'cube-drawer :program cube-program)
          *camera* (make-instance 'camera :position (vec3f 90.0 50.0 220.0)
                                          :yaw -90.0
                                          :pitch 0.0
                                          :zoom 45.0
                                          :movement-speed 10.0)
          *block-move-timer* (make-timer :end 1.0)
          *block-slide-timer* (make-timer :end 1.0))

    (load-program "cube" cube-program)
    (load-program "text" text-program)
    (load-program "rect" rect-program)

    (load-font "sans50" "./data/fonts/DejaVuSans.ttf" 50)

    (setf *current-block* (starting-block2d))
    (print *current-block*)

    (let ((view (get-view-matrix *camera*))
          (proj (kit.math:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                             (cfloat (/ *width* *height*))
                                             0.1 10000.0)))
      ;; cube shader matrices
      (gl:use-program (id cube-program))
      (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil)

      ;; rect shader matrices
      (gl:use-program (id rect-program))
      (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform rect-program "projection") proj nil))

    (let ((proj (kit.glm:ortho-matrix 0.0 (cfloat *width*)
                                      0.0 *height*
                                      -1.0 100.0)))
      ;;text shader matrices
      (gl:use-program (id text-program))
      (gl:uniform-matrix-4fv (get-uniform text-program "projection") proj nil))))

(defun handle-input2d ()
  (let ((up-p (or (key-action-p :w :press) (key-action-p :up :press)))
        (down-p (or (key-action-p :s :press) (key-action-p :down :press)))
        (left-p (or (key-action-p :a :press) (key-action-p :left :press)))
        (right-p (or (key-action-p :d :press) (key-action-p :right :press)))
        (reset-p (key-action-p :n :press))
        ;; (up-p (or (key-pressed-p :w) (key-pressed-p :up)))
        ;; (down-p (or (key-pressed-p :s) (key-pressed-p :down)))
        ;; (left-p (or (key-pressed-p :a) (key-pressed-p :left)))
        ;; (right-p (or (key-pressed-p :d) (key-pressed-p :right)))
        )
    (when (not (or (key-pressed-p :left-control)
                   (key-pressed-p :right-control)))

      (when reset-p
        (add-event :code
                   (setf *current-block*
                         (starting-block2d))))
      (when up-p
        (add-event :code
                   (setf *current-block*
                         (block-add-action *current-block* :up))))
      (when down-p
        (add-event :code
                   (setf *current-block*
                         (block-add-action *current-block* :down))))
      (when right-p
        (add-event :code
                   (setf *current-block*
                         (block-add-action *current-block* :right))))
      (when left-p
        (add-event :code
                   (setf *current-block*
                         (block-add-action *current-block* :rotate-xy)))))))

(defun handle-input ()
  (when (key-action-p :escape :press)
    (close-window))
  (when (and (key-pressed-p :left-control)
             (key-pressed-p :left-alt)
             (key-action-p :r :press))
    (set-restart-window)
    (close-window))
  (when (equalp *state* +game2d+)
    (let ((angle-change 1.0))
      (when (or (key-pressed-p :left-control)
                (key-pressed-p :right-control))
        (when (key-pressed-p :a)
          (add-event :code (process-direction-movement *camera* +left+ *dt*)))
        (when (key-pressed-p :s)
          (add-event :code (process-direction-movement *camera* +backward+ *dt*)))
        (when (key-pressed-p :d)
          (add-event :code (process-direction-movement *camera* +right+ *dt*)))
        (when (key-pressed-p :w)
          (add-event :code (process-direction-movement *camera* +forward+ *dt*)))
        (when (key-pressed-p :q)
          (add-event :code (process-rotation-movement *camera* (- angle-change) 0.0)))
        (when (key-pressed-p :e)
          (add-event :code (process-rotation-movement *camera* angle-change 0.0)))
        (when (key-pressed-p :f)
          (add-event :code (process-rotation-movement *camera* 0.0 angle-change)))
        (when (key-pressed-p :v)
          (add-event :code (process-rotation-movement *camera* 0.0 (- angle-change))))))
    (handle-input2d))
  (when *scroll-callback-p*
    (add-event :code (process-scroll-movement *camera* (cfloat *scroll-y*)))))

(defun render-piece2d (piece)
  (let ((center (@ piece :center)))
    ;; (cube-draw :position (vec3f (@ center 0) (@ center 1) +piece-radius+)
    ;;            :color (@ piece :color)
    ;;            :size (vec3f +piece-diameter+ +piece-diameter+ +piece-diameter+)
    ;;            :draw-center (vec3f 0.0 0.0 0.5))
    (rect-draw :position (vec3f (@ center 0) (@ center 1) 0.0)
               :color (@ piece :color)
               :size (vec2f +piece-diameter+ +piece-diameter+)
               :draw-center (vec3f 0.0 0.0 0.5))
    ))

(defun render-piece (piece)
  (cond ((= (size (@ piece :center)) 2)
         (render-piece2d piece))))

(defun render-block (block)
  (let ((pieces (@ block :pieces))
        (center (@ block :center)))
    (do-seq (piece pieces)
      (render-piece piece))
    (let ((indicator-dim (/ +piece-radius+ 2.0)))
      (rect-draw :position (vec3f (@ center 0) (@ center 1) 1.1)
                 :size (vec3f indicator-dim indicator-dim +piece-diameter+)
                 :color (vec4f 1.0 1.0 1.0 0.8)
                 :draw-center (vec3f 0.0 0.0 0.0)))))

(defun render-old-pieces ()
  (do-seq (piece *old-pieces*)
    (render-piece piece)))

(defun render-grid2d ()
  (let ((cols (aref *grid-dim2d* 0))
        (rows (aref *grid-dim2d* 1))
        (color (vec4f 0.5 0.5 0.5 0.4))
        (secondary-dim (/ +piece-radius+ 5.0))
        (piece-diameter (* +piece-radius+ 2.0)))

    ;; row
    (iter (for row from 0 to rows)
      (rect-draw :position (vec3f +piece-radius+
                                  (- (cfloat (* row piece-diameter))
                                     +piece-radius+)
                                  0.0)
                 :size (vec2f (* piece-diameter cols)
                              secondary-dim)
                 :color color
                 :draw-center (vec3f -0.5 0.0 0.0)))

    ;; col
    (iter (for col from 0 to cols)
      (rect-draw :position (vec3f (+ (cfloat (* col piece-diameter))
                                     +piece-radius+)
                                  (- +piece-radius+)
                                  0.0)
                 :size (vec2f secondary-dim
                              (* piece-diameter rows))
                 :color color
                 :draw-center (vec3f 0.0 -0.5 0.0)))))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)
      (timer-reset render-timer)
      (gl:enable :blend :depth-test)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (render-grid2d)
      (render-block *current-block*)
      (render-old-pieces)

      ;; fps
      (let ((text (format nil "~4f" (average-fps)))
            (font (get-font "sans50"))
            (scale (vec2f 0.3 0.3)))
        (text-draw text
                   font
                   :position (vec3f (cfloat *width*) 0.0 0.0)
                   :scale scale
                   :draw-center (vec3f 0.5 -0.5 0.0))))))

(flet ((2d-valid-move-p (block)
         (valid-move-p
          block
          (1- (* (y-val *grid-dim2d*) +piece-diameter+))
          0.0
          (* (x-val *grid-dim2d*) +piece-diameter+))))
  (defun update-game2d ()
    (timer-update *block-move-timer*)
    (when (timer-ended-p *block-move-timer*)
      ;; move right
      (add-event :code
                 (progn
                   (setf *current-block*
                         (block-add-action *current-block* :right))
                   (timer-reset *block-move-timer*))))

    (add-event :code
               (progn
                 (do-seq (action (@ *current-block* :actions))
                   (let ((move (cond ((eql action :right)
                                      (move-block *current-block*
                                                  (vec2f 10.0 0.0)))
                                     ((eql action :up)
                                      (move-block *current-block*
                                                  (vec2f 0.0 10.0)))
                                     ((eql action :down)
                                      (move-block *current-block*
                                                  (vec2f 0.0 -10.0)))
                                     ((eql action :rotate-xy)
                                      (rotate-block-xy *current-block*))
                                     (t *current-block*))))
                     (when (2d-valid-move-p move)
                       (setf *current-block* move))))
                 (with! *current-block* :actions (empty-seq))))))

(let ((update-timer (make-timer :end (/ 1.0 100.0))))
  (defun update ()
    (timer-update update-timer)
    (iter (while (timer-ended-p update-timer))
      (timer-keep-overflow update-timer)

      ;; (with-slots (yaw pitch position) *camera*
      ;;   (setf yaw -90.0
      ;;         pitch 0.0
      ;;         position (vec3f 100.0 50.0 200.0))
      ;;   (update-camera-vectors *camera*))
      (let ((cube-program (get-program "cube"))
            (rect-program (get-program "rect"))
            (view (get-view-matrix *camera*)))
        ;; update camera movement by setting uniforms for the shaders
        (gl:use-program (id cube-program))
        (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)

        (gl:use-program (id rect-program))
        (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
        )
      (cond ((equalp *state* +game2d+)
             (update-game2d))))))

(defun cleanup ()
  t)

(defmacro start-window ()
  `(err:run "pajitnov"
            :init-code (initialize)
            :input-code (handle-input)
            :render-code (render)
            :update-code (update)
            :cleanup-code (cleanup)))

(let ((restart nil))
  (defun set-restart-window (&optional (value t))
    (setf restart value))
  (defun restart-window? ()
    restart)
  (defun restart-window ()
    (setf restart nil)
    (start-window)))

(defun game ()
  (start-window)
  (iter (while (restart-window?))
    (restart-window)))
