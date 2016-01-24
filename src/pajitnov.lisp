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
                                          :movement-speed 10.0))

    (load-program "cube" cube-program)
    (load-program "text" text-program)
    (load-program "rect" rect-program)

    (load-font "sans50" "./data/fonts/DejaVuSans.ttf" 50)

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

(defun handle-input ()
  (when (key-action-p :escape :press)
    (close-window))
  (when (and (key-pressed-p :left-control)
             (key-pressed-p :left-alt)
             (key-action-p :r :press))
    (set-restart-window)
    (close-window))
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
      (add-event :code (process-rotation-movement *camera* -5.0 0.0)))
    (when (key-pressed-p :e)
      (add-event :code (process-rotation-movement *camera* 5.0 0.0)))
    (when (key-pressed-p :f)
      (add-event :code (process-rotation-movement *camera* 0.0 5.0)))
    (when (key-pressed-p :v)
      (add-event :code (process-rotation-movement *camera* 0.0 -5.0))))
  (when *scroll-callback-p*
    (add-event :code (process-scroll-movement *camera* (cfloat *scroll-y*)))))

(defun render-piece (piece))

(defun render-block ()
  ())

(defun render-grid2d ()
  (let ((rows (aref *grid-dim2d* 0))
        (cols (aref *grid-dim2d* 1))
        (color (vec4f 0.5 0.5 0.5 0.5))
        (secondary-dim (/ +piece-radius+ 10.0))
        (piece-diameter (* +piece-radius+ 2.0)))

    ;; row
    (iter (for row from 0 to rows)
      (rect-draw :position (vec3f 0.0
                                  (cfloat (* row piece-diameter))
                                  0.0)
                 :size (vec2f (+ (* piece-diameter cols) secondary-dim)
                              secondary-dim)
                 :color color
                 :draw-center (vec3f -0.5 0.5 0.0)))

    ;; col
    (iter (for col from 0 to cols)
      (rect-draw :position (vec3f (cfloat (* col piece-diameter))
                                  0.0
                                  0.0)
                 :size (vec2f secondary-dim
                              (* piece-diameter rows))
                 :color color
                 :draw-center (vec3f -0.5 -0.5 0.0)))))

(let ((render-timer (make-timer :end (/ 1.0 60.0))))
  (defun render ()
    (timer-update render-timer)
    (when (timer-ended-p render-timer)
      (timer-reset render-timer)
      (gl:enable :blend :depth-test)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0.0 0.0 0.0 1.0)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      ;; (cube-draw :draw-mode :triangles)
      ;; (let ((pos (vec3 5.0 5.0 0.0))
      ;;       (size (vec2 1.0 1.0))
      ;;       (rotate (glfw:get-time))
      ;;       (d-center (vec3 0.0 0.0 0.0))
      ;;       (r-center (vec3 -0.5 0.5 0.0)))
      ;;   (rect-draw :position pos
      ;;              :size size
      ;;              :color (vec4 1.0 0.0 0.0 1.0)
      ;;              :rotate rotate
      ;;              :draw-mode :line-strip
      ;;              :draw-center d-center
      ;;              :rotation-center r-center)
      ;;   (rect-draw :position pos
      ;;              :size size
      ;;              :color (vec4 0.0 1.0 1.0 1.0)
      ;;              :rotate 0.0
      ;;              :draw-mode :points
      ;;              :draw-center d-center
      ;;              :rotation-center r-center))

      (render-grid2d)

      ;; fps
      (let ((text (format nil "~4f" (average-fps)))
            (font (get-font "sans50"))
            (scale (vec2f 0.3 0.3)))
        (text-draw text
                   font
                   :position (vec3f (cfloat *width*) 0.0 0.0)
                   :scale scale
                   :draw-center (vec3f 0.5 -0.5 0.0))))))

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
            (view (get-view-matrix *camera*))
            ;; (proj (kit.math:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
            ;;                                    (cfloat (/ *width* *height*))
            ;;                                    0.1 100.0))
            )
        ;; update camera movement by setting uniforms for the shaders
        (gl:use-program (id cube-program))
        (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
        ;; (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil)

        (gl:use-program (id rect-program))
        (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
        ;; (gl:uniform-matrix-4fv (get-uniform rect-program "projection") proj nil)
        ))))

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
