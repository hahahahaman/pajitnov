;;;; pajitnov.lisp

(in-package #:pajitnov)

;;; "pajitnov" goes here. Hacks and glory await!

(defglobal *cube-drawer* nil)
(defglobal *camera* nil)

(defclass cube-drawer (drawer)
  ())

(defmethod initialize-instance :after ((drawer cube-drawer) &key)
  (with-slots (vao) drawer
    (let* ((buffers (gl:gen-buffers 1))
           (vbo (first buffers)))
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (with-cube-verts verts
        (gl:buffer-data :array-buffer :static-draw verts))
      (gl:enable-vertex-attrib-array 0)
      (gl:vertex-attrib-pointer 0 3 :float nil (sizeof* :float 8) 0)
      (gl:bind-buffer :array-buffer 0)
      (gl:bind-vertex-array 0)
      (gl:delete-buffers buffers))))

(defun cube-draw (&key
                    (position (vec3 0.0 0.0 0.0))
                    (size (vec3 1.0 1.0 1.0))
                    (color (vec4 1.0 1.0 1.0 1.0))
                    (rotate (vec3 0.0 0.0 0.0))
                    (draw-mode :triangles)
                    (drawer *cube-drawer*))
  (with-slots (vao program) drawer
    (gl:use-program (id program))

    (let ((model (kit.glm:matrix*
                  (kit.glm:translate position)
                  (kit.glm:scale size)
                  (kit.glm:rotate rotate))))
      (gl:uniform-matrix-4fv (get-uniform program "model")
                             model
                             nil)
      (gl:uniformfv (get-uniform program "color") color))
    (gl:bind-vertex-array vao)
    (gl:draw-arrays draw-mode 0 36)
    (gl:bind-vertex-array 0)))

(defun initialize ()
  (let ((text-program (make-program #p"./data/shaders/text.v.glsl"
                                    #p"./data/shaders/text.f.glsl"))
        (cube-program (make-program #p"./data/shaders/cube.v.glsl"
                                    #p"./data/shaders/cube.f.glsl")))
    (setf *program-manager* (make-instance 'program-manager)
          *font-manager* (make-instance 'font-manager)
          *text-drawer* (make-instance 'text-drawer :program text-program)
          *cube-drawer* (make-instance 'cube-drawer :program cube-program)
          *camera* (make-instance 'camera :position (vec3 0.0 0.0 3.0)))

    (load-program "cube" cube-program)
    (load-program "text" text-program)

    (let ((lib (ft2:make-freetype)))
      (ft2:with-open-face (sans "./data/fonts/DejaVuSans.ttf" 0 lib)
        (load-font "sans24" sans 24)))

    (gl:use-program (id cube-program))
    (let ((view (get-view-matrix *camera*))
          (proj (kit.math:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                             (cfloat (/ *width* *height*))
                                             0.1 100.0)))
      (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
      (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil))

    (gl:use-program (id text-program))
    (let ((proj (kit.glm:ortho-matrix 0.0 (cfloat *width*)
                                      0.0 *height*
                                      -1.0 100.0)))
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

(defun render ()
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (cube-draw :draw-mode :triangles)
  (text-draw "abc" (get-font "sans24") :position (vec2 0.0 0.0)))

(defun update ()
  (let ((cube-program (get-program "cube"))
        (view (get-view-matrix *camera*))
        (proj (kit.math:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                           (cfloat (/ *width* *height*))
                                           0.1 100.0)))
    (gl:use-program (id (get-program "cube")))
    (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
    (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil)))

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
