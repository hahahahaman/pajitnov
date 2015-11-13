;;;; pajitnov.lisp

(in-package #:pajitnov)

;;; "pajitnov" goes here. Hacks and glory await!

(defglobal *blocks* (empty-seq))

(defun valid-position (array position)
  (let ((result t)
        (max-pieces (first (array-dimensions array))))
    (iter (for p in position)
      (while result)
      (setf result (and (< p max-pieces)
                        (>= p 0)
                        (zerop (apply #'aref array position)))))))

(defun get-next-valid-positions (array position)
  (iter (for p in position)
    (for i from 0)
    (for forward = (1+ p))
    (for backward = (1- p))
    (when (valid-position array (set-nth i forward position))
      (collect (cons i forward)))
    (when (valid-position array (set-nth i backward position))
      (collect (cons i backward)))))

(defun create-block (min-pieces max-pieces additional-piece-chance n-dimensions)
  (let* ((array (make-array `(,max-pieces ,max-pieces)
                            :element-type 'bit
                            :initial-element 0))
         (num-pieces 0)
         (position (the list (iter (for i from 0 below n-dimensions)
                               (collect (random-in-range 0 (1- max-pieces))))))
         (block-bounds (iter (for p in position)
                         (collect (cons p p) result-type vector)))
         ;; (dims (iter (for i from 0 below n-dimensions)
         ;;             (collect 0)))
         ;; (center-position (iter (for i from 0 below n-dimensions)
         ;;                        (collect 0)))
         (block (empty-seq))
         (next-valid-positions nil))
    (iter (for i from 0 below min-pieces)
      (incf num-pieces)
      (setf (apply #'aref array position) 1

            ;; collect all valid positions
            next-valid-positions (get-next-valid-positions array position))

      (if (not (zerop (length next-valid-positions)))
          (let* ((new-position (nth (random-in-range
                                     0
                                     (1- (length next-valid-positions)))
                                    next-valid-positions))
                 (dim (car new-position))
                 (new-value (cdr new-position)))
            (cond ((< new-value (car (aref block-bounds dim)))
                   (setf (car (aref block-bounds dim)) new-value))
                  ((> new-value (cdr (aref block-bounds dim)))
                   (setf (cdr (aref block-bounds dim)) new-value)))
            (setf position (set-nth dim new-value position)))
          (leave)))

    ;; set any additional pieces
    (iter (for chance = (random-in-range 0.0 1.0))
      (while (and (<= chance additional-piece-chance)
                  (< num-pieces max-pieces)))
      (incf num-pieces)

      (setf (apply #'aref array position) 1
            next-valid-positions (get-next-valid-positions array position))

      (if (not (zerop (length next-valid-positions)))
          (let* ((new-position (nth (random-in-range
                                     0
                                     (1- (length next-valid-positions)))
                                    next-valid-positions))
                 (dim (car new-position))
                 (new-value (cdr new-position)))
            (cond ((< new-value (car (aref block-bounds dim)))
                   (setf (car (aref block-bounds dim)) new-value))
                  ((> new-value (cdr (aref block-bounds dim)))
                   (setf (cdr (aref block-bounds dim)) new-value)))
            (setf position (set-nth dim new-value position)))
          (leave)))

    ;; (setf w (1+ (- rightmost leftmost))
    ;;       h (1+ (- topmost botmost))
    ;;       center-row (+ botmost (truncate (/ (max w h) 2.0)))
    ;;       center-col (+ leftmost (truncate (/ (max w h) 2.0))))
    ;; (iter (for i from botmost to topmost)
    ;;       (iter (for j from leftmost to rightmost)
    ;;             (when (= (aref array i j) 1)
    ;;               (with! block (cons (- j center-col)
    ;;                                  (- i center-row))))))
    block))


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
