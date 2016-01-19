;;;; pajitnov.lisp

(in-package #:pajitnov)

;;; "pajitnov" goes here. Hacks and glory await!

(defglobal *blocks* (empty-seq))
(defconstant +piece-radius+ 5.0)

(defun get-symbol-values (symbols)
  (let ((li `(list)))
    (iter (for i in-vector symbols)
      (setf li (append li `(,i))))
    li))

;; dreams die.
;; (defmacro nested-iter (range-list &body body)
;;   (let* ((init-l
;;            ;; fset:last takes the car of the lastcons
;;            ;; aka: the last element, since nil is the last cdr of the lastcons
;;            (fset:last range-list))
;;          (range-list-length (length range-list))
;;          (symbols (iter (for i from 0 below range-list-length)
;;                     (collect (gensym) result-type vector)))
;;          (current `(iter (for ,(aref symbols 0) from
;;                               ,(first init-l) to ,(second init-l))
;;                      (let ((vals (make-array ,(length symbols)
;;                                              :initial-contents
;;                                              (reverse ,(get-symbol-values symbols)))))
;;                        ,@body))))
;;     (iter (for l in (cdr (reverse range-list)))
;;       (for i from 1 below range-list-length)
;;       (setf current `(iter (for ,(aref symbols i) from ,(first l) to ,(second l))
;;                        ,current)))
;;     current))
(defmacro nested-loops (dimensions variables &body body)
  (loop for range in (reverse dimensions)
        for index in (reverse variables)
        for x = body then (list y)
        for y = `(loop for ,index from 0 to ,range do ,@x)
        finally (return y)))

(defun nested-map (fn dimensions)
  (labels ((gn (args dimensions)
             (if dimensions
                 (loop for i from 0 to (car dimensions) do
                   (gn (cons i args) (cdr dimensions)))
                 (apply fn (reverse args)))))
    (gn nil dimensions)))

(defun valid-position (array position)
  "=> BOOLEAN
Checks if POSITION is a valid point in ARRAY."
  (assert (= (length position) (length (array-dimensions array))))
  (let ((result t)
        (max-pieces (first (array-dimensions array))))
    (iter (for p in position)
      (while result)
      ;; point is valid if it is within the array bounds, 0 <= p < max-pieces
      ;; and the position in the array is 0, meaning it is not being used
      (setf result (and (< p max-pieces)
                        (>= p 0)
                        (zerop (apply #'aref array position)))))
    result))

(defun get-next-valid-positions (array position)
  "=> LIST
Iterates through the dimensions of POSITION, calling VALID-POSITION on each face
of the n-dimension cube."
  (let ((pos-len (length position)))
    (iter (for p in position)
      (for i from 0)
      (for forward = (1+ p))
      (for backward = (1- p))
      ;; for dimensions >= 1 there are 2*n faces
      ;; for each dimension there a back and front
      ;; each valid position is represented by a cons cell where
      ;; car is the dimension
      ;; cdr is the value
      (when (and (>= forward 0) (< forward pos-len))
        (when (valid-position array (set-nth i forward position))
          (collect (cons i forward))))
      (when (and (>= backward 0) (< backward pos-len))
        (when (valid-position array (set-nth i backward position))
          (collect (cons i backward)))))))

(defun create-block (min-pieces max-pieces additional-piece-chance n-dimensions)
  " => MAP (BLOCK)
A block is a collection of pieces.
MIN-PIECES is the minimum number of pieces expected in the block.
MAX-PIECES is the maximum number of pieces in the block.
ADDITIONAL-PIECE-CHANCE is a value from 0.0 to 1.0 that acts as the chance
that another piece will be added.
N-DIMENSIONS is the number of dimensions of the block."
  (assert (<= min-pieces max-pieces)
          nil
          "min-pieces must be <= max-pieces")

  (let* (;; a bitmask for currently used positions in the block
         (array (make-array (iter (for i from 0 below n-dimensions)
                              (collect max-pieces))
                            :element-type 'bit
                            :initial-element 0))

         ;; counter for number of pieces
         (num-pieces 0)

         ;; keeps track of current position is the array
         (position (iter (for i from 0 below n-dimensions)
                     (collect (random-in-range 0 (1- max-pieces)))))

         ;; keeps track of all positions
         (positions (list position))

         ;; keeps track of the bounds of each dimension of the block
         (block-bounds (iter (for p in position)
                         (collect (cons p p) result-type vector)))

         ;; keeps track of the length of each dimension
         (dim-length nil
                     ;; (iter (for i from 0 below n-dimensions)
                     ;;   (collect 0))
                     )

         ;; center of the block
         (center-position nil
                          ;; (iter (for i from 0 below n-dimensions)
                          ;;   (collect 0))
                          )

         ;; keeps track of the valid places from the current position
         ;; that can be added to the block
         (next-valid-positions nil)

         ;; the block itself
         (block (empty-map)))

    (flet ((add-piece ()

             ;; count number of pieces
             (incf num-pieces)

             ;; position should always be valid, so set position to 1
             (setf (apply #'aref array position) 1)

             ;; collect all valid positions adjacent to current position
             (setf next-valid-positions (get-next-valid-positions array position))

             ;; if no valid positions, go through previous positions
             ;; looking for a valid place
             (iter (while (zerop (length next-valid-positions)))
               (for pos in positions)
               (setf next-valid-positions (get-next-valid-positions array pos)))

             ;; get the new position from the list of valid positions
             (let* ((new-position (nth (random-in-range
                                        0
                                        (1- (length next-valid-positions)))
                                       next-valid-positions))
                    ;; get dimension of the new position and its value
                    (dim (car new-position))
                    (new-value (cdr new-position)))

               ;; check if bounds of the block have expanded
               (cond ((< new-value (car (aref block-bounds dim)))
                      ;; new low
                      (setf (car (aref block-bounds dim)) new-value))
                     ((> new-value (cdr (aref block-bounds dim)))
                      ;; new high
                      (setf (cdr (aref block-bounds dim)) new-value)))

               ;; set new position
               (setf position (set-nth dim new-value position)
                     positions (cons position positions)))))

      ;; add minimum number of pieces
      (iter (for i from 0 below min-pieces)
        (add-piece))

      ;; additional pieces
      (iter (for chance = (random-in-range 0.0 1.0))
        (while (and (<= chance additional-piece-chance)
                    (< num-pieces max-pieces)))
        (add-piece)))

    ;; get length of each dimension
    (setf dim-length (iter (for (low . high) in-vector block-bounds)
                       (collect (- high low))))

    (let ((max-length (apply #'max dim-length)))
      ;; get the center of the block
      (setf center-position (iter (for (low . high) in-vector block-bounds)
                              (collect (+ low (truncate (/ max-length 2.0)))))))

    (let ((range-list (iter (for (low . high) in-vector block-bounds)
                        (collect (cons low high))))
          (pieces (empty-seq)))
      (nested-map
       (lambda (&rest vals)
         (when (= (apply #'aref vals) 1)
           (setf pieces (with-last pieces
                          (mapcar (lambda (pos center) (- pos center))
                                  vals center-position)))))
       range-list))
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
                                    #p"./data/shaders/cube.f.glsl"))
        (rect-program (make-program #p"./data/shaders/rect.v.glsl"
                                    #p"./data/shaders/rect.f.glsl")))
    (setf *program-manager* (make-instance 'program-manager)
          *font-manager* (make-instance 'font-manager)
          *text-drawer* (make-instance 'text-drawer :program text-program)
          *rect-drawer* (make-instance 'rect-drawer :program rect-program)
          *cube-drawer* (make-instance 'cube-drawer :program cube-program)
          *camera* (make-instance 'camera :position (vec3 0.0 0.0 20.0)
                                          :movement-speed 10.0))

    (load-program "cube" cube-program)
    (load-program "text" text-program)
    (load-program "rect" rect-program)

    (load-font "sans24" "./data/fonts/DejaVuSans.ttf" 24)

    (let ((view (get-view-matrix *camera*))
          (proj (kit.math:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                             (cfloat (/ *width* *height*))
                                             0.1 100.0)))
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

(defun render ()
  (gl:enable :blend :depth-test)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0.0 0.0 0.0 1.0)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (cube-draw :draw-mode :triangles)
  (let ((pos (vec3 5.0 5.0 0.0))
        (size (vec2 1.0 1.0))
        (rotate (glfw:get-time))
        (d-center (vec3 0.0 0.0 0.0))
        (r-center (vec3 -0.5 0.5 0.0)))
    (rect-draw :position pos
               :size size
               :color (vec4 1.0 0.0 0.0 1.0)
               :rotate rotate
               :draw-mode :line-strip
               :draw-center d-center
               :rotation-center r-center)
    (rect-draw :position pos
               :size size
               :color (vec4 0.0 1.0 1.0 1.0)
               :rotate 0.0
               :draw-mode :points
               :draw-center d-center
               :rotation-center r-center))
  (text-draw (format nil "~4f" (average-fps))
             (get-font "sans24")
             :position (vec2 0.0 0.0)
             :scale (vec2 0.5 0.5)))

(defun update ()
  (let ((cube-program (get-program "cube"))
        (rect-program (get-program "rect"))
        (view (get-view-matrix *camera*))
        (proj (kit.math:perspective-matrix (kit.glm:deg-to-rad (zoom *camera*))
                                           (cfloat (/ *width* *height*))
                                           0.1 100.0)))
    ;; update camera movement by setting uniforms for the shaders
    (gl:use-program (id cube-program))
    (gl:uniform-matrix-4fv (get-uniform cube-program "view") view nil)
    (gl:uniform-matrix-4fv (get-uniform cube-program "projection") proj nil)

    (gl:use-program (id rect-program))
    (gl:uniform-matrix-4fv (get-uniform rect-program "view") view nil)
    (gl:uniform-matrix-4fv (get-uniform rect-program "projection") proj nil)))

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
