(in-package :pajitnov)

(defun valid-position (array position max-pieces)
  "=> BOOLEAN
Checks if POSITION is a valid point in ARRAY."
  (assert (= (length position) (length (array-dimensions array))))
  (let ((result t))
    (iter (for p in position)
      (while result)
      ;; point is valid if it is within the array bounds, 0 <= p < max-pieces
      ;; and the position in the array is 0, meaning it is not being used
      (setf result (and (< p max-pieces)
                        (>= p 0)
                        (zerop (apply #'aref array position)))))
    result))

(defun get-next-valid-positions (array position max-pieces)
  "=> LIST
Iterates through the dimensions of POSITION, calling VALID-POSITION on each face
of the n-dimension cube."
  (iter (for p in position)
    (for i from 0)
    (for forward = (1+ p))
    (for backward = (1- p))
    ;; for dimensions >= 1 there are 2*n faces
    ;; for each dimension there a back and front
    ;; each valid position is represented by a cons cell where
    ;; car is the dimension
    ;; cdr is the value
    (when (and (>= forward 0) (< forward max-pieces)
               (< forward max-pieces))
      (when (valid-position array (set-nth i forward position) max-pieces)
        (collect (cons i forward))))
    (when (and (>= backward 0) (< backward max-pieces)
               (< backward max-pieces))
      (when (valid-position array (set-nth i backward position) max-pieces)
        (collect (cons i backward))))))

(defun make-block (min-pieces max-pieces max-piece-chance n-dimensions)
  " => MAP (BLOCK)
A block is a collection of pieces.
MIN-PIECES is the minimum number of pieces in the block.
MAX-PIECES is the maximum number of pieces expected  in the block.
MAX-PIECE-CHANCE is a value from 0.0 to 1.0 that acts as the chance
that the number of pieces in the block will be MAX-PIECES.
N-DIMENSIONS is the number of dimensions of the block."
  (assert (and (< 0 min-pieces)
               (<= min-pieces max-pieces))
          nil
          "MAKE-BLOCK: 0 < min-pieces <= max-pieces")
  (assert (and (<= 0 max-piece-chance 1.0))
          nil
          "MAKE-BLOCK: 0 <= max-piece-chance <= 1.0")

  (let* (;; a bitmask for currently used positions in the block
         (array (make-array (iter (for i from 0 below n-dimensions)
                              (collect max-pieces))
                            :element-type 'bit
                            :initial-element 0))

         ;; counter for number of pieces
         (num-pieces 0)

         ;; chance of an additional piece
         ;; get the nth root (where n is the max number of additional pieces)
         ;; of the chance of getting all pieces
         (additional-piece-chance (expt max-piece-chance
                                        (/ 1.0 (- max-pieces min-pieces))))

         ;; keeps track of current position is the array
         (position (iter (for i from 0 below n-dimensions)
                     (collect (random-in-range 0 (1- max-pieces)))))

         ;; keeps track of all positions
         (positions (list position))

         ;; keeps track of the bounds of each dimension of the block
         (block-bounds (iter (for p in position)
                         (collect (cons p p) result-type vector)))

         ;; keeps track of the length of each dimension
         (dim-length nil)

         ;; center of the block
         (center-position nil)

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
             (setf next-valid-positions (get-next-valid-positions array
                                                                  position
                                                                  max-pieces))

             ;; if no valid positions, go through previous positions
             ;; looking for a valid place
             (iter (for pos in positions)
               (while (zerop (length next-valid-positions)))
               (setf next-valid-positions (get-next-valid-positions array
                                                                    pos
                                                                    max-pieces)))

             ;; get the new position from the list of valid positions
             (when (and (> (length next-valid-positions) 0)
                        (< num-pieces max-pieces)
                        (> max-piece-chance 0.0))
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
                       positions (cons position positions))))))

      ;; add minimum number of pieces
      (iter (while (< num-pieces min-pieces))
        (add-piece))

      ;; additional pieces
      (let ((chance (random-in-range 0.0 1.0)))
        ;; (print additional-piece-chance)
        ;; (print chance)
        (iter (while (and (<= chance additional-piece-chance)
                          (< num-pieces max-pieces)))
          (add-piece)
          (setf chance (random-in-range 0.0 1.0))
          ;; (print chance)
          )))

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
         (when (= (apply #'aref array vals) 1)
           (setf pieces (with-last pieces
                          ;; relative position to the center of the block
                          (fset:map (:center (gmap:gmap
                                              :seq
                                              (lambda (pos center)
                                                (* (- pos center) +piece-radius+ 2.0))
                                              (:list vals) (:list center-position)))
                                    (:color (vec4f (random-in-range 0.0 1.0)
                                                   (random-in-range 0.0 1.0)
                                                   (random-in-range 0.0 1.0)
                                                   1.0)))))))
       range-list)
      (setf block
            (-> block
                (with :size (convert 'seq (mapcar #'1+ dim-length)))
                (with :pieces pieces)
                (with :center (convert 'seq (iter (for i from 0 below n-dimensions)
                                              (collect 0.0)))))))
    block))

(defun move-position (position dist-vec)
  "=> NEW-POSITION (SEQ)
Position is just fset:seq with numbers. Using fset:seq for positions
just because they are n dimensional and are immutable."
  (iter (for d in-vector dist-vec) (for i from 0)
    (with! position i (+ (@ position i) d)))
  position)

(defun move-pieces (pieces dist-vec)
  (let ((new-pieces (empty-seq)))
    (do-seq (piece pieces)
      (let ((pcenter (@ piece :center))
            (pcolor (@ piece :color)))
        (setf pcenter (move-position pcenter dist-vec)
              new-pieces (with-last new-pieces
                           (-> piece
                               (with :center pcenter)
                               (with :color pcolor))))))
    new-pieces))

(defun move-block (block dist-vec)
  (-> block
      (with :center (move-position (@ block :center) dist-vec))
      (with :pieces (move-pieces (@ block :pieces) dist-vec))))

(defun move-block-to-start2d (block)
  (move-block block (vec2f (* -1.0 (truncate
                                    (/ (@ (@ block :size) 0) 2.0))
                              +piece-diameter+)
                           (* (truncate (/ (y-val *grid-dim2d*) 2.0)) +piece-diameter+))))
