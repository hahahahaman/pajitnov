(in-package :pajitnov)

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

(defun make-block (min-pieces max-pieces additional-piece-chance n-dimensions)
  " => MAP (BLOCK)
A block is a collection of pieces.
MIN-PIECES is the minimum number of pieces expected in the block.
MAX-PIECES is the maximum number of pieces in the block.
ADDITIONAL-PIECE-CHANCE is a value from 0.0 to 1.0 that acts as the chance
that another piece will be added.
N-DIMENSIONS is the number of dimensions of the block."
  (assert (and (< 0 min-pieces)
               (<= min-pieces max-pieces))
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
         (when (= (apply #'aref array vals) 1)
           (setf pieces (with-last pieces
                          (mapcar (lambda (pos center) (- pos center))
                                  vals center-position)))))
       range-list)
      (setf block
            (-> block
                (with :pieces pieces)
                (with :center center-position))))
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
