(in-package :pajitnov)

(defenum:defenum *pajitnov-states* ((+menu+ 0)
                                    +game2d+
                                    +pause+))

;; (defglobal *width* 1200)
;; (defglobal *height* 800)
;; (defglobal *blocks* (empty-seq))
(defglobal *grid* (empty-seq))
(defglobal *grid-dim2d* (vec2i 20 10))

(defglobal *state* +game2d+)

(defglobal *current-block* nil)

(defconstant +piece-radius+ 5.0)
(defconstant +piece-diameter+ (* +piece-radius+ 2.0))
