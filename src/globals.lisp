(in-package :pajitnov)

(defenum:defenum *pajitnov-states* ((+menu+ 0)
                                    +game+
                                    +pause+))

;; (defglobal *width* 1200)
;; (defglobal *height* 800)
;; (defglobal *blocks* (empty-seq))
(defglobal *grid* (empty-seq))
(defglobal *grid-dim2d* (vec2i 10 20))

(defglobal *current-block* nil)

(defconstant +piece-radius+ 5.0)
