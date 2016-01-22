(in-package :pajitnov)

(defenum:defenum *pajitnov-states* ((+menu+ 0)
                                    +game+
                                    +pause+))

;; (defglobal *width* 1200)
;; (defglobal *height* 800)
(defglobal *blocks* (empty-seq))
(defconstant +piece-radius+ 5.0)
