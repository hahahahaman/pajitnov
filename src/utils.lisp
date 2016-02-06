(in-package :pajitnov)

;; dreams die.
;; (defun get-symbol-values (symbols)
;;   (let ((li `(list)))
;;     (iter (for i in-vector symbols)
;;       (setf li (append li `(,i))))
;;     li))
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

;; got these from stack overflow
(defmacro nested-loops (dimensions variables &body body)
  "Creates nested loops, storing the values in VARIABLES and iterating from 0 to
DIMENSIONS for each loop. The nesting of the loop goes from the first of VARIABLES
being the outermost loop and the last of VARIABLES being the innermost. This macro
is useful when DIMENSIONS are know at compile time."
  (loop for range in (reverse dimensions)
        for index in (reverse variables)
        for x = body then (list y)
        for y = `(loop for ,index from 0 to ,range do ,@x)
        finally (return y)))

(defun nested-map (fn dimensions)
  "Applies FN recursively on the values given by iterating through DIMENSIONS.
Good for when DIMENSIONS can only be known at runtime."
  (labels ((gn (args dimensions)
             (if dimensions
                 (if (consp (car dimensions))
                     (loop for i from (caar dimensions) to (cdar dimensions) do
                       (gn (cons i args) (cdr dimensions)))
                     (loop for i from 0 to (car dimensions) do
                       (gn (cons i args) (cdr dimensions))))
                 (apply fn (reverse args)))))
    (gn nil dimensions)))
