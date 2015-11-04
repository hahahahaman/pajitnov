;;;; pajitnov.lisp

(in-package #:pajitnov)

;;; "pajitnov" goes here. Hacks and glory await!


(defun initialize ())
(defun handle-input ()
  (when (key-action-p :escape :press)
    (cl-glfw3:set-window-should-close)))
(defun render ())
(defun update ())
(defun cleanup ()
  t)

(defun game ()
  (err:run "insert-title"
           :init-code (initialize)
           :input-code (handle-input)
           :render-code (render)
           :update-code (update)
           :cleanup-code (cleanup)))
