(in-package :pajitnov)

;;camera constants
(defconstant +camera-yaw+ -90.0)
(defconstant +camera-pitch+ 0.0)
(defconstant +camera-speed+ 3.0)
(defconstant +camera-sensitivity+ 0.30)
(defconstant +camera-zoom+ 45.0)

(defenum:defenum camera-movement
                 ((+forward+ 0)
                  +backward+
                  +left+
                  +right+))

(defclass camera ()
  ((position
    :type vec3
    :initarg :position)
   (front
    :type vec3
    :initarg :front)
   (up
    :type vec3
    :initarg :up)
   (right
    :type vec3
    :initarg :right)
   (world-up
    :type vec3
    :initarg :world-up)
   (yaw
    :type single-float
    :initarg :yaw)
   (pitch
    :type single-float
    :initarg :pitch)
   (movement-speed
    :type single-float
    :initarg :movement-speed)
   (mouse-sensitivity
    :type single-float
    :initarg :mouse-sensitivity)
   (zoom
    :type single-float
    :initarg :zoom))
  (:default-initargs
   :position (vec3 0.0 0.0 0.0)
   :up (vec3 0.0 1.0 0.0)
   :yaw +camera-yaw+
   :pitch +camera-pitch+
   :front (vec3 0.0 0.0 -1.0)
   :movement-speed +camera-speed+
   :mouse-sensitivity +camera-sensitivity+
   :zoom +camera-zoom+))

(defmethod initialize-instance ((cam camera) &key)
  (update-camera-vectors cam))

(defmethod get-view-matrix ((cam camera))
  (with-slots (position front up) cam
    (kit.glm:look-at (kit.glm:vec+ pos front) up)))

(defmethod process-direction-movement ((cam camera) direction dt)
  (with-slots (movement-speed front right position) cam
    (let ((velocity (cfloat (* movement-speed dt)))
          (original-y (y-val position)))
      (cond ((eql direction +forward+)
             (setf position (kit.glm:vec+ position (kit.glm:vec* front velocity))))
            ((eql direction +backward+)
             (setf position (kit.glm:vec- position (kit.glm:vec* front velocity))))
            ((eql direction +left+)
             (setf position (kit.glm:vec- position (kit.glm:vec* right velocity))))
            ((eql direction +right+)
             (setf position (kit.glm:vec+ position (kit.glm:vec* right velocity))))))))

(defmethod process-rotation-movement ((cam camera) x y &optional (constrain-pitch t))
  (with-slots (mouse-sensitivity yaw pitch) cam
    (let ((x-offset (* x mouse-sensitivity))
          (y-offset (* y mouse-sensitivity)))
      (incf yaw x-offset)
      (incf pitch y-offset)

      (when constrain-pitch
        (cond ((> pitch 89.0)
               (setf pitch 89.0))
              ((< pitch -89.0)
               (setf pitch -89.0))))

      (update-camera-vectors cam))))

(defmethod process-scroll-movement ((cam camera) y)
  (with-slots (zoom mouse-sensitivity) cam
    (if (and (>= zoom 1.0) (<= zoom 45.0))
        (decf zoom y)
        (if (<= zoom 1.0)
            (setf zoom 1.0)
            (setf zoom 45.0)))))

(defmethod update-camera-vectors ((cam camera))
  (with-slots (yaw pitch right up front world-up) cam
    (let* ((yaw (kit.glm:deg-to-rad yaw))
           (pitch (kit.glm:deg-to-rad pitch))
           (new-front (vec4 (cfloat (* (cos yaw)
                                       (cos pitch)))
                            (cfloat (sin pitch))
                            (cfloat (* (sin yaw)
                                       (cos pitch))))))
      (setf front (kit.glm:normalize new-front)
            right (kit.glm:normalize (kit.glm:cross-product front world-up))
            up (kit.glm:normalize (kit.glm:cross-product right front))))))
