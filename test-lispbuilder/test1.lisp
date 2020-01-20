(ql:quickload :lispbuilder-sdl)

(asdf:operate 'asdf:load-op :lispbuilder-sdl)

(defparameter *random-color* sdl:*white*)

(defun init-warning (attribute-name warning-message default-value)
  (format t
          "Attribute '~a': ~a. Setting '~a' to '~a'.~%"
          attribute-name
          warning-message
          attribute-name
          default-value))

(defclass visual-element ()
  ((x
    :initarg :x
    :initform (error "Must supply an integer for the position on axis X.")
    :accessor x)
   (y
    :initarg :y
    :initform (error "Must supply an integer for the position on axis Y.")
    :accessor y)
   (color-e
    :initarg :color-e
    :initform sdl:*white*
    :reader color-e)))

(defmethod initialize-instance :after ((element visual-element) &key)
  (when (not ;; (typep (slot-value element 'color) 'sdl:color)
         (typep (slot-value element 'color-e) (find-class 'sdl:color)))
    (progn
      (setf (slot-value element 'color-e) sdl:*white*)
      (init-warning "color" "not of type 'sdl:color'" sdl:*white*)
      ;; (format t "Attribute 'color' not of type 'sdl:color'. Setting 'color' to 'sdl:*white*'.")
      )))

(defgeneric draw-element (element)
  (:documentation "Draw the element on screen."))

(defgeneric set-color (element new-color)
  (:documentation "Change the color of the element."))

(defmethod set-color ((element visual-element) new-color)
  (if (and
       (not (typep new-color (find-class 'sdl:color)))
       (not (typep new-color (find-class 'sdl:color-a))))
      (progn
        (setf (slot-value element 'color-e) sdl:*white*)
        (init-warning "color" "not of type 'sdl:color'" sdl:*white*)
        (format t "Color: ~a~%~a~%" new-color (type-of new-color)))
      (setf (slot-value element 'color-e) new-color)))


(defclass ic-connector (visual-element)
  ((len
    :initarg :len
    :initform 7
    :accessor len
    :documentation "Length of the connector")
   (thickness
    :initarg :thickness
    :initform 3
    :accessor thickness
    :documentation "The thickness of the connector.")
   (direction
    :initarg :direction
    :initform 0
    :reader direction
    :documentation "The direction the connector is pointing to.")
   (width
    :initform 0
    :documentation "The width of the shape of the connector. Changes following the direction of the connector.")
   (height
    :initform 0
    :documentation "The height of the shape of the connector. Changes following the direction of the connector.")
   (offset-x
    :initform 0
    :documentation "The offset of the connector on the x axis. Changes following the direction of the connector.")
   (offset-y
    :initform 0
    :documentation "The offset of the connector on the y axis. Changes following the direction of the connector.")
   (final-x
    :initform 0
    :documentation "The position of the connector on the x axis for drawing the shape.")
   (final-y
    :initform 0
    :documentation "The position of the connector on the y axis for drawing the shape.")))

(defmethod initialize-instance :after ((ic-c ic-connector) &key)
  (let ((default-len 7)
        (default-thick 3))
    (when (not (typep (slot-value ic-c 'len) 'integer))
      (progn
        (setf (slot-value ic-c 'len) default-len)
        (init-warning "length" "not of type 'int'" default-len)))
    (when (not (typep (slot-value ic-c 'thickness) 'integer))
      (progn
        (setf (slot-value ic-c 'thickness) default-thick)
        (init-warning "thickness" "not of type 'int'" default-thick)))))

(defgeneric change-direction (ic-c new-direction)
  (:documentation "Change logically the direction of the connector."))

(defgeneric validate-change (ic-c)
  (:documentation "Ensure that private attributes are correctly set."))

(defmethod change-direction ((ic-c ic-connector) new-direction)
  (progn
    (if (or (not (typep (slot-value ic-c 'direction) 'integer))
            (and (typep (slot-value ic-c 'direction) 'integer)
                 (> (slot-value ic-c 'direction) 3)))
        (progn
          (setf (slot-value ic-c 'direction) 0)
          (init-warning "direction" "not of type 'int' or not a valid value" 0))
        (setf (slot-value ic-c 'direction) new-direction))
    (format t "Direction: ~a~%~%" (slot-value ic-c 'direction))
    (validate-change ic-c)))

(defmethod validate-change ((ic-c ic-connector))
  (let ((half-length (/ (slot-value ic-c 'len) 2))
        (dir (slot-value ic-c 'direction)))
    (progn
      (cond ((or (= dir 0) (= dir 2)) ; Up or Down
             (progn
               (setf (slot-value ic-c 'width) (slot-value ic-c 'thickness))
               (setf (slot-value ic-c 'height) (slot-value ic-c 'len))
               (setf (slot-value ic-c 'offset-y) half-length)
               (setf (slot-value ic-c 'final-y)
                     (if (= dir 2)
                         (+ (slot-value ic-c 'y) (slot-value ic-c 'offset-y))
                         (- (slot-value ic-c 'y) (slot-value ic-c 'offset-y))))
               (setf (slot-value ic-c 'final-x) (slot-value ic-c 'x))))
            ((or (= dir 1) (= dir 3)) ; Right or Left
             (progn
               (setf (slot-value ic-c 'width) (slot-value ic-c 'len))
               (setf (slot-value ic-c 'height) (slot-value ic-c 'thickness))
               (setf (slot-value ic-c 'offset-x) half-length)
               (setf (slot-value ic-c 'final-x)
                     (if (= dir 1)
                         (+ (slot-value ic-c 'x) (slot-value ic-c 'offset-x))
                         (- (slot-value ic-c 'x) (slot-value ic-c 'offset-x))))
               (setf (slot-value ic-c 'final-y) (slot-value ic-c 'y))))
            (t ; If any other value, consider Up
             (progn
               (setf (slot-value ic-c 'width) (slot-value ic-c 'thickness))
               (setf (slot-value ic-c 'height) (slot-value ic-c 'len))
               (setf (slot-value ic-c 'offset-y) half-length)
               (setf (slot-value ic-c 'x) (slot-value ic-c 'x))
               (setf (slot-value ic-c 'final-y)
                     (- (slot-value ic-c 'y)
                        (slot-value ic-c 'offset-y)))
               (setf (slot-value ic-c 'final-x) (slot-value ic-c 'x)))))
      (format t
              "width: ~a~%height ~a~%final-x: ~a~%final-y: ~a~%direction: ~a~%"
              (slot-value ic-c 'width)
              (slot-value ic-c 'height)
              (slot-value ic-c 'final-x)
              (slot-value ic-c 'final-y)
              (slot-value ic-c 'direction)
              ))))

(defmethod draw-element ((element ic-connector))
  (progn
    (sdl:draw-box (sdl:rectangle-from-midpoint-* (slot-value element 'final-x)
                                                 (slot-value element 'final-y)
                                                 (slot-value element 'width)
                                                 (slot-value element 'height))
                  :color (slot-value element 'color-e))))

(defun draw-option-chip (x y color)
  (let ((width 35)
        (height 35)
        (width-pin 5)
        (pin-margin 2))
    ;; (format t "Parameter: x (~a) / y (~a)~%" x y)
    ;; (sdl:draw-box (sdl:rectangle :x x :y y :w width :h height) :color chip-color)
    (sdl:draw-rectangle (sdl:rectangle :x x :y y :w width :h height) :color color)
    (sdl:draw-hline (- x width-pin 1) (- x 1) (+ y pin-margin) :color color)
    (sdl:draw-hline (- x width-pin 1) (- x 1) (+ y (- height 1 pin-margin)) :color color)
    (sdl:draw-hline (+ x width) (+ x width width-pin) (+ y pin-margin) :color color)
    (sdl:draw-hline (+ x width) (+ x width width-pin) (+ y (- height 1 pin-margin)) :color color)
    ))

(defun mouse-rect-2d ()
  (let ((pins (make-array 5 :fill-pointer 0))
        (color-list (vector sdl:*white*
                            (sdl:color :r 255 :g 0 :b 0 :a 0)
                            sdl:*green*
                            sdl:*cyan*)))
    (sdl:with-init ()
      (sdl:window 1200 500 :title-caption "Move a rectangle using the mouse")
      (setf (sdl:frame-rate) 60)
      ;; (sdl:enable-alpha t)
      
      (loop for i from 0 to 3
            with conn 
            do (progn
                 (setf conn (make-instance 'ic-connector
                                           :x 100
                                           :y 50
                                           :len 20
                                           :thickness 8))
                 (change-direction conn i)
                 (set-color conn (elt color-list i))
                 (validate-change conn)
                 (vector-push conn pins)))
      
      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event ()
                         (sdl:push-quit-event))
        (:idle ()
               ;; Change the color of the box if the left mouse button is depressed
               (when (sdl:mouse-left-p)
                 (setf *random-color* (sdl:color :r (random 255) :g (random 255) :b (random 255))))

               ;; Clear the display each game loop
               (sdl:clear-display sdl:*black*)

               ;; Draw the box having a center at the mouse x/y coordinates.
               (sdl:draw-box (sdl:rectangle-from-midpoint-* (sdl:mouse-x) (sdl:mouse-y) 20 20)
                             :color *random-color*)
               (draw-option-chip 50 30 sdl:*white*)

               (loop for i from 0 to 3
                     do (progn
                          ;; (change-direction (elt pins i) i)
                          ;; (set-color (elt pins i) (nth i color-list))
                          (draw-element (elt pins i))))

               ;; (draw-element 100 50 0 sdl:*white*) ; Up
               ;; (draw-element 100 50 1 sdl:*red*) ; Right
               ;; (draw-element 100 50 2 sdl:*green*) ; Down
               ;; (draw-element 100 50 3 sdl:*cyan*) ; Left

               ;; Redraw the display
               (sdl:update-display))))))
