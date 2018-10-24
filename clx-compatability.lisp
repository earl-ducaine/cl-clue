

(in-package :xlib)

(defun pixmap-p (object)
  (typep object 'pixmap))

(defun image-z-p (object)
  (typep object 'image-z))

(export 'xlib::image-z-p :xlib)

(defun pixmap-plist (pixmap)
  (xlib:drawable-plist pixmap))

(defun (setf pixmap-plist) (value window)
  (setf (drawable-plist window) value))

;; (defmethod (setf font-name) ((font font) value)
;;     (setf (slot-value font 'name) value))


;; (closer-mop:slot-definition-writers (nth 3 (closer-mop:class-direct-slots (find-class 'xlib:font))))

;; 'font


;; (let ((my-font (make-instance 'font)))
;;   (setf (xlib::font-name my-font) "myname")
;;   (format t "fontname: ~s" (xlib::font-name my-font)))

;; (let ((my-font (make-instance 'font)))
;;   (setf (xlib::font-name my-font) "myname")
;;   (format t "fontname: ~s" (xlib::font-name my-font)))

;; (nth 3 (closer-mop:class-slots (find-class 'xlib:font))))

;; (closer-mop:slot-definition-writers (nth 4 (closer-mop:class-direct-slots (find-class 'xlib:font))))


;; (:COPYRIGHT 1116 :FONTNAME_REGISTRY 92 :FOUNDRY 2069 :FAMILY_NAME 685
;;  :WEIGHT_NAME 515 :SLANT 552 :SETWIDTH_NAME 517 :ADD_STYLE_NAME 92 :PIXEL_SIZE
;;  20 :POINT_SIZE 190 :RESOLUTION_X 75 :RESOLUTION_Y 75 :SPACING 107
;;  :AVERAGE_WIDTH 100 :CHARSET_REGISTRY 110 :CHARSET_ENCODING 112 :FONT 2073
;;  :WEIGHT 10 :RESOLUTION 103 :X_HEIGHT 17 :QUAD_WIDTH 10)




(defun get-default-font ()
  (let ((display (xlib:open-default-display)))
    (nth 0 (xlib:list-fonts display "*fixed*-normal-*-240-*"))))


;; (xlib:font-properties (nth 5 (xlib:list-fonts *display* "*fixed*-normal-*-390-*")))




;; (xlib:list-fonts *display*)


;; (let ((display (xlib:open-default-display)))
;;   (when display
;;     (reduce (lambda (resolutions font)
;; 	      (let ((resolution (xlib:font-property font :point_size)))
;; 		(unless (member resolution resolutions)
;; 		  (push resolution resolutions))
;; 		resolutions))
;; 	    (xlib:list-fonts display "*fixed-medium*")
;; 	    :initial-value nil)))
