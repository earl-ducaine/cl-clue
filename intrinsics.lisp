;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;; Texas Instruments Incorporated
;;; P.O. Box 149149
;;; Austin, Texas 78714-9149
;;;
;;; Copyright (C)1987,1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.


(in-package :cluei)


(pushnew :clue *features*)

;; Exploit our knowlege of the gcontext implementation to set the
;; display directly.  Shouldn't be needed sinnce we *can* create a
;; default gcontext with the appropriate default display.

(defun gcontext-display (gcontext)
  (slot-value gcontext 'xlib::display))

(defun (setf gcontext-display) (value gcontext)
  (setf (slot-value gcontext 'xlib::display) value))


;;; Extend the xlib:display object for CLUE slots

(defmacro display-root-list (display)
  "Returns a list of root contacts in the order given by xlib:open-display."
  `(getf (xlib:display-plist ,display) 'root-list))

(defun display-root (display &optional number)
  "Returns the root of the display specified by the screen NUMBER."
  (if number
      (nth number (display-root-list display))
      (getf (xlib:display-plist display) 'default-root)))

(defsetf display-root (display) (screen)
  `(setf (getf (xlib:display-plist ,display) 'default-root) ,screen))

(defmacro before-actions (display)
  "Returns the alist of functions to call before event processing with arguments."
  `(the list (getf (xlib:display-plist ,display) 'event-before-handlers)))

(defmacro timer-queue (display)
  "Returns the list of display timer structures."
  `(the list (getf (xlib:display-plist ,display) 'timer-queue)))

(defmacro display-keyboard-buffer (display)
  "Returns the buffer used for keyboard input by all stream contacts on DISPLAY."
  `(getf (xlib:display-plist ,display) 'keyboard-buffer))

(defmacro display-modifier-translate (display)
  "Returns the translations used for keyboard input by all stream contacts in DISPLAY."
  `(getf (xlib:display-plist ,display) 'modifier-translate))

(defmacro display-update-flag (display)
  "Returns the flag used to indicate when update-state has work to do."
  `(getf (xlib:display-plist ,display) 'update-flag))

(defun display-mode-stack (display)
  "Returns the mode-stack of the DISPLAY. The current input mode of a
   contact-display is given by its mode-stack. The mode-stack is an
   alist containing entries of the form (contact mode-type
   restrict-action . args)."
  (getf (xlib:display-plist display) 'mode-stack))

(defsetf display-mode-stack (display) (stack)
  `(setf (getf (xlib:display-plist ,display) 'mode-stack) ,stack))

(defun display-multipress-delay-limit (display)
  "Reject a multipress that occurs more than this many milliseconds after initial press event."
  (getf (xlib:display-plist display) 'multipress-delay-limit))

(defsetf display-multipress-delay-limit (display) (msec)
  `(setf (getf (xlib:display-plist ,display) 'multipress-delay-limit) ,msec))

(defun display-multipress-verify-p (display)
  "When true, verify timeout of multipress events by requesting a timestamp."
  (getf (xlib:display-plist display) 'multipress-verify-p))

(defsetf display-multipress-verify-p (display) (flag)
  `(setf (getf (xlib:display-plist ,display) 'multipress-verify-p) ,flag))

(defun display-name (display)
  "Returns the application resource name associated with the display."
  (getf (xlib:display-plist display) 'resource-name))

(defsetf display-name (display) (name)
  `(setf (getf (xlib:display-plist ,display) 'resource-name) ,name))

(defun display-class (display)
  "Returns the application resource class associated with the display."
  (getf (xlib:display-plist display) 'resource-class))

(defsetf display-class (display) (class)
  `(setf (getf (xlib:display-plist ,display) 'resource-class) ,class))


;;; Clue applications call open-contact-display to connect to an x server.
;;; the object returned by open-contact-display is a clx display object that also contains
;;; the before and after event-handler lists, and the application keyboard buffer

(defvar *default-host* nil)
(defvar *default-display* 0)

(defvar *default-multipress-delay-limit* 250
  "Default value for display-multipress-delay-limit.")

(defvar *default-multipress-verify-p* t
  "Default value for display-multipress-verify-p.")

(defun open-contact-display (application-name
			     &key authorization-data authorization-name
			       before-actions class (default-screen 0)
			       display host protocol (root-class 'root))
  "Create and open a new contact-display."
  ;; Not included because of CLX bugs
  (declare (ignore protocol))
  ;; Set default if none defined
  (unless *default-host*
    (setq *default-host* host))
  (let ((disp
	 (cond
	   ((and display host)
	    (xlib:open-display  host
			   :display display
			   :authorization-name authorization-name
			   :authorization-data authorization-data))
	   (t
	    (xlib:open-default-display))))
	(display-class (or class application-name)))
    ;; Initialize resource name and class
    (setf (display-name disp)  application-name
	  (display-class disp) display-class)
    ;; Create a root contact for each screen of the display
    (let ((i 0)
	  roots)
      (dolist (screen (xlib:display-roots disp))
	(let ((name (intern (format nil "SCREEN-~d" i) 'keyword)))
	  (push (make-contact
		 root-class
		 :display disp
		 :screen screen
		 :parent nil
		 :name name
		 :complete-name  (list application-name name)
		 :complete-class (list display-class root-class))
		roots))
	(incf i))
      ;; Initialize root list and default root
      (setf (display-root-list disp) (nreverse roots)
	    (display-root disp)      (nth default-screen (display-root-list disp))))
    ;; Function to call BEFORE event handling
    (setf (before-actions disp) before-actions)
    ;; List of characters from the keyboard
    (setf (display-keyboard-buffer disp) nil)
    ;; Initialize multipress controls
    (setf (display-multipress-delay-limit disp) *default-multipress-delay-limit*
	  (display-multipress-verify-p disp)    *default-multipress-verify-p*)
    disp))

;; Note, inhereting the private interface (from a different package)
;; is problematic.  Be wary of the fact that xlib:window has the
;; following members: id display plist
(defcontact basic-contact (xlib:window)
  ((xlib:display :initarg :display
		 :reader contact-display)
   (parent :initarg :parent
	   :reader contact-parent)	       ; setf defined below
   (name :type symbol
	 :initarg :name
	 :initform :unnamed
	 :reader contact-name)
   (callbacks :type list
	      :reader contact-callbacks
	      :initform nil)
   (event-translations :type list
		       :initform nil)
   (event-mask :type xlib:mask32
	       :initform #.(xlib:make-event-mask :exposure)
	       :reader   contact-event-mask)   ; setf defined below
   (state :initform :mapped
	  :type (member :withdrawn :managed :mapped)
	  :reader contact-state)	       ; setf defined below
   (sensitive :initform :on
	      :type (member :off :on)
	      :reader contact-sensitive)      ; setf defined below
   (x :type xlib:int16
      :initform 0
      :reader contact-x)
   (y :type xlib:int16
      :initform 0
      :reader contact-y)
   (width :type xlib:card16
	  :initform 0
	  :reader contact-width)
   (height :type xlib:card16
	   :initform 0
	   :reader contact-height)
   (border-width :type xlib:card16
		 :initform 1
		 :reader contact-border-width)
   ;; Class allocated slots
   (compress-motion :initform :on :type (member :off :on)
		    :reader contact-compress-motion
		    :allocation :class)
   (compress-exposures :initform :off :type (member :off :on)
		       :reader contact-compress-exposures
		       :allocation :class))
  (:documentation "Basic contact using parent's window")
  (:resources
   ;; Selects screen when parent is a display
   (screen :type (or null xlib:card8))
   name
   callbacks
   event-translations
   event-mask
   state
   sensitive
   x
   y
   width
   height
   border-width))

(defcontact contact (basic-contact)
  ((background :type (or (member :none :parent-relative) xlib:pixel xlib:pixmap)
	       :initform :parent-relative
	       ;; setf defined below
	       :reader contact-background)
   (depth :type xlib:card16
	  :initform 0
	  :reader contact-depth)
   ;; internal slot for window initialization and destruction
   (initialization :type (or (member :destroy) list)))
  (:documentation "Basic contact")
  (:resources
   (documentation :type (or list string))
   ;; Slots
   background
   depth
   ;; Window attributes for create-window
   (backing-store :type (or null (member :not-useful :when-mapped :always)))
   (border :type (or null (member :copy) xlib:pixel xlib:pixmap))
   (cursor :type (or null (member :none) xlib:cursor))
   (override-redirect :type (or null (member :on :off)))
   (save-under :type (or null (member :on :off)))
   ;; These window attributes are NOT defined as resources, because it's not worth
   ;; the cost in initialization time.
   ;;    (backing-pixel :type (or null pixel))
   ;;    (backing-planes :type (or null pixel))
   ;;    (bit-gravity :type (or null bit-gravity))
   ;;    (class :type (member :copy :input-output :input-only) :initform :copy)
   ;;    (colormap :type (or null (member :copy) colormap))
   ;;    (do-not-propagate-mask :type (or null device-event-mask))
   ;;    (gravity :type (or null win-gravity))
   ;;    (visual :type (or (member :copy) card29) :initform :copy)
   )
  (:documentation "The base class for all interactive objects in CLUE."))

(defcontact composite (contact)
  ((children :initform nil
	     :type list
	     :reader composite-children)
   (focus    :initform nil
	     :type (or null contact)
	     :reader composite-focus)
   (shells   :type list
	     :initform nil
	     :reader composite-shells))
  (:resources
   (event-mask :initform #.(xlib:make-event-mask))
   (focus-name :type symbol))
  (:documentation "A basic CLUE contact with children"))

;;; Utility functions

(defmethod print-object ((instance contact) stream)
  (let ((name (if (slot-boundp instance 'name)
		  (contact-name instance)
		  :uninitialized)))
    (progn
      (write-string "#<" stream)
      (princ (class-name-of instance) stream)
      (write-char #\space stream)
      (princ name stream)
      (write-char #\> stream))))

(defun contact-complete-name (contact &optional nconc-name)
  ;; Return the complete name for contact
  ;; when present, nconc-name is put at the END of the name list.
  ;; This speeds getting the complete name of a contact given its parent and name.
  (let ((result (if nconc-name
		    (list (contact-name contact) nconc-name)
		    (list (contact-name contact)))))
    ;; Prepend names up to contact root
    (do ((parent (contact-resource-parent contact) (contact-resource-parent parent)))
	((null parent))
      (push (contact-name parent) result))
    ;; Prepend application name
    (push (display-name (contact-display contact)) result)
    result))

(defun contact-complete-class (contact &optional nconc-class)
  ;; Return the complete class for contact
  ;; when present, nconc-class is put at the END of the class list.
  ;; This speeds getting the complete class of a contact given its parent and class.
  (let ((result (if nconc-class
		    (list (class-name-of contact) nconc-class)
		    (list (class-name-of contact)))))
    ;; Prepend classes up to contact root
    (do ((parent (contact-resource-parent contact) (contact-resource-parent parent)))
	((null parent))
      (push (class-name-of parent) result))
    ;; Prepend application class
    (push (display-class (contact-display contact)) result)
    result))

(defmethod contact-resource-parent ((contact contact))
  (slot-value contact 'parent))

(defgeneric find-contact (parent &key name class)
  (:documentation
   "Return contact with given NAME and CLASS in the hierarchy starting
    with PARENT.  PARENT may be a contact or a contact-display. If a
    NAME or CLASS is not specified, it is ignored."))

(flet
    ((test  (contact name class)
       (when (and (or (null name)  (eq name  (contact-name contact)))
		  (or (null class) (eq class (class-name-of contact))))
	 contact)))
  (defmethod find-contact ((parent xlib:display) &key name class)
    (some #'(lambda (contact) (find-contact contact :name name :class class))
	  (display-root-list parent)))
  (defmethod find-contact ((parent contact) &key name class)
    (test parent name class))
  (defmethod find-contact ((parent composite) &key name class)
    (or (test parent name class)
	(some #'(lambda (contact) (find-contact contact :name name :class class))
	      (composite-children parent)))))

(defun ancestor-p (child parent)
  "Returns T when CHILD is a descendant of PARENT"
  (do ((p (contact-parent child) (contact-parent p)))
      ((null p))
    (when (eq p parent) (return t))))

(defun realized-p (contact)
  "Returns T when contact's window is created and not destroyed"
  (plusp (xlib:window-id contact)))

(defun destroyed-p (contact)
  "Returns true when contact's window is (being) destroyed."
  (getf (xlib:window-plist contact) :destroyed-p))

(defsetf destroyed-p (contact) (boolean)
  `(setf (getf (xlib:window-plist ,contact) :destroyed-p) ,boolean))

(defun managed-p (contact)
  "Returns non-nil when contact is geometry managed by its parent"
  (NOT (EQ (contact-state contact) :withdrawn)))

(defun mapped-p (contact)
  "Returns non-nil when contact is mapped"
  (eq (contact-state contact) :mapped))

(defun viewable-p (contact)
  "Returns T when contact is viewable."
  (declare (type contact contact))
  (with-slots (parent state) (the contact contact)
    (or (not parent)
	(and (realized-p contact)
	     (eq :mapped state)
	     (viewable-p parent)))))

(defun top-level-p (contact)
  "Returns T when CONTACT is a top-level window
 (i.e. under control of a window manager)"
  (and (contact-parent contact) ;; Not a root
       (null (contact-parent (contact-parent contact)))))

(defmethod (setf contact-callbacks) (list (self basic-contact))
  (with-slots (callbacks) self
    (setf callbacks list)))

(defmethod (setf contact-sensitive) (value (self contact))
  (declare (type (member :off :on) value))
  (check-type value (member :off :on) ":ON or :OFF")
  (with-slots (x y width height sensitive parent contact-display) self
    (let ((old sensitive))
      (setf sensitive value)
      ;; Redisplay when changing sensitive
      (when (and (not (eq old value)) (viewable-p self))
	(refresh self)
	;; Give up focus if insensitive
	(when (and (eq :off value) (owns-focus-p self))
	  ;; Send focus to parent.
	  (set-input-focus contact-display parent :parent)))))
  value)

(defun refresh (window &key (x 0) (y 0) width height)
  "Generate :exposure events for the given region of the WINDOW and for any descendant
   within this region. By default, WIDTH/HEIGHT is the distance from X/Y to the right/bottom
   edge of the WINDOW."
  (let ((transient-window
	 (xlib:create-window
	  :parent window :override-redirect :on
	  :x x
	  :y y
	  :width (or width (- (contact-width window) x))
	  :height (or height (- (contact-height window) y)))))
    (map-window transient-window)
    (destroy-window transient-window)))

(defmethod owns-focus-p ((contact contact))
  (with-slots (xlib:display) contact
    (eq contact (input-focus xlib:display))))

(defmethod owns-focus-p ((composite composite))
  (with-slots (xlib:display) composite
    (let ((focus (input-focus xlib:display)))
      (and
       (typep focus 'basic-contact)
       (or (eq focus composite)
	   (ancestor-p focus composite))))))

(defun sensitive-p (contact)
  "Returns T when a contact and all its ancestors are sensitive
   If there's a mode-stack, the contact, or one of its ancestors,
   must be in the current mode."
  (do ((p contact (contact-parent p)))
      ((null p) t)
    (when (eq (slot-value (the contact p) 'sensitive) :off) (return nil))))

(defmethod inside-contact-p ((contact contact) x y)
  "Returns T when X/Y (in contact coordinates) is inside CONTACT"
  (with-slots ((contact-width width) (contact-height height)) (the contact contact)
    (and (<= 0 x)
         (< x contact-width)
	 (<= 0 y)
         (< y contact-height))))

(defmethod (setf contact-event-mask) (mask (contact contact))
  (let ((mask (convert contact mask 'xlib:mask32)))
    (assert mask nil "~s is not an EVENT-MASK.")
    (when (realized-p contact)
      (setf (window-event-mask contact) mask))
    (with-slots (event-mask) contact
      (setf event-mask mask))))

(defmethod (setf contact-background) (new-value (contact contact))
  (declare (type contact contact))
  (setf new-value (convert contact new-value '(or (member :none :parent-relative) pixel pixmap)))
  (assert new-value nil
	  "~a could not be converted to :NONE, :PARENT-RELATIVE, a PIXEL, or a PIXMAP."
	  new-value)
  (when (realized-p contact)
    (setf (window-background contact) new-value))
  (with-slots (background) contact
    (setf background new-value)))

;;; Constraint resources

(defmacro contact-constraints (contact)
  "Return the list of constraint resource values for the CONTACT."
  `(getf (xlib:window-plist ,contact) 'constraints))

(defmacro contact-constraint (contact name)
  "Return the value of the constraint resource NAME for the CONTACT."
  `(getf (contact-constraints ,contact)
	 ,(cond ((keywordp name)
		 `,name)
		((and (consp name) (eq (car name) 'quote))
		 (intern (symbol-name (second name)) 'keyword))
		(:else
		 `(intern (symbol-name ,name) 'keyword)))))

(defun class-constraints (class &optional full-p)
  "Return the constraint resource specification list for the given CLASS.
If FULL-P is true, then the full list is returned; otherwise, a list of names is returned."
  (let ((full-list (clue-constraints class)))
    (if full-p
	full-list
	(mapcar #'first full-list))))


;;; Contact creation

(defun make-contact (class-name &rest options)
  "Make a contact of type CLASS-NAME, initializing with OPTIONS or from the resource database.
   Every contact must have a :PARENT."
  (apply #'make-instance class-name
	 :allow-other-keys t	;; temporary until we find a better fix
	 (default-options class-name options)))

(defmethod default-options ((class-name t) options)
  (declare (ignore options))
  ;; An (eql class-name) method should be defined by defcontact.
  (error "~s isn't the name of a contact subclass" class-name))


(defun get-contact-resource-table (class-name parent initargs)
  ;; Get the resource database table
  (declare (special *database*))

  (multiple-value-bind (complete-name complete-class)
      (if parent
	  (values
	   (contact-complete-name parent (or (getf initargs :name) class-name))
	   (contact-complete-class parent class-name))

	  (values
	   ;; These must be given when creating a root.
	   (getf initargs :complete-name)
	   (getf initargs :complete-class)))

    (assert (and complete-name complete-class)
	    nil "No parent specified for new ~a." class-name)

    (xlib:get-search-table *database* complete-name complete-class)))

(defmethod initialize-instance :after ((self basic-contact)
				       &rest initargs
				       &key resource-table defaults
					 &allow-other-keys)
  (with-slots (name xlib:display parent event-translations event-mask initialization callbacks) self
    ;; Initialize and save initial values for slot resources
    (setf initialization
	  (initialize-resource-slots self resource-table defaults))
    ;; Copy initial callback list, because this is destructively modified by add/delete-callback.
    (setf callbacks (copy-tree callbacks))
    ;; Save initial values for non-slot resources
    (let ((options (copy-list initargs)))
      ;; Allow resource-table to be GC'd
      (remf options :resource-table)
      (setf initialization
	    (nconc initialization options)))
    ;; Initialize and save initial values for constraint resources
    (when parent
      (setf initialization
	    (nconc initialization
		   (setf (contact-constraints self)
			 (initialize-constraints parent initargs resource-table)))))
    ;; Initialize name to class name by default
    (when (eq name :unnamed)
      (setf name (class-name-of self)))
    ;; Parse event-translations
    (setf event-translations
	  (mapcar #'(lambda (et) (parse-event-translation (first et) (rest et)))
		  event-translations)
	  event-mask
	  (xlib::encode-event-mask event-mask))
    ;; Add to composition hierarchy
    (when parent ; root contacts don't have a parent
      (setf xlib:display (contact-display parent))
      (add-to-parent self))))

(defmethod initialize-instance :after ((self contact) &rest initargs)
  (declare (type list initargs))

  (with-slots (border-width) self
    ;; Validate initargs for window class
    (assert
     (or (not (eq :input-only (getf initargs :class)))
	 (zerop border-width))
     () "An :input-only contact must have border-width 0."))

  (setf (display-update-flag (contact-display self)) t))


;;; CALLBACKS

(defun callback-p (contact callback-name)
  (cdr (assoc callback-name (slot-value contact 'callbacks) :test #'eq)))

(defun function-equal-p (f g)
  (eq (if (symbolp f) (symbol-function f) f)
      (if (symbolp g) (symbol-function g) g)))

(defun add-callback (contact name function &rest args)
  "Associate CONTACT callback NAME with the given FUNCTION and ARGS."
  (with-slots (callbacks) contact
    (let ((functions    (assoc name callbacks :test #'eq))
	  (new-function (list* function (copy-list args))))
      (if functions
	  ;; Append behind any previous functions for this callback
	  (rplacd functions (nconc (delete function (rest functions)
					   :test #'function-equal-p
					   :key #'first
					   :count 1)
				   (list new-function)))

	  ;; Else add first callback function
	  (push (list name new-function) callbacks))
      name)))

(defun delete-callback (contact name &optional function)
  "Disassociate the given FUNCTION and its args from the CONTACT callback NAME.
   If no FUNCTION is given, then all callback functions are deleted."
  (with-slots (callbacks) contact
    (let ((functions (assoc name callbacks :test #'eq)))
      (when functions
	(let ((new-functions (when function
			       (delete function (rest functions)
				       :test #'function-equal-p
				       :key #'first
				       :count 1))))
	  (if new-functions
	      (rplacd functions new-functions)
	      (setf callbacks (delete name callbacks
				      :test #'eq
				      :key #'first)))))))
  name)


(defmacro apply-callback-else ((contact name &rest args) &body body)
  "Invoke callback functions associated with NAME for CONTACT,
   using ARGS followed by the callback arguments. If no such callback
   functions exist, then execute and return the value(s) of the BODY forms."
  (let ((functions (gensym)))
    `(let ((,functions (callback-p ,contact ,name)))

       (if ,functions
	   (catch :abort-callback
	     (do (function) (())
	       (setf function (pop ,functions))

	       (unless ,functions
		 (return
		   ;; Return value(s) of last callback function
		   (apply (first function) ,@args (rest function))))

	       (apply (first function) ,@args (rest function))))

	   ,@(when body
	       `((progn ,@body)))))))

(defmacro apply-callback (contact name &rest args)
  "Invoke callback functions associated with NAME for CONTACT,
   using ARGS followed by the callback arguments."
  `(apply-callback-else (,contact ,name ,@args)))



;;;-----------------------------------------------------------------------------
;;; Basic contact methods

(defmethod add-to-parent ((self basic-contact))
  (add-child (contact-parent self) self))

(defmethod (setf contact-state) (state (contact contact))
  ;; Bound during contact realization to optimize initial mapping.
  (declare (special *all-children-mapped-p*))

  (check-type state (member :withdrawn :managed :mapped))

  (let ((old-state (slot-value (the contact contact) 'state)))
    (unless (eq old-state state)
      (setf (slot-value (the contact contact) 'state) state)
      (if (realized-p contact)
	  ;; When realized, change state immediately
	  (progn
	    (when (or (eq old-state :withdrawn)
		      (eq state     :withdrawn))
	      ;; Let parent react to transition to/from g.mgmt.
	      (change-layout (contact-parent contact) contact))

	    (if (eq state :mapped)

		;; Was unmapped, now mapped
		(unless (and (boundp '*all-children-mapped-p*) *all-children-mapped-p*)
		  (map-window contact))

		(when (eq old-state :mapped)
		  ;; Was mapped, now unmapped
		  (unmap-window contact))))

	  ;; Not realized, let UPDATE-STATE do the work
	  (setf (display-update-flag (contact-display contact)) t))))
  state)


(defmethod (setf contact-parent) (new-parent (contact contact) &key x y)
  (let ((c (or (when (destroyed-p contact) contact)
	       (when (destroyed-p new-parent) new-parent))))
    (when c
      (error "~s is being destroyed." c)))

  (with-slots (parent) contact

    ;; Forestall any MATCH errors from reparent-window
    (unless (eq (contact-screen new-parent) (contact-screen parent))
      (error "New parent screen (~s) must be the same as old parent screen (~s)."
	     (contact-screen new-parent) (contact-screen parent)))
    (when (eq new-parent contact)
      (error "Cannot reparent ~s to itself." contact))
    (when (ancestor-p new-parent contact)
      (error "New parent ~s is already a descendant of ~s." new-parent contact))
    (when (and (eq (contact-background contact) :parent-relative)
	       (/= (contact-depth contact) (contact-depth new-parent)))
      (error "New parent depth (~s) must be the same as contact depth (~s)."
	     (contact-depth new-parent) (contact-depth contact)))

    (let ((actual-state (contact-state contact))
	  (new-x        (or x (contact-x contact)))
	  (new-y        (or y (contact-y contact))))

      ;; Unmap and unmanage until reparented
      (setf (contact-state contact) :withdrawn)

      ;; Tell server to reparent window
      (reparent-window contact new-parent new-x new-y)

      ;; Update contact hierarchy
      (delete-child parent contact)
      (setf parent new-parent)
      (add-child new-parent contact)

      ;;Restore state
      (setf (contact-state contact) actual-state)))

  new-parent)



;; Compatibility hack - remove soon
(defun present (contact) (setf (contact-state contact) :mapped))

;; Compatibility hack - remove soon
(defun dismiss (contact &optional (unmanage-p t))
  (if unmanage-p
      (setf (contact-state contact) :withdrawn)
      (setf (contact-state contact) :managed)))

(defun update-state (display)
  (when (display-update-flag display)
    (dolist (root (display-root-list display))
      (update-tree root))
    (setf (display-update-flag display) nil)))

(defun contact-event-translations-mask (contact)
  "Return the event mask from the event translations (class and instance) for CONTACT."
  (with-slots (event-translations) contact
    (logior
     ;; Instance translations
     (event-translations-mask event-translations)
     ;; Class translations
     (class-name-event-mask (class-name-of contact)))))

(defun realize-state (contact)
  "Make the initial contact-state of a newly-realized CONTACT effective."
  (multiple-value-bind (old-state new-state) (initial-state-transition contact)
    (when old-state
      ;; Problem:  This is a special case because the value of state slot after
      ;;           initialization is not yet in effect and doesn't reflect reality.
      ;; Solution: Temporarily set initial value of state slot to reality (i.e. old-state)
      ;;           so that (setf contact-state) will take effect correctly.
      (setf (slot-value (the contact contact) 'state) old-state)
      (setf (contact-state contact) new-state))))

;;; Contact DESTRUCTION

;; Helper function
(defun map-over-children (contact function &rest args)
  ;; Apply FUNCTION first to contact's children, then to contact.
  (when (typep contact 'composite)
    (dolist (child (composite-children contact))
      (apply #'map-over-children child function args)))
  (apply function contact args))

(defun destroy-cleanup (contact)
  "Perform side-effects of destroying the CONTACT."

  ;; Mark contact destroyed
  (setf (destroyed-p contact) t)

  ;; Remove contact's timers
  (delete-timer contact)

  ;; Ensure modes are popped
  (delete-mode contact)

  ;; Destroy a composite's shells
  (when (typep contact 'composite)
    (dolist (shell (slot-value (the composite contact) 'shells))
      (destroy shell)))

  ;; Invoke any :destroy callback
  (apply-callback contact :destroy))

(defun destroy-finish (contact)
  ;; Called from destroy-notify event processing to remove
  ;; contact and its descendents from the resource-id hash-table.
  (map-over-children
   contact
   #'(lambda (contact)
       (xlib::deallocate-resource-id (window-display contact) (xlib:window-id contact) 'window)
       #+(and ti (not clos))
       (setf (si:array-leader contact 1) 'destroyed-contact) ;; Debug hack to catch errors
       )))

;;; Root contact
;;;
;;; For each screen of the display there's a root contact.
;;; The root contact is used as the root parent contact for all the contacts
;;; on a screen

(defcontact root (composite)
  ((screen :type xlib:screen :initarg :screen)
   (x :initform 0)
   (y :initform 0)
   ;; actual value filled in by initialize-instance
   (width :initform 0)
   ;; actual value filled in by initialize-instance
   (height :initform 0)
   (border-width :initform 0)
   (depth :initform 0)
   (background :initform :none))
  (:resources
   ;; Remove all inherited resources that cannot actually be changed by user
   (background :remove t)
   (backing-store :remove t)
   (border :remove t)
   (border-width :remove t)
   (depth :remove t)
   (documentation :remove t)
   (focus-name :remove t)
   (height :remove t)
   (name :remove t)
   (override-redirect :remove t)
   (save-under :remove t)
   (sensitive :remove t)
   (screen :remove t)
   (state :remove t)
   (width :remove t)
   (x :remove t)
   (y :remove t)))

(defun contact-screen (contact)
  ;; Return the xlib:screen associated with CONTACT
  (declare (type contact contact))
  (slot-value (the root (contact-root contact)) 'screen))

(defun contact-top-level (contact)
  "Return the top-level ancestor of the CONTACT, or nil, if CONTACT is a root."
  (do () ((or (null contact) (top-level-p contact)) contact)
    (setf contact (contact-parent contact))))

(defun contact-translate (from from-x from-y &optional to)
  "Translate the position given by FROM-X and FROM-Y relative to the FROM contact
into a position relative to the TO contact. By default, TO is (contact-root FROM).
If FROM and TO are on different screens, then nil is returned."
  (if to
      (when (eq (contact-root from) (contact-root to))
        ;; Translate both to position and from position to mutual root coordinate system
        ;; and take difference
        (multiple-value-bind (root-from-x root-from-y) (contact-translate from from-x from-y)
          (multiple-value-bind (root-to-x root-to-y) (contact-translate to 0 0)
            (values (- root-from-x root-to-x) (- root-from-y root-to-y)))))
      ;; Translate to root coordinate system
      (do* ((to-x   from-x)
            (to-y   from-y)
            (from   from                        parent)
            (bw     (contact-border-width from) (contact-border-width from))
            (parent (contact-parent from)       (contact-parent from)))
           ((null parent) (values to-x to-y))
        (incf to-x (+ bw (contact-x from)))
        (incf to-y (+ bw (contact-y from))))))


;;; Stream support
;;;
;;; Philosophy
;;;
;;; CLUE keeps a single character buffer for all windows, instead of a
;;; separate buffer for every window.  The reason its done this way is
;;; to prevent focus management problems within an application.  We
;;; reason that a single application will use a single display (or one
;;; display per process), and that when users type on the keyboard,
;;; they're typing to the APPLICATION, not to a (sub)widow of an
;;; application.  In particular, users shouldn't have to care about
;;; keyboard focus within an application.
;;;
;;; If there are several stream contacts for a particular display
;;; (server connection) then the contact getting keystrokes is the
;;; contact that's doing the read.  With a single buffer there's no need
;;; to worry about where the mouse is within the application, or which
;;; window has the keyboard focus.  The user is never left typing
;;; into a dead window, only to have the buffered key events appear
;;; later when the keyboard focus changes.

(defun read-character (display &optional timeout)
  "Enters an input loop which can be exited whenever a character is
   available in the display keyboard buffer. The function's return value
    is the next char from this buffer."
  (or (pop (display-keyboard-buffer display))
      (loop
	 (process-next-event display timeout)
	 (let ((char (pop (display-keyboard-buffer display))))
	   (when (or char timeout)
	     (return char))))))

(defun unread-character (display character)
  "Make CHARACTER be the next character returned from GET-CHARACTER"
  (push character (display-keyboard-buffer display)))

(defun listen-character (display &optional (timeout 0))
  "If a character is available within TIMEOUT seconds, return it without
    removing it from the display keyboard buffer. Otherwise return NIL."
  (let ((char (read-character display timeout)))
    (when char
      (unread-character display char)
      char)))

(defun append-characters (display character &optional (start 0) end)
  "Put a character or string in the display keyboard buffer"
  (declare (type display display)
	   (type (or character string) character))
  ;; When event-handlers return a character or string, stuff it into the keyboard buffer
  (etypecase character
    (character (setf (display-keyboard-buffer display)
		     (nconc (display-keyboard-buffer display) (cons character nil))))
    (string
     (do ((i start (1+ i))
	  (end (or end (length character))))
	 ((>= i end))
       (setf (display-keyboard-buffer display)
	     (nconc (display-keyboard-buffer display) (cons (char character i) nil)))))))

(defun clear-characters (display)
  "Clear the display keyboard buffer"
  (declare (type display display))
  (setf (display-keyboard-buffer display) nil))


;;; Geometry management

;; Utility functions for geometry management
(defun previous-sibling (contact)
  "Return the first managed contact BEFORE CONTACT"
  (let ((previous nil))
    (dolist (sibling (composite-children (contact-parent contact)))
      (when (eq sibling contact) (return previous))
      (when (managed-p sibling) (setq previous sibling)))))

(defun next-sibling (contact)
  "Return the first managed contact AFTER CONTACT"
  (dolist (sibling (cdr (member contact (composite-children (contact-parent contact)) :test #'eq)))
    (when (managed-p sibling) (return sibling))))

(defmacro composite-changing-layout-p  (composite)
  "While true, calls to (CHANGE-LAYOUT COMPOSITE) are ignored."
  `(getf (xlib:window-plist ,composite) :changing-layout-p))

(defmacro while-changing-layout ((composite) &body body)
  "Postpone calls to (CHANGE-LAYOUT COMPOSITE) until BODY has been executed."
  `(progn
     (setf (composite-changing-layout-p  ,composite) t)
     (unwind-protect (progn ,@body)
       (let ((changed-p (composite-changing-layout-p ,composite)))
	 (setf (composite-changing-layout-p  ,composite) nil)
	 (when (eq :changed changed-p) (change-layout ,composite))))))

(defun change-priority (contact priority &key sibling accept-p)
  "Request stacking order change for CONTACT. The first value returned indicates
   if the request was approved and performed. If nil, then the request was not
   approved, and the remaining values specify an acceptable compromise change.
   If ACCEPT-P, then any compromise request is performed immediately."
  (declare (type contact           contact)
	   (type (member :above :below :top-if :bottom-if :opposite) priority)
	   (type (or null contact) sibling))
  ;; Refuse request, with no compromise, if unrealized
  (when (and (realized-p contact) (not (destroyed-p contact)))
    (with-slots (parent) contact
      (assert parent () "Cannot change the priority of a root.")
      (multiple-value-bind (success-p new-priority new-sibling)
	  (if (and (managed-p contact) (realized-p parent))
	      ;; Ask for approval from parent
	      (manage-priority parent contact priority sibling)
	      ;; Else approve immediately.
	      (values t priority sibling))
	(when (or success-p			       ; Approved or...
		  (and accept-p new-priority))	       ; Compromise exists and is acceptable
	  ;; Get after-effect function, if any.
	  (let ((after-effect (or success-p
				  (manage-priority parent contact new-priority new-sibling))))
	    (assert after-effect ()
		    "MANAGE-PRIORITY for ~a failed to accept its own compromise priority." parent)
	    ;; Perform approved change
	    (setf (contact-priority contact new-sibling) new-priority)
	    ;; Perform after-effect
	    (when (and (functionp after-effect) (not (composite-changing-layout-p parent)))
	      (funcall after-effect parent))))
	(values success-p new-priority new-sibling)))))

(defun change-geometry (contact &key x y width height border-width accept-p)
  "Request geometry change for CONTACT. The first value returned indicates
   if the request was approved and performed. If nil, then the request was not
   approved, and the remaining values specify an acceptable compromise change.
   If ACCEPT-P, then any compromise request is performed immediately."
  (unless (destroyed-p contact)
    (with-slots
	  ((contact-x            x)
	   (contact-y            y)
	   (contact-width        width)
	   (contact-height       height)
	   (contact-border-width border-width)
	   (contact-parent       parent))
	(the basic-contact contact)
      (assert contact-parent () "Cannot change the geometry of a root.")
      (multiple-value-bind
	    (success-p approved-x approved-y approved-width approved-height approved-border-width)
	  (manage-geometry contact-parent contact x y width height border-width)
	(when (and
	       (or success-p			; Approved or...
		   (and accept-p approved-x))	; Compromise exists and is acceptable, and ...
	       (or				; Not already done by window mgr...
		(slot-value contact-parent 'parent)	;   (i.e. either non-top-level or unrealized.)
		(not (realized-p contact))))	;   (See manage-geometry for root)
	  ;; Perform approved change
	  (let
	      ;; Get after-effect function, if any.
	      ((after-effect (or success-p
				 (manage-geometry contact-parent contact
						  approved-x approved-y
						  approved-width approved-height
						  approved-border-width))))
	    (assert
	     after-effect ()
	     "MANAGE-GEOMETRY for ~a failed to accept its own compromise geometry."
	     contact-parent)
	    ;; Change contact geometry.
	    (when
		;; Perform after-effect if...
		(and
		 ;; Something actually changed...
		 (let ((moved-p (move contact approved-x approved-y))
		       (sized-p (resize contact approved-width approved-height approved-border-width)))
		   (or moved-p sized-p))
		 ;; and after-effect function returned...
		 (functionp after-effect)
		 ;; and not in the middle of a batch of layout changes...
		 (or (not (composite-changing-layout-p contact-parent))
		     ;; Remember that after-effect was postponed.
		     (not (setf (composite-changing-layout-p  contact-parent) :changed))))
	      (funcall after-effect contact-parent))))
	;; Return result of geometry mgmt.
	(values (when success-p t)
		approved-x approved-y
		approved-width approved-height
		approved-border-width)))))

;; NIL outside without-requests
(defparameter *contact-notified* nil)

(defmacro without-requests (contact &body body)
  "Any server requests on CONTACT ordinarily sent within BODY should be skipped.
This wrapper is used when CONTACT needs to update its state to reflect window changes
already performed by the user/wm."
  `(let ((*contact-notified* ,contact)) ,@body))
