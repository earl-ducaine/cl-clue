;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
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
;;;


(in-package :cluei)

(defconstant +default-contact-height+ 16)
(defconstant +default-contact-width+ 16)

(defmethod update-tree ((composite composite))
  ;; Search for a composite with an unrealized child and update it.
  (let ((children (composite-children composite)))
    (if (dolist (child children)
	  (when (and (not (realized-p child)) (managed-p child))
	    (return t)))
	(progn
	  (initialize-geometry composite)
	  (dolist (child children)
	    (when (and (not (realized-p child)) (managed-p child))
	      (realize child)
	      (realize-state child))))
	;; No unrealized children here, continue the search lower down
	(dolist (child children)
	  (when (realized-p child)
	    (update-tree child))))))

(defmethod update-tree ((contact contact))
  ;; nop
  )


(defmethod display ((contact basic-contact) &optional x y width height &key)
  "Display self on server"
  ;; This function needs to be over-ridden by the subclasses
  (declare (ignore x y width height))
  ;; not used
  contact)

;; Create the x window associated with a contact.
(defmethod realize ((contact contact))
  "Create the window for CONTACT. This function should not be called
   by application programs."
  (with-slots (background border-width depth event-mask height initialization
			  parent width x y)
      contact
    ;; Ensure the parent is realized
    (assert (realized-p parent) ()
	    "Parent of ~s is not realized." contact)
    ;; Ensure width/height initialized
    (assert (and (plusp width) (plusp height)) ()
    	    "Width and height have not been initialized for ~s" contact)
    ;; Calculate event-mask
    (setf event-mask (contact-event-translations-mask contact))
    (let ((input-only-p (eq :input-only (getf initialization :class))))
      ;; Create the contact window
      (apply
       #'xlib:create-window
       :window contact
       :parent parent
       :x x
       :y y
       :width width
       :height height
       :border-width border-width
       :event-mask event-mask
       :background (unless input-only-p background)
       :depth depth
       :allow-other-keys t
       initialization)
      ;; Record depth, if inherited from parent
      (unless (or (plusp depth) input-only-p)
	(setf depth (contact-depth parent))))
    (let* ((documentation (getf initialization :documentation)))
      ;; Keep initialiation around for awhile, it's useful for
      ;; debugging (setf initialization nil) ;; Give initialization
      ;; list to the garbage collector
      (when documentation
	(setf (window-documentation contact) documentation)))))

(defmethod realize :after ((contact composite))
  ;; Default focus from the :focus-name initialization
  (with-slots (initialization focus) contact
    (let ((focus-name (getf initialization :focus-name)))
      (when (and focus-name (not focus))
	(setf focus (find-contact contact :name focus-name)))))
  ;; Map children here, to ensure the composite is mapped AFTER its children
  ;; This eliminates the screen flash that would happen if children were
  ;; mapped on top of a visible parent.
  (let* ((children (composite-children contact))
	 (*all-children-mapped-p*
	  (dolist (child children t)
	    (unless (mapped-p child)
	      (return nil)))))
    (declare (special *all-children-mapped-p*))
    ;; Recursively realize all managed children of COMPOSITE
    ;; Note: by definition, all children are unrealized
    (dolist (child children)
      (when (managed-p child)
	(realize child)
	(realize-state child)))
    ;; Map all children at once, if possible
    (when *all-children-mapped-p*
      (map-subwindows contact)))
  ;; Initialize default shell colormaps, if necessary.
  (with-slots (shells) contact
    (dolist (shell shells)
      ;; Shell colormap defaulted?
      (unless (getf (slot-value shell 'initialization) :colormap)
	;; Yes, shell inherits default colormap from owner.
	(if (realized-p shell)
	    (setf (window-colormap shell)
		  (window-colormap contact))
	    (setf (getf (slot-value shell 'initialization) :colormap)
		  (window-colormap contact)))))))

(defmethod initialize-geometry ((composite composite))
  ;; Negotiate initial managed geometry from the bottom up
  (declare (type composite composite))
  (let ((newly-managed 0)
	new-child)
    ;; Recursively descend to initialize-geometry for all unrealized
    ;; managed children
    (dolist (child (composite-children composite))
      (when (and (not (realized-p child)) (managed-p child))
	(setf new-child child)
	(incf newly-managed)
	(initialize-geometry child)))
    ;; Optimization: don't bother to change layout unless necessary
    (when new-child
      (change-layout composite
		     (unless (> newly-managed 1)
		       new-child)))))

(defmethod initialize-geometry ((contact contact))
  ;; nop
  )

(defmethod initial-state-transition ((contact basic-contact))
  "Return the old-state/new-state for the initial (setf contact-state) after CONTACT
   is realized. Return nil if (setf contact-state) need not be called, i.e. no
   initial state transition is necessary."
  (with-slots (state) contact
    (when (eq :mapped state)
      (values :managed :mapped))))

(defmethod destroy ((contact contact))
  "Destroy the CONTACT."
  (when (and (not (destroyed-p contact))	; only destroy once
	     (contact-parent contact))		; don't destroy root
    ;; Unmanage the contact (parent's change-layout is called)
    (setf (contact-state contact) :withdrawn)
    (when (realized-p contact)
      ;; Select for :structure-notify to receive :destroy-notify events
      (setf (window-event-mask contact) #.(xlib:make-event-mask :structure-notify))
      ;; Destroy the contact's window subtree
      (xlib:destroy-window contact))
    ;; Destroy other server resources and intrinsics hooks associated with contact
    (map-over-children contact #'destroy-cleanup)
    ;; Delete contact from its parent's child list
    (delete-child (contact-parent contact) contact)))

(defmethod initialize-instance :after ((self root) &rest options)
  (declare (ignore options))
  (with-slots
	(xlib:display screen (id xlib::id) x y width height border-width depth initialization)
      self
    ;; A root contact represents a root window
    (setf
     id (xlib:window-id (xlib:screen-root screen))
     initialization nil			;; Root window is already realized
     x 0
     y 0
     width (xlib:screen-width screen)
     height (xlib:screen-height screen)
     border-width 0
     depth (xlib:screen-root-depth screen))
    ;; Update CLX resource id lookup to associate root id with root contact
    (xlib::save-id xlib:display id self)))

(defmethod contact-root ((contact contact))
  ;; Return the root contact associated with CONTACT
  (do* ((root contact parent)
	(parent (contact-parent contact) (contact-parent root)))
       ((null parent) root)))

(defmethod manage-geometry ((parent composite) (contact contact) x y width height border-width &key)
  (declare (type (or null xlib:int16) x y)
	   (type (or null xlib:card16) width height border-width))
  (with-slots ((contact-x x)
	       (contact-y y)
	       (contact-width width)
	       (contact-height height)
	       (contact-border-width border-width)) (the contact contact)

    ;; Just ensure positive size
    (let* ((requested-width   (or width contact-width))
	   (acceptable-width  (if (zerop requested-width)
				  +default-contact-width+
				  requested-width))
	   (requested-height  (or height contact-height))
	   (acceptable-height (if (zerop requested-height)
				  +default-contact-height+
				  requested-height)))

      (values (and (= requested-width acceptable-width)
		   (= requested-height acceptable-height))
	      (or x contact-x)
	      (or y contact-y)
	      acceptable-width
	      acceptable-height
	      (or border-width contact-border-width)))))

(defmethod manage-geometry :around ((parent composite) (contact basic-contact)
				    x y width height border-width &key)
  (declare (type contact          contact)
	   (type (or null xlib:int16)  x y)
	   (type (or null xlib:card16) width height border-width))
  ;; Approve immediately?
  (if (and (realized-p parent) (realized-p contact) (managed-p contact))
      ;; No, do full policy check.
      (call-next-method)
      ;; Yes, just return requested or current values.
      (with-slots ((contact-x x)
		   (contact-y y)
		   (contact-width width)
		   (contact-height height)
		   (contact-border-width border-width))
	  contact
	(values t
		(or x contact-x)
		(or y contact-y)
		(or width contact-width)
		(or height contact-height)
		(or border-width contact-border-width)))))

(defmethod add-child ((self composite) contact &key)
  "Put CONTACT on its parent's list of managed contacts"
  ;; Default is to put at end of list
  (with-slots (children) self
    (setf children (nconc children (cons contact nil)))))

(defmethod delete-child ((self composite) contact &key)
  "Remove CONTACT from the list of contacts"
  (with-slots ((manager-children children)) self
    (setf manager-children (delete contact manager-children :count 1 :test #'eq))))

;;; Most composites will probably want to over-ride this
(defmethod change-layout ((composite composite) &optional newly-managed)
  "Called whenever the set of managed children changes."
  (declare (type (or null contact) newly-managed))
  (if newly-managed
      (change-geometry newly-managed :accept-p t)
      (dolist (child (composite-children composite))
	(change-geometry child :accept-p t))))

;; This method supports while-changing-layout for all composite classes.
(defmethod change-layout :around ((composite composite) &optional newly-managed)
  (declare (ignore newly-managed))
  (if (composite-changing-layout-p composite)
      ;; Remember that layout actually needs to change
      (setf (composite-changing-layout-p composite) :changed)

      ;; Else, change immediately.
      (call-next-method)))

(defmethod (setf contact-priority) (new-priority (contact contact) &optional new-sibling)
  (setf (window-priority contact new-sibling) new-priority))

(defmethod manage-priority ((self composite) contact priority sibling &key)
  "Change the stacking order of CONTACT relative to SIBLING.
   PRIORITY is one of :above :below :top-if :bottom-if :opposite."
  (declare (type (member :above :below :top-if :bottom-if :opposite) priority)
	   (type (or null contact) sibling))
  self contact ;; not used
  (values t priority sibling))

(defmethod accept-focus-p ((contact contact))
  "Returns non-nil when CONTACT is willing to become the keyboard input focus"
  (and (viewable-p contact)
       (plusp (logand (contact-event-mask contact)
		      #.(xlib:make-event-mask :key-press :key-release)))
       (sensitive-p contact)))

(defmethod move-focus ((composite composite) &optional (direction :next) &key start revert-to)
  "Move the input focus to the :next :previous or :set contact from
   START. start defaults to the current focus if there is one, or the
   first child.  Returns the new focus contact or NIL if no contacts
   will accept the focus (see accept-focus-p)."
  (let* ((start (or start (composite-focus composite)))
	 (focus (or start (first (composite-children composite)))))
    (when focus ;; focus nil when composite has no children
      (assert (member focus (composite-children composite) :test #'eq) ()
	      "~s isn't a child of ~s" focus composite)
      (when
	  (setf focus
		(if (eq :set direction)
		    ;; Ensure requested focus is ready to accept
		    (when (accept-focus-p focus) focus)
		    ;; Else look for next focus ready to accept
		    (do* ((get-sibling (ecase direction (:next 'next-sibling) (:previous 'previous-sibling)))
			  (focus       (funcall get-sibling focus) (funcall get-sibling focus)))
			 ((or (not focus) (eq focus start)))
		      (when (accept-focus-p focus) (return focus)))))
	;; Tell server to change input focus
	(set-input-focus (contact-display focus) focus (or revert-to :parent)))
      ;; Record focus child found
      (setf (slot-value composite 'focus) focus))))

(defmethod preferred-size ((contact contact) &key width height border-width)
  "Return preferred size, based on given changes to current values."
  ;; Primary method is compliant
  (with-slots ((current-width width)
	       (current-height height)
	       (current-border-width border-width)) contact
    (values (or width current-width)
	    (or height current-height)
	    (or border-width current-border-width))))

(defmethod move ((contact contact) x y)
  "Move CONTACT to coordinates X/Y relative to its parent."
  (with-slots ((contact-x x) (contact-y y)) contact
    (unless (eq contact *contact-notified*)
      (when (realized-p contact)
	(with-state (contact)
	  (unless (= contact-x x) (setf (drawable-x contact) x))
	  (unless (= contact-y y) (setf (drawable-y contact) y)))))
    (setf contact-x x)
    (setf contact-y y)))

(defmethod move :around ((contact contact) x y)
  ;; Skip primary and auxiliary methods if no change.
  (with-slots ((contact-x x) (contact-y y)) contact
    (let ((position-changed-p (or (not (= contact-x x)) (not (= contact-y y)))))
      (when position-changed-p
	(call-next-method))
      position-changed-p)))

(defmethod resize ((contact contact) width height border-width)
  "Change the size of CONTACT."
  (with-slots ((contact-width width)
	       (contact-height height)
	       (contact-border-width border-width)) contact
    (unless (eq contact *contact-notified*)
      (when (realized-p contact)
	(with-state (contact)
	  (unless (= contact-width width)
	    (setf (drawable-width contact) width))
	  (unless (= contact-height height)
	    (setf (drawable-height contact) height))
	  (unless (= contact-border-width border-width)
	    (setf (drawable-border-width contact) border-width)))))
    (setf contact-width width)
    (setf contact-height height)
    (setf contact-border-width border-width)))

(defmethod resize :around ((contact contact) width height border-width)
  ;; Skip primary and auxiliary methods if no change.
  (with-slots
	((contact-width        width)
	 (contact-height       height)
	 (contact-border-width border-width))
      contact
    (let ((size-changed-p (or (not (= contact-width width))
			      (not (= contact-height height))
			      (not (= contact-border-width border-width)))))
      (when size-changed-p
	(call-next-method))
      size-changed-p)))
