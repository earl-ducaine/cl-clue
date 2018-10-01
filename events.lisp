;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 texas instruments incorporated
;;;				  p.o. box 149149
;;;			       austin, texas 78714-9149
;;;
;;; copyright (c)1987,1988,1989,1990 texas instruments incorporated.
;;;
;;; permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; texas instruments incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

;;;  to do:
;;;
;;; 1. hooks are in place to handle button mult-click and hold using 3 extra modifier state bits
;;;    (:hold :click :double), but the interface isn't there yet.
;;;
;;; 2. add translation from symbolic mouse button names to actual mouse button/modifier specs.
;;;    use the keysym translation facilities (define 5 keysyms for each of the mouse keys,
;;;    and use define-keysym)


(in-package :cluei)

;; dynamically bound when making certain function calls for reasons
;; that are not yet completely clier to me.
(defvar $event$)

;; input processing
;;
;; rather than pass around event parameters in long plists,
;; parameters are stuffed into this structure.  for any one event,
;; most slots are undefined, and these are initialized to nil.
;; event structures are kept on a resource and re-used.
;; the only reason event is a class and not a structure is because
;; we want to use with-slots.

(defclass event ()

  ((key)
   ;; display event was reported to
   (display)
   ;; contact the event is directed to
   (contact)
   ;; character from code and state
   (character)
   ;; keysym from code and state
   (keysym)
   ;; place for extension data
   (xlib::plist :initform nil :type list)
   ;; the following are from the clx event
   (above-sibling)				; used by :configure-notify :configure-request
   (atom)					; used by :property-notify
   (border-width)				; used by :create-notify :configure-notify :configure-request
   (child)					; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify
   (code)					; used by :key-press :key-release :button-press :button-release
   (colormap)					; used by :colormap-notify
   (sequence)					; used by all except :keymap-notify
   (configure-p)				; used by :unmap-notify
   (count)					; used by :exposure :graphics-exposure :mapping-notify
   (data)					; used by :client-message :timer
   (drawable)					; used by :graphics-exposure :no-exposure
   (event-window)				; used by :destroy-notify :unmap-notify :map-notify :reparent-notify
						;         :configure-notify :gravity-notify :circulate-notify
   (focus-p)					; used by :enter-notify :leave-notify
   (format)					; used by :client-message
   (height)					; used by :exposure :graphics-exposure :create-notify :configure-notify
						;         :configure-request :resize-request
   (hint-p)					; used by :motion-notify
   (installed-p)				; used by :colormap-notify
   (keymap)					; used by :keymap-notify
   (kind)					; used by :enter-notify :leave-notify :focus-in :focus-out
   (major)					; used by :graphics-exposure :no-exposure
   (minor)					; used by :graphics-exposure :no-exposure
   (mode)					; used by :enter-notify :leave-notify :focus-in :focus-out
   (name)					; used by :timer
   (new-p)					; used by :colormap-notify
   (override-redirect-p)			; used by :create-notify :map-notify
						;                         :reparent-notify :configure-notify
   (parent)					; used by :create-notify :map-request :reparent-notify :configure-request
						;         :circulate-notify :circulate-request
   (place)					; used by :circulate-notify :circulate-request
   (property)					; used by :selection-request :selection-notify
   (requestor)					; used by :selection-request
   (root)					; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify
   (root-x)					; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify
   (root-y)					; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify
   (same-screen-p)				; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify
   (selection)					; used by :selection-clear :selection-request :selection-notify
   (send-event-p)				; used by -all events-
   (state)					; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify
						;         :visibility-notify :property-notify
   (target)					; used by :selection-request :selection-notify
   (time)					; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify :property-notify
						;         :selection-clear :selection-request :selection-notify
   (type)					; used by :client-message
   (width)					; used by :exposure :graphics-exposure :create-notify :configure-notify
						;         :configure-request :resize-request
   (window)					; used by all events except :graphics-exposure :no-exposure :mapping-notify
   (x)						; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify :exposure
						;         :graphics-exposure :create-notify :reparent-notify
						;         :configure-notify :configure-request :gravity-notify
   (y)						; used by :key-press :key-release :button-press :button-release
						;         :motion-notify :enter-notify :leave-notify :exposure
						;         :graphics-exposure :create-notify :reparent-notify
						;         :configure-notify :configure-request :gravity-notify
  )
  (:documentation "clue event structure, one slot for every event value.  no methods."))

(defmethod print-object ((instance event) stream)
  (progn
    (write-string "#<event " stream)
    (with-slots (key contact) instance
      (princ key stream)
      (when (typep contact 'contact)
	(write-string " for " stream)
	(princ (contact-name contact) stream)))
    (write-char #\> stream)))

;;; process-next-event copies event data into an event structure.  after the
;;; event is processed, its put back into *event-cache* to be re-used on the
;;; next event.  this is done to reduce consing.  care is taken to shield the
;;; application progammer from the actual event structure to prevent saving the
;;; event structure in application data structures.  if an application did this,
;;; the event would be destructively modified on subsequent events.
(defvar *event-cache* nil)

(defun allocate-event ()
  ;; get an event structure, initializing all slots to nil
  (let ((event (or (pop *event-cache*)
		   (make-instance 'event))))
    (with-slots (key xlib:display contact character keysym plist code state time event-window
		     root drawable window child parent root-x root-y x y
		     width height border-width override-redirect-p same-screen-p
		     configure-p hint-p kind mode keymap focus-p count major minor
		     above-sibling place atom selection requestor target property
		     colormap new-p installed-p format type data name send-event-p
		     )
	event
      (setf key	nil
	    xlib:display nil				; display event was reported to
	    contact nil				; contact the event is directed to
	    character nil			; character from code and state
	    keysym nil				; keysym from code and state
	    ;; the following are from the clx event
	    code nil
	    state nil
	    time nil
	    event-window nil
	    root nil
	    drawable nil
	    window nil
	    child nil
	    parent nil
	    root-x nil
	    root-y nil
	    x nil
	    y nil
	    width nil
	    height nil
	    border-width nil
	    override-redirect-p nil
	    same-screen-p nil
	    configure-p nil
	    hint-p nil
	    kind nil
	    mode nil
	    keymap nil
	    focus-p nil
	    count nil
	    major nil
	    minor nil
	    above-sibling nil
	    place nil
	    atom nil
	    selection nil
	    requestor nil
	    target nil
	    property nil
	    colormap nil
	    new-p nil
	    installed-p nil
	    format nil
	    type nil
	    data nil
	    send-event-p nil
	    name nil
	    xlib::plist nil
	    ))
    event))

(defun deallocate-event (event)
  ;; return an event to the cache, where it can be re-used.
  (push event *event-cache*))

;;-----------------------------------------------------------------------------
;; modes

;;; applications may find it necessary to establish a special input
;;; "mode" in which the user is temporarily required to direct input
;;; to one or more specific contacts. in such a mode, user input
;;; events directed to other contacts are not handled normally, but
;;; instead are either ignored or acknowledged with some kind of
;;; warning.

(deftype mode-type () '(member :non-exclusive :exclusive :spring-loaded))

(defparameter *remap-events* '(:key-press :key-release :button-press :button-release)
  "these events are sent to the most recent :spring-loaded contact on the mode-stack.")

(defparameter *restrict-events*
	      '(:motion-notify :enter-notify :leave-notify)
  "these 'user' events are sent to the restrict-action of the first
   :exclusive contact on the mode-stack")

(defparameter *sensitive-events*
	      '(:key-press :key-release :button-press :button-release
		:motion-notify :enter-notify :leave-notify
		:focus-in)
  "these 'user' events are ignored by an insensitive contact.")

;; other events (not in *remap-events* or *restrict-events*) are handled normally

;;; when dispatching *restrict-events*, if the mode-stack is non-nil,
;;; the event is restricted as follows.  for each entry of the
;;; mode-stack, if the event is for the contact on the stack, or one of
;;; its descendents, it is dispatched.  when a stack-entry with
;;; :exclusive or :spring-loaded mode-type is encountered, the search
;;; stops, and the event is sent to the restrict-action action of the
;;; mode contact with args.  if there are no :exclusive or
;;; :spring-loaded contacts on the stack, the event is dispatched
;;; normally.
;;;
;;; when dispatching *remap-events*, if the mode-stack is non-nil, the
;;; event is sent (re-mapped) to the first :spring-loaded contact on the
;;; mode-stack.  if there is no :spring-loaded contact, *remap-events*
;;; are handled like *restrict-events*

(defun add-mode (contact &optional (mode-type :non-exclusive) (action 'restrict) &rest args)
  "push contact with (mode-type action . args) onto the mode-stack"
  (declare (type contact contact)
	   (type mode-type mode-type)
	   (type symbol action)
	   (type list args))
  (when (and (not (eq mode-type :non-exclusive))
	     (not (sensitive-p contact)))
    (error "add-mode on insensitive contact ~s" contact))
  (push (list* contact mode-type action (copy-list args))
	(display-mode-stack (contact-display contact))))

(defun delete-mode (contact)
  "pop contact (and everything above contact) off the mode-stack
   returns t when found and removed, else nil"
  (declare (type contact contact)
	   (values boolean))
  (let* ((display (contact-display contact))
	 (mode-stack (display-mode-stack display)))
    (when mode-stack
      (do ((stack mode-stack (cdr stack)))
	  ((endp stack)
	   ;; if contact not found, check its children
	   ;; this feature utilized when un-mapping the parent of a modal contact
	   (do ((stack mode-stack (cdr stack))
		(found-p nil)
		(result nil))
	       ((endp stack)
		(when found-p
		  (setf (display-mode-stack display) result)
		  t))
	     (when (ancestor-p contact (caar stack))
	       (setq found-p t
		     result stack))))
	(when (eq contact (caar stack))
	  (setf (display-mode-stack display) (cdr stack))
	  (return t))))))

(defmacro with-mode ((contact &key (mode-type :exclusive)
			      (action 'ignore-action) args)
		     body-form &body cleanup-forms)
  "while executing body-form, user events will only be delivered to contact
and its children.  non-user events (e.g.  exposure,property-notify, etc)
will be delivered normally.  user events to other contacts will cause
the action action for contact's class to be invoked with args.  the
primary contact method for the default action, ignore-action, beeps on
*remap-events*, and ignores all others.

with-mode executes body-form within an unwind-protect.  with-mode
returns the value of its body-form, and executes cleanup-forms before
exiting. "
  (let ((local-contact (gensym)))
    `(let ((,local-contact ,contact))
       (unwind-protect
	   (progn
	     (add-mode ,local-contact ,mode-type (function ,action) ,@args)
	     ,body-form)
	 (delete-mode ,local-contact)
	 ,@cleanup-forms))))

(defun contact-mode (contact)
  "if contact is the descendent of a modal contact, return the modal contact, else nil."
  (let ((modes (display-mode-stack (contact-display contact))))
    (if modes ;; no mode stack means everything is in "on the stack"
	(do ((p contact (contact-parent p)))
	    ((null p) nil)
	  (dolist (mode modes)
	    (cond ((eq p (car mode))
		   (return-from contact-mode p))
		  ((eq (cadr mode) :exclusive)
		   (return nil))))))))

(defun contact-super-mode (contact)
  "if contact is the descendent of a modal contact, return the superior modal contact, else nil."
  (let ((modes (cluei::display-mode-stack (contact-display contact))))
    (if modes ;; no mode stack means everything is in "on the stack"
	(do ((p contact (contact-parent p)))
	    ((null p) nil)
	  (do ((mode modes (cdr mode))
	       (supermode nil))
	      ((endp mode))
	    (when (eq p (caar mode))
	      (return-from contact-super-mode supermode))
	    (unless (eq (cadar mode) :non-exclusive)
	      (setq supermode (caar mode))))))))


(defmacro with-event-mode ((contact &rest translations) &body body)
  "The given event translations are defined for the contact only
   within the dynamic extent of the body. the translations are
   processed before any other previously-defined instance or class
   translations for contact."
  (let ((previous-translations (gensym))
	(new-translations (gensym))
	(translation      (gensym))
	(previous         (gensym))
	(slot             (gensym)))
  `(let* ((,new-translations (list ,@translations))
	  (,slot             (slot-value ,contact 'event-translations))
	  (,previous-translations
	   ;; save any actions from previous instance translations for these event specs
	   (let (,previous-translations)
	     (dolist (,translation ,new-translations (nreverse ,previous-translations))
	       (when (assoc (first (parse-event-translation (first ,translation) (rest ,translation)))
			    ,slot
			    :test #'equal)
		 (push ,translation ,previous-translations))))))
     (unwind-protect
	 (progn
	   ;; add modal translations
	   (dolist (,translation ,new-translations)
	     (apply #'add-event ,contact ,translation))
	   ,@body)
       ;; delete modal translations and restore any previous ones
       (dolist (,translation ,new-translations)
	 (let ((,previous (pop ,previous-translations)))
	   (if ,previous
	       (apply #'add-event ,contact ,previous)
	       (delete-event ,contact (first ,translation)))))))))


;;; :ctions

;; Retained temporarily for compatibility purposes
(defmacro defaction (name lambda-list &body body)
  "define an action method. this macro is now obsolete. just use defmethod."
  (let (qualifier self)
    ;; handle method qualifiers (:before or :after)
    (when (atom lambda-list)
      (setq qualifier   (list lambda-list)
	    lambda-list (pop body)))
    ;; get the first specialized parameter in the lambda-list
    (dolist (arg lambda-list)
      (when (member arg lambda-list-keywords) (return nil))
      (when (consp arg)
	(setf self (first arg))))
    `(progn
       (compiler-let (($contact$ ',self))	; hook for call-action
	 (defmethod ,name ,@qualifier ,lambda-list
	   ,@body)))))

(defmacro using-event (&body body)
  `(locally
     (declare (special $event$))
     ,@body))

(defmacro processing-event-p ()
  `(using-event (boundp '$event$)))

(defmacro with-event (slots &body body)
  "used within an action method to access event slots."
  `(using-event
     (assert
       (boundp '$event$) nil
       "with-event used outside the dynamic extent of process-next-event.")
     (with-slots ,slots $event$ ,@body)))

(defmacro call-action (action &rest args)
  "used within defaction to call another action. this macro is now obsolete. replace
with a direct reference to the action function."
  (declare (special $contact$))
  (unless (boundp '$contact$)
    (error "call-action used outside defaction."))
  `(,action ,$contact$ ,@args))

(defun call-action-internal (contact action)
  (if (consp action)
      (apply (car action) contact (cdr action))
      (funcall action contact)))


(defun add-before-action (display class action-name &rest args)
    "call the action named action-name with arguments before every event
   on display directed to a contact whose class is the same as
   or superclass of the action class."
    (setf (before-actions display)
	  (cons
	    (list* class action-name (copy-list args))
	    (delete-if #'(lambda (entry)
			   (and (eq class (first entry))
				(eq action-name (second entry))))
		       (before-actions display)
		       :count 1)))
    action-name)

(defun delete-before-action (display class action-name)
  "remove a before event-handler from display"
  (setf (before-actions display)
	(delete-if #'(lambda (entry)
		       (and (eq class (first entry))
			    (eq action-name (second entry))))
		   (before-actions display)
		   :count 1))
  action-name)

;;; built-in actions
(defmethod perform-callback ((contact basic-contact) name &rest args)
  ;; warning: duplicates apply-callback code, instead of (eval
  ;; (apply-callback...))
  (let ((functions (callback-p contact name)))
    (when functions
      ;; cons alert!!
      (let ((args (copy-list args)))
	(catch
	    ;; abort-callback
	    (do* ((functions functions         (rest functions))
		  (function  (first functions) (first functions)))
		 ((null (rest functions))
		  ;; return value(s) of last callback function
		  (apply (first function) (nconc args (rest function))))
	      (setf args (nconc args (rest function)))
	      (apply (first function) args)
	      (setf args (nbutlast args (length (rest function))))))))))

(defmethod apply-action ((contact basic-contact) function &rest args)
  (let ((*contact* contact))
    (declare (special *contact*))
    (apply function args)))

(defmethod eval-action ((contact basic-contact)  &rest forms)
  (let ((*contact* contact))
    (declare (special *contact*))
    (dolist (form forms)
      (eval form))))

(defmethod trace-action ((event-contact basic-contact)  &rest exceptions)
  (let (value result
	(name (contact-name event-contact)))
    (with-event ((event-key key))
      (unless (member event-key exceptions :test #'eq)
	(format *trace-output* "~%~s on ~a:"
		event-key name)
	(dolist (slot-name '(above-sibling atom border-width character child code colormap configure-p
			     count drawable event-window focus-p format height hint-p installed-p keymap
			     keysym kind major minor mode name new-p override-redirect-p parent place
			     plist property requestor selection send-event-p state target type width
			     window x y))
	  (when (and (setf value (slot-value $event$ slot-name))
		     (not (eq value event-contact)))
	    (when (typep value 'contact) (setf value (contact-name value)))
	    (setf result (nconc result (list slot-name value)))))
	(format *trace-output* "~{~<~%~20@t~1:; ~s ~s~>~^ ~}." result)))))

(defmethod describe-action ((event-contact basic-contact) &rest exceptions)
  (with-event ((event-key key))
    (unless (member event-key exceptions :test #'eq)
      (format *trace-output* "~%~s on ~a:"
	      event-key (contact-name event-contact))
      ;; loop over slots in alphabetical order
      (dolist (slot-name '(above-sibling atom border-width character child code colormap configure-p
			   count drawable event-window focus-p format height hint-p installed-p keymap keysym
			   kind major minor mode name new-p override-redirect-p parent place plist
			   property requestor selection send-event-p state target type width window x y))
	(let ((value (slot-value $event$ slot-name)))
	  (when value
	    (when (typep value 'contact) (setf value (contact-name value)))
	    (format *trace-output* "~%~5t~20s~20s" slot-name value))))
      (terpri *trace-output*))))

(defmethod ignore-action ((contact basic-contact))
  ;; beep on *remap-events* else ignore
  (with-event (key display)
    (when (member key *remap-events* :test #'eq)
      (bell display))))

(defmethod throw-action ((contact basic-contact) tag &optional value)
  (throw tag value))

;; Event Translations

(defmacro defevent (class event-spec &rest actions)
  "Add an event binding to the event-translations property of class,
   where it can be shared by all instances of class."
  (let ((event-parse (parse-event-translation event-spec actions))
	(canonical-event-spec (gensym)))
    `(progn
       ;; generate compiler warnings for missing actions
       ;; Old TI code
       ;; ,@(mapcar #'(lambda (action)
       ;; 		  (when (consp action) (setq action (first action)))
       ;; 		  `(eval-when (compile)
       ;; 		     (compiler:function-referenced
       ;; 		      ',action ',(intern (format nil "defevent ~s ~s" class event-spec)))))
       ;; 	      (rest event-parse))
       (let ((,canonical-event-spec ',(first event-parse)))
	 (setf
	  ;; Update class event translations
	  (class-name-event-translations ',class)
	  (cons
	   (cons ,canonical-event-spec ',(rest event-parse))
	   (delete ,canonical-event-spec
		   (class-name-event-translations ',class)
		   :key #'first :test #'equal :count 1))
	  ;; Flush cached class event mask, event precedence list
	  (class-name-event-mask ',class)
	  nil
	  (class-name-event-precedence-list ',class)
	  nil)))))

(defmacro undefevent (class event-spec &rest actions)
  "remove an event binding from the event-translations property of class."
  (declare (ignore actions))
  `(setf
       ;; update class event translations
       (class-name-event-translations ',class)
       (delete ',(first (parse-event-translation event-spec nil))
	       (class-name-event-translations ',class)
	       :key #'first :count 1 :test #'equal)
       ;; flush cached class event mask, event precedence list
       (class-name-event-mask ',class)
       nil
       (class-name-event-precedence-list ',class)
       nil))

(defmethod event-actions ((contact basic-contact) event-spec)
  "return the list of actions for event-spec."
  ;; check instance translations
  (let ((event-binding (car (parse-event-translation event-spec nil))))
    (cdr
      (or
	;; instance translation?
	(assoc event-binding
	       (slot-value contact 'event-translations)
	       :test #'equal)
	;; class translation?
	(dolist (class (class-name-event-precedence-list (class-name-of contact)))
	  (let ((actions (assoc event-binding
				(class-name-event-translations class)
				:test #'equal)))
	    (when actions
	      (return actions))))))))

(defmethod add-event ((contact basic-contact) event-spec &rest actions)
  "add event-spec and actions to the event translations for contact."

  ;; compute canonical event translation.
  (let ((translation  (parse-event-translation event-spec (copy-list actions))))
    (with-slots (event-mask event-translations) contact

      ;; translation for this event spec already exists?
      (let ((previous (assoc (first translation) event-translations :test #'equal)))
	(if previous
	    ;; yes, modify it with the new actions.
	    (setf (rest previous) (rest translation))

	    ;; no, add new translation.
	    (push translation event-translations)))

      ;; update window event mask, if necessary
      (when (realized-p contact)
	(let ((new-mask (event-translation-mask event-mask translation)))
	  (unless (= new-mask event-mask)
	    (setf (window-event-mask contact) (setf event-mask new-mask)))))))
  (values))

(defmethod delete-event ((contact basic-contact) event-spec)
  "remove any translation for event-spec from the event translations for contact."

    ;; compute a canonical event translation for the event spec
  (let ((translation (parse-event-translation event-spec nil)))
    (with-slots (event-mask event-translations) contact

      ;; remove any matching translation.
      (setf event-translations
	    (delete (first translation) event-translations
		    :key #'first :count 1 :test #'equal))

      ;; update window event mask, if necessary
      (when (realized-p contact)
	(let ((old-bit  (event-translation-mask 0 translation)))

	  ;; don't change event mask if some other translation sets this bit
	  (when (zerop (logand (contact-event-translations-mask contact) old-bit))

	    ;; only modify the event-mask bit for the event being deleted
	    (setf (window-event-mask contact)
		  (setf event-mask (logandc2 event-mask old-bit))))))))
  (values))





;;;-----------------------------------------------------------------------------
;;; check/match functions


(defun encode-button-number (button)
  (or (position button
		#(:any :button-1 :button-2 :button-3 :button-4 :button-5))
      (xlib::x-type-error
	button 'button
	"one of :any :button-1 :button-2 :button-3 :button-4 :button-5")))

;; alist associating modifier keys with modifier keysyms
(defvar *meta-modifier-alist*
  `((:meta  ,(xlib:keysym :left-meta)
	    ,(xlib:keysym :right-meta))
    (:super ,(xlib:keysym :left-super)
	    ,(xlib:keysym :right-super))
    (:hyper ,(xlib:keysym :left-hyper)
	    ,(xlib:keysym :right-hyper))))

(defconstant meta-shift 16.) ;; where to shift meta-modifier keystates
(defconstant mod-1-shift (position :mod-1 xlib::+state-mask-vector+))
(defconstant button-0-shift (1- (position :button-1 xlib::+state-mask-vector+)))

(defun get-display-modifier-translate (display &optional update-p)
  ;; returns a table that translates meta-modifier bits
  ;; into mod1/mod2/mod3/mod4/mod5 modifier state bits.
  (declare (type display display))
  (or (and (not update-p) (display-modifier-translate display))
      (let* ((mapping (xlib::get-display-modifier-mapping display))
	     (mod-length (length *meta-modifier-alist*))
	     (translate-length (ash 1 mod-length))
	     (display-translate (display-modifier-translate display))
	     (translate (or (and (>= (length display-translate) translate-length)
				 display-translate)
			    (make-array translate-length)))
	     (mod-vector (make-array mod-length)))
	(declare (type simple-vector translate mod-vector))
	(do* ((modifiers *meta-modifier-alist* (cdr modifiers))
	      (i 0 (1+ i))
	      (temp))
	     ((endp modifiers))
	  (setf (aref mod-vector i)
		(dolist (modifier (cdar modifiers) 0)
		  (when (setq temp (assoc modifier mapping :test #'eq))
		    (return (cdr temp))))))
	(dotimes (i translate-length)
	  (let ((mask 0))
	    (dotimes (j mod-length)
	      (when (logbitp j i)
		(setq mask (logior mask (aref mod-vector j)))))
	    (setf (aref translate i) mask)))
	(setf (display-modifier-translate display) translate))))

(defun translate-meta-modifiers (state translate)
  ;; translate the meta/super/hyper modifiers in state to mod-1/mod-2/mod3/mod4/mod5 modifiers.
  ;; translate is the result from get-display-modifier-translate
  (logior (ldb (byte meta-shift 0) state)
	  (aref translate (ash state (- meta-shift)))))

(defun encode-clue-modifier-mask (modifiers)
  ;; make a state-mask from modifiers
  (declare (type (or mask16 state-mask-key (member :meta :super :hyper) list) modifiers))
  (typecase modifiers
    (fixnum (ldb (byte meta-shift 0) modifiers))
    (cons (let ((mask 0))
	    (dolist (modifier modifiers)
	      (setf mask (logior mask (encode-clue-modifier-mask modifier))))
	    mask))
    (otherwise
     (let ((temp (position modifiers *meta-modifier-alist* :key #'car :test #'eq)))
       (if temp
	   (ash 1 (+ temp meta-shift))
	 (xlib:make-state-mask modifiers))))))

(defun event-spec-match (display state select event-state)
  (let ((translate (get-display-modifier-translate display)))
    (setq state (translate-meta-modifiers state translate)
	  select (translate-meta-modifiers select translate)))
						; the modifiers common to select and state must be down and
						; the modifiers in select but not state must be up
  (and (= (logand state select) (logand event-state select))
						; when there are modifiers in state that aren't in select
       (or (zerop (logandc2 state select))
						; at least one of them must be down
	   (plusp (logand event-state (logandc2 state select)))))
						; modifiers that aren't in state or select are ignored
  )

#| ;; event-spec-match implements the following relationships:

  .-------------------------------.
  | event-state  4 4 4 4 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  |4 4 .-------------------. 4 4 4|
  |4 4 |  select 1 1 1 1 1 | 4 4 4|
  |4 4 | 1 1 1 1 1 1 1 1 1 | 4 4 4|
  |4 4 | 1 .-----------. 1 | 4 4 4| this would look better in color
  |4 4 | 1 | 2 2 2 2 2 | 1 | 4 4 4|
  |4 4 | 1 | 2 2 2 2 2 | 1 | 4 4 4|
  |4 4 | 1 | 2 2 2 2 2 | 1 | 4 4 4|
  |4 4 `---+-----------+---' 4 4 4|
  |4 4 4 4 | 3 3 3 3 3 | 4 4 4 4 4|
  |4 4 4 4 | 3 3 3 3 3 | 4 4 4 4 4|
  |4 4 4 4 | state 3 3 | 4 4 4 4 4|
  |4 4 4 4 `-----------' 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  |4 4 4 4 4 4 4 4 4 4 4 4 4 4 4 4|
  `-------------------------------'

 1. modifiers in select but not state		; must be up in the event
 2. modifiers both in state and select		; must be down in the event
 3. modifiers in state but not select		; if any, at least one must be down
 4. modifiers in neither select or state	; are ignored

|#

(defun key-check (event-key &optional char state select)
  ;; returns the canonical form of the key (i.e. one that may speed up
  ;; the matching operation)
  (declare (ignore event-key))
  (unless (typep char '(or xlib:card16 character (member :any)))
    (error "~a is not a character, xlib:card16 or :any." char))

  (let*
    ((modifiers
       (cond ((or (null state) (eq state :none))   0)
	     ((numberp state)                      state)
	     ((eq state :any)                      (setf select 0))
	     (t                                    (encode-clue-modifier-mask state))))
     (mask
       (cond ((null select)                        (if (characterp char) 0 modifiers))
	     ((numberp select)                     select)
	     ((eq select :same)                    modifiers)
	     ((eq select :all)                     #xffff)
	     (t                                    (encode-clue-modifier-mask select)))))

    (list 'key-match char modifiers mask)))

(defun key-match (event spec-code spec-state spec-select)
  ;; returns t if event-spec matches event
  (with-slots (xlib:display character state keysym) event
    (and (event-spec-match xlib:display spec-state spec-select state)
	 (cond
	   ((characterp spec-code) (eql spec-code character))
	   ((eq spec-code :any)    t)
	   (t                      (eql spec-code keysym))))))

(defun button-check (event-key &optional code state select)
  (let*
    ((click-option
       (when (consp state)
	 ;; process state list for click type keys
	 (let* ((single (find :single-click state :test #'eq))
		(double (find :double-click state :test #'eq))
		(click  (or single double)))
	   (when click
	     (assert (not (and single double)) ()
		     "can't specify both :single-click and :double-click.")
	     (setf state (remove click state)))
	   click)))

     (button
       (encode-button-number (or code :any)))

     (modifiers
       (cond ((or (null state) (eq state :none))   0)
	     ((numberp state)                      state)
	     ((eq state :any)                      (setf select 0))
	     ((eq state :single-click)             (setf click-option state) 0)
	     ((eq state :double-click)             (setf click-option state) 0)
	     (t                                    (encode-clue-modifier-mask state))))
     (mask
       (cond ((or (null select) (eq select :same)) modifiers)
	     ((numberp select)                     select)
	     ((eq select :all)                     #xffff)
	     (t                                    (encode-clue-modifier-mask select))))

     (predicate
       (if (eq event-key :button-press)
	   'button-press-match
	   'button-release-match)))

    (list* predicate button modifiers mask (when click-option (list click-option)))))

(defun button-press-mask (button state select &optional option)
  (declare (ignore button state select))
  (if option
      #.(xlib:make-event-mask :button-press :button-release)
      #.(xlib:make-event-mask :button-press)))

(defun button-release-mask (button state select &optional option)
  (declare (ignore button state select))
  (if option
      #.(xlib:make-event-mask :button-press :button-release)
      #.(xlib:make-event-mask :button-release)))

(defun button-press-match (event button state select &optional option)
  (with-slots ((event-code code)
	       (event-state state)
	       display key plist time x y)
      event
    (let* ((code event-code)
	   (mask (ash 1 (+ code button-0-shift))))
      (and
	(or (zerop button)			; zero button means "any" button
	    (=  button code))
	(event-spec-match display state select
			  (logandc2 event-state mask))	; clear state bit for current button
	(case option
	  (:single-click
	   (= (click-lookahead display 1 2 time (logior event-state mask) x y) 2))
	  (:double-click
	   (= (click-lookahead display 1 4 time (logior event-state mask) x y) 4))
	  (otherwise t))))))

(defun button-release-match (event button state select &optional option)
  (with-slots ((event-code code)
	       (event-state state)
	       key display plist time x y)
      event
    (let* ((code event-code)
	   (mask (ash 1 (+ code button-0-shift))))
      (and
	(or (zerop button)			; zero button means "any" button
	    (=  button code))
	(event-spec-match display state select (logandc2 event-state mask))	; clear state bit for current button
	(case option
	  (:single-click
	   (= (click-lookahead display 2 2 time event-state x y) 2))
	  (:double-click
	   (= (click-lookahead display 2 4 time event-state x y) 4))
	  (otherwise t))))))

(defconstant all-button-mask
	     (xlib:make-state-mask :button-1 :button-2 :button-3 :button-4 :button-5))

(defun motion-check (event-key &optional state select)
  (declare (ignore event-key))
  (let*
    ((modifiers
       (cond ((or (null state) (eq state :none))   0)
	     ((numberp state)                      state)
	     ((eq state :any)                      (setf select 0) all-button-mask)
	     (t                                    (encode-clue-modifier-mask state))))
     (mask
       (cond ((or (null select) (eq select :same)) modifiers)
	     ((numberp select)                     select)
	     ((eq select :all)                     #xffff)
	     (t                                    (encode-clue-modifier-mask select)))))

    (list 'motion-match modifiers mask)))

(defun motion-match (event state select)
  (with-slots (xlib:display) event
    (or (eq state :any)
	(event-spec-match xlib:display state select (slot-value event 'state)))))

(defun motion-event-mask (state select)
  (if (= all-button-mask (logand (logior state select) all-button-mask))
      #.(xlib:make-event-mask :button-motion)
      (let ((mask (logand state select all-button-mask)))
	(when (zerop mask)
	  (setq mask #.(xlib:make-event-mask :pointer-motion)))
	mask)))

;;(eval-when (compile)
;; motion-event-mask makes the following assumption:
(eval-when (:compile-toplevel)
  (assert (and (= (xlib:make-event-mask :button-1-motion) (xlib:make-state-mask :button-1))
	       (= (xlib:make-event-mask :button-2-motion) (xlib:make-state-mask :button-2))
	       (= (xlib:make-event-mask :button-3-motion) (xlib:make-state-mask :button-3))
	       (= (xlib:make-event-mask :button-4-motion) (xlib:make-state-mask :button-4))
	       (= (xlib:make-event-mask :button-5-motion) (xlib:make-state-mask :button-5)))
	  () "button event-mask is shifted relative to button state-mask"))

(defun enter-leave-check (event-key &rest kinds)
  (dolist (kind kinds)
    (unless (member kind '(:ancestor :virtual :inferior :nonlinear :nonlinear-virtual))
      (error "~s isn't an enter/leave kind for ~s" kind (cons event-key kinds))))
  (list 'enter-leave-match kinds))

(defun enter-leave-match (event kinds)
  (member (slot-value event 'kind) kinds :test #'eq))

(setf (check-function :key-press) 'key-check)
(setf (check-function :key-release) 'key-check)
(setf (check-function :button-press) 'button-check)
(setf (check-function :button-release) 'button-check)
(setf (check-function :motion-notify) 'motion-check)
(setf (check-function :enter-notify) 'enter-leave-check)
(setf (check-function :leave-notify) 'enter-leave-check)

(defun key-up-check (event-key &rest parms)
  (declare (ignore event-key))
  ;; convert (:up ...) to (:key-press ...)
  (values (apply #'key-check :key-release parms)
	   :key-release))

(setf (check-function :up) #'key-up-check)

(defun client-message-check (event-key type &rest accessors)
  (declare (ignore event-key))
  (assert (typep type 'xlib:xatom) () "~s must be an x atom." type)
  (do* ((accessors accessors          (cddr accessors))
	(function  (first accessors)  (first accessors))
	(rest      (rest accessors)   (rest accessors)))
       ((null accessors))
    (assert rest () "no value given for ~s accessor." function))
  (values (list* 'client-message-match
		 (intern (string type) 'keyword)
		 accessors)
	  :client-message))

(defun client-message-match (event type &rest accessors)
  (with-slots ((event-type type) (event-data data) (event-display contact-display)) event
    ;; bind display for use in accessor functions
    (let ((*event-display* event-display))
      (declare (special *event-display*))
      (and (eq type event-type)
	   (do* ((accessors accessors          (cddr accessors))
		 (function  (first accessors)  (first accessors))
		 (value     (second accessors) (second accessors)))
		((null accessors) t)
	     (unless (equal value (funcall function event-data))
	       (return nil)))))))

(setf (check-function :client-message) #'client-message-check)


(defun wm-protocol-check (event-key &rest accessors)
  (apply 'client-message-check
	 :client-message :wm_protocols
	 'wm-message-protocol-atom event-key
	 accessors))

(setf (check-function :wm_take_focus)    #'wm-protocol-check)
(setf (check-function :wm_save_yourself) #'wm-protocol-check)
(setf (check-function :wm_delete_window) #'wm-protocol-check)

(defun timer-check (event-key timer-name)
  (declare (ignore event-key))
  (assert (symbolp timer-name) ()
	  "~a is not a timer name symbol." timer-name)
  (values
    (list 'timer-match timer-name)
    :timer))

(defun timer-match (event timer-name)
  (with-slots (name) event
    (eq timer-name name)))

(setf (check-function :timer) #'timer-check)

(defun property-check (event &optional property state)
  (declare (ignore event))
  (check-type property (or null xlib:xatom) "an xatom")
  (check-type state    (or null (member :new-value :deleted)) ":new-value or :deleted")
  (cons
    'property-match
    (when property
      (cons (intern (string property) :keyword)
	    (when state
	      (cons state nil))))))

(defun property-match (event &optional property state)
  (with-slots ((event-property atom) (event-state state)) event
    (or (not property)
	(and (eq property event-property)
	     (or (not state) (eq state event-state))))))

(setf (check-function :property-notify) 'property-check)


;;; double-click events

(defun click-lookahead (display count max first-time state first-x first-y)
  ;; even when the button is up, odd when down
  (let* ((multipress-verify-p (display-multipress-verify-p display))
	 (multipress-delay-limit (display-multipress-delay-limit display))
	 (timeout (/  multipress-delay-limit 1000.0))
	 (distance-limit 5))
    (flet ((get-result (count timeoutp)
	     ;; if the result from get-result is nil, all lookahead events
	     ;; remain on the event queue, otherwise the events are removed,
	     ;; and the result from get-result is returned.
	     (if (or (evenp count)	; hold events only occur on timeout
		     timeoutp)
		 count
		 0)))
      (loop
	 (let*
	     ((timeout-p t)
	      (result
	       (block result
		 ;; when succeeding, we want to "eat" the events.
		 ;; when failing, we want to leave events on the event queue.
		 ;; we're careful to return non-nil from event-case only on success.
		 ;; on failure, we return-from result, which leaves events on the queue.
		 ;; the timeout-p hair is to detect the difference between failure and timeout.
		 (xlib:event-case (display :timeout timeout :force-output-p nil)
		   ;; fail when pointer moves more than a jiggle
		   ((motion-notify) (x y)
		    (setq timeout-p nil)
		    (when (> (+ (abs (- x first-x))
				(abs (- y first-y)))
			     distance-limit)
		      (return-from result (get-result count nil))))
		   ((enter-notify leave-notify) ()	; fail when pointer moves to a new window
		    (setq timeout-p nil)
		    (return-from result (get-result count nil)))
		   (button-press (time (state event-state) code)
				 (setq timeout-p nil)
				 (cond ((>= count max) (return-from result :count))
				       ((> time (+ first-time multipress-delay-limit))
					(return-from result :timeout))
				       ((or (oddp count)
					    (not (= state (logior event-state (ash 1 (+ code button-0-shift))))))
					(return-from result (get-result count nil)))
				       (t (let ((result (click-lookahead display (1+ count) max
									 time state first-x first-y)))
					    (if (plusp result)
						result
						(if (plusp (setq result (get-result count nil)))
						    (return-from result result)
						    nil ;; else fall-through returning nil
						    ))))))
		   (button-release (time (state event-state))
				   (setq timeout-p nil)
				   (cond ((>= count max) (return-from result :count))
					 ((> time (+ first-time multipress-delay-limit))
					  (return-from result :timeout))
					 ((or (evenp count)
					      (not (= state event-state)))
					  (return-from result (get-result count nil)))
					 (t (let ((result (click-lookahead display (1+ count) max
									   time state first-x first-y)))
					      (if (plusp result)
						  result
						  (if (plusp (setq result (get-result count nil)))
						      (return-from result result)
						      nil ;; else fall-through returning nil
						      ))))))))))
	   (if timeout-p
	       ;; event-case timed out
	       (if (or (zerop timeout)
		       (not multipress-verify-p))
		   (return (get-result count :local-timeout))
		   (progn
		     ;; verify timeout with a server round-trip and event-queue recheck
		     (display-finish-output display)
		     (setq timeout 0)))
	       ;; else exit loop with result
	       (return (case result
			 (:timeout  (get-result (1- count) :timeout))
			 (:count    0)
			 ((nil)     0)
			 (otherwise result)))))))))

;;; event-processing

(defun process-all-events (display &optional (update-state-p t))
  "repeatedly flush output and process resulting events until event queue is empty."
  (loop
    ;; flush output buffer and wait for resulting events
    (display-finish-output display)
    ;; any events left to process?
    (if (event-listen display 0)
	;; yes, process remaining event queue
	(loop
	  (unless (process-next-event display 0 update-state-p) (return)))
	;; no
	(return))))

(defun process-next-event (display &optional timeout (update-state-p t))
  "Process one event. Call update-state iff update-state-p is true. "
  ;; ensure consistent contact states
  (when update-state-p
    (update-state display))
  ;; Process any timers that have expired
  (let*
      ((interval-until-next-timer (execute-timers display))
       ;; Compute true timeout
       (wait-for-timer-p (when (or (null timeout)
				   (and interval-until-next-timer
					(< interval-until-next-timer timeout)))
			   interval-until-next-timer))
       (event (allocate-event))
       (result nil))
    (setf (slot-value event 'display) display)
    (macrolet ((set-event (&rest parameters)
		 `(progn ,@(mapcar #'(lambda (parm)
				       `(setf (slot-value event ',parm) ,parm))
				   parameters)))
	       (dispatch (contact)
		 `(progn
		    (dispatch-event event event-key send-event-p sequence ,contact)
		    t)))
      ;; Wait for an event, copy info into the event structure then
      ;; call dispatch-event
      (setf
       result
       (or
	(xlib:event-cond (display :timeout (or wait-for-timer-p timeout)
				  :force-output-p t
				  :discard-p t)
	  ((:key-press :key-release :button-press :button-release)
	   (code time root window child root-x root-y x y
		 state same-screen-p event-key sequence send-event-p) t
	   (format t "recieved event, :key-press :key-release :button-press :button-release")
	   (set-event code time root window child root-x root-y x y
		      state same-screen-p)
	   (dispatch window))
	  (:motion-notify
	   (hint-p time root window child root-x root-y x y
		   state same-screen-p event-key sequence send-event-p) t
		   (set-event hint-p time root window child root-x root-y x y
			      state same-screen-p)
		   (dispatch window))
	  ((:enter-notify :leave-notify)
	   (kind time root window child root-x root-y x y
		 state mode focus-p same-screen-p event-key sequence send-event-p) t
	   (set-event kind time root window child root-x root-y x y
		      state mode focus-p same-screen-p)
	   (dispatch window))
	  ((:focus-in :focus-out)
	   (kind window mode event-key sequence send-event-p) t
	   (set-event kind window mode)
	   (dispatch window))
	  (:exposure
	   (window x y width height count event-key sequence send-event-p) t
	   (set-event window x y width height count)
	   (dispatch window))
	  (:graphics-exposure
	   (drawable x y width height count major minor event-key sequence send-event-p) t
	   (set-event drawable x y width height count major minor)
	   (dispatch drawable))
	  (:no-exposure
	   (drawable major minor event-key sequence send-event-p) t
	   (set-event drawable major minor)
	   (dispatch drawable))
	  (:visibility-notify
	   (window state event-key sequence send-event-p) t
	   (set-event window state)
	   (dispatch window))
	  (:create-notify
	   (parent window x y width height border-width
		   override-redirect-p event-key sequence send-event-p) t
		   (set-event parent window x y width height border-width
			      override-redirect-p)
		   (dispatch parent))
	  (:destroy-notify
	   (event-window window event-key sequence send-event-p) t
	   (set-event event-window window)
	   (dispatch event-window))
	  (:unmap-notify
	   (event-window window configure-p event-key sequence send-event-p) t
	   (set-event event-window window configure-p)
	   (dispatch event-window))
	  (:map-notify
	   (event-window window override-redirect-p event-key sequence send-event-p) t
	   (set-event event-window window override-redirect-p)
	   (dispatch event-window))
	  (:map-request
	   (parent window event-key sequence send-event-p) t
	   (set-event parent window)
	   (dispatch parent))
	  (:reparent-notify
	   (event-window window parent x y override-redirect-p event-key sequence send-event-p) t
	   (set-event event-window window parent x y override-redirect-p)
	   (dispatch event-window))
	  (:configure-notify
	   (event-window window above-sibling x y width height border-width
			 override-redirect-p event-key sequence send-event-p) t
			 (set-event event-window window above-sibling x y width height
				    border-width override-redirect-p)
			 (dispatch event-window))
	  (:configure-request
	   (parent window above-sibling x y width height border-width event-key sequence send-event-p) t
	   (set-event parent window above-sibling x y width height border-width)
	   (dispatch parent))
	  (:gravity-notify
	   (event-window window x y event-key sequence send-event-p) t
	   (set-event event-window window x y)
	   (dispatch event-window))
	  (:resize-request
	   (window width height event-key sequence send-event-p) t
	   (set-event window width height)
	   (dispatch window))
	  (:circulate-notify
	   (event-window window parent place event-key sequence send-event-p) t
	   (set-event event-window window parent place)
	   (dispatch event-window))
	  (:circulate-request
	   (parent window place event-key sequence send-event-p) t
	   (set-event parent window place)
	   (dispatch parent))
	  (:property-notify
	   (window atom time state event-key sequence send-event-p) t
	   (set-event window atom time state)
	   (dispatch window))
	  (:selection-clear
	   (time window selection event-key sequence send-event-p) t
	   (set-event time window selection)
	   (dispatch window))
	  (:selection-request
	   (time window requestor selection target property event-key sequence send-event-p) t
	   (set-event time window requestor selection target property)
	   (dispatch window))
	  (:selection-notify
	   (time window selection target property event-key sequence send-event-p) t
	   (set-event time window selection target property)
	   (dispatch window))
	  (:colormap-notify
	   (window colormap new-p installed-p event-key sequence send-event-p) t
	   (set-event window colormap new-p installed-p)
	   (dispatch window))
	  (:client-message
	   (format window type data event-key sequence send-event-p) t
	   (set-event format window type data)
	   (dispatch window))
	  ;; special case
	  (:keymap-notify
	   (keymap event-key send-event-p) t
	   ;; keymap-notify doesn't have an associated window.
	   (set-event keymap)
	   (let ((sequence 0))
	     ;; send keymap-notify events to the root.
	     (dispatch (display-root display))))
	  ;; special case
	  (:mapping-notify
	   (request start count) t
	   (mapping-notify display request start count)
	   ;; update the modifier mapping translate table
	   (when (eq request :modifier)
	     (get-display-modifier-translate display :update))
	   t))
	;; no event read -- return true (i.e. no timeout) if we now have a timer ready
	(when wait-for-timer-p
	  t))))
    ;; we could add an unwind protect to ensure that the event is
    ;; always deallocated (process-next-event is sometimes thrown out
    ;; of).  however, we judge that an unwind-protect all the time is
    ;; more expensive than garbage collecting an event structure some
    ;; of the time.
    (deallocate-event event)
    result))

(defun dispatch-event (event event-key send-event-p sequence contact)
  ;; Called from process-next-event to filter events and call event
  ;; handlers.
  (format t "processing event: ~s~%"
	  (list event event-key send-event-p sequence contact))
  (with-slots ((event_key key)
	       (event-sequence sequence)
	       (event-send-event-p send-event-p)
	       (event-contact contact)) event
    (setf event_key event-key
	  event-send-event-p send-event-p
	  event-sequence sequence
	  event-contact contact))
  (let ((class (class-name-of contact)))
    ;; check for non-contact event drawables.
    (if (or (eq class 'window) (eq class 'pixmap))
	(handle-event (display-root (drawable-display contact)) event)
	(if (destroyed-p contact)
	    ;; destroyed-contact!
	    (when (eq event-key :destroy-notify)
	      (destroy-finish contact))
	    ;; bind event for reference within with-event forms
	    (let ((display (slot-value contact 'display))
		  ($event$ event))
	      (declare (special $event$))
	      ;; do key translation
	      (when (or (eq event-key :key-press)
			(eq event-key :key-release))
		(with-slots (keysym character code state) event
		  (let ((keysym-index (default-keysym-index display code state)))
		    (setf keysym (keycode->keysym display code keysym-index)
			  character (keycode->character display code state :keysym-index keysym-index)))))
	      ;; call the before event handlers
	      (dolist (before-action (before-actions display))
		(when (subtypep class (first before-action))
		  (call-action-internal contact (rest before-action))))
	      ;; handle insensitive contacts
	      (when (and (member event-key *sensitive-events* :test #'eq)
			 (not (sensitive-p contact)))
		(return-from dispatch-event nil))
	      ;; handle modes
	      (let ((modes (display-mode-stack display)))
		(when (and modes (not (contact-mode contact)))
		  (when
		    (or (member event-key *restrict-events* :test #'eq)
			(and (member event-key *remap-events* :test #'eq)
			     (dolist (mode modes t) ;; search for first :spring-loaded mode
			       (when (eq (second mode) :spring-loaded)
				 (format t "~%remapping ~s from ~s to ~s" event-key contact (first mode)) ;; *** debug ***
				 (setq contact (first mode)) ;; remap contact
				 (return nil)))))
		    ;; call mode action on for first :exclusive or :spring-loaded mode
		    (dolist (mode modes)
		      (unless (eq (second mode) :non-exclusive)
			(call-action-internal (first mode) (cddr mode))
			;; quit
			(return-from dispatch-event nil))))))
	      ;; handle event compression
	      (with-slots ((contact-compress-motion compress-motion)
			   (contact-compress-exposures compress-exposures))
		  contact
		(case event-key
		  (:exposure			; check for exposure compression
		   (when (and (eq contact-compress-exposures :on)
			      (plusp (slot-value event 'count)))
		     ;; accumulate total exposed area into one event
		     (let* ((exposed-min-x (slot-value event 'x))
			    (exposed-min-y (slot-value event 'y))
			    (exposed-max-x (+ exposed-min-x (slot-value event 'width)))
			    (exposed-max-y (+ exposed-min-y (slot-value event 'height)))
			    (compressed    0))
		       (xlib:event-case (display :force-output-p nil :discard-p t)
			 ;; assert: we can discard all events up to 0-count :exposure
			 ;; because the protocol says that no non-exposure events can intervene.
			 (:exposure (x y width height count)
				    (setf exposed-min-x (min x exposed-min-x)
					  exposed-min-y (min y exposed-min-y)
					  exposed-max-x (max (+ x width)  exposed-max-x)
					  exposed-max-y (max (+ y height) exposed-max-y))
				    (incf compressed)
				    (zerop count)))
		       (setf (slot-value event 'x)      exposed-min-x
			     (slot-value event 'y)      exposed-min-y
			     (slot-value event 'width)  (- exposed-max-x exposed-min-x)
			     (slot-value event 'height) (- exposed-max-y exposed-min-y)
			     (slot-value event 'count)  0)
		       ;; ensure all of exposed region reported has been cleared.
		       (when (> compressed 1)
			 (xlib:clear-area
			   contact
			   :x      (slot-value event 'x)
			   :y      (slot-value event 'y)
			   :width  (slot-value event 'width)
			   :height (slot-value event 'height))))))
		  (:motion-notify		; check for motion compression
		   (when (eq contact-compress-motion :on)
		     (let ((count 0))
		       ;; count consecutive :motion-notify's currently in queue
		       (xlib:event-case (display :force-output-p nil :peek-p t :timeout 0)
			 (:motion-notify (window)
					 (not (and (eq window contact) (incf count))))
			 (otherwise ()   t))
		       (when (plusp count)
			 ;; remove all but last and quit immediately
			 (do () ((zerop (decf count)))
			   (event-case (display :timeout 0)
			     (otherwise ()   t)))
			 (return-from dispatch-event nil)))))))
	      ;;
	      ;; handle event translations
	      ;;
	      (handle-event contact event))))))

(defmethod handle-event ((contact basic-contact) (event event))
  "do event/callback translation based on the event-translations slot."
  (format t "handle-event ((contact basic-contact) (event event))~%")
  ;; handle universal events
  (when (eq :exposure (slot-value event 'key))
    (with-slots (x y width height) event
      (display contact x y width height)))
;; the following "universal event" is obsolete -- use shells for top-level windows
;    (:configure-notify
;     ;; a contact's x/y/width/height/border-width get updated immediately when
;     ;; changing geometry.  top-level windows however, have their geometry
;     ;; arbitrated by the window-manager.  it probably doesn't make sense for
;     ;; a non-top-level contact to select structure-notify.  because clue allows
;     ;; any contact to be top-level, clue automatically selects structure-notify
;     ;; for top-level contracts, and we set the size/position here.
;     ;; if non-top-level contacts select structure-notify, we let them handle it
;     ;; themselves.
;     ;;
;     ;; this is inadequate.  the geometry manager for the
;     ;; root should be used instead, waiting for the configure-notify,
;     ;; and returning an appropriate successs-p parameter.
;     ;;
;     (with-slots (x y width height border-width window) event
;       (when (and (eq window contact) (top-level-p contact))
;	 (without-requests contact
;	   (move contact x y)
;	   (resize contact width height border-width)))))
  ;;
  ;; translate event and perform contact actions
  ;;
  (dolist (action (translate-event contact event))
    (call-action-internal contact action))
  t)

(defun translate-event (contact event)
  "returns the actions for the first event-translation matching event"
  (format t "Trying to translate event: contact(~s), event(~s)~%"
	  contact event)
  (labels ((find-translation (event event-key translations)
	     (dolist (event-binding translations)
	       (let ((event-spec (car event-binding)))
		 (when (if (atom event-spec)
			   ;; Simple eq test when event spec is an atom
			   (eq event-key event-spec)
			   ;; When event spec is a list, and the car
			   ;; of the list is eq to the event, and the
			   ;; matcher function returns t
			   (and (eq event-key (car event-spec))
				(apply (cadr event-spec) event (cddr event-spec))))
		   (return event-binding))))))
    (let ((key (slot-value event 'key)))
      (cdr
       (or
	;; Instance translations
	(find-translation event key (slot-value contact 'event-translations))
	;; class translations
	(dolist (class (class-name-event-precedence-list
			(class-name-of contact)))
	  (let ((translation
		 (find-translation
		  event
		  key
		  (class-name-event-translations class))))
	    (when translation
	      (return translation)))))))))

(defmethod translate-key ((contact contact) event)
  ;; Find a translation for :key-press event event which was
  ;; originally sent to contact.
  (let* ((parent (contact-parent contact))
	 (siblings (and parent (composite-children parent)))
	 actions)
    (or ;; check for handled by a sibling
      (dolist (sibling siblings)
	(unless (eq sibling contact)
	  (when (setq actions (translate-event sibling event))
	    (setf (slot-value event 'contact) sibling)
	    (dolist (action actions t)
	      (call-action-internal sibling action))
	    (return t))))
      ;; if not handled by a sibling of contact, check the parent
      (when (and parent
		 (setq actions (translate-key parent event)))
	(setf (slot-value event 'contact) parent)
	(dolist (action actions t)
	  (call-action-internal parent action)))
      ;; not handled by parent, recurse up to the parent
      (translate-key parent contact))))

;; Timers

(defstruct timer
  name
  time
  interval
  contact
  data)

(defun add-timer (contact name interval &optional data)
  "Send a :timer event to contact every interval seconds passing data
   the timer will be named name.  the event is passed data name
   contact and display"
  ;; timers are automatically removed when contact is destroyed
  (declare (type contact contact)
	   (type number interval) ;; in seconds
	   (values timer))
  (delete-timer contact name)
  (insert-timer (make-timer
		  :name name
		  :interval (* interval internal-time-units-per-second)
		  :contact contact
		  :data data))
  name)

;; internal function
(defun insert-timer (timer)
  ;; insert timer into its timer-queue
  (let* ((display  (contact-display (timer-contact timer)))
	 (queue    (timer-queue display))
	 (interval (timer-interval timer))
	 (time     (+ interval (get-internal-real-time))))
    (setf (timer-time timer) time)
    ;; insert in order of execution (youngest first)
    (if (or (null queue) (< time (timer-time (first queue))))
	(push timer (timer-queue display))
	(loop
	  (when (or (null (cdr queue))
		    (< time (timer-time (cadr queue))))
	    (return (setf (cdr queue) (cons timer (cdr queue)))))
	  (pop queue)))
    timer))

(defun delete-timer (contact &optional timer-name)
  "remove timer named timer-name from contact if timer-name is nil,
   remove all timers from contact.  returns nil when timer not found,
   else t."
  (let* ((display (contact-display contact))
	 (timer-queue (timer-queue display))
	 (deletedp nil))
    (dolist (timer timer-queue)
      (when (and (eq (timer-contact timer) contact)
		 (or (null timer-name)
		     (equal (timer-name timer) timer-name)))
	(setq deletedp t)
	(setf (timer-queue display)
	      (delete timer timer-queue :test #'eq :count 1))
	(when timer-name
	  (return t))))
    ;; return t when timers deleted
    deletedp))

(defun execute-timers (display)
  "execute all timers whose time has come, returning the time (in
   seconds) before the next timer executes for display"
  (loop
    (let ((next-timer (car (timer-queue display))))
      (unless next-timer
	;; no timers active
	(return nil))
      (let ((next-time  (timer-time next-timer)))
	(when (> next-time (get-internal-real-time))
	  ;; return time interval before next timer fires
	  (return
	    (/ (- next-time (get-internal-real-time))
	       #.(float internal-time-units-per-second)))))
      ;; Reinsert timer for next firing warning: if an abort happens
      ;; here, there's a short interval where a timer may be lost.
      (pop (timer-queue display))
      (insert-timer next-timer)
      ;; dispatch a :timer event
      (let ((event (allocate-event)))
	(with-slots ((event-display contact-display)
		     name data) event
	  (setf event-display display
		name (timer-name next-timer)
		data (timer-data next-timer)))
	(dispatch-event event :timer nil 0 (timer-contact next-timer))
	(deallocate-event event)))))

(defun describe-event-translations (contact &optional (stream *standard-output*))
  "print the event translations for contact. if contact is a contact
   class name, print the event translations for that contact class."
  (flet ((print-event (class event stream)
	   (format stream "~%from ~20a ~s" class (car event))
	   (dolist (action (cdr event))
	     (write-char #\space stream)
	     (prin1 action stream))))
    (let ((translations (when (typep contact 'basic-contact)
			  (slot-value contact 'event-translations))))
      ;; Print instance event translations for the contact
      (dolist (event translations)
	(print-event contact event stream))
      ;; Print event-translations for the contact's superclasses
      (dolist (class (class-name-event-precedence-list
		       (if (symbolp contact) contact (class-name-of contact))))
	(dolist (event (class-name-event-translations class))
	  (unless (assoc (car event) translations :test #'equal)
	    (print-event class event stream)
	    (push event translations)))))))
