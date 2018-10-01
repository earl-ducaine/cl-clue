;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-


(in-package :cl-user)

;; site dependent
(progn
  ;; NOTE: All pathname strings must end in
  (defvar *clx-directory*  "/usr/X11/lib/CLX/*")
  (defvar *clos-kludge-directory*)
  (defvar *clue-directory* nil)
  (defvar *clue-examples-directory* nil)
  (defvar *clue-demo-directory* nil))

;; Ensure *features* knows about the Common Lisp Error Handler
(when (find-package 'conditions)
  (pushnew :cleh *features*))

(asdf:defsystem clue
  :depends-on (clx closer-mop trivial-gray-streams)
  :components
  ((:file "package")
   (:file "clx-compatability")
   (:file "defcontact" :depends-on(event-parse))
   (:file "event-parse" :depends-on(package clx-compatability))
   (:file "intrinsics" :depends-on(event-parse defcontact))
   (:file "intrinsics-methods" :depends-on("intrinsics"))
   (:file "events" :depends-on("intrinsics" "event-parse"))
   (:module resource
	    :pathname ""
	    :depends-on("caches")
	    :components
	    ((:file "resource")
	     (:file "gray")
	     (:file "cursor")))
   (:file "root-gmgmt" :depends-on("shells"))
   (:file "shells" :depends-on("intrinsics" "resource" "events"))
   (:file "stream" :depends-on("intrinsics" "resource" "events"))
   (:file "virtual" :depends-on("intrinsics" "resource" "events"))
   (:file "caches" :depends-on("intrinsics"))
   (:module examples
	    :depends-on("caches" "root-gmgmt" "virtual")
	    :components
	    ((:file "menu-macros")
	     (:file "menu")))))
