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
    :depends-on (clx)
    :components
    ((:file "clue" :depends-on("event-parse"))
     (:file "package" :depends-on("event-parse"))
     ;;  (:module clx-patch   ("clx-patch" "window-doc"))
     (:file "defcontact" :depends-on("package"))
     (:file "events" :depends-on("intrinsics" "event-parse"))
     (:file "event-parse" :depends-on("defcontact"))
     (:file "intrinsics" :depends-on("event-parse"))
     (:module resource
	      :pathname ""
	      :depends-on("caches")
	      :components
	      (("resource"
		"gray"
		"cursor")))
     (:file "root-gmgmt" :depends-on("shells"))
     (:file "shells" :depends-on("intrinsics" "resource" "events"))
     (:file "stream" :depends-on("intrinsics" "resource" "events"))
     (:file "virtual" :depends-on("intrinsics" "resource" "events"))
     (:file "caches" :depends-on("intrinsicts"))
     (:file "obsolete" :depends-on("package"))))
