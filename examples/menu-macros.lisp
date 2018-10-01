;;; -*- Mode:Lisp; Package:USER; Base:10; Lowercase:T; Syntax:Common-Lisp -*-

;;;                          TEXAS INSTRUMENTS INCORPORATED
;;;                                   P.O. BOX 149149
;;;                                AUSTIN, TEXAS 78714-9149
;;;
;;;                Copyright (C) 1989,1990 Texas Instruments Incorporated
;;;
;;; Permission is granted to any individual or institution to use, copy, modify, and
;;; distribute this software, provided that  this complete copyright and  permission
;;; notice is maintained, intact, in all copies and supporting documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without express or
;;; implied warranty.
;;;
;;; Source code for clue examples described in Explorer X Window
;;; System Programmer's Reference Manual.


(in-package :clue-example)


(defcontact menu (override-shell)
  ()
  (:resources
    (font :type xlib:font)
    (foreground :type xlib:pixel)
    (title :type string)
    (state :initform :withdrawn))
  (:documentation
    "Presents a column of menu items."))


(defcontact title-frame (composite)
  ((font
     :accessor title-font
     :initarg  :font
     ;; :initform (xlib::get-default-font)
     :initform "fixed"
     :type     xlib:font)
   (foreground
     :accessor title-foreground
     :initarg  :foreground
     :initform :black
     :type     xlib:pixel)
   (text
     :accessor title-text
     :initarg  :text
     :type     string)
   (compress-exposures
     :allocation :class
     :initform   :on
     :reader     contact-compress-exposures
     :type       (member :off :on)))
  (:resources
    xlib:font
    foreground
    text
    (event-mask :initform #.(xlib:make-event-mask :exposure)))
  (:documentation
    "A composite consisting of a text title and another contact."))


(defcontact column (composite) ()
  (:documentation
   "Arranges its children in a vertical column."))

(defcontact choices (column)
  ((selection
     :reader   choice-selection
     :initform nil
     :type     (or null contact)))
  (:documentation
    "A column of items to choose from."))

(defcontact button (contact)
  ((label
     :accessor   button-label
     :initarg    :label
     :initform   ""
     :type       string)
   (font
     :accessor   button-font
     :initarg    :font
     ;; :initform   (xlib::get-default-font)
     :initform   "fixed"
     :type       xlib:font)
   (foreground
     :accessor   button-foreground
     :initarg    :foreground
     :initform   :black
     :type       xlib:pixel)
   (compress-exposures
     :allocation :class
     :initform   :on
     :reader     contact-compress-exposures
     :type       (member :off :on)))
  (:resources
    (background :initform :white)
    (border     :initform :white)
    font
    foreground
    label)
  (:documentation
    "Triggers an action."))
