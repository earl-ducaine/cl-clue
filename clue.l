;;; -*- Mode:Lisp; Package:USER; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

;;;
;;;			 TEXAS INSTRUMENTS INCORPORATED
;;;				  P.O. BOX 149149
;;;			       AUSTIN, TEXAS 78714-9149
;;;
;;; Copyright (C)1988,1989,1990 Texas Instruments Incorporated.
;;;
;;; Permission is granted to any individual or institution to use, copy, modify,
;;; and distribute this software, provided that this complete copyright and
;;; permission notice is maintained, intact, in all copies and supporting
;;; documentation.
;;;
;;; Texas Instruments Incorporated provides this software "as is" without
;;; express or implied warranty.
;;;

(in-package "USER")
;;
;; Check required packages
;;
(unless (find-package "XLIB")
  #-explorer
  (error "CLX must be loaded before making CLUE.")
  #+explorer
  (make-system 'clx :noconfirm))

(assert
  (find-package "COMMON-LISP") ()
  "COMMON-LISP package does not exist. 

 Please create a package named COMMON-LISP which exports CLOS and CLCS.
 (You may need to make this a nickname of the LISP package.)
")

;;
;; Make packages
;;
;;
;; Internal interfaces...
(unless (find-package "CLUEI")
  (make-package "CLUEI" :use '(common-lisp xlib)))

;; External interfaces...
(unless (find-package "CLUE")
  (make-package "CLUE" :use '(common-lisp xlib cluei)))

;;
;; Ensure CLUE and CLUEI use a CLOS
;;
(cond ((find-symbol "DEFCLASS" 'cluei)) ;; clos-kludge must be around      

      ((find-package "CLOS")
       (use-package "CLOS" "CLUEI")
       (use-package "CLOS" "CLUE"))

      ((find-package "PCL")
       (use-package "PCL" "CLUEI")
       (use-package "PCL" "CLUE"))

      (t (error "CLOS must be loaded before making CLUE.")))
