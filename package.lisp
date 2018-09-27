;;; -*- Mode:Lisp; Package:CLUEI; Syntax:COMMON-LISP; Base:10; Lowercase:T -*-

(rename-package :closer-mop :closer-mop (list :mop))

(defpackage :clue-internal
  (:use cl xlib)
  (:nicknames cluei)
  (:export defcontact))

(defpackage :clue
  (:use common-lisp xlib cluei))
