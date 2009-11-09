;; To compile, perform the following shell script (on this directory, with the
;; iRTM.lisp and the iRTM.nib files):
;;
;; ./:$ open MilkPack.lisp -a /Applications/ccl/Clozure\ CL64.app
;;
;; press Shift-Apple-E to evaluate the entire file. wait for the IDE to close.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Package initialization
(require :cocoa)
(load #p"/Users/eemg/ccl-init.lisp")

;; Uncomment the following two lines to enable SLIME debugging (attent the port):
;; (asdf:operate 'asdf:load-op 'swank)

(asdf:operate 'asdf:load-op 'rtm-lisp-api)
(asdf:operate 'asdf:load-op 'cl-store)

(defpackage #:irtm
 (:use :common-lisp #:rtm #:ccl))

(in-package #:irtm)

(defconstant  PROJECT-SOURCE-PATH "~/Documents/projects/rtm-lisp/")

(defvar  RTM-API-KEY-PATH "~/rtm-api.lisp"
  "Path to a lisp file that sets the api key and shared secret variables on the `RTM-LISP-API' package. The file must contain the following s-expressions (values are lisp strings):

(setf rtm:rtm-api-key \"xxxxxxxx-your-api-key-xxxxxxxxxx\")
(setf rtm:rtm-api-shared-secret \"xxx-your-shared-secret-xxx\")

")

(defun load-module (name)
  "Loads a lisp file on this folder. `NAME' is converted to lowercase."
  (load (format nil "~a~(~a~)" PROJECT-SOURCE-PATH name)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Utilities, both lisp and cocoa bridging:
(load-module "utils")

	      
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Model layer

(load-module "rtm")  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Controller layer

(load-module "rtmcontroller.lisp")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Code to build the MilkPack application bundle in MacOS:

;; #|

(setf (current-directory) PROJECT-SOURCE-PATH)
(require "build-application")


(ccl::build-application :name "MilkPack"
                        :main-nib-name "MilkPack"
                        :nibfiles 
  '(#P"/Users/eemg/Documents/projects/rtm-lisp/MilkPack.nib"))


;; |#
