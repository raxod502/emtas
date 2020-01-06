;;; emtas.el --- Absurdly fast Emacs startup time -*- lexical-binding: t -*-

;; Copyright (C) 2019 Radon Rosborough

;; Author: Radon Rosborough <radon.neon@gmail.com>
;; Created: 2 Jan 2020
;; Homepage: https://github.com/raxod502/emtas
;; Keywords: convenience
;; Package-Requires: ((emacs "25.1") (heap "0.5"))
;; Version: 0

;;; Commentary:

;; Please see https://github.com/raxod502/emtas for more information.

;;; Code:

;; To see the outline of this file, run M-x outline-minor-mode and
;; then press C-c @ C-t. To also show the top-level functions and
;; variable declarations in each section, run M-x occur with the
;; following query: ^;;;;* \|^(

(require 'cl-lib)

;; Yucky hack because `heap' loads `cl' which it really really
;; shouldn't, and Emacs rightfully prints an annoying message about
;; the deprecation of `cl' which happened like a decade ago, but we
;; don't want to bother *our* users. In case you're wondering why the
;; heck we're binding `run-with-idle-timer', see
;; `do-after-load-evaluation' which is what generates the deprecation
;; message (using an idle timer, no less).
(cl-letf* ((run-with-idle-timer (symbol-function #'run-with-idle-timer))
           ((symbol-function #'run-with-idle-timer)
            (lambda (secs repeat function &rest args)
              (unless (and (= (length args) 1)
                           (stringp (car args))
                           (string-match-p "^Package .+ is deprecated$"
                                           (car args)))
                (apply run-with-idle-timer secs repeat function args)))))
  (require 'heap))

(defgroup emtas nil
  "Helping you make your Emacs startup time absurdly fast, since 2019."
  :group 'convenience
  :prefix "emtas-"
  :link '(url-link "https://github.com/raxod502/emtas"))

(defcustom emtas-idle-require-delay 0.1
  "Number of seconds to wait between each idle `require'."
  :type 'number)

(defcustom emtas-max-desired-require-latency 0.1
  "Longest number of seconds an idle `require' should take."
  :type 'number)

(defvar emtas--idle-require-queue
  (make-heap (lambda (a b)
               (or (< (nth 0 a) (nth 0 b))
                   (and (= (nth 0 a) (nth 0 b))
                        (< (nth 1 a) (nth 1 b))))))
  "Priority queue of features to `require'.
The elements are lists of three elements (ORDER SERIAL FEATURE).
ORDER is the numerical order passed to `emtas-idle-require'.
SERIAL is an integer that is incremented each time the same ORDER
is used, so that ORDER and SERIAL together establish a total
ordering on elements of the queue. FEATURE is the name of the
feature to `require', a symbol.

Invariant: the queue is non-empty if and only if there's an idle
timer scheduled for `emtas--process-idle-require-queue'.")

(defvar emtas--idle-require-serial-table (make-hash-table :test #'eql)
  "Table of serial numbers used in `emtas--idle-require-queue'.
The keys are ORDER values passed to `emtas-idle-require', while
the values are the serial numbers, starting at 0 and
increasing.")

(defvar emtas--current-feature nil
  "Feature currently being required, a symbol.")

(defvar emtas--feature-dependencies (make-hash-table :test #'eq)
  "Hash table mapping features to lists of their dependencies.")

(defun emtas--process-idle-require-queue ()
  "Require a feature the idle queue, and reschedule this function if needed."
  (cl-destructuring-bind (_order _serial feature)
      (heap-delete-root emtas--idle-require-queue)
    (require 'feature))
  (unless (heap-empty emtas--idle-require-queue)
    (run-with-idle-timer
     emtas-idle-require-delay nil
     #'emtas--process-idle-require-queue)))

(defun emtas-idle-require (feature &optional order)
  "Call `require' on FEATURE when Emacs is idle.
ORDER defaults to 0 and can be used to establish an ordering;
features are processed in order of increasing ORDER, with ties
broken by the order in which `emtas-idle-require' is called."
  (when (heap-empty emtas--idle-queue)
    (run-with-idle-timer
     emtas-idle-queue-delay
     nil #'emtas--pop-action-and-reschedule))
  (heap-add emtas--idle-queue
            (list order
                  (cl-incf (gethash
                            order
                            emtas--idle-require-serial-table
                            0))
                  feature)))

(provide 'emtas)

;; Local Variables:
;; indent-tabs-mode: nil
;; outline-regexp: ";;;;* "
;; End:

;;; emtas.el ends here
