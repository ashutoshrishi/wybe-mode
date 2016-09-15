;;; wybe-mode --- Major mode for the wybe programming language

;; Copyright 2016 Ashutosh Rishi Ranjan

;; Author: Ashutosh Rishi Ranjan (ashutoshrishi92@gmail.com)
;; Version: 1.0.0
;; Created: 20 Feb 2015
;; Keywords: languages

;;; Commentary:
;; Font locks for the Wybe Major mode to for writing Wybe modules.

;;; Code:
(require 'rx)
(require 'font-lock)

;; Faces for font-locking

(defgroup wybe-font-lock nil
  "Font locking for Wybe code."
  :group 'faces)

(defface wybe-font-lock-operators-face
  '((t :inherit font-lock-operator-face))
  "The default face used to highlight operator functions/procs."
  :group 'wybe-font-lock)
(defvar wybe-font-lock-operators-face
  'wybe-font-lock-operators-face)


;; Font locking for various syntactical features of Wybe

;; Keywords

(defconst wybe--keywords
  '("if" "then" "else" "proc" "end" "public" "private" "use"
    "do" "until" "unless" "or" "test" "import" "while")
  "Keywords of the Wybe language.")

(defconst wybe--keywords-regexp
  (concat (regexp-opt wybe--keywords 'words) "[^']")
  "The regular expression matching Wybe keywords.")


;; Primitive Types and user defined types
(defconst wybe--prim-types
  '("int" "float" "string" "char" "bool")
  "Wybe primitive types.")


(defconst wybe--prim-types-regexp
  (regexp-opt wybe--prim-types 'words)
  "The regular expression matching certain primitive Wybe types.")

(defconst wybe--custom-types-regexp
  (rx (char ?:) (0+ space)
      (group (1+ (any alnum))))
  "Regex for custom type annotations.")

;; Constants
(defconst wybe--constant-regexp
  (rx symbol-start (1+ digit) symbol-end)
  "Regex for Wybe constants.")

;; Functions and Procedure names
(defconst wybe--func-regexp
  (rx (group symbol-start (or "proc" "func" "type") (1+ space))
      (group (1+ (any alnum ?+ ?- ?*)))
      (any space ?\())
  "The regular expression for matching function and proc names.")

;; Operators
(defconst wybe--operators-regexp
  (rx (any ?+ ?- ?* ?< ?= ?> ?? ?/))
  "The regular expression for matching Wybe in-built operators.")


;; Syntax Table

(defvar wybe--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table of the Wybe mode.")


;; Final Highlighting

(defvar wybe-font-lock-keywords
  `(("\\<\\(false\\|true\\)\\>" . font-lock-constant-face)
    (,wybe--func-regexp (1 font-lock-keyword-face)
                        (2 font-lock-function-name-face))
    (,wybe--keywords-regexp . font-lock-keyword-face)
    (,wybe--custom-types-regexp 1 font-lock-type-face)
    (,wybe--prim-types-regexp font-lock-type-face)
    ;; (,wybe--constant-regexp . font-lock-constant-face)
    (wybe--operators-regexp . wybe-font-lock-operators-face))
  "Combined highlighting for various syntactical features of Wybe.")


(defun turn-on-wybe-font-lock ()
  "Turn on Wybe-mode font lock."
  (set-syntax-table wybe--syntax-table)
  (setq-local font-lock-defaults '(wybe-font-lock-keywords
                                   nil nil nil nil)))



(provide 'wybe-font-lock)
;;; wybe-font-lock.el ends here
