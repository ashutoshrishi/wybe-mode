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

(defgroup wybe-font-lock nil
  "Font locking for Wybe code."
  :group 'faces)

(defface wybe-font-lock-operators
  '((t :inherit font-lock-builtin-face))
  "The default face used to highlight operator functions/procs."
  :group 'wybe-font-lock)

(defcustom elm-font-lock-operators-face 'wybe-font-lock-operators
  "The face used to highlight operators.
To disable this highlighting, set to nil."
  :type '(choice (const nil)
                 face)
  :group 'wybe-font-lock)

;; Font locking for various syntactical features of Wybe

;; Keywords

(defconst wybe--keywords
  '("func" "if" "then" "else" "proc" "end" "public" "private" "use"
    "type" "do" "until" "unless" "or" "test" "import")
  "Keywords of the Wybe language.")

(defconst wybe--keywords-regexp
  (concat (regexp-opt wybe--keywords 'words) "[^']")
  "The regular expression matching Wybe keywords.")

(defconst wybe--font-lock-keywords
  (cons wybe--keywords-regexp font-lock-keyword-face)
  "Highlighting for keywords.")

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

(defconst wybe--font-lock-prim-types
  (cons wybe--prim-types-regexp font-lock-type-face)
  "Highlighting for primitive types.")

(defconst wybe--font-lock-custom-types
  (cons wybe--custom-types-regexp '(1 font-lock-type-face))
  "Highlighting for custom Wybe types.")


;; Functions and Procedure names

(defconst wybe--func-regexp
  (rx symbol-start (or "proc" "func" "type") (1+ space)
      (group (1+ (any alnum ?+ ?- ?*)))
      (any space ?\())
  "The regular expression for matching function and proc names.")

(defconst wybe--font-lock-func
  (cons wybe--func-regexp '(1 font-lock-function-name-face))
  "Highlighting for function and procedure names.")

;; Operators

(defconst wybe--operators-regexp
  "[][;,()|{}]\\|[-@^!:*=<>&/%+~?#]"
  "The regular expression for matching Wybe in-built operators.")

(defconst wybe--font-lock-operators
  (cons wybe--operators-regexp '(1 wybe-font-lock-operators-face))
  "Highlighting for operator functions.")


;; Syntax Table

(defvar wybe--syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?# "< b" st)
    (modify-syntax-entry ?\n "> b" st)
    st)
  "Syntax table of the Wybe mode.")


;; Final Highlighting

(defconst wybe--font-lock-highlighting
  (list (list wybe--font-lock-keywords
              wybe--font-lock-func
              wybe--font-lock-custom-types
              wybe--font-lock-prim-types
              wybe--font-lock-operators)
        nil nil)
  "Combined highlighting for various syntactical features of Wybe.")

(defun turn-on-wybe-font-lock ()
  "Turn on Wybe-mode font lock."
  (setq font-lock-multiline t)
  (set-syntax-table wybe--syntax-table)
  (set (make-local-variable 'font-lock-defaults) wybe--font-lock-highlighting))

(provide 'wybe-font-lock)
;;; wybe-font-lock.el ends here
