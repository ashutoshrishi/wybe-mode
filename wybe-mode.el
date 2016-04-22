;;; wybe-mode --- Major mode for the wybe programming language

;; Copyright 2016 Ashutosh Rishi Ranjan

;; Author: Ashutosh Rishi Ranjan (ashutoshrishi92@gmail.com)
;; Version: 1.0.0
;; Created: 20 Feb 2015
;; Keywords: languages

;;; Commentary:
;; Major mode to for writing Wybe modules.

;;; Code:


(defgroup wybe nil
  "Wybe Programming Language Major Mode."
  :group 'languages)


;; Wybe key map
(defvar wybe-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Wybe major mode")


;; Wybe faces
(defgroup wybe-faces nil
  "Custom faces for syntax highlighting."
  :group 'wybe)

(defface wybe-font-lock-operator-face
  '((((background light)) (:foreground "brown"))
    (t (:foreground "khaki")))
  "Face description for all operators."
  :group 'wybe-faces)
(defvar wybe-font-lock-operator-face
  'wybe-font-lock-operator-face)



;; define several category of keywords
(defconst wybe-keywords
  '("func" "if" "then" "else" "proc" "end" "public" "private" "use"
    "type" "do" "until" "unless" "or" "test")
  "Keywords of the language.")

(defconst wybe-types '("int" "float" "string" "char" "bool")
  "Wybe primitive types.")


;; Define REGEXs
;; generate regex string for each category of keywords
(defconst wybe-keywords-re (regexp-opt wybe-keywords 'words)
  "Regex for matching Wybe keywords.")

(defconst wybe-types-re (regexp-opt wybe-types 'words)
  "Regex for matching Wybe primitive types.")

(defconst wybe-func-re
  (rx symbol-start (or "proc" "func" "type") (1+ space)
      (group (1+ (any alnum ?+ ?- ?*)))
      (any space ?\())
  "Regex for matching function and proc names.")


(defconst wybe-custom-types-re
  (rx (char ?:) (0+ space)
      (group (1+ (any alnum))))
  "Regex for custom type annotations.")


(defconst wybe-operators-re
  "[][;,()|{}]\\|[-@^!:*=<>&/%+~?#]"
  "Regex for matching Wybe in-built operators.")


;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar wybe-font-lock-keywords
  `((,wybe-keywords-re . font-lock-keyword-face)
    (,wybe-types-re . font-lock-type-face)
    (,wybe-operators-re . wybe-font-lock-operator-face)
    (,wybe-func-re . (1 font-lock-function-name-face))
    (,wybe-custom-types-re . (1 font-lock-type-face)))
  "Wybe language font-locks.")


;; Wybe syntax table
(defvar wybe-syntax-table nil "Syntax table for `wybe-mode'.")
(setq wybe-syntax-table
      (let ((synTable (make-syntax-table c-mode-syntax-table)))

        (modify-syntax-entry ?# "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))


;; Indentation
(defun wybe-indent-line ()
  "Indent current line as Wybe code"
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (wybe-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun wybe-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (current-indentation))


;;;###autoload
(define-derived-mode wybe-mode prog-mode
  "wybe mode"
  "Major mode for editing wybe language"

  ;; code for syntax highlighting
  (set-syntax-table wybe-syntax-table)
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (setq-local font-lock-defaults '((wybe-font-lock-keywords)))
  ;; (setq-local indent-line-function 'wybe-indent-line)
  )

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wybe\\'" . wybe-mode))

(provide 'wybe-mode)
;;; wybe-mode.el ends here
