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

;; define several category of keywords
(defconst wybe-keywords
  '("func" "if" "then" "else" "proc" "end" "public" "private" "use"
    "type")
  "Keywords of the language.")

;; (defconst wybe-flags '("public" "private")
;;   "Function flags.")

(defconst wybe-types '("int" "float" "string" "char" "bool")
  "Wybe primitive types.")

;; generate regex string for each category of keywords
(defconst wybe-keywords-re (regexp-opt wybe-keywords 'words)
  "Regex for matching Wybe keywords.")
(defconst wybe-types-re (regexp-opt wybe-types 'words)
  "Regex for matching Wybe primitive types.")
;; (defconst wybe-flags-re '(regexp-opt wybe-flags 'words)
;;   "Regex for matching function flags.")
(defconst wybe-func-re
  (rx symbol-start (or "proc" "func" "type") (1+ space)
      (group (1+ (any alnum ?+ ?- ?*)))
      (any space ?\())
  "Regex for matching function and proc names.")
(defconst wybe-comment-re
  (rx (0+ space) (char ?#) (0+ anything) eol)
  "Regex for matching comments.")

(defconst wybe-custom-types-re
  (rx (char ?:) (0+ space)
      (group (1+ (any alnum))))
  "Regex for custom type annotations.")

;; create the list for font-lock.
;; each category of keyword is given a particular face
(defvar wybe-font-lock-keywords
  `((,wybe-comment-re . font-lock-comment-face)
    (,wybe-keywords-re . font-lock-keyword-face)    
    (,wybe-types-re . font-lock-type-face)
    (,wybe-func-re . (1 font-lock-function-name-face))
    (,wybe-custom-types-re . (1 font-lock-type-face)))
  "Wybe language font-locks.")

;;;###autoload
(define-derived-mode wybe-mode prog-mode
  "wybe mode"
  "Major mode for editing wybe language"

  ;; code for syntax highlighting
  (setq-local font-lock-defaults '((wybe-font-lock-keywords)))
  (setq-local comment-start "#")
  (setq-local comment-end ""))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wybe\\'" . wybe-mode))

(provide 'wybe-mode)
;;; wybe-mode.el ends here
