;;; wybe-mode --- Major mode for the wybe programming language

;; Copyright 2016 Ashutosh Rishi Ranjan

;; Author: Ashutosh Rishi Ranjan (ashutoshrishi92@gmail.com)
;; Version: 1.0.0
;; Created: 20 Feb 2015
;; Keywords: languages

;;; Commentary:
;; Major mode to for writing Wybe modules.

;;; Code:
(require 'wybe-font-lock)

(defgroup wybe nil
  "Wybe Programming Language Major Mode."
  :group 'languages)


;; Wybe mode hook
(defvar wybe-mode-hook nil)

;; Wybe key map
(defvar wybe-mode-map
  (let ((map (make-keymap)))
    (define-key map "\C-j" 'newline-and-indent)
    map)
  "Keymap for Wybe major mode.")



;; Indentation
(defun wybe-indent-line ()
  "Indent current line as Wybe code."
  (interactive)
  (let ((savep (> (current-column) (current-indentation)))
        (indent (condition-case nil (max (wybe-calculate-indentation) 0)
                  (error 0))))
    (if savep
        (save-excursion (indent-line-to indent))
      (indent-line-to indent))))

(defun wybe-calculate-indentation ()
  "Return the column to which the current line should be indented."
  (save-excursion
    (beginning-of-line)
    ;; Definite 0 column indentations
    (if (looking-at "^[ \t]*\\(proc\\|test\\|public\\|private\\|type\\)")
        0
      ;; Check for block ending statements, then move back 4 spaces based
      ;; on previous line indentation.
      (if (looking-at "^[ \t]*end")
          (progn
            (forward-line -1)
            (- (current-indentation) 4))
        ;; Decide indentation by looking at the previous line(s)
        (progn
          ;; Move upwards until non-blank line
          (forward-line -1)
          (while (and (looking-at "[[:space:]]*$") (not (bobp)))
            (forward-line -1))
          ;; Heuristic based on previous line
          (if (looking-at "[ \t]*\\(proc\\|if\\|type\\)")
              (+ (current-indentation) 4)
            (if (looking-at "^[ \t]*end")
                (- (current-indentation) 4)
              (current-indentation))))))))



;;;###autoload
(define-derived-mode wybe-mode prog-mode
  "Wybe"
  "Major mode for editing Wybe modules."
  (setq-default indent-tabs-mode nil)
  ;; code for syntax highlighting
  (setq-local comment-start "#")
  (setq-local comment-end "")
  (turn-on-wybe-font-lock)
  (setq-local indent-line-function 'wybe-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wybe\\'" . wybe-mode))

(provide 'wybe-mode)
;;; wybe-mode.el ends here

