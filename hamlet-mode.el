; An emacs mode for editing files written in Hamlet, Yesod's HTML-like templating language.
(defvar hamlet-mode-hook nil)

; tab stops from column 2 to 80. more than that, and you're doing it wrong.
(add-hook 'hamlet-mode-hook '(lambda ()
          (setq tab-stop-list (number-sequence 2 80 2))))


; autoload on .hamlet and .lucius files
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . hamlet-mode))
(add-to-list 'auto-mode-alist '("\\.lucius\\'" . css-mode))

(defconst hamlet-name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")

(defconst hamlet-font-lock-highlighting
  `(
    ;; tag names
    (,(concat "</?\\(" hamlet-name-regexp "\\)") 1 font-lock-function-name-face)
    ;; attributes; the three groups, in order, are attribute name,
    ;; attribute string value, and .class or #id
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\("
              hamlet-name-regexp "\\)=\\(\\sw*\\)\\|\\([.#]"
              hamlet-name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t)
     (2 font-lock-string-face nil t)
     (3 font-lock-variable-name-face nil t)
     )
    ;; variable interpolation
    ("\\([@^#]{[^}]+}\\)" 1 font-lock-preprocessor-face t)
    ;; control flow
    ("^[ \t]*\\($\\w+\\)" 1 font-lock-keyword-face)
    )
)

(defvar hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?\\ "w" st)
    st)
)

(defun hamlet-mode-auto-fill-function ()
  (when (> (current-column) fill-column)
    ;; Split at 2 chars before fill-column. This is so that we can insert a
    ;; space and a hash
    (while (> (current-column) (- fill-column 2))
      (skip-syntax-backward "-")
      (skip-syntax-backward "^-"))
    (let ((auto-fill-function nil)
          (indent (current-indentation)))
      (insert "#")
      (newline)
      (indent-to indent)
      (insert "\\")
      (end-of-line))))

(define-derived-mode hamlet-mode fundamental-mode "Hamlet"
  "Major mode for editing Hamlet files."
  (kill-local-variable 'normal-auto-fill-function)
  (kill-local-variable 'font-lock-defaults)
  (set (make-local-variable 'font-lock-defaults)
       '(hamlet-font-lock-highlighting))
  (set (make-local-variable 'normal-auto-fill-function)
       'hamlet-mode-auto-fill-function))

(provide 'hamlet-mode)
