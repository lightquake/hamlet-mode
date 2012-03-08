; An emacs mode for editing files written in Hamlet, Yesod's HTML-like templating language.

(defvar hamlet-mode-hook nil)

; autoload on .hamlet files
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . hamlet-mode))


(defconst hamlet-name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")

(defconst hamlet-font-lock-highlighting
  
  
  `(
    ;; tag names
    (,(concat "<\\(" hamlet-name-regexp "\\)") 1 font-lock-function-name-face)
    ;; attributes
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\(" hamlet-name-regexp "\\)=\\(\\sw+\\)\\|\\([.#]" hamlet-name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t)
     (2 font-lock-string-face nil t)
     (3 font-lock-variable-name-face nil t)
     )
    (,(concat "\\([@^#]{[^}]+}\\)") 1 font-lock-preprocessor-face t)
    )
    ;; variable interpolation

)

(defvar hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    st)
)

(define-derived-mode hamlet-mode fundamental-mode "Hamlet"
  "Major mode for editing Hamlet files."
  (setq font-lock-defaults '(hamlet-font-lock-highlighting)))