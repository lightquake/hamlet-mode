; An emacs mode for editing files written in Hamlet, Yesod's HTML-like templating language.

(defvar hamlet-mode-hook nil)

; autoload on .hamlet files
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . hamlet-mode))


(defconst hamlet-font-lock-highlighting
  '(
    ("<\\(\\w+\\)" 1 font-lock-function-name-face)
    ("[.#]\\(\\(\\w\\|-\\)+\\)" . font-lock-variable-name-face)
    )
  )


(define-derived-mode hamlet-mode fundamental-mode "Hamlet"
  "Major mode for editing Hamlet files."
  (setq font-lock-defaults '(hamlet-font-lock-highlighting)))
