;;; hamlet-mode.el --- Hamlet editing mode

;; Author: Kata <lightquake@amateurtopologist.com
;; Keywords: wp, languages, comm
;; URL: https://github.com/lightquake/hamlet-mode
;; Version: 0.1
;; Package-Requires: ((cl-lib "0.3"))

;; Copyright (c) 2013 Kata

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; hamlet-mode is an Emacs major mode for editing files written in Hamlet, a
;; Haskell compile-time HTML templating engine. Currently it only provides
;; syntax highlighting.

(require 'cl-lib)

(defgroup hamlet nil
  "Hamlet editing mode."
  :group 'languages)

(defcustom hamlet/basic-offset 2
  "The basic indentation level for `hamlet/indent-line'."
  :type 'integer
  :group 'hamlet)

(defcustom hamlet-mode-hook nil
  "Hook run by hamlet-mode."
  :type 'hook
  :group 'hamlet)

;; Associate ourselves with hamlet files.
(add-to-list 'auto-mode-alist '("\\.hamlet\\'" . hamlet-mode))

(defun hamlet/indent-line ()
  "Indent the current line according to
`hamlet/calculate-next-indentation'."
  (save-excursion
    (indent-line-to (hamlet/calculate-next-indentation))))

;;; Indentation-related functions.
(defun hamlet/calculate-next-indentation ()
  "Calculate the next indentation level for the given line. The
next indentation level is the next largest value
in (hamlet/valid-indentations), or 0 if the line is maximally
indented."
  (let* ((indentation (current-indentation))
         (next-indentation (cl-find-if (lambda (x) (> x indentation))
                                       (hamlet/valid-indentations))))
    (if (numberp next-indentation) next-indentation 0)))

(defun hamlet/valid-indentations ()
  "Calculate valid indentations for the current line. Valid
indentations are the next multiple of `hamlet/basic-offset' after
the indentation of the previous nonblank line and all smaller
multiples. i.e., if `hamlet/basic-offset' is 2 and the previous
line is indented 9 spaces, the valid indentations are 0, 2, 4, 6,
8, 10."
  (save-excursion
    ; Move point back to the previous non-blank line.
    (beginning-of-line 0)
    (while (and (> (point) 0)
                (looking-at "[ \t]*$"))
      (forward-line -1))
    (cl-loop for n from 0 to (+ hamlet/basic-offset (current-indentation))
          by hamlet/basic-offset collect n)))

(defconst hamlet/name-regexp "[_:[:alpha:]][-_.:[:alnum:]]*")

(defconst hamlet/font-lock-keywords
  `(
    ;; Doctype declaration.
    ("^!!!$" . font-lock-keyword-face)
    ;; Tag names.
    (,(concat "</?\\(" hamlet/name-regexp "\\)") . font-lock-function-name-face)

    ;; Attributes can be either name=val, #id, or .class.
    (,(concat "\\(?:^\\|[ \t]\\)\\(?:\\("
              hamlet/name-regexp "\\)=\\([^@^ \r\n]*\\)\\|\\([.#]"
              hamlet/name-regexp "\\)\\)")
     (1 font-lock-variable-name-face nil t) ; Attribute names
     (2 font-lock-string-face nil t) ; Attribute values
     (3 font-lock-variable-name-face nil t)) ; #id and .class

    ;; Variable interpolation, like @{FooR} or ^{bar} or #{2 + 2}.
    ("\\([@^#]{[^}]+}\\)" . font-lock-preprocessor-face)
    ;; Control flow statements start with a $.
    ("^[ \t]*\\($\\w+\\)" . font-lock-keyword-face)))

(defvar hamlet-mode-syntax-table
  (let ((st (make-syntax-table)))
    (modify-syntax-entry ?< "(>" st)
    (modify-syntax-entry ?> ")<" st)
    (modify-syntax-entry ?\\ "w" st)
    st)
  "The hamlet mode syntax table.")

(define-derived-mode hamlet-mode fundamental-mode "Hamlet"
  "Major mode for editing Hamlet files."
  (kill-local-variable 'normal-auto-fill-function)
  (kill-local-variable 'font-lock-defaults)
  (set (make-local-variable 'font-lock-defaults)
       '(hamlet/font-lock-keywords))
  (set (make-local-variable 'indent-line-function)
       'hamlet/indent-line))

(provide 'hamlet-mode)

;;; hamlet-mode.el ends here
