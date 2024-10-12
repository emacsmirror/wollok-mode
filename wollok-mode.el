;;; wollok-mode.el --- Major mode for the Wollok programming language  -*- lexical-binding: t -*-

;; Authors: Tomás Ralph <tomasralph2000@gmail.com>
;; Created: 2024
;; Version: 0.1
;; Package-Requires: ((emacs "24.4"))
;; Homepage: https://github.com/tralph3/wollok-mode
;; Keywords: wollok languages

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A simple major mode for Wollok https://wollok.org


;;; Code:

(defvar wollok-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?! ?< ?> ?=))
      (modify-syntax-entry i "." syntax-table))

    ;; Strings
    (modify-syntax-entry ?\" "\"" syntax-table)
    (modify-syntax-entry ?\\ "\\" syntax-table)
    (modify-syntax-entry ?\' "\"" syntax-table)

    ;; Comments
    (modify-syntax-entry ?/  ". 124b" syntax-table)
    (modify-syntax-entry ?*  ". 23n"  syntax-table)
    (modify-syntax-entry ?\n "> b"    syntax-table)
    (modify-syntax-entry ?\^m "> b"   syntax-table)
    syntax-table)
  "Syntax table for `wollok-mode'.")

(defun wollok-keywords ()
  "List of keywords for Wollok."
  '("var" "const" "mixin" "self" "return" "import" "method" "object"
    "program" "new" "val" "class" "package" "inherits" "override" "try"
    "catch" "throw" "property" "test" "describe" "then" "always" "if"
    "else" "while" "for"))

(defun wollok-constants ()
  "List of constants for Wollok."
  '("or" "and" "not" "true" "false" "null"))

(defun wollok-builtins ()
  "List of builtins for Wollok."
  '("assert" "console"))

(defun wollok-mode-font-lock-keywords ()
  "Font lock rules for Wollok."
  (list
   `(,(regexp-opt (wollok-keywords) 'symbols) 0 font-lock-keyword-face)
   `(,(regexp-opt (wollok-constants) 'symbols) 0 font-lock-constant-face)
   `(,(regexp-opt (wollok-builtins) 'symbols) 0 font-lock-builtin-face)

   ;; Numbers
   '("\\b\\(\\(?:[0-9]*\\.?[0-9]+\\|0x[0-9a-fA-F]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\)\\b" 0 font-lock-constant-face)

   ;; Objects and classes
   '("\\(?:object\\s-+inherits\\|inherits\\)\\s-+\\([[:alpha:]_][[:alnum:]_\\.]*\\)\\b" 1 font-lock-type-face)
   '("\\(?:object\\s-+inherits\\|object\\|program\\|class\\|inherits\\|mixin\\|and\\)\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\b" 1 font-lock-type-face)
   '("\\binherits.*?\\(and\\)" 1 font-lock-keyword-face)
   '("\\([[:alpha:]_][[:alnum:]_]*\\)\\>\\s-*=\\s-*object" 1 font-lock-type-face)
   '("\\<new\\>\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*(" 1 font-lock-type-face)

   ;; Exceptions
   '("\\bcatch\\s-*[[:alpha:]_][[:alnum:]_]*\\s-*:\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)" 1 font-lock-type-face)

   ;; Functions
   '("\\bmethod\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)?\\b" 1 font-lock-function-name-face)
   '("\\b\\(super\\)\\s-*(" 1 font-lock-function-name-face)
   '("\\(?:[[:alpha:]_][[:alnum:]_]*\\|[[:digit:]]\\|)\\|\s*\\)\\.\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*(" 1 font-lock-function-name-face)))

(defun wollok--previous-non-empty-line ()
  "Return the previous non-emtpy line from point."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun wollok--indentation-of-previous-non-empty-line ()
  "Return indentation of previous non-empty line from point."
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun wollok--desired-indentation ()
  "Return indentation level for the current line."
  (let* ((cur-line (string-trim-right (thing-at-point 'line t)))
         (prev-line (string-trim-right (wollok--previous-non-empty-line)))
         (indent-len 4)
         (half-indent 2)
         (prev-indent (wollok--indentation-of-previous-non-empty-line))
         (prev-indent (if (and (string-prefix-p "." (string-trim-left prev-line))
                               (not (string-prefix-p "." (string-trim-left cur-line)))
                               (not (equal (string-trim cur-line) "")))
                          (- prev-indent half-indent)
                        prev-indent)))
    (cond
     ((and (string-suffix-p "{" prev-line)
           (string-prefix-p "}" (string-trim-left cur-line)))
      prev-indent)
     ((or (string-suffix-p "{" prev-line)
          (string-suffix-p "=>" prev-line))
      (+ prev-indent indent-len))
     ((and (string-prefix-p "." (string-trim-left cur-line))
           (not (string-prefix-p "." (string-trim-left prev-line))))
      (+ prev-indent half-indent))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

(defun wollok-indent-line ()
  "Indent current line according to its context."
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (wollok--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;;###autoload
(define-derived-mode wollok-mode prog-mode "Wollok"
  "Major mode for the Wollok programming language."
  (setq-local comment-start "// ")

  (setq-local font-lock-defaults '(wollok-mode-font-lock-keywords))
  (setq-local indent-line-function #'wollok-indent-line))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wlk\\'" . wollok-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wpgm\\'" . wollok-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wtest\\'" . wollok-mode))

(provide 'wollok-mode)

;;; wollok-mode.el ends here
