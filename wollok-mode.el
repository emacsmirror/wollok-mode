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
  "Syntax table for `wollok-mode'")

(defun wollok-keywords ()
  '("var" "const" "mixin" "self" "return" "import" "method" "object"
    "program" "new" "val" "class" "package" "inherits" "override" "try"
    "catch" "throw" "property" "test" "describe" "then" "always" "if"
    "else" "while" "for"))

(defun wollok-constants ()
  '("or" "and" "not" "true" "false" "null"))

(defun wollok-builtins ()
  '("assert" "console"))

(defun wollok-mode-font-lock-keywords ()
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
   '("\\(?:[[:alpha:]_][[:alnum:]_]*\\|[[:digit:]]\\|)\\)\\.\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*(" 1 font-lock-function-name-face)))

(defun wollok--previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (thing-at-point 'line t)))

(defun wollok--indentation-of-previous-non-empty-line ()
  (save-excursion
    (forward-line -1)
    (while (and (not (bobp))
                (string-empty-p
                 (string-trim-right
                  (thing-at-point 'line t))))
      (forward-line -1))
    (current-indentation)))

(defun wollok--desired-indentation ()
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
     ((string-suffix-p "{" prev-line)
      (+ prev-indent indent-len))
     ((and (string-prefix-p "." (string-trim-left cur-line))
           (not (string-prefix-p "." (string-trim-left prev-line))))
      (+ prev-indent half-indent))
     ((string-prefix-p "}" (string-trim-left cur-line))
      (max (- prev-indent indent-len) 0))
     (t prev-indent))))

(defun wollok-indent-line ()
  (interactive)
  (when (not (bobp))
    (let* ((desired-indentation
            (wollok--desired-indentation))
           (n (max (- (current-column) (current-indentation)) 0)))
      (indent-line-to desired-indentation)
      (forward-char n))))

;;;###autoload
(define-derived-mode wollok-mode prog-mode "Wollok"
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
