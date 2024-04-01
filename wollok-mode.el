(defvar wollok-mode-syntax-table
  (let ((syntax-table (make-syntax-table)))
    ;; Operators
    (dolist (i '(?+ ?- ?* ?/ ?% ?& ?| ?! ?< ?> ?= ?[ ?] ?{ ?}))
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

;;;###autoload
(define-derived-mode wollok-mode prog-mode "wollok"
  ;; Enable electric-indent-mode
  (electric-indent-local-mode 1)

  ;; Set comment delimiters
  (setq-local comment-start "// ")
  (setq-local comment-end "")

  (setq font-lock-defaults
        '((
           ;; Keywords
           ("\\b\\(var\\|const\\|mixin\\|self\\|return\\|import\\|method\\|object\\|program\\|new\\|val\\|class\\|package\\|inherits\\|override\\|try\\|catch\\|throw\\|property\\|test\\|describe\\|then\\|always\\)\\b" 0 font-lock-keyword-face)
           ("\\bif\\|else\\|while\\|for\\b" 0 font-lock-keyword-face)

           ("\\bor\\b" 0 font-lock-constant-face)
           ("\\band\\b" 0 font-lock-constant-face)
           ("\\bnot\\b" 0 font-lock-constant-face)

           ;; Booleans
           ("\\b\\(true\\|false\\|null\\)\\b" 1 font-lock-constant-face)

           ;; Builtins
           ("\\b\\(assert\\|console\\)\\b" 1 font-lock-builtin-face)

           ;; Numbers
           ("\\b\\(\\(?:[0-9]*\\.?[0-9]+\\|0x[0-9a-fA-F]+\\)\\(?:[eE][+-]?[0-9]+\\)?\\)\\b" 0 font-lock-constant-face)

           ;; Objects and classes
           ("\\(?:object\\s-+inherits\\|inherits\\)\\s-+\\([[:alpha:]_][[:alnum:]_\\.]*\\)\\b" 1 font-lock-type-face)
           ("\\(?:object\\s-+inherits\\|object\\|program\\|class\\|inherits\\|mixin\\|and\\)\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)\\b" 1 font-lock-type-face)
           ("\\binherits.*?\\(and\\)" 1 font-lock-keyword-face)

           ("\\([[:alpha:]_][[:alnum:]_]*\\)\\>\\s-*=\\s-*object" 1 font-lock-type-face)
           ("\\<new\\>\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*(" 1 font-lock-type-face)

           ;; Exceptions
           ("\\bcatch\\s-*[[:alpha:]_][[:alnum:]_]*\\s-*:\\s-*\\([[:alpha:]_][[:alnum:]_]*\\)" 1 font-lock-type-face)

           ;; Functions
           ("\\bmethod\\s-+\\([[:alpha:]_][[:alnum:]_]*\\)?\\b" 1 font-lock-function-name-face)
           ("\\b\\(super\\)\\s-*(" 1 font-lock-function-name-face)
           ("\\(?:[[:alpha:]_][[:alnum:]_]*\\|[[:digit:]]\\|)\\)\\.\\([[:alpha:]_][[:alnum:]_]*\\)\\s-*(" 1 font-lock-function-name-face)))))

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wlk\\'" . wollok-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wpgm\\'" . wollok-mode))
;;;###autoload
(add-to-list 'auto-mode-alist '("\\.wtest\\'" . wollok-mode))

(provide 'wollok-mode)

;;; wollok-mode.el ends here
