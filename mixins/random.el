;; Stolen from https://esrh.me/posts/2021-11-27-emacs-config#nyaatouch

(global-set-key (kbd "C-x w") 'ace-swap-window)

;; Furcula
;; Implementation of the furcula (branching arrow) as described in the
;; swiss-arrows macroset for clojure. I am a huge fan of this idea, and I
;; included an implementation of it in my janet fork, matsurika. The basic
;; idea is that the first entry will get inserted as either the first or
;; last item (argument, usually) in every subsequent lisp form, or function.
;; The result is then a list of the execution of all of these forms or
;; functions. For instance, (-< 5 (+ 1) (* 2) (- 1)) results in '(6 10 4).
;; Things can get really exciting when you start combining -< and ->>!

;; This particular implementation comes from Adam Porter’s (alphapapa) code
;; from his 2018 proposal to dash.el. Kind of bummed this never got merged.

;; (defmacro -< (expr &rest forms) (declare (indent defun)) (let ((var (gensym))) `(let ((,var ,expr)) (list ,@(--map (pcase it ((pred symbolp) (list it var)) ((pred listp) (-snoc it var))) forms))))) (defmacro -<< (expr &rest forms) (declare (indent defun)) (let ((var (gensym))) `(let ((,var ,expr)) (list ,@(--map (pcase it ((pred symbolp) (list it var)) (`(,first . ,rest) `(,first ,var ,@rest))) forms)))))

;; JIT
(setq comp-deferred-compilation t) (setq warning-suppress-log-types '((comp)))

;; straight-use-package is a bit of a keyful to type, especially interactively.
(defalias 'sup 'straight-use-package)

;; Libraries
;; (sup 's) (sup 'dash)

;; Nullary lambda
(defmacro fn (&rest forms) (declare (indent 0)) `(lambda () ,@forms))

(defmacro add-fs-to-hook (hook &rest funcs) "Add functions to hook. A function is either an unquoted token, or a form. If it's a token, then its treated as a function and enabled. Otherwise, the form is run." `(add-hook ,hook (fn ,@(mapcar (lambda (el) (if (listp el) el (list el 1))) funcs))))

(defmacro add-to-hooks (f &rest hooks) "Add a single function to many quoted hooks" `(progn ,@(mapcar (lambda (hook) `(add-hook ,hook ,f)) hooks)))

(defun my-asm-mode-hook () (setq tab-always-indent (default-value 'tab-always-indent))) (add-fs-to-hook 'asm-mode-hook (local-unset-key (vector asm-comment-char)) (setq tab-always-indent (default-value 'tab-always-indent)))


;; Better parenthesis location

;; I can’t count parentheses. I use an advice override to change how the
;; parenthesis locating functionality works. This is because I use a block
;; cursor with meow, which makes cursor position slightly deceptive.

;; Basically, the block cursor by default highlights the parenthesis when your
;; cursor is immediately AFTER the parenthesis in question, because the point
;; is always between two characters in emacs (the point is really right after
;; the parenthesis as well). So, if you have nested parentheses, as we often
;; do, it’s strange to see the “wrong parenthesis” highlighted.

;; This advice first checks before the point and only then after the point
;; for a parenthesis. I think this behavior is very intuitive. The defined
;; function overrides the internal function used to find parentheses.

(column-number-mode) (show-paren-mode) (defun show-paren--locate-near-paren-ad () "Locate an unescaped paren \"near\" point to show. If one is found, return the cons (DIR . OUTSIDE), where DIR is 1 for an open paren, -1 for a close paren, and OUTSIDE is the buffer position of the outside of the paren. Otherwise return nil." (let* ((before (show-paren--categorize-paren (point)))) (when (or (eq (car before) 1) (eq (car before) -1)) before))) (advice-add 'show-paren--locate-near-paren :override #'show-paren--locate-near-paren-ad)

;; Colorize color strings.

(sup 'rainbow-mode) (add-hook 'prog-mode #'rainbow-mode)

;; Auto whitespace cleanup
(add-fs-to-hook 'prog-mode-hook (add-hook 'after-save-hook (fn (whitespace-cleanup))))

;;  Pixel scrolling

(defun pixel-scroll-setup () (interactive) (setq pixel-scroll-precision-large-scroll-height 1) (setq pixel-scroll-precision-interpolation-factor 1)) (when (boundp 'pixel-scroll-precision-mode) (pixel-scroll-setup) (add-hook 'prog-mode-hook #'pixel-scroll-precision-mode) (add-hook 'org-mode-hook #'pixel-scroll-precision-mode))


;; Fill in a nice way
(straight-use-package '(far :type git :repo "https://github.com/eshrh/far.el")) (meow-normal-define-key '("`" . far-fill-paragraph))


;; Tree-sitter
;; Introduced in emacs 30.

(setq treesit-available (and (fboundp 'treesit-available-p) (treesit-available-p)))

;; Create a list of grammar urls.

(when treesit-available (defun treesitter-grammar-url (lang) (concat "https://github.com/tree-sitter/tree-sitter-" lang)) (setq treesit-langs '(bash c cpp haskell html java javascript julia rust python)) (setq treesit-language-source-alist (--map `(,it . (,(treesitter-grammar-url (symbol-name it)))) treesit-langs)))

(defun treesit-ensure (lang) (unless (treesit-language-available-p lang) (treesit-install-language-grammar lang)))

(when treesit-available (treesit-ensure 'c) (treesit-ensure 'cpp) (treesit-ensure 'rust) (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))) (setq-default c-basic-offset 4 kill-whole-line t indent-tabs-mode nil)


;; Ace window
;; don’t hint me for things outside the frame

;; (setq aw-scope 'frame)

;; I never want to switch to the current buffer

;; (setq aw-ignore-current t) (setq aw-background nil)
