;;; Emacs Bedrock
;;;
;;; Mixin: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el mixin if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(if (string-equal system-type "darwin")
    (setq dired-use-ls-dired nil))

(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          ;; (python-mode . python-ts-mode)
          ))
 :hook
 ;; Auto parenthesis matching
 (
  (prog-mode . electric-pair-mode)
  (prog-mode . rainbow-delimiters-mode)
  (prog-mode . which-function-mode)
  )
  )

(setq which-func-unknown "n/a")

;; (setq which-func-modes '(prog-mode))
;; (setq which-func-display-in-header-line t)
;; (which-function-mode)
;; (add-hook 'prog-mode-hook
          ;; (defun which-function-header-line-enable ()
            ;; (require 'which-func)
            ;; (setq header-line-format
                  ;; '((:eval (which-function))))))

;; (use-package fancy-narrow
;;   :ensure t
;;   :hook ((prog-mode . fancy-narrow-mode))
;; )
;; ;;(add-hook 'prog-mode-hook 'fancy-narrow-mode)

;; (use-package blacken
  ;; :ensure t
  ;; :hook ((python-mode . blacken-mode)))

;;;
;;; debug
;;;


;; (use-package dape
;;   :preface
;;   ;; By default dape shares the same keybinding prefix as `gud'
;;   ;; If you do not want to use any prefix, set it to nil.
;;   ;; (setq dape-key-prefix "\C-x\C-a")

;;   :hook
;;   ;; Save breakpoints on quit
;;   ;; ((kill-emacs . dape-breakpoint-save)
;;   ;; Load breakpoints on startup
;;   ;;  (after-init . dape-breakpoint-load))

;;   :init
;;   ;; To use window configuration like gud (gdb-mi)
;;   ;; (setq dape-buffer-window-arrangement 'gud)

;;   :config
;;   ;; Info buffers to the right
;;   (setq dape-buffer-window-arrangement 'right)
;;   (setq dape-request-timeout 30)
;;   ;; Global bindings for setting breakpoints with mouse
;;   (dape-breakpoint-global-mode)

;;   ;; To not display info and/or buffers on startup
;;   ;; (remove-hook 'dape-on-start-hooks 'dape-info)
;;   ;; (remove-hook 'dape-on-start-hooks 'dape-repl)

;;   ;; To display info and/or repl buffers on stopped
;;   ;; (add-hook 'dape-on-stopped-hooks 'dape-info)
;;   ;; (add-hook 'dape-on-stopped-hooks 'dape-repl)

;;   ;; Kill compile buffer on build success
;;   (add-hook 'dape-compile-compile-hooks 'kill-buffer)

;;   ;; Save buffers on startup, useful for interpreted languages
;;   ;; (add-hook 'dape-on-start-hooks (lambda () (save-some-buffers t t)))

;;   ;; Projectile users
;;   (setq dape-cwd-fn 'projectile-project-root)
;;   )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Frame management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Hook burly-tabs-mode to tab-bar-mode
(use-package burly
  :ensure t
  :hook (tab-bar-mode . burly-tabs-mode))

;; hook tab-bar-mode hook
(add-hook 'tab-bar-mode-hook 'burly-tabs-mode)

(use-package bufler
  :straight (bufler :fetcher github :repo "alphapapa/bufler.el"
                    :files (:defaults (:exclude "helm-bufler.el")))
  :init (bufler-mode)
  ;; :bind
  ;; (("C-x C-a C-n" . bufler-new)
  ;;  ("C-x C-a C-d" . bufler-define)
  ;;  ("C-x C-a C-a" . bufler-resume)
  ;;  ("C-x C-a C-s" . bufler-suspend)
  ;;  ("C-x C-a C-k" . bufler-kill)
  ;;  ("C-x C-a RET" . bufler-switch)
  ;;  ("C-x C-a b" . bufler-switch-buffer)
  ;;  ("C-x C-a g" . bufler-revert)
  ;;  ("C-x C-a l" . bufler-list))
  (setq bufler-reverse t))

;; (use-package activities
;;   :ensure t
;;   :init
;;   (activities-mode)
;;   ;; (activities-tabs-mode)
;;   ;; Prevent `edebug' default bindings from interfering.
;;   (setq edebug-inhibit-emacs-lisp-mode-bindings t)
;;   :hook (tab-bar-mode 'activities-tabs-mode)
;;   :bind
;;   (("C-x C-a C-n" . activities-new)
;;    ("C-x C-a C-d" . activities-define)
;;    ("C-x C-a C-a" . activities-resume)
;;    ("C-x C-a C-s" . activities-suspend)
;;    ("C-x C-a C-k" . activities-kill)
;;    ("C-x C-a RET" . activities-switch)
;;    ("C-x C-a b" . activities-switch-buffer)
;;    ("C-x C-a g" . activities-revert)
;;    ("C-x C-a l" . activities-list)))
;; (add-hook 'tab-bar-mode-hook 'activities-tabs-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (
         ;; ("s-g" . magit-status)
         ("C-c g" . magit-status)))

;(use-package libgit
;  :ensure t
;  :after magit)
;
;(use-package magit-libgit
;  :ensure t
;  :after (magit libgit))

;(use-package forge
;  :ensure t
;  :after magit)
;
;(use-package code-review
;  :after magit
;  :bind (:map forge-topic-mode-map ("C-c r" . #'code-review-forge-pr-at-point))
;  :bind (:map code-review-mode-map (("C-c n" . #'code-review-comment-jump-next)
;                                    ("C-c p" . #'code-review-comment-jump-previous))))

;; hack to eliminate weirdness
;(unless (boundp 'bug-reference-auto-setup-functions)
;  (defvar bug-reference-auto-setup-functions '()))

;; Use git-timemachine to browse historic versions of files
;; Bind it to C-c t
;; Make sure git timemachine toggles evil-local-mode and
;; display-line-numbers-mode
;; (use-package git-timemachine
;;   :ensure t
;;   :bind (("C-c t" . git-timemachine-toggle))
;;   :hook
;;  (git-timemachine-mode . evil-local-mode)
;;   ;; (git-timemachine-mode . meow-normal-mode)
;;   (git-timemachine-mode . display-line-numbers-mode)
;;   )

;; ;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
;; ;; http://blog.binchen.org/posts/use-git-timemachine-with-evil.html
;; (with-eval-after-load 'git-timemachine
;;  (evil-make-overriding-map git-timemachine-mode-map 'normal)
;;   force update evil keymaps after git-timemachine-mode loaded
;;  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


;; Define buffer-local variable
;; (defvar-local my-meow-desired-state 'motion
  ;; "Buffer-local variable to specify the desired Meow state.")

;; Function to set the buffer-local value of my-meow-desired-state
;; (defun my-meow-set-desired-state (state)
  ;; "Set the buffer-local variable 'my-meow-desired-state' to the specified state."
  ;; (setq-local my-meow-desired-state state))

;; Advice function to modify 'meow--mode-get-state' based on 'my-meow-desired-state'
;; (defun my-meow-mode-get-state-advice (orig-func &rest args)
  ;; "Advice function to modify 'meow--mode-get-state' based on 'my-meow-desired-state'."
  ;; (if my-meow-desired-state
      ;; my-meow-desired-state
    ;; (apply orig-func args)))

;; Apply advice to 'meow--mode-get-state'
;(advice-add 'meow--mode-get-state :around #'my-meow-mode-get-state-advice)

;; Hook to set my-meow-desired-state to 'motion' when entering git-timemachine mode
;; (defun my-meow-git-timemachine-hook ()
  ;; "Hook to set my-meow-desired-state to 'motion' when entering git-timemachine mode."
  ;; (my-meow-set-desired-state 'motion))

;; Check if git-timemachine is loaded and add the hook
;(when (featurep 'git-timemachine)
;  (add-hook 'git-timemachine-mode-hook 'my-meow-git-timemachine-hook))


;(use-package diff-hl :ensure t)
;(add-hook 'prog-mode-hook 'diff-hl-mode)
;; (setf (cdr (assq 'continuation fringe-indicator-alist))
      ;; '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      ;; )
;; https://ianyepan.github.io/posts/emacs-git-gutter/

;(fringe-mode '(1 . 1))
;(fringe-mode '(3 . 0))
(fringe-mode '(0 . 0))

;;(use-package git-gutter
;;  :ensure t
;;  :hook (prog-mode . git-gutter-mode)
;;  :config
;;  (setq git-gutter:update-interval 0.02)
;;  )
;;
;;(use-package git-gutter-fringe
;;  :ensure t
;;  :config
;;  (define-fringe-bitmap 'git-gutter-fr:added [0] nil nil '(center repeated))
;;  (define-fringe-bitmap 'git-gutter-fr:modified [0] nil nil '(center repeated))
;;  (define-fringe-bitmap 'git-gutter-fr:deleted [0] nil nil 'bottom)
;;  )
;;
;;(custom-set-variables
;; '(git-gutter:modified-sign "  ") ;; two space
;; '(git-gutter:added-sign "++")    ;; multiple character is OK
;; '(git-gutter:deleted-sign "--"))

;; Get rid of flycheck in the gutter and margins
;(setq flycheck-indication-mode nil)

;; Left fringe flycheck-indication-mode

;; (setq-default flycheck-indication-mode 'left-fringe)
;; (add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)

;; (use-package flycheck-checkbashisms
;;   :ensure t
;;   :config
;;   (flycheck-checkbashisms-setup))


;; Add these commands to embark directory map at some point
;; Also figure out how to use embark to add bookmarks to files

(use-package consult-ag
  :straight (consult-ag
             :type git
             :host github
             :repo "wigol/consult-ag"
             :branch "file-handler-support")
  :ensure t)


(use-package ag
  :ensure t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package markdown-mode
;;  :ensure t)
;;  :hook ((markdown-mode . visual-line-mode)))

;; (use-package yaml-mode
;;   :ensure t)

;; (use-package json-mode
;;   :ensure t)


;; https://old.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package google-c-style
  :ensure t
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))

;; use // instead of /* */ for comments
 (add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
 (add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style -1)))

;; Torvalds Linux-style from Documentation/CodingStyle
;; (defun c-lineup-arglist-tabs-only (ignored)
;;   "Line up argument lists by tabs, not spaces"
;;   (let* ((anchor (c-langelem-pos c-syntactic-element))
;; 	 (column (c-langelem-2nd-pos c-syntactic-element))
;; 	 (offset (- (1+ column) anchor))
;; 	 (steps (floor offset c-basic-offset)))
;;     (* (max steps 1)
;;        c-basic-offset)))

;; (add-hook 'c-mode-common-hook
;;           (lambda ()
;;             ;; Add kernel style
;;             (c-add-style
;;              "linux-tabs-only"
;;              '("linux" (c-offsets-alist
;;                         (arglist-cont-nonempty
;;                          c-lineup-gcc-asm-reg
;;                          c-lineup-arglist-tabs-only))))))

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (setq indent-tabs-mode t)
;;             (c-set-style "linux-tabs-only")))

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (let ((filename (buffer-file-name)))
;;               ;; Enable kernel mode for the appropriate files
;;               (when (and filename
;;                          (string-match (expand-file-name "~/src/linux-trees")
;;                                        filename))
;;                 (setq indent-tabs-mode t)
;;                 (c-set-style "linux-tabs-only")))))

;; (add-hook 'c-mode-hook
;;           (lambda ()
;;             (let ((filename (buffer-file-name)))
;;               ;; Enable kernel mode for the appropriate files
;;               (when (and filename
;;                          (string-match (expand-file-name "/var/linus")
;;                                        filename))
;;                 (setq indent-tabs-mode t)
;;                 (c-set-style "linux-tabs-only")))))


;; (defun my/c-mode-common-hook ()
;; ;  (c-set-style "Linux")
;;   (c-set-style "linux-tabs-only")
;;   (setq
;;    indent-tabs-mode nil
;;    ;indent-tabs-mode t
;;    c-basic-offset 4))

;; (defun my/c-ts-mode-common-hook ()
;; ;  (c-set-style "Linux")
;;   (c-ts-mode-set-style "linux-tabs-only")
;;   (setq
;;    indent-tabs-mode nil
;;    ;indent-tabs-mode t
;;    c-basic-offset 4))

;(add-hook 'c-mode-common-hook 'my/c-mode-common-hook)
;(add-hook 'c-ts-mode-hook 'my/c-ts-mode-common-hook)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

;; C-c C-C is a worthless binding for comments, so unbind it
;; Use C-x C-; instead or M-; for line comments
;; (define-key c-mode-base-map (kbd "C-c C-c") nil)
;(define-key c-ts-mode-map (kbd "C-c C-c") nil)
(define-key prog-mode-map (kbd "C-c C-c") nil)

(use-package smart-compile
  :ensure t)
;; (define-key c-mode-base-map (kbd "C-c C-c") 'smart-compile)
(define-key prog-mode-map (kbd "C-c C-c") 'smart-compile)
;(define-key c-ts-mode-map (kbd "C-c C-c") 'smart-compile)
;(add-hook 'c-mode-common-hook 
;          (lambda () (define-key c-mode-base-map (kbd "C-c C-c") 'compile)))

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Lsp stuff
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package format-all
  :ensure t
  )
(defvar-local my/format-buffer-function 'format-all-buffer
  "Function to call in order to format the current buffer.")
(defun my/format-buffer ()
  "Run `my/format-buffer-function' to format the current buffer."
  (interactive)
  (funcall my/format-buffer-function))
(bind-key "C-c f f" 'my/format-buffer)


;; (use-package consult-flycheck
;;   :ensure t)

(add-to-list 'load-path "~/.emacs.d/localpkgs/")

;; (add-hook 'c-mode-hook (lambda () (setq flycheck-clang-language-standard "c99")))
;; (add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

;; (add-to-list 'auto-mode-alist '("\\.upphtml\\'" . shell-script-mode))
;; (add-to-list 'auto-mode-alist '("\\.uppcss\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.upphtml\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.uppcss\\'" . css-mode))

;; (use-package eldoc-box
  ;; :ensure t
  ;; :config
  ;; (set-face-attribute 'eldoc-box-border nil
                      ;; :background "darkgray")
  ;; (set-face-attribute 'eldoc-box-markdown-separator nil
                      ;; :foreground "darkgray")
;; )

(setq eldoc-echo-area-use-multiline-p nil)
;;
;; (setq lsp-eldoc-enable-hover nil)
(setq eldoc-echo-area-prefer-doc-buffer t)

;; LSP RUST
;; (use-package rust-mode :ensure t)
;(add-hook 'rust-mode-hook 'eglot-ensure)
;; (add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

;(setq lsp-rust-analyzer-server-display-inlay-hints t)

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

;; (use-package zig-mode
;;   :ensure t)

;; (use-package vterm
;;   :ensure t
;;   :config
;;   (setq vterm-max-scrollback 10000)
;;   (setq vterm-shell "bash")
;;   (setq vterm-kill-buffer-on-exit t)
;;   (setq vterm-always-compile-module t)
;;   (setq vterm-buffer-name-string "vterm %s")
;;   )

;; ;; vterm
;; (use-package vterm-toggle
;;   :ensure t
;;   :config
;;   (setq vterm-toggle-reset-window-configration-after-exit t)
;;   (setq vterm-toggle-hide-method 'reset-window-configration)
;;   )

;(use-package multi-vterm
;  :ensure t
;)

;; add keybind C-c e for eshell and C-c s for vterm
(global-set-key (kbd "C-c v") 'vterm)
(global-set-key (kbd "C-c e") 'eat)
(global-set-key (kbd "C-c s") 'eshell)

;; (define-key global-map (kbd "<f4>") #'vterm-toggle)

;(define-key vterm)
;(define-key vterm-mode-map [?\C-c] nil)
;; (define-key vterm-mode-map (kbd "<f4>") 'vterm-toggle)
;; (define-key vterm-mode-map (kbd "<f5>") 'ef-themes-toggle)

;; (use-package cmake-mode
  ;; :ensure t)

;; (use-package cmake-project
  ;; :ensure t)

;; (autoload 'cmake-project-mode "cmake-project" nil t)

;; (defun maybe-cmake-project-mode ()
  ;; (if (or (file-exists-p "CMakeLists.txt")
          ;; (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      ;; (cmake-project-mode)))

;; (add-hook 'c-mode-hook 'maybe-cmake-project-mode)
;; (add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

;; (setq spice-simulator "Ngspice"
;;       spice-waveform-viewer "ngplot")
;; (load "~/.emacs.d/spice-mode.el")

;; ;; ngspice -b file.ng

;; (use-package spice-mode
;;   :ensure t
;;   :config
;;   (setq spice-simulator "Ngspice"
;;       spice-waveform-viewer "ngplot"))

;; ;; :mode "\\.\\(js\\|jsx\\)\\'"
;; (add-to-list 'auto-mode-alist '("\\.ng\\'" . spice-mode))


;;;;;;;;;;;
;; EDIFF ;;
;;;;;;;;;;;

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defun local/change-commit-author (arg)
;;  "Change the commit author during an interactive rebase in Magit.
;; With a prefix argument, insert a new change commit author command
;; even when there is already another rebase command on the current
;; line.  With empty input, remove the change commit author action
;; on the current line, if any."
;;  (interactive "P")
;;  (let ((author
;;         (magit-transient-read-person "Select a new author for this commit"
;;                               nil
;;                               nil)))
;;    (git-rebase-set-noncommit-action
;;     "exec"
;;     (lambda (_) (if author
;;                     (format "git commit --amend --author='%s'" author)
;;                   ""))
;;     arg)))

;; (define-key git-rebase-mode-map (kbd "h") #'local/change-commit-author)


;; install phi-search and phi-search-mc using use-package

(use-package phi-search
  :ensure t)

(use-package phi-search-mc
  :ensure t)

;; install multiple-cursor

(use-package multiple-cursors
  :ensure t)

(use-package mc-extras
  :ensure t)


(define-key phi-search-default-map (kbd "C-, C-,") 'phi-search-mc/mark-here)


(define-key phi-search-default-map (kbd "C-, M-C-f") 'mc/mark-next-sexps)
(define-key phi-search-default-map (kbd "C-, M-C-b") 'mc/mark-previous-sexps)
(define-key phi-search-default-map (kbd "C-, <") 'mc/mark-all-above)
(define-key phi-search-default-map (kbd "C-, >") 'mc/mark-all-below)

(define-key phi-search-default-map (kbd "C-, C-d") 'mc/remove-current-cursor)
(define-key phi-search-default-map (kbd "C-, C-k") 'mc/remove-cursors-at-eol)
(define-key phi-search-default-map (kbd "C-, d")   'mc/remove-duplicated-cursors)
(define-key phi-search-default-map (kbd "C-, C-o") 'mc/remove-cursors-on-blank-lines)

;(define-key phi-search-default-map (kbd "C-, C-,") 'mc/freeze-fake-cursors-dwim)

(define-key phi-search-default-map (kbd "C-, .")   'mc/move-to-column)
(define-key phi-search-default-map (kbd "C-, =")   'mc/compare-chars)

;; Emacs 24.4+ comes with rectangle-mark-mode.
;(define-key rectangle-mark-mode-map (kbd "C-, C-,") 'mc/rect-rectangle-to-multiple-cursors)

;(define-key cua--rectangle-keymap   (kbd "C-, C-,") 'mc/cua-rectangle-to-multiple-cursors)

(phi-search-mc/setup-keys)
(add-hook 'isearch-mode-hook 'phi-search-from-isearch-mc/setup-keys)


;; (use-package indent-bars
  ;; :ensure t
  ;; :hook ((prog-mode) . indent-bars-mode)) ; or whichever modes you prefer

;; An Emacs "jump to definition" package for 50+ languages
(use-package dumb-jump
  :straight t
  :after xref
  :custom
  (dumb-jump-selector 'completing-read)
  :init
  ;; Use `dumb-jump' as `xref' backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

;; Combine multiple Xref backends
(use-package xref-union
  :straight t
  :commands (xref-union-mode)
  :custom
  ;; BUG+HACK: When in `xref-union-mode', the `xref-union--backend' seems to
  ;; access all the backends, including the ones that aren't enabled locally.
  ;; The list includes `etags' which is annoying since it asks about the TAGS
  ;; file, which interrupts `xref-union' from trying the rest of the backends.
  ;; So, lets exclude `etags' since I'm not using it, the predicate function can
  ;; be modified to check for other conditions (for example: enable `etags' in
  ;; some circumstances)
  (xref-union-excluded-backends #'+xref-union--exclude-backends-predicate)
  :config
  (defun +xref-union--exclude-backends-predicate (backend)
    (memq backend '(etags--xref-backend))))


;; (use-package dumb-jump
;;   :ensure t)

;; (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
;; (setq dumb-jump-quiet t)

;; (use-package dumb-jump
  ;; :ensure t
  ;; :bind (("M-g o" . dumb-jump-go-other-window)
         ;; ("M-g j" . dumb-jump-go)
         ;; ("M-g b" . dumb-jump-back)
         ;; ("M-g q" . dumb-jump-quick-look)
         ;; ("M-g x" . dumb-jump-go-prefer-external)
         ;; ("M-g z" . dumb-jump-go-prefer-external-other-window))
  ;; :config (setq dumb-jump-selector 'ivy))

;; (use-package indent-bars
  ;; :ensure t
  ;; :hook ((prog-mode) . indent-bars-mode)) ; or whichever modes you prefer

(use-package drag-stuff
  :ensure t)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

;; (use-package move-text 
  ;; :ensure t  
  ;; :config (move-text-default-bindings))

(use-package auto-header
  :ensure t)
(add-hook 'c-mode-hook #'auto-header-mode)

(use-package go-mode
  :ensure t)

;; (use-package smartparens
  ;; :ensure t
  ;; :hook ((prog-mode . smartparens-mode))
  ;; :config
  ;; (require 'smartparens-config))


;; fix python


(use-package python
:config
(defun python-info-current-defun () nil)
)

(global-eldoc-mode -1)

;; Out of the box code execution from editing buffer
(use-package quickrun
  :straight t
  :bind (([f5] . quickrun)))

(setq quickrun-focus-p nil)

;; Emacs headerline indication of where you are in a large project
(use-package breadcrumb
  :straight t
  :hook ((c-mode c++-mode c-ts-base-mode python-base-mode rust-ts-mode sh-mode bash-ts-mode) . breadcrumb-local-mode)
  :config
  ;; Don't show the project/file name in the header by just a file icon
  (with-eval-after-load 'nerd-icons
    (advice-add
     'breadcrumb-project-crumbs :override
     (defun +breadcrumb--project:override-a ()
       (concat " " (if-let* ((file buffer-file-name))
                       (nerd-icons-icon-for-file file)
                     (nerd-icons-icon-for-mode major-mode)))))))
