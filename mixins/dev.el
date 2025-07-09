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
  ;; (prog-mode . which-function-mode) ;; show current function in modeline
  )
  )

(setq which-func-unknown "n/a")

(use-package blacken
  :ensure t
  :hook ((python-mode . blacken-mode)))


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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (
         ("C-c g" . magit-status)))

(with-eval-after-load 'magit-mode
  (add-hook 'after-save-hook 'magit-after-save-refresh-status t))

(use-package forge
  :after magit
  :ensure t
  :config
  (setq auth-sources '("~/.authinfo"))
  (setq ghub-graphql-items-per-request 50)
  (setq forge-topic-list-limit  -1))

(fringe-mode '(0 . 0))

;; Get rid of flycheck in the gutter and margins
;; (setq flycheck-indication-mode nil)

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


;; https://old.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package google-c-style
  :ensure t
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))

;; use // instead of /* */ for comments
 (add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
 (add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style -1)))

(setq-default indent-tabs-mode nil)
(setq-default tab-width 8)

(use-package smart-compile
  :ensure t)
;; (define-key c-mode-base-map (kbd "C-c C-c") 'smart-compile)
(define-key prog-mode-map (kbd "C-x c") 'smart-compile)
;; (global-set-key (kbd "C-x c") 'smart-compile)
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
  ;; :custom
  ;; (dumb-jump-selector 'completing-read)
  :init
  ;; Use `dumb-jump' as `xref' backend
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))


(use-package dumb-jump
  :ensure t
  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate))

(setq dumb-jump-force-searcher 'rg)

(use-package drag-stuff
  :ensure t)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(use-package go-mode
  :ensure t)


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
;; (use-package breadcrumb
;;   :straight t
;;   :hook ((c-mode c++-mode c-ts-base-mode python-base-mode rust-ts-mode sh-mode bash-ts-mode) . breadcrumb-local-mode)
;;   :config
;;   ;; Don't show the project/file name in the header by just a file icon
;;   (with-eval-after-load 'nerd-icons
;;     (advice-add
;;      'breadcrumb-project-crumbs :override
;;      (defun +breadcrumb--project:override-a ()
;;        (concat " " (if-let* ((file buffer-file-name))
;;                        (nerd-icons-icon-for-file file)
;;                      (nerd-icons-icon-for-mode major-mode)))))))

;; Emacs text actions using LSP symbol information
(use-package gambol
  :straight (:host codeberg :repo "woolsweater/gambol.el")
  :hook (eglot-managed-mode . gambol-mode)
  :bind
  (("M-g ," . gambol:go-to-previous)
   ("M-g ." . gambol:go-to-next)
   ([remap mc/mark-all-dwim] . gambol:edit-all)
   ([remap occur] . +gambol:occur-dwim)
   :map gambol-repeat-map
   ("," . gambol:go-to-previous)
   ("." . gambol:go-to-next)
   ("e" . gambol:edit-all)
   ("o" . gambol:occur))
  :init
  (with-eval-after-load 'embark (gambol:install-embark-integration)) ; Integrate with `embark'
  (defun +gambol:occur-dwim ()
    "Call `gambol:occur' if in an Eglot managed buffer, fallback to `occur'."
    (interactive)
    (unless (and (featurep 'eglot) (eglot-managed-p) (ignore-errors (gambol:occur) t))
      (call-interactively #'occur))))

;; Structured editing and navigation in Emacs with Tree-Sitter
(use-package combobulate-setup
  :straight (combobulate
             :host github
             :repo "mickeynp/combobulate"
             :nonrecursive t ; Cloning the `html-ts-mode' submodule causes problems
             :files (:defaults (:exclude "combobulate.el"))) ; TEMP: The "combobulate.el" contains a lot of autoloads that prevent lazy loading
  :custom
  (combobulate-key-prefix "C-c b") ; "C-c o" is used by `minemacs-open-thing-map'
  :config
  ;; TEMP+FIX: Basically, load the same features that would be loaded by "combobulate.el"
  (dolist (feature '(combobulate-rules
                     combobulate-procedure combobulate-navigation
                     combobulate-manipulation combobulate-envelope combobulate-display
                     combobulate-ui combobulate-misc combobulate-query combobulate-cursor
                     combobulate-toml combobulate-html combobulate-python combobulate-js-ts
                     combobulate-css combobulate-yaml combobulate-json combobulate-go))
    (require feature))

  ;; The "M-<up/down/left/right>" keys are used globally by `drag-stuff', lets
  ;; unset them for `combobulate' and use "M-S-<up/down/left/right>" instead.
  (mapc (lambda (k) (keymap-unset combobulate-key-map k 'remove)) '("M-<up>" "M-<down>" "M-<left>" "M-<right>"))
  (keymap-set combobulate-key-map "M-S-<up>" #'combobulate-splice-up)
  (keymap-set combobulate-key-map "M-S-<down>" #'combobulate-splice-down)
  (keymap-set combobulate-key-map "M-S-<left>" #'combobulate-splice-self)
  (keymap-set combobulate-key-map "M-S-<right>" #'combobulate-splice-parent))


(defun reverse-selection (beg end)
 "Reverse characters between BEG and END."
 (interactive "r")
 (let ((region (buffer-substring beg end)))
   (delete-region beg end)
   (insert (nreverse region))))

;; Insert paths into the minibuffer prompt
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :package vertico
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))
