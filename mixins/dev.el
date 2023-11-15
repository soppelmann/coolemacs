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

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)


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
          (python-mode . python-ts-mode)))
  ;:hook
  ;; Auto parenthesis matching
  ;((prog-mode . electric-pair-mode))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)))



(use-package diff-hl :ensure t)
(add-hook 'prog-mode-hook 'diff-hl-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package markdown-mode
;;  :ensure t)
;;  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)


;; https://old.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package google-c-style
  :ensure t
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))


;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Eglot, the built-in LSP client for Emacs
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :hook (
         ;;(rust-mode . eglot-ensure)
         (eglot-managed-mode . flycheck-mode))

  :custom
  (eglot-send-changes-idle-time 0.1)

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ;;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)

  ;; (add-hook 'eldoc-mode-hook
  ;;        'eldoc-box-hover-at-point-mode)

  ;;(add-hook 'eldoc-mode-hook
  ;;          'eldoc-box-hover-mode)

  (global-set-key (kbd "C-h ,") #'eldoc-box-help-at-point)

  (add-to-list 'eglot-stay-out-of 'flymake)

  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))

  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))

  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )


;; Enable LSP support by default in programming buffers
(add-hook 'prog-mode-hook #'eglot-ensure)
(add-to-list 'auto-mode-alist '("\\.upphtml\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.uppcss\\'" . shell-script-mode))

(use-package eldoc-box
  :ensure t
  )


;;(use-package flymake-clippy
;;  :hook (rust-mode . flymake-clippy-setup-backend))
;;
;;(defun manually-activate-flymake ()
;;  (add-hook 'flymake-diagnostic-functions #'eglot-flymake-backend nil t)
;;  (flymake-mode 1))


(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package zig-mode
  :ensure t)

;; Posframe stuff, trying it out

(use-package posframe
  :ensure t)

(use-package ivy-posframe
  :ensure t
  :config
  (ivy-posframe-mode))

(use-package which-key-posframe
  :ensure t
  :config
  (which-key-posframe-mode))

;;(use-package dired-posframe
;;  :ensure t
;;  :config
;;  (dired-posframe-mode))

;;(use-package vertico-posframe
;;  :ensure t
;;  :config
;;  (vertico-posframe-mode))

(use-package transient-posframe
  :ensure t
  :config
  (transient-posframe-mode))

(load "~/.emacs.d/consult-tramp.el")

(use-package cmake-mode
  :ensure t)

(use-package cmake-project
  :ensure t)

(autoload 'cmake-project-mode "cmake-project" nil t)

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

(setq spice-simulator "Ngspice"
      spice-waveform-viewer "ngplot")
(load "~/.emacs.d/spice-mode.el")

;; ngspice -b file.ng

(use-package spice-mode
  :ensure t
  :config
  (setq spice-simulator "Ngspice"
      spice-waveform-viewer "ngplot"))

;; :mode "\\.\\(js\\|jsx\\)\\'"
(add-to-list 'auto-mode-alist '("\\.ng\\'" . spice-mode))
