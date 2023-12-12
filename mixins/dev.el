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
 :hook
 ;; Auto parenthesis matching
 (
  (prog-mode . electric-pair-mode)
  (prog-mode . rainbow-delimiters-mode)
  )
  )

(use-package fancy-narrow
  :ensure t
  :hook ((prog-mode . fancy-narrow-mode))
)
;;(add-hook 'prog-mode-hook 'fancy-narrow-mode)


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



;(use-package diff-hl :ensure t)
;(add-hook 'prog-mode-hook 'diff-hl-mode)
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )
;; https://ianyepan.github.io/posts/emacs-git-gutter/

;(fringe-mode '(1 . 1))
(fringe-mode '(3 . 0))

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
(setq flycheck-indication-mode nil)

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

;; use // instead of /* */ for comments
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style -1)))

;; C-c C-C is a worthless binding for comments, so unbind it
;; Use C-x C-; instead or M-; for line comments
(define-key c-mode-base-map (kbd "C-c C-c") nil)

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


(use-package consult-flycheck
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/localpkgs/")

(add-hook 'c-mode-hook (lambda () (setq flycheck-clang-language-standard "c99")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

(add-to-list 'auto-mode-alist '("\\.upphtml\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.uppcss\\'" . shell-script-mode))

(use-package eldoc-box
  :ensure t
  )

 (setq eldoc-echo-area-use-multiline-p nil)
;;
;; (setq lsp-eldoc-enable-hover nil)
(setq eldoc-echo-area-prefer-doc-buffer t)

;; LSP RUST
(use-package rust-mode :ensure t)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq lsp-rust-analyzer-server-display-inlay-hints t)



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

;;(use-package which-key-posframe
;;  :ensure t
;;  :config
;;  (which-key-posframe-mode))

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

;(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
(setq vterm-tramp-shells '(("docker" "sh")
                           ("scpx" "'zsh'")
                           ("ssh" "'zsh'")))
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
