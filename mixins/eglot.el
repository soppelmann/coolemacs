(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  ;; Use :bind to have C-, run eldoc-box-help-at-point
  :bind
  ("C-," . eldoc-box-help-at-point)

  ;;:hook (
  ;;;;       ;;(rust-mode . eglot-ensure)
  ;;       ;;(eglot-managed-mode-hook . flycheck-mode)
  ;;       )

  ;; these caused verilog to not work
  ;; :custom
  ;; (eglot-send-changes-idle-time 0.1)
  ;;  (eglot-ignored-server-capabilities
  ;;   '(;:hoverProvider
  ;;     :documentHighlightProvider
  ;;     ;:documentFormattingProvider
  ;;     ;:documentRangeFormattingProvider
  ;;     ;:documentOnTypeFormattingProvider
  ;;     :colorProvider
  ;;     :inlayHintProvider
  ;;     :foldingRangeProvider))

  :config
  (fset #'jsonrpc--log-event #'ignore)  ; massive perf boost---don't log every event
  ;; Sometimes you need to tell Eglot where to find the language server
  ;;(add-hook 'eglot-managed-mode-hook #'eldoc-box-hover-at-point-mode t)

  ;; (add-hook 'eldoc-mode-hook
  ;;        'eldoc-box-hover-at-point-mode)

  ;;(add-hook 'eldoc-mode-hook
  ;;          'eldoc-box-hover-mode)

  ;(global-set-key (kbd "C-,") #'eldoc-box-help-at-point)

  ;; (add-to-list 'eglot-stay-out-of 'flymake)

  (add-to-list 'eglot-server-programs '(nix-mode . ("rnix-lsp")))

  (add-to-list 'eglot-server-programs '(php-mode "intelephense" "--stdio"))

  ; (add-to-list 'eglot-server-programs
  ;              '(haskell-mode . ("haskell-language-server-wrapper" "--lsp")))
  )

(setq eglot-sync-connect nil)

;; (use-package flycheck-eglot
  ;; :ensure t
  ;; :after (flycheck eglot)
  ;; :config
  ;; (global-flycheck-eglot-mode 1))

;; Enable LSP support by default in programming buffers
;(add-hook 'prog-mode-hook #'eglot-ensure)

;; bind key C-c l e to eglot enable
(global-set-key (kbd "C-c l e") 'eglot)

;; bind key C-c l r to eglot rename
(global-set-key (kbd "C-c l r") 'eglot-rename)

;; bind key C-c l r to eglot fix
(global-set-key (kbd "C-c l l") 'eglot-code-actions)



;; (defun my/eglot-capf ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-capf-super
;;                      #'eglot-completion-at-point
;;                      #'cape-file
;;                      (cape-company-to-capf #'company-yasnippet)))))

;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;; Option 1: Specify explicitly to use Orderless for Eglot
;; (setq completion-category-overrides '((eglot (styles orderless))))

;; Option 2: Undo the Eglot modification of completion-category-defaults
;(with-eval-after-load 'eglot
;   (setq completion-category-defaults nil))


;; LSP RUST
;(use-package rust-mode :ensure t)
;(add-hook 'rust-mode-hook 'eglot-ensure)
;(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))
;
;(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; (unless (package-installed-p 'eglot)
;;   (package-install 'eglot))
; (setq eldoc-echo-area-use-multiline-p nil)
;;
;; (setq lsp-eldoc-enable-hover nil)
;(setq eldoc-echo-area-prefer-doc-buffer t)
;; (setq lsp-signature-auto-activate nil)

;(add-to-list 'load-path "~/.emacs.d/elisp/eglot-x.el")
;
;(with-eval-after-load 'eglot
;  (require 'eglot-x)
;  (eglot-x-setup))

(use-package flymake-collection 
  :ensure t 
  :hook ((after-init . flymake-collection-hook-setup) 
         (emacs-lisp-mode . flymake-mode)))
