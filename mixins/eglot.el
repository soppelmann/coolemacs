(use-package eglot

  ;; Use :bind to have C-, run eldoc-box-help-at-point
  :bind
  ("C-," . eldoc-box-help-at-point)

  ;;:hook (
  ;;;;       ;;(rust-mode . eglot-ensure)
  ;;       ;;(eglot-managed-mode-hook . flycheck-mode)
  ;;       )

  ;; these caused verilog to not work
  :custom
  ;; (eglot-send-changes-idle-time 0.1)
   (eglot-ignored-server-capabilities
    '(;:hoverProvider
      ;; :documentHighlightProvider
      ;:documentFormattingProvider
      ;:documentRangeFormattingProvider
      ;:documentOnTypeFormattingProvider
      ;; :colorProvider
      :inlayHintProvider
      ;; :foldingRangeProvider
      ))

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

(use-package flymake-collection 
  :ensure t 
  :hook ((after-init . flymake-collection-hook-setup) 
         (emacs-lisp-mode . flymake-mode)))

(use-package eglot-booster
  :straight (:host github :repo "jdtsmith/eglot-booster")
  :after eglot
  :init
  (when (executable-find "emacs-lsp-booster")
    (eglot-booster-mode 1)))

  ;; (defun my/orderless-dispatch-flex-first (_pattern index _total)
    ;; (and (eq index 0) 'orderless-flex))
;; Consult integration with Eglot
(use-package consult-eglot
  :straight t)

;; a bit hacky, we want verilog-ext-capf only for verilog mode and NOT regular eglot modes
;; (defun my/eglot-capf ()
;;   (unless (derived-mode-p 'verilog-mode)
;;     (setq-local completion-at-point-functions
;;                 (list (cape-capf-super
;;                        #'cape-file
;;                        #'yasnippet-capf
;;                        ;; #'verilog-ext-capf
;;                        #'eglot-completion-at-point
;;                        )))))

;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;; (defun my/verilog-capf ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-capf-super
;;                      #'cape-file
;;                      #'yasnippet-capf
;;                      #'verilog-ext-capf
;;                      ))))

;; (add-hook 'verilog-ext-mode-hook #'my/verilog-capf)
;; (add-hook 'verilog-ts-mode-hook #'my/verilog-capf)


(with-eval-after-load 'eglot
  (defun my/eglot-capf ()
    ;; (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    (setq-local completion-at-point-functions
		(list (cape-capf-super
		       #'eglot-completion-at-point
                       #'verilog-ext-capf
		       #'yasnippet-capf
		       #'cape-file))))

)
(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
