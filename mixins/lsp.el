;;(defun verilog-eglot-hook ()
;;  ;(company-mode)
;;  (yas-minor-mode 1)
;;  (eglot-ensure)
;;  (add-to-list 'eglot-server-programs '(verilog-mode . ("svls"))))
;;  ;(add-to-list 'eglot-server-programs '(verilog-mode . ("vls"))))
;;(add-hook 'verilog-mode-hook 'verilog-eglot-hook)
;;
;;(use-package verilog-ext
;;  :ensure t
;;  :after verilog-mode
;;  :demand
;;  :hook ((verilog-mode . verilog-ext-mode))
;;  :init
;;   ;;  - Verilog Ext Feature List (provides info of different features)
;;   ;; Comment out/remove the ones you do not need
;;  (setq verilog-ext-feature-list
;;        '(font-lock
;;          xref
;;          capf
;;          hierarchy
;;          eglot
;;          ;;lsp
;;          flycheck
;;          beautify
;;          navigation
;;          template
;;;          formatter
;;;          compilation
;;          imenu
;;          which-func
;;          hideshow
;;          typedefs
;;          time-stamp
;;          block-end-comments
;;;          company-keywords
;;          ports))
;;  :config
;;  (verilog-ext-mode-setup))
;;
;;;;(use-package verilog-ts-mode
;;  ;;:ensure t
;;  ;; :mode (("\\.s?vh?\\'" . verilog-ts-mode))
;;;;  )
;;
;;(defun vhdl-eglot-hook ()
;;  (eglot-ensure)
;;  (vhdl-ext-eglot-set-server 've-hdl-checker)
;;  )
;;(add-hook 'vhdl-mode-hook 'vhdl-eglot-hook)
;;
;;(use-package vhdl-ext
;;  :after vhdl-mode
;;  :ensure t
;;  :demand
;;  :hook ((vhdl-mode . vhdl-ext-mode))
;;  :init
;;  ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
;;  ;;  - Vhdl Ext Feature List (provides info of different features)
;;  ;; Comment out/remove the ones you do not need
;;  (setq vhdl-ext-eglot-set-server 've-hdl-checker)
;;  (setq vhdl-ext-feature-list
;;        '(font-lock
;;          hierarchy
;;          eglot
;;          ;lsp
;;          flycheck
;;          beautify
;;          navigation
;;          template
;;          compilation
;;          imenu
;;          which-func
;;          hideshow
;;          ;time-stamp
;;          ;company-keywords
;;          ports))
;;  :config
;;  (vhdl-ext-mode-setup))

;; To use `vhdl-ts-mode' as the default major-mode also add the lines below:
;(use-package vhdl-ts-mode
;  :ensure t
;  :mode (("\\.vhdl?\\'" . vhdl-ts-mode))
;                                        )

(use-package eglot
  ;; no :ensure t here because it's built-in

  ;; Configure hooks to automatically turn-on eglot for selected modes
  ; :hook
  ; (((python-mode ruby-mode elixir-mode) . eglot))

  :hook (
         ;;(rust-mode . eglot-ensure)
         (eglot-managed-mode . flycheck-eglot-mode))

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


;; LSP RUST
(use-package rust-mode :ensure t)
(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

(setq lsp-rust-analyzer-server-display-inlay-hints t)

;; (unless (package-installed-p 'eglot)
;;   (package-install 'eglot))
 (setq eldoc-echo-area-use-multiline-p nil)
;;
;; (setq lsp-eldoc-enable-hover nil)
(setq eldoc-echo-area-prefer-doc-buffer t)
;; (setq lsp-signature-auto-activate nil)
