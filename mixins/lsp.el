(defun verilog-eglot-hook ()
  ;(company-mode)
  (yas-minor-mode 1)
  (eglot-ensure)
  ;(add-to-list 'eglot-server-programs '(verilog-mode . ("svls"))))
  (add-to-list 'eglot-server-programs '(verilog-mode . ("vls"))))
(add-hook 'verilog-mode-hook 'verilog-eglot-hook)

(use-package verilog-ext
  :ensure t
  :after verilog-mode
  :demand
  :hook ((verilog-mode . verilog-ext-mode))
  :init
   ;;  - Verilog Ext Feature List (provides info of different features)
   ;; Comment out/remove the ones you do not need
  (setq verilog-ext-feature-list
        '(font-lock
          xref
          capf
          hierarchy
          eglot
          ;lsp
          flycheck
          beautify
          navigation
          template
;          formatter
;          compilation
          imenu
          which-func
          hideshow
          typedefs
          time-stamp
          block-end-comments
;          company-keywords
          ports))
  :config
  (verilog-ext-mode-setup))

(use-package verilog-ts-mode
  :ensure t
  :mode (("\\.s?vh?\\'" . verilog-ts-mode)))

(defun vhdl-eglot-hook ()
  (eglot-ensure)
  (vhdl-ext-eglot-set-server 've-hdl-checker)
  )
(add-hook 'vhdl-mode-hook 'vhdl-eglot-hook)

(use-package vhdl-ext
  :after vhdl-mode
  :ensure t
  :demand
  :hook ((vhdl-mode . vhdl-ext-mode))
  :init
  ;; Can also be set through `M-x RET customize-group RET vhdl-ext':
  ;;  - Vhdl Ext Feature List (provides info of different features)
  ;; Comment out/remove the ones you do not need
  (setq vhdl-ext-eglot-set-server 've-hdl-checker)
  (setq vhdl-ext-feature-list
        '(font-lock
          hierarchy
          eglot
          ;lsp
          flycheck
          beautify
          navigation
          template
          compilation
          imenu
          which-func
          hideshow
          ;time-stamp
          ;company-keywords
          ports))
  :config
  (vhdl-ext-mode-setup))

;; To use `vhdl-ts-mode' as the default major-mode also add the lines below:
;(use-package vhdl-ts-mode
;  :ensure t
;  :mode (("\\.vhdl?\\'" . vhdl-ts-mode))
;                                        )
