(use-package verilog-mode
  :straight (:repo "veripool/verilog-mode")
  :ensure t
  ;; :init
  ;; (setq verilog-indent-level 3)
  ;; (setq verilog-indent-level-module 0)
  ;; (setq verilog-indent-level-declaration 0)
  ;; (setq verilog-indent-level-behavioral 0)
  ;; (setq verilog-indent-level-directive 0)
  ;; (setq verilog-indent-lists t)
  ;; (setq verilog-cexp-indent 4)
  ;; (setq verilog-case-indent 4)
  ;; (setq verilog-auto-newline t)
  )

;; (use-package verilog-ts-mode
;;   :straight t
;;   :hook ((verilog-ts-mode . verilog-ext-mode))
;;   ;; :mode (("\\.s?vh?\\'" . verilog-ts-mode))
;;   )
;; (add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(verilog-mode . verilog-ts-mode))
;; (unless (treesit-language-available-p 'verilog)
  ;; (verilog-ts-install-grammar)
  ;; )
(add-to-list 'auto-mode-alist '("\\.s?vh?\\'" . verilog-mode))
(eval-after-load 'verilog-mode
  '(progn
     ;; same for all the electric-verilog-* commands in                
     ;; the mode's map (see verilog-mode.el)                      
     (define-key verilog-mode-map (kbd "RET") 'electric-newline-and-maybe-indent)))

;; (eval-after-load 'verilog-ts-mode
;;   '(progn
;;      ;; same for all the electric-verilog-* commands in                
;;      ;; the mode's map (see verilog-mode.el)                      
;;      (define-key verilog-ts-mode-map (kbd "RET") 'electric-newline-and-maybe-indent)))

(use-package verilog-ext
  ;; :straight t
  :ensure t
  ;; :straight (:repo "veripool/verilog-mode")
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
         lsp
         flycheck
         beautify
         navigation
         template
         formatter
         compilation
         imenu
         which-func
         hideshow
         typedefs
         time-stamp
         block-end-comments
         company-keywords
         ports))
 :config
 (verilog-ext-mode-setup))

(require 'verilog-ext)

(verilog-ext-lsp-set-server 've-svls) ;`eglot' config
;; (verilog-ext-eglot-set-server 've-svls) ;`eglot' config
;; (verilog-ext-eglot-set-server 've-svlangserver) ;`eglot' config

(setq verilog-ext-flycheck-verible-rules '("-line-length"
                                           "+parameter-type-name-style"))

;; put entire path here instead
;; (setq verilog-ext-flycheck-verilator-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
;; (setq verilog-ext-flycheck-verilator-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))
;; (setq verilog-ext-flycheck-svlang-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
;; (setq verilog-ext-flycheck-svlang-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))
;; (setq verilog-ext-flycheck-iverilog-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
;; (setq verilog-ext-flycheck-iverilog-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))
;; (setq verilog-ext-flycheck-slang-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
;; (setq verilog-ext-flycheck-slang-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))

; set variable verilog-ext-flycheck-linter to slang
;; (setq verilog-ext-flycheck-linter 'slang)

;; (setq verilog-ext-tags-backend 'tree-sitter)
(setq verilog-ext-tags-backend 'builtin)

(setq verilog-ext-project-alist
      `(("icp2" ; Project name
         :root "/Users/getz/Virtual/Shared/ICP2_Verification_LABS" ; Root directory of project
         :dirs ("-r ICP2") ; -r to add directories recursively
         :files ("*.sv"
                 "**/*.sv"
                 "**/*.svh"
                 "**/**/*.sv"
                 "**/**/*.svh"
                 "**/**/**/*.sv"
                 "**/**/**/*.svh"
                 )
         :ignore-dirs ("ICP2/work")
         ;:ignore-files ("sources/SPHD110420.v")
         ;:compile-cmd "make tb_top" ; command used to compile current project
         :lib-search-path nil)))

(setq verilog-ext-project-alist
      (append verilog-ext-project-alist
              `(("bch_coder" ; Reusable verification components
                 :root "~/Developer/git/bch_verilog"
                 :dirs ("-r ./")
                 :files ("*.v"
                         "*.vh")
                 ;; :ignore-dirs ("coverage"
                 ;; "waves")
                 ;; :compile-cmd "make vip"
                 :lib-search-path nil))))

(setq verilog-ext-project-alist
      (append verilog-ext-project-alist
              `(("ecc_thesus" ; Reusable verification components
                 :root "~/Documents/Courses/thesis/RTL/src"
                 :dirs ("-r ./")
                 :files ("*.sv"
                 "**/*.sv"
                 "**/*.svh"
                 "**/**/*.sv"
                 "**/**/*.svh"
                 "**/**/**/*.sv"
                 "**/**/**/*.svh"
                 )
                 ;; :ignore-dirs ("coverage"
                 ;; "waves")
                 ;; :compile-cmd "make vip"
                 :lib-search-path nil))))

;; (use-package outshine
  ;; :straight (outshine :fetcher github :repo "alphapapa/outshine")
  ;; :ensure t)

(use-package fpga
  :straight t
  :init
  (setq fpga-feature-list '(xilinx yosys)))
