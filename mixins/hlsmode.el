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
        ; eglot
         lsp
         flycheck
         ;beautify
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

;; we use M-TAB for completion

(setq verilog-ext-flycheck-verible-rules '("-line-length"
                                           "+parameter-type-name-style"))


;; put entire path here instead
(setq verilog-ext-flycheck-verilator-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
(setq verilog-ext-flycheck-verilator-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))
(setq verilog-ext-flycheck-svlang-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
(setq verilog-ext-flycheck-svlang-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))
(setq verilog-ext-flycheck-iverilog-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
(setq verilog-ext-flycheck-iverilog-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))
(setq verilog-ext-flycheck-slang-include-path `(,(file-name-concat (getenv "UVM_HOME") "src")))
(setq verilog-ext-flycheck-slang-file-list `(,(file-name-concat (getenv "UVM_HOME") "src/uvm_pkg.sv")))

; set variable verilog-ext-flycheck-linter to slang
(setq verilog-ext-flycheck-linter 'slang)

(setq verilog-ext-tags-backend 'tree-sitter)
;(setq verilog-ext-tags-backend 'builtin)

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
         ;; `vhier' related properties
         ;:command-file "commands.f" ; vhier command file
         :lib-search-path nil)))    ; list of dirs to look for include directories or libraries

; (use-package verilog-ts-mode
;  :ensure t
;   :mode (("\\.s?vh?\\'" . verilog-ts-mode))
;  )
