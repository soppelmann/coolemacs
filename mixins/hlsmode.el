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
        ; lsp
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

;; we use M-TAB for completion
(define-key verilog-ext-mode-map (kbd "M-TAB") nil) 

;(setq verilog-ext-tags-backend 'tree-sitter)
(setq verilog-ext-tags-backend 'builtin)

(setq verilog-ext-project-alist
      `(("icp2" ; Project name
         :root "~/Documents/Courses/ICP2/Labs/" ; Root directory of project
         :dirs ("-r Lab3_4") ; -r to add directories recursively
         :files ("*.sv"
                 "**/*.sv"
                 "**/*.svh"
                 "**/**/*.sv"
                 "**/**/*.svh"
                 )
         ;:ignore-dirs ("src/ignored_ip")
         ;:ignore-files ("sources/SPHD110420.v")
         ;:compile-cmd "make tb_top" ; command used to compile current project
         ;; `vhier' related properties
         ;:command-file "commands.f" ; vhier command file
         :lib-search-path nil)))    ; list of dirs to look for include directories or libraries

; (use-package verilog-ts-mode
;  :ensure t
;   :mode (("\\.s?vh?\\'" . verilog-ts-mode))
;  )
