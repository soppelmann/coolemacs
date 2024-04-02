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
         ;;eglot
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

(setq verilog-ext-tags-backend 'builtin)

(setq verilog-ext-project-alist
      `(("ucontroller" ; Project name
         :root "~/Documents/Courses/ICP/MatrixMult/matrix_multiplication" ; Root directory of project
         :dirs ("-r simulation" ; -r to add directories recursively
                "-r sources")
         ;:ignore-dirs ("src/ignored_ip")
         :ignore-files ("sources/SPHD110420.v")
         ;:compile-cmd "make tb_top" ; command used to compile current project
         ;; `vhier' related properties
         ;:command-file "commands.f" ; vhier command file
         :lib-search-path nil)))    ; list of dirs to look for include directories or libraries

;; (use-package verilog-ts-mode
;;  :ensure t
;;   :mode (("\\.s?vh?\\'" . verilog-ts-mode))
;;  )
