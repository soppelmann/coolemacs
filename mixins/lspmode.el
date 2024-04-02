;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/


(use-package lsp-mode
  :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (setq lsp-keymap-prefix "C-c l")

  ;; this tends to mess up lenses
  (setq lsp-auto-guess-root t)
  ;; disable cruft
  ;; (setq lsp-completion-provider :none)

  ;; avoid having the doc box pop up all the time
  (setq lsp-ui-doc-enable nil)

  ;header
  (setq lsp-headerline-breadcrumb-enable nil)

  ; dont show function signature in echo bar
  ;(setq lsp-eldoc-enable-hover nil)

  ;completion
  (setq lsp-completion-show-kind nil)
  (setq lsp-completion-show-detail nil)
  (setq lsp-ui-doc-position 'at-point)


  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;(prog-mode . lsp)
         (c-mode . lsp)
         (c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; optionally
(use-package lsp-ui
  :ensure t
  :init
  (global-set-key (kbd "C-h ,") #'lsp-ui-doc-glance)
  (global-set-key (kbd "C-c l i") #'lsp-ui-imenu)

  :commands lsp-ui-mode)
;; if you are ivy user
;; (use-package lsp-ivy
;;   :ensure t
;;   :commands lsp-ivy-workspace-symbol)
;(use-package lsp-treemacs :commands lsp-treemacs-errors-list)

(setq lsp-warn-no-matched-clients nil)
(add-hook 'prog-mode-hook 'lsp-deferred) ;; Less chatty for unsupported modes (setq lsp-warn-no-matched-clients nil)


(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      lsp-idle-delay 0.1)  ;; clangd is fast

;(use-package dap-mode
;  :ensure t)

;(with-eval-after-load 'lsp-mode
;  (require 'dap-cpptools)
;  (require 'dap-gdb-lldb)
;  (yas-global-mode)
;  )

;; remote rust-analyzer
(with-eval-after-load "lsp-rust"
 (lsp-register-client
  (make-lsp-client
   :new-connection (lsp-stdio-connection
                    (lambda ()
                      `(,(or (executable-find
                              (cl-first lsp-rust-analyzer-server-command))
                             (lsp-package-path 'rust-analyzer)
                             "rust-analyzer")
                        ,@(cl-rest lsp-rust-analyzer-server-args))))
   :remote? t
   :major-modes '(rust-mode rustic-mode)
   :initialization-options 'lsp-rust-analyzer--make-init-options
   :notification-handlers (ht<-alist lsp-rust-notification-handlers)
   :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
   :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
   :after-open-fn (lambda ()
                    (when lsp-rust-analyzer-server-display-inlay-hints
                      (lsp-rust-analyzer-inlay-hints-mode)))
   :ignore-messages nil
   :server-id 'rust-analyzer-remote)))

(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)
