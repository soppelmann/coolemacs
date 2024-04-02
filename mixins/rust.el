(use-package rustic
  :ensure t
  :custom
  (rustic-analyzer-command '("rustup" "run" "stable" "rust-analyzer")) ;eglot
  )

;(setq rustic-analyzer-command '("/usr/bin/env rust-analyzer")) ;for lsp-mode

(setq rustic-lsp-client 'eglot)

(add-hook 'eglot--managed-mode-hook (lambda () (flymake-mode -1)))

;(with-eval-after-load "lsp-rust"
; (lsp-register-client
;  (make-lsp-client
;   :new-connection (lsp-stdio-connection
;                    (lambda ()
;                      `(,(or (executable-find
;                              (cl-first lsp-rust-analyzer-server-command))
;                             (lsp-package-path 'rust-analyzer)
;                             "rust-analyzer")
;                        ,@(cl-rest lsp-rust-analyzer-server-args))))
;   :remote? t
;   :major-modes '(rust-mode rustic-mode)
;   :initialization-options 'lsp-rust-analyzer--make-init-options
;   :notification-handlers (ht<-alist lsp-rust-notification-handlers)
;   :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
;   :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
;   :after-open-fn (lambda ()
;                    (when lsp-rust-analyzer-server-display-inlay-hints
;                      (lsp-rust-analyzer-inlay-hints-mode)))
;   :ignore-messages nil
;   :server-id 'rust-analyzer-remote)))
;
;(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
;  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
;  ;; On remote hosts, the local `shell-file-name' might be useless.
;  (let ((command (mapconcat 'identity args " ")))
;    (funcall start-file-process-shell-command name buffer command)))

;(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

;(custom-set-faces
;  '(rustic-compilation-column ((t (:inherit compilation-column-number))))
;  '(rustic-compilation-line ((t (:foreground "LimeGreen")))))

;(push 'rustic-clippy flycheck-checkers)
;(setq rustic-flycheck-clippy-params "--message-format=json")

(evil-set-initial-state 'rustic-popup-mode 'emacs)

;(remove-hook 'rustic-mode-hook 'flycheck-mode)
