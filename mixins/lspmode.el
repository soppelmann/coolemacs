;https://emacs-lsp.github.io/lsp-mode/tutorials/how-to-turn-off/

(use-package lsp-mode
  :custom
  (lsp-completion-provider :none) ;; we use Corfu!
  :init
  ;; (defun my/orderless-dispatch-flex-first (_pattern index _total)
    ;; (and (eq index 0) 'orderless-flex))

  (defun my/lsp-mode-setup-completion ()
    ;; (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          ;; '(orderless))

    ;; Optionally configure the first word as flex filtered.
    ;; (add-hook 'orderless-style-dispatchers #'my/orderless-dispatch-flex-first nil 'local)
    ;; (setq-local completion-at-point-functions
    ;;             (list (cape-capf-super
    ;;                    #'lsp-completion-at-point
    ;;                    #'yasnippet-capf
    ;;                    ;; #'verilog-ext-capf
    ;;                    #'cape-file
    ;;                    )))

    (setq-local completion-at-point-functions
	        (list
	         (cape-capf-buster
                  (cape-capf-super
                   #'lsp-completion-at-point
                   #'yasnippet-capf
                   #'verilog-ext-capf
                   #'cape-file
                   )
                  'equal)
	         ))
    
    ;; Optionally configure the cape-capf-buster. Add more here
      ;; (setq-local completion-at-point-functions (list (cape-capf-buster #'lsp-completion-at-point)))
      )

  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  (setq lsp-inhibit-message t)
  (setq lsp-message-project-root-warning t)
  (setq lsp-keymap-prefix "C-c l")

  ;; this tends to mess up lenses
  ;; (setq lsp-auto-guess-root t)

  ;; avoid having the doc box pop up all the time
  (setq lsp-ui-doc-enable nil)

  ;  Sideline code actions * disable whole sideline via 
  (setq lsp-ui-sideline-enable nil)

  ;  * hide code actions
 ; (setq lsp-ui-sideline-show-code-actions nil)

  ;header
  (setq lsp-headerline-breadcrumb-enable nil)

  ; Sideline hover symbols * disable whole sideline via 
  (setq lsp-ui-sideline-enable nil)

  ; hide only hover symbols
  (setq lsp-ui-sideline-show-hover nil)
  ; dont show function signature in echo bar
  (setq lsp-eldoc-enable-hover nil)

  (global-set-key (kbd "C-c l e") 'lsp)
  ;; (global-set-key (kbd "C-c l e") 'lsp-workspace-restart)

  ;completion
  ;; (setq lsp-completion-show-kind nil)
  ;; (setq lsp-completion-show-detail nil)
  (setq lsp-ui-doc-position 'at-point)

  (setq lsp-enable-symbol-highlighting nil)

  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         ;(prog-mode . lsp)
         ;(c-mode . lsp)
         ;(c++-mode . lsp)
         ;; if you want which-key integration
         (lsp-completion-mode . my/lsp-mode-setup-completion)
         (lsp-completion-mode . lsp-enable-which-key-integration))
  :commands lsp)

;; setup completion
;; (setq lsp-completion-provider :none)
;; (defun corfu-lsp-setup ()
;;   (setq-local completion-styles '(orderless)
;;               completion-category-defaults nil))
;; (add-hook 'lsp-mode-hook #'corfu-lsp-setup)

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
;(add-hook 'prog-mode-hook 'lsp-deferred) ;; Less chatty for unsupported modes (setq lsp-warn-no-matched-clients nil)

;; Only autostart lsp for local projects
;; (defun lsp/notramp ()
;;   "Run specific actions only if the current buffer is not a Tramp buffer."
;;   (unless (tramp-tramp-file-p (or buffer-file-name default-directory))
;;     ;; Place your actions here
;;     (message "This is not a Tramp buffer. Running my-hook-function actions.")
;;     'lsp-deferred
;;     ))

;; (add-hook 'prog-mode-hook 'lsp/notramp)

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
;; (with-eval-after-load "lsp-rust"
;;  (lsp-register-client
;;   (make-lsp-client
;;    :new-connection (lsp-stdio-connection
;;                     (lambda ()
;;                       `(,(or (executable-find
;;                               (cl-first lsp-rust-analyzer-server-command))
;;                              (lsp-package-path 'rust-analyzer)
;;                              "rust-analyzer")
;;                         ,@(cl-rest lsp-rust-analyzer-server-args))))
;;    :remote? t
;;    :major-modes '(rust-mode rustic-mode)
;;    :initialization-options 'lsp-rust-analyzer--make-init-options
;;    :notification-handlers (ht<-alist lsp-rust-notification-handlers)
;;    :action-handlers (ht ("rust-analyzer.runSingle" #'lsp-rust--analyzer-run-single))
;;    :library-folders-fn (lambda (_workspace) lsp-rust-library-directories)
;;    :after-open-fn (lambda ()
;;                     (when lsp-rust-analyzer-server-display-inlay-hints
;;                       (lsp-rust-analyzer-inlay-hints-mode)))
;;    :ignore-messages nil
;;    :server-id 'rust-analyzer-remote)))

(defun start-file-process-shell-command@around (start-file-process-shell-command name buffer &rest args)
  "Start a program in a subprocess.  Return the process object for it. Similar to `start-process-shell-command', but calls `start-file-process'."
  ;; On remote hosts, the local `shell-file-name' might be useless.
  (let ((command (mapconcat 'identity args " ")))
    (funcall start-file-process-shell-command name buffer command)))

(advice-add 'start-file-process-shell-command :around #'start-file-process-shell-command@around)

(defun lsp-booster--advice-json-parse (old-fn &rest args)
  "Try to parse bytecode instead of json."
  (or
   (when (equal (following-char) ?#)
     (let ((bytecode (read (current-buffer))))
       (when (byte-code-function-p bytecode)
         (funcall bytecode))))
   (apply old-fn args)))
(advice-add (if (progn (require 'json)
                       (fboundp 'json-parse-buffer))
                'json-parse-buffer
              'json-read)
            :around
            #'lsp-booster--advice-json-parse)

(defun lsp-booster--advice-final-command (old-fn cmd &optional test?)
  "Prepend emacs-lsp-booster command to lsp CMD."
  (let ((orig-result (funcall old-fn cmd test?)))
    (if (and (not test?)                             ;; for check lsp-server-present?
             (not (file-remote-p default-directory)) ;; see lsp-resolve-final-command, it would add extra shell wrapper
             lsp-use-plists
             (not (functionp 'json-rpc-connection))  ;; native json-rpc
             (executable-find "emacs-lsp-booster"))
        (progn
          (when-let ((command-from-exec-path (executable-find (car orig-result))))  ;; resolve command from exec-path (in case not found in $PATH)
            (setcar orig-result command-from-exec-path))
          (message "Using emacs-lsp-booster for %s!" orig-result)
          (cons "emacs-lsp-booster" orig-result))
      orig-result)))
(advice-add 'lsp-resolve-final-command :around #'lsp-booster--advice-final-command)
