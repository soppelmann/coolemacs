(require 'tramp)

;; Load consult-tramp
(load "~/.emacs.d/consult-tramp.el")

;; ============================================================================
;; TRAMP Performance Optimizations
;; ============================================================================

;; Reduce TRAMP verbosity for better performance (0=silent, 1=errors, 3=info, 10=debug)
(setq tramp-verbose 1)

;; Use SSH instead of scpx for better performance
;; (setq tramp-default-method "ssh")

;; Disable ControlMaster to avoid connection issues
(setq tramp-use-ssh-controlmaster-options nil)

;; Enable connection caching (reuse connections)
(setq tramp-persistency-file-name (expand-file-name "tramp" user-emacs-directory))

;; Increase timeout for slow connections
(setq tramp-connection-timeout 10)

;; Optimize remote file operations
(setq remote-file-name-inhibit-cache nil)  ; Enable caching
(setq tramp-completion-reread-directory-timeout nil)  ; Cache directory listings

;; Disable auto-revert for remote files (major performance drain)
(defun my-auto-revert-off-if-remote ()
  "Disable auto-revert for remote files."
  (when (and (buffer-file-name)
             (file-remote-p (buffer-file-name)))
    (setq-local auto-revert-mode nil)
    (setq-local global-auto-revert-mode nil)))
(add-hook 'find-file-hook 'my-auto-revert-off-if-remote)

;; Disable VC for remote files (major performance improvement)
(defun my-vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))
(add-hook 'find-file-hook 'my-vc-off-if-remote)

(setq vc-follow-symlinks t)
(setq vc-handled-backends nil)
(remove-hook 'find-file-hook 'vc-find-file-hook)

;; Disable projectile for remote files
(advice-add 'projectile-project-root :before-while
  (lambda (&optional dir)
    (not (file-remote-p (or dir default-directory)))))

;; Disable centaur-tabs processing for remote files
;; (with-eval-after-load 'centaur-tabs
;;   (defun my-centaur-tabs-off-if-remote ()
;;     "Disable centaur-tabs for remote files."
;;     (when (and (buffer-file-name)
;;                (file-remote-p (buffer-file-name)))
;;       (centaur-tabs-local-mode)))
;;   (add-hook 'find-file-hook 'my-centaur-tabs-off-if-remote))

;; Enable remote dir-locals
(setq enable-remote-dir-locals t)

;; Required for eglot to find lsp servers on remote
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)
;; (setq tramp-backup-directory-alist nil) ;;eshell cant open visual over tramp

;; Set up the popup shell
;; (add-to-list 'display-buffer-alist
;;              ;; *shell*  *eshell*  *eat*
;;              '("\\*\\(e?shell\\|eat\\)\\*"
;;                (display-buffer-in-side-window)
;;                (side . bottom)
;;                (slot . -1) ;; -1 == L  0 == Mid 1 == R
;;                (window-height . 0.33) ;; take 2/3 on bottom left
;;                (window-parameters
;;                 (no-delete-other-windows . nil))))



                                        ;(setq tramp-default-method "ssh")
;; (setq vterm-tramp-shells '(("docker" "/bin/sh")
                           ;; ("scpx" "/bin/sh")
                           ;; ("ssh" "/bin/sh")))



;; Speedup tramp

;; (setq vc-ignore-dir-regexp
;;       (format "\\(%s\\)\\|\\(%s\\)"
;;               vc-ignore-dir-regexp
;;               tramp-file-name-regexp))

;; (setq debug-ignored-errors
      ;; (cons 'remote-file-error debug-ignored-errors))
