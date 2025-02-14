(require 'tramp)

;; Load consult-tramp
(load "~/.emacs.d/consult-tramp.el")

(setq enable-remote-dir-locals t)
(setq tramp-use-ssh-controlmaster-options nil)

(defun my-vc-off-if-remote ()
  (if (file-remote-p (buffer-file-name))
      (setq-local vc-handled-backends nil)))
(add-hook 'find-file-hook 'my-vc-off-if-remote)
(setq vc-follow-symlinks t)

(setq vc-handled-backends nil)
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq tramp-default-method "scpx")


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
