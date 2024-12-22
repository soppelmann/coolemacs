;; Load consult-tramp
(load "~/.emacs.d/consult-tramp.el")
(setq enable-remote-dir-locals t)
(setq tramp-use-ssh-controlmaster-options nil)
(remove-hook 'find-file-hook 'vc-find-file-hook)
(setq tramp-default-method "scp")
