;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Projectile
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile: project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;(setq projectile-project-search-path '("~/projects/"))
  ;(setq projectile-completion-system 'consult)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-globally-ignored-directories
        (append '(
                  ".git"
                  ".svn"
                  ".hg"
                  ".bzr"
                  "node_modules"
                  "build"
                  "dist"
                  "target"
                  "bin"
                  "obj"
                  "out"
                  "build"
                  "buildroot"
                  )))
  )

;; Bind key C-c p A to add a project on the global map
(define-key global-map (kbd "C-c p A") 'projectile-add-known-project)

;; Bind key C-c p B to remove a project
(define-key global-map (kbd "C-c p B") 'projectile-remove-known-project)

;; Add these commands to embark directory map at some point
;; Also figure out how to use embark to add bookmarks to files


;; consult-project-extra is also an alternative
;(use-package consult-projectile
;  :ensure t
;  )

;; (use-package consult-ag
  ;; :ensure t
  ;; )

;; (defun my-consult-projectile-ag ()
;;   "Run a consult-ag search in the project."
;;   (interactive)
;;   (require 'projectile)
;;   (let* ((ignores (unless (eq (projectile-project-vcs) 'git)
;;                     ;; ag supports git ignore files
;;                     (append
;;                      (projectile-ignored-files-rel) (projectile-ignored-directories-rel)
;;                      (projectile--globally-ignored-file-suffixes-glob)
;;                      grep-find-ignored-files grep-find-ignored-directories)))
;;          (ignores-args (apply #'append
;;                               (mapcar (lambda (item) (list "--ignore" item)) ignores))))
;;     (funcall-interactively #'consult-ag
;;                            (if-let ((s (symbol-at-point)))
;;                                (symbol-name s)
;;                              "")
;;                            (projectile-project-root)
;;                            ignores-args)))

;; (let ((projectile-switch-project-action #'my-consult-projectile-ag)
;;   (call-interactively #'projectile-switch-project)))

(use-package ag
  :ensure t)

;; Why is this a thing
;; (use-package projectile-ripgrep
;;   :ensure t
;;   )


;; Recommended keymap prefix on macOS
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Need to overwrite proctile ripgrep cause it sucks
(define-key projectile-mode-map (kbd "C-c p s r") 'consult-ripgrep)

;(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

;; (use-package consult-project-extra
;;   :ensure t
;; )

;; use consult and ivy for projectile, i dont use this
;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (counsel-projectile-mode))

;; (autoload 'projectile-project-root "projectile")
;; (setq consult-project-function (lambda (_) (projectile-project-root)))
