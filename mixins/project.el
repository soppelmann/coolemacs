

;; Projectile like project management library built on Emacs' `project'
(use-package projection
  :straight t
  :hook (ibuffer . ibuffer-projection-set-filter-groups)
  :after project
  :demand
  :bind-keymap ("C-x P" . projection-map)
  :init
  ;; This ensures that `ibuffer-projection-set-filter-groups' takes effect
  (add-hook 'ibuffer-hook (lambda () (run-at-time 0.1 nil (lambda () (call-interactively #'ibuffer-update)))))
  ;; Mark compile commands as safe (customized in ".dir-locals.el")
  (dolist (var '(projection-commands-configure-project projection-commands-build-project
                 projection-commands-test-project projection-commands-run-project
                 projection-commands-package-project projection-commands-install-project))
    (put var 'safe-local-variable #'stringp))
  ;; Enable `projection-hook', adds the possibility to run functions in per-project basis
  (global-projection-hook-mode 1))


;;also add ffip
;; Quick access to project files using `fd'
(use-package find-file-in-project
  :straight t
  :custom
  (ffip-use-rust-fd (and (executable-find "fd") t))
  :config
  (require 'project) ; for `project--file-completion-table'
  (advice-add ;; This adds `nerd-icons-completion-mode' support for `ffip'
   'ffip-completing-read :override
   (defun +ffip-completing-read (prompt collection &optional action)
     (when-let* ((selected
                  (if (= 1 (length collection))
                      (car collection)
                    (let ((sel (completing-read prompt (project--file-completion-table collection))))
                      (or (assoc sel collection) sel)))))
       (let* ((default-directory (ffip-get-project-root-directory))
              (result (if (consp selected) (cdr selected) selected)))
         (if action (funcall action result) result))))))




(setq project-switch-commands
      '((?f "Find file" project-find-file)
        (?b "Switch to buffer" project-switch-to-buffer)
        (?s "Shell" project-shell)))


; bind C-x p f to ffip
(global-set-key (kbd "C-x p f") 'find-file-in-project)

;; add consult-project-extra
(use-package consult-project-extra
  :ensure t
  :bind
  (("C-c p f" . consult-project-extra-find)
   ("C-c p s" . consult-ripgrep)
   ("C-c p o" . consult-project-extra-find-other-window)))


;; project-x using straight to github

(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))
