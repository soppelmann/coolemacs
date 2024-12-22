


;; add consult-ag from git

;;also add ffio

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
   ("C-c p s" . consult-ag)
   ("C-c p o" . consult-project-extra-find-other-window)))


;; project-x using straight to github

(use-package project-x
  :straight (project-x :type git :host github :repo "karthink/project-x")
  :after project
  :config
  (setq project-x-save-interval 600)    ;Save project state every 10 min
  (project-x-mode 1))
