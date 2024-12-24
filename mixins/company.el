; yas minor mode and company


;; (use-package company
;;   :ensure
;;   :defer 0.1
;;   ;:bind
;;   ;(:map
;;   ; company-active-map
;;   ; ("C-n" . company-select-next)
;;   ; ("C-p" . company-select-previous)
;;   ; ("M-<" . company-select-first)
;;   ; ("M->" . company-select-last)
;;   ; ("M-<tab>" . company-complete-selection)
;;   ; ("M-TAB" . company-complete-selection))
;;   :config
;;   (global-company-mode t)
;;   (setq-default
;;    company-idle-delay 0.05
;;    company-require-match nil
;;    company-minimum-prefix-length 0

;;    ;; get only preview
;;    ;company-frontends '(company-preview-frontend)
;;    ;; also get a drop down
;;    ;company-frontends '(company-pseudo-tooltip-frontend company-preview-frontend)
;;    company-frontends '(company-pseudo-tooltip-frontend)
;;    )


;;   )


(use-package company
  :ensure t
  )


;; Enable Company by default in programming buffers
(add-hook 'prog-mode-hook #'company-mode)
(add-hook 'company-mode-hook 'yas-minor-mode)
;'(add-to-list 'company-backends 'company-pseudo-tooltip-frontend)
;'(add-to-list 'company-backends 'company-preview-frontend)
;'(add-to-list 'company-backends 'company-irony)
