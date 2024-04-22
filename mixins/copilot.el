(use-package copilot
  :straight (:host github :repo "zerolfx/copilot.el" :files ("dist" "*.el"))
  :ensure t)
;; you can utilize :map :hook and :config to customize copilot

;(add-hook 'prog-mode-hook 'copilot-mode)
(define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)
(define-key copilot-completion-map (kbd "<backtab>") 'copilot-next-completion)
(define-key copilot-completion-map (kbd "S-<tab>") 'copilot-next-completion)


;; (defun local/no-copilot-mode ()
;;   "Helper for `local/no-copilot-modes'."
;;   (copilot-mode -1))

;; (defvar local/no-copilot-modes '(shell-mode
;;                               inferior-python-mode
;;                               eshell-mode
;;                               term-mode
;;                               vterm-mode
;;                               comint-mode
;;                               compilation-mode
;;                               debugger-mode
;;                               dired-mode-hook
;;                               compilation-mode-hook
;;                               flutter-mode-hook
;;                               minibuffer-mode-hook)
;;   "Modes in which copilot is inconvenient.")

;; (defun local/copilot-disable-predicate ()
;;   "When copilot should not automatically show completions."
;;   (or local/copilot-manual-mode
;;       (member major-mode local/no-copilot-modes)
;;       (company-active-p)))

;; (add-to-list 'copilot-disable-predicates #'local/copilot-disable-predicate)

(defvar local/copilot-manual-mode nil
  "When `t' will only show completions when manually triggered, e.g. via M-s-<return>.")

(defun local/copilot-change-activation ()
  "Switch between three activation modes:
- automatic: copilot will automatically overlay completions
- manual: you need to press a key (M-C-<return>) to trigger completions
- off: copilot is completely disabled."
  (interactive)
  (if (and copilot-mode local/copilot-manual-mode)
      (progn
        (message "deactivating copilot")
        (global-copilot-mode -1)
        (setq local/copilot-manual-mode nil))
    (if copilot-mode
        (progn
          (message "activating copilot manual mode")
          (setq local/copilot-manual-mode t))
      (message "activating copilot mode")
      (global-copilot-mode))))

(define-key global-map (kbd "M-s-<escape>") #'local/copilot-change-activation)

(defun local/copilot-quit ()
  "Run `copilot-clear-overlay' or `keyboard-quit'. If copilot is
cleared, make sure the overlay doesn't come back too soon."
  (interactive)
  (condition-case err
      (when copilot--overlay
        (lexical-let ((pre-copilot-disable-predicates copilot-disable-predicates))
          (setq copilot-disable-predicates (list (lambda () t)))
          (copilot-clear-overlay)
          (run-with-idle-timer
           1.0
           nil
           (lambda ()
             (setq copilot-disable-predicates pre-copilot-disable-predicates)))))
    (error handler)))

(advice-add 'keyboard-quit :before #'local/copilot-quit)

(setq copilot-indent-offset-warning-disable t)
