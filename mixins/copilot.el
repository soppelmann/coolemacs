(use-package copilot
  :straight (:host github :repo "copilot-emacs/copilot.el" :files ("dist" "*.el"))
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




(use-package gptel
  :ensure t
  :config
  (add-hook 'gptel-post-response-functions 'gptel-end-of-response)
  ;; (add-hook 'gptel-post-stream-hook 'gptel-auto-scroll)
  (setq
   gptel-model "llama3.2"
   gptel-backend (gptel-make-ollama "Ollama"
                   :host "bifrost:11434"
                   :stream t
                   :models '(
                             granite3.1-dense:latest
                             command-r:latest
                             llama3.1:8b
                             reader-lm:latest
                             hf.co/sm54/FuseO1-DeepSeekR1-QwQ-SkyT1-Flash-32B-Preview-Q4_K_M-GGUF:latest
                             ;; llama3.3:latest
                             dolphin-mixtral:latest
                             gemma:latest
                             qwen2.5-coder:latest
                             deepseek-coder-v2:latest
                             qwq:latest
                             llama3.2:latest
                             )))
  (setq gptel-org-branching-context t)
  )

(global-set-key (kbd "C-c C-g") 'gptel-menu)
(define-key gptel-mode-map (kbd "C-c C-c") #'gptel-send)


(defun my-gptel-deepseek-wrap-think-block (beg end)
   "Wrap '<think>' blocks in an Org-mode drawer if not already wrapped."
   (interactive)
    (when (derived-mode-p 'org-mode)
      (save-excursion
        (goto-char beg)
        ;; Find all occurrences of <think> blocks
        (while (re-search-forward "^<think>" end t)
          (let ((start (line-beginning-position)))
            ;; Check if the block is already wrapped
            (unless (save-excursion
                      (forward-line -4)
                      (looking-at "^:THINKING:$"))
              ;; Insert Org-mode drawer start
              (goto-char start)
              (kill-region (point) (line-end-position))
              (insert-and-inherit ":THINKING:\n")
              (insert-and-inherit
               "#+attr_shortcode: :title Thinking ...\n")
              (insert-and-inherit "#+begin_expand\n")
              (insert-and-inherit "<pre>")
              (forward-line 4)
              ;; Find the closing tag again after insertion
              (when (re-search-forward "</think>" nil t)
                (end-of-line)
                (kill-region (line-beginning-position) (point))
                (insert-and-inherit "</pre>")
                (end-of-line)
                ;; Ensure we don't add duplicate :END:
                (unless (looking-at "\n#+end_expand")
                  (insert-and-inherit "\n#+end_expand\n")
                  (insert-and-inherit ":END:\n"))
                ;; Move back to the start of the drawer for org-cycle
                (goto-char start)
                (org-cycle)))))))
    (message "Think blocks wrapped and folded."))

(add-hook
 'gptel-post-response-functions
 #'my-gptel-deepseek-wrap-think-block)
