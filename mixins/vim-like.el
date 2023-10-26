;;; Emacs Bedrock
;;;
;;; Mixin: Vim emulation

;;; Usage: Append or require this file from init.el for bindings in Emacs.

;;; Contents:
;;;
;;;  - Core Packages

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Core Packages
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Evil: vi emulation
(use-package evil
  :ensure t
  :init
  (setq evil-respect-visual-line-mode t)
  (setq evil-undo-system 'undo-redo)
  ;(setq evil-want-keybinding nil)       ; prep to load evil-collection
  :config
  ;(evil-mode)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs))

(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(evil-set-undo-system 'undo-redo)


(define-key evil-normal-state-map (kbd "C-e") 'evil-local-mode)
(global-set-key (kbd "C-e") 'evil-local-mode)


;(add-hook 'prog-mode-hook 'evil-local-mode)

(setq evil-insert-state-message nil)

(defun turn-buffer-modeline-green ()
  (set-face-foreground 'mode-line-inactive "white")
  (set-face-foreground 'mode-line "#00ff00"))

(defun turn-buffer-modeline-white ()
  (set-face-foreground 'mode-line-inactive "white")
  (set-face-foreground 'mode-line "#ffffff"))

(defun turn-buffer-modeline-blue ()
  (set-face-foreground 'mode-line-inactive "white")
  (set-face-foreground 'mode-line "orange"))

(defun do--status-bar-change-mode-line-color ()
  (cond
   ((memq evil-state '(hybrid insert emacs))
    (turn-buffer-modeline-green))
   ((memq evil-state '(visual))
    (turn-buffer-modeline-blue))
   (t
    (turn-buffer-modeline-white))))

(add-hook 'post-command-hook 'do--status-bar-change-mode-line-color)
(add-hook 'windmove-do-window-select 'do--status-bar-change-mode-line-color)
(add-hook 'find-file-hook 'do--status-bar-change-mode-line-color)
(add-hook 'dired-sidebar-mode-hook 'evil-normal-state)


(defun dotspacemacs/user-config ()
  (add-hook 'evil-hybrid-state-entry-hook 'turn-buffer-modeline-green)
  (add-hook 'evil-hybrid-state-exit-hook 'turn-buffer-modeline-white))


;; Highlight yanked region
;; Source: https://blog.meain.io/2020/emacs-highlight-yanked/
(defun meain/evil-yank-advice (orig-fn beg end &rest args)
  "Advice to be added to `evil-yank' to highlight yanked region.
Pass ORIG-FN, BEG, END, TYPE, ARGS."
  (pulse-momentary-highlight-region beg end 'mode-line)
  (apply orig-fn beg end args))
(advice-add 'evil-yank :around 'meain/evil-yank-advice)
