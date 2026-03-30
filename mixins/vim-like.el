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
  (setq evil-want-keybinding nil)       ; prep to load evil-collection
  (setq evil-want-fine-undo t)
  (setq evil-ex-search-persistent-highlight nil)
  :config
  (evil-mode 1)

  ;; Configuring initial major mode for some modes
  (evil-set-initial-state 'vterm-mode 'emacs)
  )

;; Evil Collection: evil bindings for many modes
(use-package evil-collection
  :after evil
  :ensure t
  :config
  (evil-collection-init))

(global-set-key [remap evil-quit] 'kill-buffer-and-window)
(evil-set-undo-system 'undo-redo)

(define-key evil-normal-state-map (kbd "C-.") nil)
(define-key evil-insert-state-map (kbd "C-.") nil)

;; Unbind C-o from evil normal map
;; (define-key evil-normal-state-map (kbd "C-o") nil)
;; (define-key evil-motion-state-map (kbd "C-o") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "C-p") nil)

(setq evil-insert-state-message nil)

;; (defun turn-buffer-modeline-green ()
;;   (set-face-foreground 'mode-line-inactive "white")
;;   (set-face-foreground 'mode-line "#00ff00"))

;; (defun turn-buffer-modeline-white ()
;;   (set-face-foreground 'mode-line-inactive "white")
;;   (set-face-foreground 'mode-line "#ffffff"))

;; (defun turn-buffer-modeline-blue ()
;;   (set-face-foreground 'mode-line-inactive "white")
;;   (set-face-foreground 'mode-line "orange"))

;; (defun do--status-bar-change-mode-line-color ()
;;   (cond
;;    ((memq evil-state '(hybrid insert emacs))
;;     (turn-buffer-modeline-green))
;;    ((memq evil-state '(visual))
;;     (turn-buffer-modeline-blue))
;;    (t
;;     (turn-buffer-modeline-white)
;;     )
;;    ))

;; (add-hook 'post-command-hook 'do--status-bar-change-mode-line-color)
;; (add-hook 'windmove-do-window-select 'do--status-bar-change-mode-line-color)
;; (add-hook 'find-file-hook 'do--status-bar-change-mode-line-color)
;; (add-hook 'dired-sidebar-mode-hook 'evil-normal-state)

;; Highlight yanked region
;; Source: https://blog.meain.io/2020/emacs-highlight-yanked/
;; (defun meain/evil-yank-advice (orig-fn beg end &rest args)
;;   "Advice to be added to `evil-yank' to highlight yanked region.
;; Pass ORIG-FN, BEG, END, TYPE, ARGS."
;;   (pulse-momentary-highlight-region beg end 'mode-line)
;;   (apply orig-fn beg end args))
;; (advice-add 'evil-yank :around 'meain/evil-yank-advice)

(use-package evil-goggles
  :ensure t
  :config
  (evil-goggles-mode)

  ;; optionally use diff-mode's faces; as a result, deleted text
  ;; will be highlighed with `diff-removed` face which is typically
  ;; some red color (as defined by the color theme)
  ;; other faces such as `diff-added` will be used for other actions
  (evil-goggles-use-diff-faces)
  )



(use-package hardtime
  :config
  (defun evil-hardtime-check-command ()
    "Return non-nil if the currently executed command should be checked."
    (memq this-command '( next-line previous-line evil-previous-visual-line
                          right-char left-char left-word right-word
                          evil-forward-char evil-backward-char
                          evil-next-line evil-previous-line)))
  :custom
  (hardtime-predicate #'evil-hardtime-check-command)
  :commands hardtime-mode
  :hook (prog-mode . hardtime-mode)
  :init
  (dolist (hook '(prog-mode-hook org-agenda-mode-hook org-mode-hook))
    (add-hook hook #'hardtime-mode))
  )


;; (use-package pulsar
;;   :hook (after-init . pulsar-global-mode)
;;   :config
;;   (setopt pulsar-pulse t
;;           pulsar-delay 0.055
;;           pulsar-iterations 12
;;           pulsar-highlight-face 'pulsar-yellow
;;           pulsar-face 'pulsar-cyan
;;           pulsar-pulse-functions '(ace-window
;;                                    backward-page
;;                                    bookmark-jump
;;                                    consult--jump
;;                                    delete-other-windows
;;                                    delete-window
;;                                    evil-delete
;;                                    evil-delete-line
;;                                    evil-jump-item
;;                                    evil-scroll-down
;;                                    evil-scroll-line-down
;;                                    evil-scroll-line-up
;;                                    evil-scroll-page-down
;;                                    evil-scroll-page-up
;;                                    evil-scroll-up
;;                                    evil-window-down
;;                                    evil-window-left
;;                                    evil-window-right
;;                                    evil-window-rotate-downwards
;;                                    evil-window-rotate-upwards
;;                                    evil-window-split
;;                                    evil-window-up
;;                                    evil-window-vsplit
;;                                    evil-yank
;;                                    evil-yank-line
;;                                    forward-page
;;                                    goto-char
;;                                    handle-switch-frame
;;                                    move-to-window-line-top-bottom
;;                                    next-buffer
;;                                    org-backward-heading-same-level
;;                                    org-forward-heading-same-level
;;                                    org-next-visible-heading
;;                                    org-previous-visible-heading
;;                                    other-window
;;                                    outline-backward-same-level
;;                                    outline-forward-same-level
;;                                    outline-next-visible-heading
;;                                    outline-previous-visible-heading
;;                                    outline-up-heading
;;                                    previous-buffer
;;                                    recenter
;;                                    recenter-top-bottom
;;                                    reposition-window
;;                                    scroll-down-command
;;                                    scroll-up-command
;;                                    switch-to-buffer
;;                                    switch-to-buffer-other-frame
;;                                    switch-to-buffer-other-tab
;;                                    switch-to-buffer-other-window
;;                                    tab-close
;;                                    tab-new
;;                                    tab-next
;;                                    windmove-down
;;                                    windmove-left
;;                                    windmove-right
;;                                    windmove-swap-states-down
;;                                    windmove-swap-states-left
;;                                    windmove-swap-states-right
;;                                    windmove-swap-states-up
;;                                    windmove-up
;;                                    )))
