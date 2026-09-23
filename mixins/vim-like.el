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
(define-key evil-insert-state-map (kbd "C-a") nil)

;; Unbind C-o from evil normal map
;; (define-key evil-normal-state-map (kbd "C-o") nil)
;; (define-key evil-motion-state-map (kbd "C-o") nil)
(define-key evil-normal-state-map (kbd "C-p") nil)
(define-key evil-motion-state-map (kbd "C-p") nil)

(setq evil-insert-state-message nil)

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
