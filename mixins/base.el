;;; Emacs Bedrock
;;;
;;; Mixin: Base UI enhancements

;;; Usage: Append or require this file from init.el to enable various UI/UX
;;; enhancements.

;;; Contents:
;;;
;;;  - Motion aids
;;;  - Power-ups: Embark and Consult
;;;  - Minibuffer and completion

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("s-j"   . avy-goto-char-timer)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line))    ; orig. isearch
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  (setq consult-narrow-key "<"))

(use-package embark
  :ensure t
  :demand t
  :after avy
  :bind (("C-c a" . embark-act))        ; bind this to an easy key to hit
  :init
  ;; Add the option to run embark when using avy
  (defun bedrock/avy-action-embark (pt)
    (unwind-protect
        (save-excursion
          (goto-char pt)
          (embark-act))
      (select-window
       (cdr (ring-ref avy-ring 0))))
    t)

  ;; After invoking avy-goto-char-timer, hit "." to run embark at the next
  ;; candidate you select
  (setf (alist-get ?. avy-dispatch-alist) 'bedrock/avy-action-embark))

(use-package embark-consult
  :ensure t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer and completion
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Vertico: better vertical completion for minibuffer commands
(use-package vertico
  :ensure t
  :init
  ;; You'll want to make sure that e.g. fido-mode isn't enabled
  (vertico-mode))

(use-package vertico-directory
  :after vertico
  :bind (:map vertico-map
              ("M-DEL" . vertico-directory-delete-word)))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

(use-package eshell
  :bind (("C-r" . consult-history)))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

(use-package rainbow-delimiters
  :ensure t)

(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode)
(add-hook 'ielm-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-interaction-mode-hook 'rainbow-delimiters-mode)
(add-hook 'lisp-mode-hook 'rainbow-delimiters-mode)

(use-package ranger
  :ensure t)
(setq ranger-show-hidden t)
(setq ranger-cleanup-on-disable t)

;; Dim windows
(use-package dimmer
 :ensure t
 :config
 (setq dimmer-fraction 0.3)
 (setq dimmer-adjustment-mode :foreground)
 (setq dimmer-use-colorspace :rgb)
 (dimmer-configure-which-key)
 (dimmer-configure-helm)
 (dimmer-mode 1))

(use-package dired-sidebar
 :ensure t
 :commands (dired-sidebar-toggle-sidebar)
 :init
 (add-hook
  'dired-sidebar-mode-hook
  (lambda ()
    (unless (file-remote-p default-directory)
      (auto-revert-mode))))
 :config
 (push 'toggle-window-split dired-sidebar-toggle-hidden-commands)
 (push 'rotate-windows dired-sidebar-toggle-hidden-commands)
 (setq dired-sidebar-use-one-instance t)
 (setq dired-sidebar-subtree-line-prefix "__")
 (setq dired-sidebar-use-term-integration t)
 (setq dired-use-ls-dired nil))

(use-package ibuffer-sidebar
  :ensure t
  :commands (ibuffer-sidebar-toggle-sidebar)
  :config
  (setq ibuffer-sidebar-use-custom-font t)
  )


(defvar +sidebar-toggle-flag t
  "Flag to toggle whether to run `ibuffer-update'.")

(defun +sidebar-toggle ()
  "Toggle both `dired-sidebar' and `ibuffer-sidebar'."
  (interactive)
  (dired-sidebar-toggle-with-current-directory)
  (ibuffer-sidebar-toggle-sidebar)
  (when +sidebar-toggle-flag
    (ibuffer-update nil t))
  (setq +sidebar-toggle-flag (not +sidebar-toggle-flag))
  (other-window 2)
  )

;(global-set-key (kbd "C-c d") 'dired-sidebar-toggle-with-current-directory)
;(global-set-key (kbd "C-c d") 'dired-sidebar-toggle-sidebar)
(global-set-key (kbd "C-c d") '+sidebar-toggle)




;; Omit files in dired
(setq dired-omit-files
      (rx
       (or (seq bol (?  ".") "#") ;; emacs autosave files
           (seq bol "." (not (any "."))) ;; dot-files
           (seq "~" eol) ;; backup-files
           (seq bol "CVS" eol) ;; CVS dirs
           )))

;; Toggle on hide by default
;(add-hook 'dired-mode-hook 'dired-omit-mode)


(setq winner-dont-bind-my-keys t)
;(setq winner-mode-map (make-sparse-keymap))
(winner-mode 1)
(setq winner-dont-bind-my-keys t)

(global-set-key (kbd "C-x C-z") 'winner-undo)
(global-set-key (kbd "C-x C-r") 'winner-redo)
