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
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; which-key: shows a popup of available keybindings when typing a long key
(use-package which-key
  :ensure t
  :config
  (which-key-mode))

(use-package nerd-icons
  :ensure t
  )
;; icons with completion
(use-package nerd-icons-completion
  :ensure t
  :after (marginalia nerd-icons)
  :hook (marginalia-mode . nerd-icons-completion-marginalia-setup)
  )
(nerd-icons-completion-mode)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Motion aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package avy
  :ensure t
  :demand t
  :bind (("C-c j" . avy-goto-line)
         ("C-c l j" . avy-goto-line)
         ("C-c l w" . avy-goto-symbol-1)
         ("C-<tab>" . centaur-tabs-ace-jump)
         ("C-c l s"   . avy-goto-char-timer)))
(use-package link-hint
  :ensure t
  :bind
  ("C-c l o" . link-hint-open-link)
  ("C-c l c" . link-hint-copy-link))
;(define-key evil-normal-state-map (kbd "SPC f") 'link-hint-open-link)

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

;; Persist history over Emacs restarts. Vertico sorts by history position.
(use-package savehist
  :ensure t
  :init
  (savehist-mode))

;; Marginalia: annotations for minibuffer
(use-package marginalia
  :ensure t
  :config
  (marginalia-mode))

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

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(use-package ranger
  :ensure t)
(setq ranger-show-hidden t)
(setq ranger-cleanup-on-disable t)

(defun local/dired-mode-hook ()
  (local-set-key (kbd "<tab>") nil) ; Unbind Tab first
  (local-set-key (kbd "<tab>") 'dired-subtree-toggle))

(add-hook 'dired-mode-hook 'local/dired-mode-hook)

(global-set-key (kbd "C-x C-d") 'dired)
(global-set-key (kbd "C-x C-b") 'switch-to-buffer)

;; ;; Dont open hundreds of dired buffers
(setf dired-kill-when-opening-new-dired-buffer t)

;; Single dired buffer
(use-package dired-single
  :straight (dired-single :type git :host github :repo "emacsattic/dired-single")
  :commands dired-single-buffer dired-single-buffer-mouse
  :defines dired-mode-map
  :init
  (add-hook 'dired-mode-hook
            (lambda ()
              ;; Enable all commands
              (setq disabled-command-function nil)

              (define-key dired-mode-map [return] 'dired-single-buffer)
              ;; (define-key dired-mode-map [down-mouse-1] 'dired-single-buffer-mouse)
              (define-key dired-mode-map [^]
                (lambda ()
                  (interactive)
                  (dired-single-buffer "..")))

              ;; Auto-refresh dired on file change
              (auto-revert-mode)
              (setq-default auto-revert-interval 1)
              (auto-revert-set-timer))))


;; Colourful dired-mode
(use-package diredfl
  :ensure t
  :hook (dired-mode . diredfl-mode))

;; (use-package dired-k
;;   :ensure t)

;; ;; always execute dired-k when dired buffer is opened
;; (add-hook 'dired-after-readin-hook #'dired-k-no-revert)
;; ;; (add-hook 'dired-initial-position-hook 'dired-k)

(use-package dired-subtree
  :ensure t)

;; (use-package dired-hacks-utils
;;   :ensure t)

(use-package eat
  :straight (eat
             :type git
             :host codeberg
             :repo "akib/emacs-eat"
             :files ("*.el" ("term" "term/*.el") "*.texi"
                     "*.ti" ("terminfo/e" "terminfo/e/*")
                     ("terminfo/65" "terminfo/65/*")
                     ("integration" "integration/*")
                     (:exclude ".dir-locals.el" "*-tests.el")))
  :ensure t
  :config
  (eat-eshell-mode +1)
  (eat-eshell-visual-command-mode +1)
  ;; (eat-eshell-mode)
  ;; (setq eshell-visual-commands '())
 )

;; Clear commands eshell considers visual by default.
(setq eshell-visual-commands '()) ;; fixes tramp

(setq eat-minimum-latency 0.002)

  ;; Eat settings
(setq eat-kill-buffer-on-exit t
      eat-term-name "xterm-256color"  ;; easier than tryin to hack eat-term to work
      eat-shell "/bin/bash"
      eat-tramp-shells '(("docker" . "/bin/sh")
                         ("scpx" . "/bin/bash")
                         ("ssh" . "/bin/bash")))

(defun eshell/clear ()
  "Clear the eshell buffer."
  (let ((inhibit-read-only t))
    (erase-buffer)
    (eshell-send-input)))

;; Omit files in dired
(setq dired-omit-files
      (rx
       (or (seq bol (?  ".") "#") ;; emacs autosave files
           (seq bol "." (not (any "."))) ;; dot-files
           (seq "~" eol) ;; backup-files
           (seq bol "CVS" eol) ;; CVS dirs
           )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Power-ups: Embark and Consult
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Unbind C-c C-s keybinding from cc-mode
(add-hook 'c-mode-hook
          (lambda () (local-unset-key (kbd "C-c C-s"))))

;; Consult: Misc. enhanced commands
(use-package consult
  :ensure t
  ;; Other good things to bind: consult-ripgrep, consult-line-multi,
  ;; consult-history, consult-outline
  :bind (("C-x b" . consult-buffer) ; orig. switch-to-buffer
         ("M-y" . consult-yank-pop) ; orig. yank-pop
         ("C-s" . consult-line)     ; orig. isearch
         ("C-c C-s" . consult-ripgrep)
         )
  :config
  ;; Narrowing lets you restrict results to certain groups of candidates
  ;; (setq consult-narrow-key "<"))
)
(use-package embark
  :ensure t
  :demand t
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
	       '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
		 nil
		 (window-parameters (mode-line-format . none))))
  (setq embark-cycle-key "SPC")
  (setq embark-keymap-prompter-key "'")
  (setq embark-quit-after-action t)
  :after avy
  :bind (("C-c a" . embark-act)
         ("C-." . embark-act))        ; bind this to an easy key to hit
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
  :ensure t
  :bind (:map embark-become-file+buffer-map
              ("m" . consult-bookmark)
              ("b" . consult-buffer)
              ("j" . consult-find))
  :bind (:map embark-file-map
              ("x" . embark-open-externally))
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

;; Upload to envs.net
(use-package 0x0
  :ensure t
  ;:config
  ;(setq 0x0-default-server "envs.sh")
  )

;; Add action to embark keymap
(define-key embark-file-map (kbd "U") '0x0-upload-file)
(define-key embark-region-map (kbd "U") '0x0-dwim)

;; Might aswell add it to the general map instead
(define-key embark-heading-map (kbd "SPC") 'embark-cycle)
(define-key embark-identifier-map (kbd "SPC") 'embark-cycle)

(define-key embark-general-map (kbd "C-SPC") 'embark-select)

;; Add consult-bookmark as an action to embark bound to kb "b"
(define-key embark-region-map (kbd "b") 'consult-bookmark)
(define-key embark-defun-map (kbd "b") 'consult-bookmark)
(define-key embark-symbol-map (kbd "b") 'consult-bookmark)

;; Use which key for embark
(defun embark-which-key-indicator ()
  "An embark indicator that displays keymaps using which-key.
The which-key help message will show the type and value of the
current target followed by an ellipsis if there are further
targets."
  (lambda (&optional keymap targets prefix)
    (if (null keymap)
        (which-key--hide-popup-ignore-command)
      (which-key--show-keymap
       (if (eq (plist-get (car targets) :type) 'embark-become)
           "Become"
         (format "Act on %s '%s'%s"
                 (plist-get (car targets) :type)
                 (embark--truncate-target (plist-get (car targets) :target))
                 (if (cdr targets) "…" "")))
       (if prefix
           (pcase (lookup-key keymap prefix 'accept-default)
             ((and (pred keymapp) km) km)
             (_ (key-binding prefix 'accept-default)))
         keymap)
       nil nil t (lambda (binding)
                   (not (string-suffix-p "-argument" (cdr binding))))))))

(setq embark-indicators
  '(embark-which-key-indicator
    embark-highlight-indicator
    embark-isearch-highlight-indicator))

(defun embark-hide-which-key-indicator (fn &rest args)
  "Hide the which-key indicator immediately when using the completing-read prompter."
  (which-key--hide-popup-ignore-command)
  (let ((embark-indicators
         (remq #'embark-which-key-indicator embark-indicators)))
      (apply fn args)))

(advice-add #'embark-completing-read-prompter
            :around #'embark-hide-which-key-indicator)

;; End of which key for embark
(use-package ace-window
  :ensure t
  :init
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l))
  :bind (("C-x o" . ace-window)
         ;("C-o" . ace-window)
;         ("C-p" . ace-window)
         ))
;; Nice tabs
(use-package centaur-tabs
  :ensure t
  :hook ;; centaur-tabs-local-mode disables centaur-tabs-mode
  ;; we have it globally enabled but locally disable it
  (dashboard-mode . centaur-tabs-local-mode)
  (helpful-mode . centaur-tabs-local-mode)
  (ibuffer-mode . centaur-tabs-local-mode)
  (magit-mode . centaur-tabs-local-mode)
  (org-agenda-mode . centaur-tabs-local-mode)
  (org-mode . centaur-tabs-local-mode)
  (org-journal-mode . centaur-tabs-local-mode)
  (org-roam-mode . centaur-tabs-local-mode)
  (pdf-view-mode . centaur-tabs-local-mode)
  (treemacs-mode . centaur-tabs-local-mode)
  (vterm-mode . centaur-tabs-local-mode)
  (web-mode . centaur-tabs-local-mode)
  (xref--xref-buffer-mode . centaur-tabs-local-mode)
  (dired-sidebar-mode . centaur-tabs-local-mode)
  (dired-mode . centaur-tabs-local-mode)
  (occur-mode . centaur-tabs-local-mode)
  (compilation-mode . centaur-tabs-local-mode)
  (eww-mode . centaur-tabs-local-mode)
  (apropros-mode . centaur-tabs-local-mode)
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-x <left>" . centaur-tabs-backward)
  ("C-x <right>" . centaur-tabs-forward))

(require 'centaur-tabs)
;; without this centaur tabs blocks tramp according to profiler
(centaur-tabs-group-by-projectile-project)

(defun tdr/fix-centaur-tabs ()
  "Reset Centaur Tabs."
  (centaur-tabs-mode -1) ; Disable Centaur Tabs
  (centaur-tabs-mode 1)  ; Re-enable Centaur Tabs
  (centaur-tabs-headline-match)) ; Update tabs

(add-hook 'focus-in-hook 'tdr/fix-centaur-tabs)

;; When the currently selected tab(A) is at the right of the last visited
;; tab(B), move A to the right of B. When the currently selected tab(A) is
;; at the left of the last visited tab(B), move A to the left of B
(setq centaur-tabs-adjust-buffer-order t)

;; Move the currently selected tab to the left of the the last visited tab.
(setq centaur-tabs-adjust-buffer-order 'left)

;; Move the currently selected tab to the right of the the last visited tab.
(setq centaur-tabs-adjust-buffer-order 'right)

;; Cycle through visible tabs (that is, the tabs in the current group)
(setq centaur-tabs-cycle-scope 'tabs)

;; No close button
(setq centaur-tabs-set-close-button nil)

;; Icons
(setq centaur-tabs-set-icons t)
;(setq centaur-tabs-plain-icons t)
;(setq centaur-tabs-gray-out-icons 'buffer)

(setq centaur-tabs-set-bar 'under)
;(setq centaur-tabs-set-bar 'left)
;; Note: If you're not using Spacmeacs, in order for the underline to display
;; correctly you must add the following line:
(setq x-underline-at-descent-line t)

;; Change defvar centaur-tabs-icon-scale-factor 1.0 to 0.8
(setq centaur-tabs-icon-scale-factor 0.8)
;(setq centaur-tabs-icon-scale-factor 1.0)

;; Style
;(setq centaur-tabs-style "alternate")

(use-package vundo
  :ensure t)

;; bind C-c u to vundo
(global-set-key (kbd "C-c u") 'vundo)

(setq vundo-glyph-alist vundo-unicode-symbols)

(defun eww-render-current-buffer ()
  "Render HTML in the current buffer with EWW"
  (interactive)
  (beginning-of-buffer)
  (eww-display-html 'utf-8 (buffer-name) nil (point-min) (current-buffer)))


(use-package rainbow-mode
  :ensure t)

(use-package htmlize
  :ensure t)

;; (use-package scpaste
  ;; :ensure t)
(require 'scpaste)

(defun scpaste-make-name-combined (&optional suffix)
  (concat (file-name-sans-extension (buffer-name))
          "-"
          (format-time-string "%s")
          suffix
          (file-name-extension (buffer-name) t)))

(setq scpaste-http-destination "https://dflund.se/~getz/pastes"
      scpaste-scp-destination "brutus.df.lth.se:~/public_html/pastes/"
      scpaste-make-name-function 'scpaste-make-name-combined
      )

;; If you have a different keyfile, you can set that, too:
;; (setq scpaste-scp-pubkey "~/.ssh/my_keyfile.pub")

;; (setq scpaste-user-name "Technomancy"
;;       scpaste-user-address "https://technomancy.us/")

(use-package unkillable-scratch
  :ensure t
  :config (unkillable-scratch t))

(setq unkillable-scratch-do-not-reset-scratch-buffer t)
(setq unkillable-scratch-behavior 'bury)
;; (setq unkillable-scratch-behavior 'do-nothing)

(setq initial-scratch-message
      (concat
       ";; ╔═╗┌─┐┬─┐┌─┐┌┬┐┌─┐┬ ┬\n"
       ";; ╚═╗│  ├┬┘├─┤ │ │  ├─┤\n"
       ";; ╚═╝└─┘┴└─┴ ┴ ┴ └─┘┴ ┴\n\n"))

;; Pulse highlight on demand or after select functions
(use-package pulsar
  :ensure t
  :straight (:host github :repo "protesilaos/pulsar")
  :hook (minemacs-first-file . pulsar-global-mode)
  :custom
  (pulsar-iterations 6)
  (pulsar-pulse-region t)
  (pulsar-pulse-on-window-change t)
  (pulsar-region-face 'pulsar-green)
  (pulsar-highlight-face 'pulsar-cyan)
  (pulsar-region-change-face 'pulsar-red)
  (pulsar-window-change-face 'pulsar-yellow)
  :config
  (cl-callf append pulsar-pulse-functions '(what-cursor-position)))

;; View, edit, search and compare very large files in batches, trading memory for processor time
(use-package vlf-setup
  :straight (vlf :source gnu-elpa-mirror)
  :demand
  :config
  (with-eval-after-load 'so-long
    (add-to-list 'so-long-mode-preserved-variables 'vlf-mode)))


;; Fast opening of large files
(use-package guard-lf
  :straight (:host github :repo "jcs-elpa/guard-lf")
  :init
  (guard-lf-mode 1)
  :config
  (cl-callf append guard-lf-intact-major-modes '(rosbag-info-mode ein:ipynb-mode)))


;; Same functionality as `find-dired' and `find-grep-dired', using fd/rg instead
(use-package fd-dired
  :straight t)
