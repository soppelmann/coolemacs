;;; Minimal init.el
;;; (benchmark-init/activate)
;;; Contents:
;;;
;;;  - Basic settings
;;;  - Discovery aids
;;;  - Minibuffer/completion settings
;;;  - Interface enhancements/defaults
;;;  - Tab-bar configuration
;;;  - Theme
;;;  - Optional mixins
;;;  - Built-in customization framework
(setq warning-minimum-level :emergency)

;;; TODO:
;;
;; keybinds for consult-tramp
;; tramp in general
;; eglot functions
;; burly bookmarks
;; ranger
;; copilot mode

;; This is at the top of the file to ensure that
;; it benfits startup time of stuff later on.

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Package initialization
;;
;; We'll stick to the built-in GNU and non-GNU ELPAs (Emacs Lisp Package
;; Archive) for the base install, but there are some other ELPAs you could look
;; at if you want more packages. MELPA in particular is very popular. See
;; instructions at:
;;
;;    https://melpa.org/#/getting-started
;;
(setq package-check-signature nil)


(with-eval-after-load 'package
  (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t))
(require 'package)
(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("elpa" . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))
(package-initialize)

;; Upgrade built-in packages
(setopt package-install-upgrade-built-in t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Straight
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'el-patch)

;; Enable defer and ensure by default for use-package
;; (setq use-package-always-defer t
     ;; use-package-always-ensure t)

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure
  :init (exec-path-from-shell-initialize))

;; Path for daemon
(when (daemonp)
  (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(setq initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
(setq display-time-default-load-average nil) ; this information is useless for most

;; Fixing some defaults

(setq
 ;; No need to see GNU agitprop.
 inhibit-startup-screen t
 ;; No need to remind me what a scratch buffer is.
 initial-scratch-message nil
 ;; Never ding at me, ever.
 ring-bell-function 'ignore
 ;; search should be case-sensitive by default
 case-fold-search nil
 ;; prompt for the read command _every_ time
 compilation-read-command t
 ;; scroll to first error
 compilation-scroll-output 'first-error
 ;; prefer newer elisp files
 load-prefer-newer t
 ;; when I say to quit, I mean quit
 confirm-kill-processes nil
 )

(setq
 make-backup-files nil
 auto-save-default nil
 create-lockfiles nil)

;; Automatically reread from disk if the underlying file changes

(setopt auto-revert-avoid-polling t)
(setq auto-revert-interval 1)
(setq auto-revert-check-vc-info t)
(setq global-auto-revert-mode t)

;; Delete trailing whitespace on save
;(add-hook 'before-save-hook
;          'delete-trailing-whitespace)

;; Disable bell sound
(setq ring-bell-function 'ignore)

;; Overwrite selected text when typing
(setq delete-selection-mode 1)

;; Save history of minibuffer
(savehist-mode)

;; Dont warn on opening BIG files
(setq large-file-warning-threshold nil)
;(setq large-file-warning-threshold 200000000)

;; Have dired stop complaining about ls
(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; Move through windows with Ctrl-<arrow keys>
;(windmove-default-keybindings 'C-c) ; You can use other modifiers here
;(windmove-default-keybindings 'control) ; You can use other modifiers here
;(global-set-key (kbd "C-c <left>") 'windmove-left)
;(global-set-key (kbd "C-c <right>") 'windmove-right)
;(global-set-key (kbd "C-c <up>") 'windmove-up)
;(global-set-key (kbd "C-c <down>") 'windmove-down)

;; Fix archaic defaults
(setq sentence-end-double-space nil)

;; Spellcheck with C-c l f
(global-set-key (kbd "C-c l f") 'ispell-word)

;; Make right-click do something sensible
;;(when (display-graphic-p)
;;  (context-menu-mode))
  (context-menu-mode)

;; DELETE MOUSE 3
(global-unset-key (kbd "<mouse-3>"))
(global-unset-key (kbd "<down-mouse-3>"))

;; function to reload config
(defun reload-config ()
  (interactive)
  (load-file (concat user-emacs-directory "init.el")))

;; Write customizations to a separate file instead of this file.
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(load custom-file t)

;; Currently tramp is messed up
;; Backup and Autosave Directories
(setq temporary-file-directory "~/.emacs.d/tmp")
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Remember last cursor location of opened files
(save-place-mode 1)

;; Highlight matching pairs of parentheses.
(setq show-paren-delay 0)
(show-paren-mode)

;; Insane
(defalias 'yes-or-no-p 'y-or-n-p)

;; Highlight trailing whitespace
;(set-default 'show-trailing-whitespace t)

;(setq whitespace-style '(face tabs tab-mark trailing))
;(custom-set-faces
; '(whitespace-tab ((t (:foreground "#636363")))))
;(setq whitespace-display-mappings
;  '((tab-mark 9 [124 9] [92 9]))) ; 124 is the ascii ID for '\|'
;(global-whitespace-mode)

;; Fix something with symlinks
(setq find-file-visit-truename t)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

;(setq enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
;(setq completion-cycle-threshold 1)                  ; TAB cycles candidates
;(setq completions-detailed t)                        ; Show annotations
;(setq tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
;(setq completion-styles '(basic initials substring)) ; Different styles to match input to candidates

;(setq completion-auto-help 'always)                  ; Open completion always; `lazy' another option
;(setq completions-max-height 20)                     ; This is arbitrary
;(setq completions-detailed t)
;(setq completions-format 'one-column)
;(setq completions-group t)
;(setq completion-auto-select 'second-tab)            ; Much more eager
;(setq completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

;(keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

;; For a fancier built-in completion option, try ido-mode or fido-mode. See also
;; the file mixins/base.el
;(setq icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq-default major-mode
              (lambda () ; guess major mode from file name
                (unless buffer-file-name
                  (let ((buffer-file-name (buffer-name)))
                    (set-auto-mode)))))

;; (setq doom-modeline-icon t)
;; (setq doom-modeline-support-imenu t)



(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :custom
  (doom-modeline-height 25)
  (doom-modeline-bar-width 1)
  (doom-modeline-icon t)
  (doom-modeline-major-mode-icon t)
  (doom-modeline-major-mode-color-icon t)
  (doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (doom-modeline-buffer-state-icon t)
  (doom-modeline-buffer-modification-icon t)
  (doom-modeline-minor-modes nil)
  (doom-modeline-enable-word-count nil)
  (doom-modeline-buffer-encoding t)
  (doom-modeline-indent-info nil)
  (doom-modeline-checker-simple-format t)
  (doom-modeline-vcs-max-length 12)
  (doom-modeline-env-version t)
  (doom-modeline-irc-stylize 'identity)
  (doom-modeline-github-timer nil)
  (doom-modeline-gnus-timer nil))

(display-battery-mode)
;; (use-package doom-modeline
;;   :ensure t
;;   :hook (after-init . doom-modeline-mode))


;; Mode line information
(setq line-number-mode nil)                        ; Show current line in modeline
(setq column-number-mode nil)                      ; Show column as well
(setq mode-line-percent-position nil)              ; No percent
(setq doom-modeline-percent-position nil)

(setq x-underline-at-descent-line nil)           ; Prettier underlines
(setq switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

;(setq-default show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
;(setq-default indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

;; use the k&r c style for c mode
;;(setq c-default-style "k&r")
;; use tabs instead of spaces
(setq-default indent-tabs-mode nil)
;; set the tab width to 4
(setq-default tab-width 4)

(add-hook 'makefile-mode-hook
  '(lambda()
     (setq indent-tabs-mode t)
   )
)

;; We won't set these, but they're good to know about
;;
;; (setq-default indent-tabs-mode nil)
;; (setq-default tab-width 4)

;; Use spaces, not tabs, for indentation.
;(setq-default indent-tabs-mode nil)
;
;;; Never use tabs, use spaces instead.
;(setq tab-width 2)
;(setq js-indent-level 2)
;(setq css-indent-offset 2)
;(setq c-basic-offset 2)
;(setq-default indent-tabs-mode nil)
;(setq-default c-basic-offset 2)
;(setq-default tab-width 2)
;(setq-default c-basic-indent 2)

(setq backward-delete-char-untabify-method 'hungry)

;; Misc. UI tweaks
(blink-cursor-mode -1)                                ; Steady cursor
;(pixel-scroll-precision-mode)                         ; Smooth scrolling
;(pixel-scroll-mode)                                   ; Smooth scrolling
;(setq scroll-preserve-screen-position 'always)        ; Scroll commands keep cursor position

(defun pixel-scroll-setup () 
  (interactive) 
  (setq
   pixel-scroll-precision-large-scroll-height 1) 
  (setq
   pixel-scroll-precision-interpolation-factor 1)) 
(when (boundp
       'pixel-scroll-precision-mode) 
  (pixel-scroll-setup) 
  (add-hook
   'prog-mode-hook #'pixel-scroll-precision-mode) 
  (add-hook
   'org-mode-hook #'pixel-scroll-precision-mode))

;; Set line width to 80 characters and display a vertical line at that column
(setq-default fill-column 80)
(setq-default display-fill-column-indicator-column 80)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)

;(set-face-attribute 'fill-column-indicator nil :foreground "purple4")
;(set-face-attribute 'fill-column-indicator nil :background "grey80")

;; Use fill-column-indicator to highlight lines that exceed the fill column
;; have the background color be the same as the background color of the buffer
;(set-face-attribute 'fill-column-indicator nil :background (face-attribute 'default :background))

;;; https://emacs.stackexchange.com/questions/147/how-can-i-get-a-ruler-at-column-80
;(setq-default fill-column 80)
;(add-hook 'prog-mode-hook 'highlight-beyond-fill-column)
;(custom-set-faces '(highlight-beyond-fill-column-face
;                    ((t (:foreground "red" )))))

;(add-hook 'prog-mode-hook '(lambda () (highlight-lines-matching-regexp ".\\{81\\}" 'hi-yellow)))

;; (use-package smooth-scrolling
;;   :ensure t
;;   :config
;;   (smooth-scrolling-mode 1))

;; Use common keystrokes by default
;(cua-mode)

;; Display line numbers in programming mode
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq-default display-line-numbers-width 3)           ; Set a minimum width

;; tabs for prog and term buffers
;; now handled by centaur-tabs
;(add-hook 'prog-mode-hook 'tab-line-mode)
;(add-hook 'org-mode-hook 'tab-line-mode)
;(add-hook 'text-mode-hook 'tab-line-mode)
;(add-hook 'term-mode-hook 'tab-line-mode)

;; lets make emacs pretty
;; we want to hide the modeline sometimes, for example when interacting with terminals

;; (use-package hide-mode-line
;;   :ensure t)
;; ;; hide it for vterm
;; (add-hook 'vterm-mode-hook 'hide-mode-line-mode)
;; (add-hook 'which-key-mode-hook 'hide-mode-line-mode)
;(add-hook 'which-key-init-buffer-hook 'hide-mode-line-mode)

;; Nice line wrapping when working with text
(add-hook 'text-mode-hook 'visual-line-mode)

;; Modes to highlight the current line with
(let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
  (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;; colorize output in compile buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (ansi-color-apply-on-region compilation-filter-start (point)))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)


(use-package eww
  :config (setq eww-search-prefix "https://duckduckgo.com/html/?q="))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Show the tab-bar as soon as tab-bar functions are invoked
(setq tab-bar-show 0)

;; Add the time to the tab-bar, if visible
(add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
(add-to-list 'tab-bar-format 'tab-bar-format-global 'append)
(set-face-attribute 'tab-bar nil ;; background behind all tabs on the tab bar
:height 1.15)


(setq display-time-format "%a %F %T")
(setq display-time-interval 1)
(display-time-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq custom-safe-themes t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Custom functions
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun clear-register ()
  "Clear the contents of an interactively chosen register."
  (interactive)
  (let ((register (register-read-with-preview "Clear register: ")))
    (when register
      (set-register register nil)
      (message "Cleared register %c." register))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Optional mixins
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; UI/UX enhancements mostly focused on minibuffer and autocompletion interfaces
(load-file (expand-file-name "mixins/base.el" user-emacs-directory))

;; tramp
(load-file (expand-file-name "mixins/tramp.el" user-emacs-directory))

;; Use project.el for managing projects
(load-file (expand-file-name "mixins/project.el" user-emacs-directory))

;; Use projectile.el for managing projects
;(load-file (expand-file-name "mixins/projectile.el" user-emacs-directory))

;; Packages for software development here
(load-file (expand-file-name "mixins/dev.el" user-emacs-directory))

;; Vim-bindings in Emacs (evil-mode configuration)
;; (load-file (expand-file-name "mixins/vim-like.el" user-emacs-directory))

;; meow setup for emacs
(load-file (expand-file-name "mixins/meow.el" user-emacs-directory))

;; Dashboard for emacs
(load-file (expand-file-name "mixins/dashboard.el" user-emacs-directory))

;; Nice fonts for emacs
 ;; (when (or (daemonp) (display-graphic-p))
 ;; (load-file (expand-file-name "mixins/fontaine.el" user-emacs-directory))
 ;; )

;; Themes
(use-package doom-themes
  :ensure t)

(use-package nyan-mode
  :ensure t)
(nyan-mode)

(defun disable-all-themes ()
  "disable all active themes."
  (dolist (i custom-enabled-themes)
    (disable-theme i)))

(defadvice load-theme (before disable-themes-first activate)
  (disable-all-themes))

(use-package modern-tab-bar
  :straight (modern-tab-bar :host github :repo "aaronjensen/emacs-modern-tab-bar" :protocol ssh)
  :init
  (setq tab-bar-show t
        tab-bar-new-button nil
        tab-bar-close-button-show nil)

  ;; (modern-tab-bar-mode)
  )


(load-theme 'doom-monokai-machine-private t)
;; (load-theme 'leuven t)
;; (load-file (expand-file-name "mixins/themes.el" user-emacs-directory))

;; (load-file (expand-file-name "mixins/new.el" user-emacs-directory))

;; Completion settings (corfu capf cape)
(load-file (expand-file-name "mixins/completion.el" user-emacs-directory))


;; Eglot config
(load-file (expand-file-name "mixins/eglot.el" user-emacs-directory))

;; lsp-bridge config
;; (load-file (expand-file-name "mixins/lsp-bridge.el" user-emacs-directory))

;; Rust config
;(load-file (expand-file-name "mixins/rust.el" user-emacs-directory))


;; verilog config
(load-file (expand-file-name "mixins/hlsmode.el" user-emacs-directory))

;; Cargo config
;(load-file (expand-file-name "mixins/cargo.el" user-emacs-directory))
;(add-to-list 'load-path "~/.emacs.d/elisp/cargo-transient.el")

;; Company config
;; (load-file (expand-file-name "mixins/company.el" user-emacs-directory))

;; Set up codeium AI assistant
;; (load-file (expand-file-name "mixins/codeium.el" user-emacs-directory))

;; Set up copilot AI assistant
(load-file (expand-file-name "mixins/copilot.el" user-emacs-directory))

;; lspmode config
;; (load-file (expand-file-name "mixins/lspmode.el" user-emacs-directory))

;; Org-mode configuration
;; WARNING: need to customize things inside the mixin file before use! See
;; the file mixins/org-intro.txt for help.
;; (load-file (expand-file-name "mixins/org.el" user-emacs-directory))

;; Email configuration in Emacs
;; WARNING: needs the `mu' program installed; see the mixin file for more
;; details.
;(load-file (expand-file-name "mixins/email.el" user-emacs-directory))

;; Tools for academic researchers
;(load-file (expand-file-name "mixins/researcher.el" user-emacs-directory))


;; org

;; https://old.reddit.com/r/emacs/comments/tbj09/using_shift_selection_in_orgmode_buffers_without/
;(setq org-support-shift-select t)
;
;(eval-after-load "org"
;    '(progn
;       (eval-after-load "cua-base"
;         '(progn
;            (define-advice org-call-for-shift-select (before org-call-for-shift-select-cua activate)
;              (if (and cua-mode
;                       org-support-shift-select
;                       (not (use-region-p)))
;                  (cua-set-mark)))))))
