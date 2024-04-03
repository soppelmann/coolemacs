;;; Emacs Bedrock
;;;
;;; Mixin: Development tools

;;; Usage: Append or require this file from init.el for some software
;;; development-focused packages.
;;;
;;; It is **STRONGLY** recommended that you use the base.el mixin if you want to
;;; use Eglot. Lots of completion things will work better.
;;;
;;; This will try to use tree-sitter modes for many languages. Please run
;;;
;;;   M-x treesit-install-language-grammar
;;;
;;; Before trying to use a treesit mode.

;;; Contents:
;;;
;;;  - Built-in config for developers
;;;  - Version Control
;;;  - Common file types
;;;  - Eglot, the built-in LSP client for Emacs

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Built-in config for developers
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t)


(use-package emacs
  :config
  ;; Treesitter config

  ;; Tell Emacs to prefer the treesitter mode
  ;; You'll want to run the command `M-x treesit-install-language-grammar' before editing.
  (setq major-mode-remap-alist
        '((yaml-mode . yaml-ts-mode)
          (bash-mode . bash-ts-mode)
          (js2-mode . js-ts-mode)
          (typescript-mode . typescript-ts-mode)
          (json-mode . json-ts-mode)
          (css-mode . css-ts-mode)
          (python-mode . python-ts-mode)))
 :hook
 ;; Auto parenthesis matching
 (
  (prog-mode . electric-pair-mode)
  (prog-mode . rainbow-delimiters-mode)
  )
  )

(use-package fancy-narrow
  :ensure t
  :hook ((prog-mode . fancy-narrow-mode))
)
;;(add-hook 'prog-mode-hook 'fancy-narrow-mode)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Version Control
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Magit: best Git client to ever exist
(use-package magit
  :ensure t
  :bind (("s-g" . magit-status)
         ("C-c g" . magit-status)))

;; Use git-timemachine to browse historic versions of files
;; Bind it to C-c t
;; Make sure git timemachine toggles evil-local-mode and
;; display-line-numbers-mode
(use-package git-timemachine
  :ensure t
  :bind (("C-c t" . git-timemachine-toggle))
  :hook
  (git-timemachine-mode . evil-local-mode)
  (git-timemachine-mode . display-line-numbers-mode)
  )

;; @see https://bitbucket.org/lyro/evil/issue/511/let-certain-minor-modes-key-bindings
;; http://blog.binchen.org/posts/use-git-timemachine-with-evil.html
(with-eval-after-load 'git-timemachine
  (evil-make-overriding-map git-timemachine-mode-map 'normal)
  ;; force update evil keymaps after git-timemachine-mode loaded
  (add-hook 'git-timemachine-mode-hook #'evil-normalize-keymaps))


;(use-package diff-hl :ensure t)
;(add-hook 'prog-mode-hook 'diff-hl-mode)
(setf (cdr (assq 'continuation fringe-indicator-alist))
      '(nil nil) ;; no continuation indicators
      ;; '(nil right-curly-arrow) ;; right indicator only
      ;; '(left-curly-arrow nil) ;; left indicator only
      ;; '(left-curly-arrow right-curly-arrow) ;; default
      )
;; https://ianyepan.github.io/posts/emacs-git-gutter/

;(fringe-mode '(1 . 1))
;(fringe-mode '(3 . 0))
(fringe-mode '(0 . 0))

;;(use-package git-gutter
;;  :ensure t
;;  :hook (prog-mode . git-gutter-mode)
;;  :config
;;  (setq git-gutter:update-interval 0.02)
;;  )
;;
;;(use-package git-gutter-fringe
;;  :ensure t
;;  :config
;;  (define-fringe-bitmap 'git-gutter-fr:added [0] nil nil '(center repeated))
;;  (define-fringe-bitmap 'git-gutter-fr:modified [0] nil nil '(center repeated))
;;  (define-fringe-bitmap 'git-gutter-fr:deleted [0] nil nil 'bottom)
;;  )
;;
;;(custom-set-variables
;; '(git-gutter:modified-sign "  ") ;; two space
;; '(git-gutter:added-sign "++")    ;; multiple character is OK
;; '(git-gutter:deleted-sign "--"))

;; Get rid of flycheck in the gutter and margins
;(setq flycheck-indication-mode nil)

;; Left fringe flycheck-indication-mode
(setq-default flycheck-indication-mode 'left-fringe)
(add-hook 'flycheck-mode-hook #'flycheck-set-indication-mode)

(use-package flycheck-checkbashisms
  :ensure t
  :config
  (flycheck-checkbashisms-setup))

;; I only really use git, stamp on vc-mode....
(with-eval-after-load 'vc
  (remove-hook 'find-file-hook 'vc-find-file-hook)
  (remove-hook 'find-file-hook 'vc-refresh-state)
  (setq vc-handled-backends nil))

;; As the built-in project.el support expects to use vc-mode hooks to
;; find the root of projects we need to provide something equivalent
;; for it.
(defun my-git-project-finder (dir)
  "Integrate .git project roots."
  (let ((dotgit (and (setq dir (locate-dominating-file dir ".git"))
                     (expand-file-name dir))))
    (and dotgit
         (cons 'transient (file-name-directory dotgit)))))

(add-hook 'project-find-functions 'my-git-project-finder)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Projectile
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Projectile: project management
(use-package projectile
  :ensure t
  :config
  (projectile-mode +1)
  ;(setq projectile-project-search-path '("~/projects/"))
  ;(setq projectile-completion-system 'consult)
  (setq projectile-enable-caching t)
  (setq projectile-indexing-method 'alien)
  (setq projectile-sort-order 'recently-active)
  (setq projectile-globally-ignored-directories
        (append '(
                  ".git"
                  ".svn"
                  ".hg"
                  ".bzr"
                  "node_modules"
                  "build"
                  "dist"
                  "target"
                  "bin"
                  "obj"
                  "out"
                  "build"
                  "buildroot"
                  )))
  )

;; Bind key C-c p A to add a project on the global map
(define-key global-map (kbd "C-c p A") 'projectile-add-known-project)

;; Bind key C-c p B to remove a project
(define-key global-map (kbd "C-c p B") 'projectile-remove-known-project)

;; Add these commands to embark directory map at some point
;; Also figure out how to use embark to add bookmarks to files


;; consult-project-extra is also an alternative
;(use-package consult-projectile
;  :ensure t
;  )

(use-package consult-ag
  :ensure t
  )

(defun my-consult-projectile-ag ()
  "Run a consult-ag search in the project."
  (interactive)
  (require 'projectile)
  (let* ((ignores (unless (eq (projectile-project-vcs) 'git)
                    ;; ag supports git ignore files
                    (append
                     (projectile-ignored-files-rel) (projectile-ignored-directories-rel)
                     (projectile--globally-ignored-file-suffixes-glob)
                     grep-find-ignored-files grep-find-ignored-directories)))
         (ignores-args (apply #'append
                              (mapcar (lambda (item) (list "--ignore" item)) ignores))))
    (funcall-interactively #'consult-ag
                           (if-let ((s (symbol-at-point)))
                               (symbol-name s)
                             "")
                           (projectile-project-root)
                           ignores-args)))

(let ((projectile-switch-project-action #'my-consult-projectile-ag)
  (call-interactively #'projectile-switch-project)))


;; Why is this a thing
;; (use-package projectile-ripgrep
;;   :ensure t
;;   )


;; Recommended keymap prefix on macOS
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
;; Recommended keymap prefix on Windows/Linux
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Need to overwrite proctile ripgrep cause it sucks
(define-key projectile-mode-map (kbd "C-c p s r") 'consult-ripgrep)

;(define-key projectile-mode-map (kbd "C-x p") 'projectile-command-map)

;; (use-package consult-project-extra
;;   :ensure t
;; )

;; use consult and ivy for projectile, i dont use this
;; (use-package counsel-projectile
;;   :ensure t
;;   :config
;;   (counsel-projectile-mode))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Common file types
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(use-package markdown-mode
;;  :ensure t)
;;  :hook ((markdown-mode . visual-line-mode)))

(use-package yaml-mode
  :ensure t)

(use-package json-mode
  :ensure t)


;; https://old.reddit.com/r/emacs/comments/audffp/tip_how_to_use_a_stable_and_fast_environment_to/
(use-package google-c-style
  :ensure t
  :hook ((c-mode c++-mode) . google-set-c-style)
         (c-mode-common . google-make-newline-indent))

;; use // instead of /* */ for comments
(add-hook 'c-mode-hook (lambda () (c-toggle-comment-style -1)))
(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style -1)))

;; C-c C-C is a worthless binding for comments, so unbind it
;; Use C-x C-; instead or M-; for line comments
(define-key c-mode-base-map (kbd "C-c C-c") nil)

;; Emacs ships with a lot of popular programming language modes. If it's not
;; built in, you're almost certain to find a mode for the language you're
;; looking for with a quick Internet search.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Lsp stuff
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package format-all
  :ensure t
  )
(defvar-local my/format-buffer-function 'format-all-buffer
  "Function to call in order to format the current buffer.")
(defun my/format-buffer ()
  "Run `my/format-buffer-function' to format the current buffer."
  (interactive)
  (funcall my/format-buffer-function))
(bind-key "C-c f f" 'my/format-buffer)


(use-package consult-flycheck
  :ensure t)

(add-to-list 'load-path "~/.emacs.d/localpkgs/")

(add-hook 'c-mode-hook (lambda () (setq flycheck-clang-language-standard "c99")))
(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++11")))

(add-to-list 'auto-mode-alist '("\\.upphtml\\'" . shell-script-mode))
(add-to-list 'auto-mode-alist '("\\.uppcss\\'" . shell-script-mode))

(use-package eldoc-box
  :ensure t
  )

 (setq eldoc-echo-area-use-multiline-p nil)
;;
;; (setq lsp-eldoc-enable-hover nil)
(setq eldoc-echo-area-prefer-doc-buffer t)

;; LSP RUST
(use-package rust-mode :ensure t)
;(add-hook 'rust-mode-hook 'eglot-ensure)
(add-hook 'rust-mode-hook (lambda () (setq indent-tabs-mode nil)))

;(setq lsp-rust-analyzer-server-display-inlay-hints t)

(use-package php-mode
  :ensure t
  :mode "\\.php\\'")

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'")

(use-package zig-mode
  :ensure t)

;(require 'tramp)
(add-to-list 'tramp-remote-path 'tramp-own-remote-path)


(use-package vterm
  :ensure t
  :config
  (setq vterm-max-scrollback 10000)
  (setq vterm-shell "bash")
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-always-compile-module t)
  (setq vterm-buffer-name-string "vterm %s")
  )

(use-package vterm-toggle
  :ensure t
;  :config (setq vterm-toggle-fullscreen-p t)
  )

(use-package multi-vterm
  :ensure t
)


(setq vterm-tramp-shells '(("docker" "sh")
                           ("scpx" "zsh")
                           ("ssh" "zsh")))

(define-key global-map (kbd "<f4>") #'vterm-toggle)

;(dddefine-key vterm)
;(define-key vterm-mode-map [?\C-c] nil)
(define-key vterm-mode-map (kbd "<f4>") 'vterm-toggle)
(define-key vterm-mode-map (kbd "<f5>") 'ef-themes-toggle)

(load "~/.emacs.d/consult-tramp.el")

;; Speedup tramp

(setq vc-ignore-dir-regexp
      (format "\\(%s\\)\\|\\(%s\\)"
              vc-ignore-dir-regexp
              tramp-file-name-regexp))

(setq debug-ignored-errors
      (cons 'remote-file-error debug-ignored-errors))


(use-package cmake-mode
  :ensure t)

(use-package cmake-project
  :ensure t)

(autoload 'cmake-project-mode "cmake-project" nil t)

(defun maybe-cmake-project-mode ()
  (if (or (file-exists-p "CMakeLists.txt")
          (file-exists-p (expand-file-name "CMakeLists.txt" (car (project-roots (project-current))))))
      (cmake-project-mode)))

(add-hook 'c-mode-hook 'maybe-cmake-project-mode)
(add-hook 'c++-mode-hook 'maybe-cmake-project-mode)

;; (setq spice-simulator "Ngspice"
;;       spice-waveform-viewer "ngplot")
;; (load "~/.emacs.d/spice-mode.el")

;; ;; ngspice -b file.ng

;; (use-package spice-mode
;;   :ensure t
;;   :config
;;   (setq spice-simulator "Ngspice"
;;       spice-waveform-viewer "ngplot"))

;; ;; :mode "\\.\\(js\\|jsx\\)\\'"
;; (add-to-list 'auto-mode-alist '("\\.ng\\'" . spice-mode))


;;;;;;;;;;;
;; EDIFF ;;
;;;;;;;;;;;

(setq ediff-window-setup-function 'ediff-setup-windows-plain)
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-diff-options "-w")
(add-hook 'ediff-after-quit-hook-internal 'winner-undo)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MAGIT
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(defun local/change-commit-author (arg)
;  "Change the commit author during an interactive rebase in Magit.
;With a prefix argument, insert a new change commit author command
;even when there is already another rebase command on the current
;line.  With empty input, remove the change commit author action
;on the current line, if any."
;  (interactive "P")
;  (let ((author
;         (magit-transient-read-person "Select a new author for this commit"
;                               nil
;                               nil)))
;    (git-rebase-set-noncommit-action
;     "exec"
;     (lambda (_) (if author
;                     (format "git commit --amend --author='%s'" author)
;                   ""))
;     arg)))
;
;(define-key git-rebase-mode-map (kbd "h") #'local/change-commit-author)
