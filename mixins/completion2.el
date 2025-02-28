;;; completion.el --- Completion packages -*- lexical-binding: t; -*-

;; Copyright (C) 2022-2024  Abdelhak Bougouffa

;; Author: Abdelhak Bougouffa (rot13 "nobhtbhssn@srqbencebwrpg.bet")

;;; Commentary:

;;; Code:

;; Pretty icons for corfu
(use-package kind-icon
  :ensure t
;  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))

(use-package yasnippet
  :ensure t
  :config
  (yas-reload-all))

(define-key yas-minor-mode-map (kbd "<tab>") nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(use-package yasnippet-snippets
  :ensure t
  )

(use-package yasnippet-classic-snippets
  :ensure t
  )

(setq yas-snippet-dirs '("~/.emacs.d/snippets")) ;; path to snippets
(yas-recompile-all)
(yas-reload-all)
(yas-global-mode 1)

;; Completion-At-Point Extension for YASnippet
(use-package yasnippet-capf
  :straight t
  :hook ((prog-mode text-mode conf-mode) . +cape-yasnippet--setup-h)
  :bind (("C-c p y" . yasnippet-capf))
  :init
  (defun +cape-yasnippet--setup-h ()
    (when (bound-and-true-p yas-minor-mode)
      (add-to-list 'completion-at-point-functions #'yasnippet-capf))))

;; The Doom Emacs snippets library
(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

;; (setq yasnippet-capf-lookup-by 'name) ;; Prefer the name of the snippet instead
(setq yas-indent-line 'fixed)

;; Completion at point extensions which can be used in combination with Corfu, Company or the default completion UI
(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'yasnippet-capf)
  ;; (add-hook 'completion-at-point-functions #'cape-tex)
  ;; (add-hook 'completion-at-point-functions #'verilog-ext-capf)
  :bind (("C-c p p" . completion-at-point) ; capf
         ("C-c p t" . complete-tag) ; etags
         ("C-c p d" . cape-dabbrev) ; or dabbrev-completion
         ("C-c p h" . cape-history)
         ("C-c p f" . cape-file)
         ("C-c p k" . cape-keyword)
         ;; ("C-c p s" . cape-elisp-symbol)
         ("C-c p e" . cape-elisp-block)
         ("C-c p a" . cape-abbrev)
         ;; ("C-c p l" . cape-line)
         ("C-c p w" . cape-dict)
         ("C-c p :" . cape-emoji)
         ("C-c p \\" . cape-tex)
         ("C-c p _" . cape-tex)
         ("C-c p ^" . cape-tex)
         ("C-c p &" . cape-sgml)
         ("C-c p r" . cape-rfc1345))
  :init
  ;; Silence the pcomplete capf, no errors or messages! Important for corfu!
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; (add-hook
   ;; '(emacs-lisp-mode-hook git-commit-mode-hook)
   ;; (lambda () (add-hook 'completion-at-point-functions #'cape-elisp-symbol nil t)))

  (add-hook
   '(emacs-lisp-mode-hook git-commit-mode-hook)
   (lambda () (add-hook 'completion-at-point-functions (list (cape-capf-super
                                                              #'cape-file
                                                              #'cape-elisp-symbol
                                                              #'cape-elisp-block)))))

  ;; (add-hook
  ;;  'org-mode-hook
  ;;  (lambda () (add-hook 'completion-at-point-functions #'cape-elisp-block nil t)))

  (add-hook
   '(TeX-mode-hook LaTeX-mode-hook)
   (lambda () (add-hook 'completion-at-point-functions #'cape-tex nil t))))

;; quit completion
(add-hook 'completion-in-region-mode-hook
          (lambda () (setq completion-in-region-mode--predicate #'always)))

;; Corfu enhances in-buffer completion with a small completion popup
(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :hook ((eshell-mode shell-mode) . +corfu-less-intrusive-h)
  :hook (minibuffer-setup . +corfu-enable-in-minibuffer-h)
  :hook (corfu-mode . corfu-history-mode)
  :hook ((prog-mode . corfu-mode))
  :hook ((circe-mode . corfu-mode))
  :bind (:map corfu-map
              ("M-TAB"      . corfu-next)
              ("M-<tab>"    . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ;; ("SPC"        . corfu-insert-separator)
              ("<backtab>"  . corfu-previous)
              )
  :custom
  (corfu-auto t) ; Enable auto completion
  (corfu-auto-delay  0.1)
  (corfu-auto-prefix 2)
  (corfu-quit-no-match t)
  ;; (corfu-quit-at-boundary t)
  ;; (corfu-quit-no-match t)
  (corfu-on-exact-match 'show)
  (corfu-cycle t) ; Allows cycling through candidates
  ;; (corfu-min-width 30)
  ;; (corfu-preview-current nil) ; Disable previewing the current candidate
  :init
  ;; (add-hook 'completion-in-region-mode-hook
            ;; (lambda () (setq completion-in-region-mode--predicate #'always)))

  ;; (add-hook 'prog-mode-hook #'global-corfu-mode nil nil :transient t)
  :config
  ;; HACK: Prevent the annoting completion error when no `ispell' dictionary is set, prefer `cape-dict'
  (when (>= emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  (defun +corfu-enable-in-minibuffer-h ()
    "Enable Corfu in the minibuffer if `completion-at-point' is bound."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto t ; Enable/disable auto completion
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun +corfu-less-intrusive-h ()
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match t
                corfu-auto nil)
    (corfu-mode 1))

  ;; Ensure `savehist-mode' is on and add `corfu-history' to the saved variables
  (unless (bound-and-true-p savehist-mode) (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))

;; Candidate information popup for Corfu
;; (use-package corfu-popupinfo
;;   :hook (corfu-mode . corfu-popupinfo-mode)
;;   :bind ( ; Bind these to toggle/scroll documentation
;;          :map corfu-map
;;          ("M-p" . corfu-popupinfo-scroll-down)
;;          ("M-n" . corfu-popupinfo-scroll-up)
;;          ("M-d" . corfu-popupinfo-toggle))
;;   :custom
;;   (corfu-popupinfo-delay nil)
;;   (corfu-popupinfo-max-height 15)
;;   :config
;;   ;; Otherwise, the popupinfo will stay open on ESC or `C-g'!
;;   (add-hook
;;    'completion-in-region-mode-hook
;;    (defun +corfu--hide-popupinfo-h ()
;;      (when (and (not completion-in-region-mode) (boundp 'corfu-popupinfo--hide))
;;        (corfu-popupinfo--hide)))))

;; Icons for Corfu using `nerd-icons'
;; (use-package nerd-icons-corfu
;;   :straight t
;;   :after corfu
;;   :init
;;   (add-to-list 'corfu-margin-formatters #'nerd-icons-corfu-formatter))

;; Insert paths into the minibuffer prompt
(use-package consult-dir
  :straight t
  :bind (("C-x C-d" . consult-dir)
         :package vertico
         :map minibuffer-local-completion-map
         ("C-x C-d" . consult-dir)
         ("C-x C-j" . consult-dir-jump-file)))

