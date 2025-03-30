;; TAB-only configuration
(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ;; ("SPC"        . corfu-quit)
              ("M-TAB"      . corfu-next)
              ("M-<tab>"      . corfu-next)
              ("M-TAB"      . corfu-complete)
              ("M-<tab>"      . corfu-complete)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("<backtab>"  . corfu-insert)
              ("RET"        . corfu-insert))

  :hook ((eshell-mode shell-mode) . +corfu-less-intrusive-h)
  :hook (minibuffer-setup . +corfu-enable-in-minibuffer-h)
  :hook (corfu-mode . corfu-history-mode)
  :hook ((prog-mode . corfu-mode))
  :hook ((circe-mode . corfu-mode))
  :custom
  (corfu-auto t)               ;; Enable auto completion
  (corfu-auto-delay  0.1)
  (corfu-auto-prefix 2)
  ;; (corfu-preselect 'directory) ;; Select the first candidate, except for directories
  (corfu-on-exact-match 'show)
  (corfu-cycle t) ; Allows cycling through candidates

  :init
  (global-corfu-mode)

  :config
  (setq corfu-quit-at-boundary t)
  (setq corfu-quit-no-match 'separator)
  ;; Free the RET key for less intrusive behavior.
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

  (unless (bound-and-true-p savehist-mode) (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history)

  ;; Option 1: Unbind RET completely
  ;; (keymap-unset corfu-map "RET")
  ;; Option 2: Use RET only in shell modes
  ;; (keymap-set corfu-map "RET" `( menu-item "" nil :filter
  ;;                                ,(lambda (&optional _)
  ;;                                   (and (derived-mode-p 'eshell-mode 'comint-mode)
  ;;                                        #'corfu-send))))
  )


(use-package cape
  :straight t
  :init
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'yasnippet-capf)
 
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'yasnippet-capf)  
  ;; (add-to-list 'completion-at-point-functions #'cape-tex)
  ;; (add-hook 'completion-at-point-functions #'cape-tex)
  ;; (add-hook 'completion-at-point-functions #'verilog-ext-capf)
  ;; :hook
  ;; (eglot-managed-mode . my/eglot-capf)

  :config
  (transient-define-prefix sn/cape ()
    "explicit Completion type"
    [[
      ("d" "Dabbrev" cape-dabbrev)
      ("s" "Spelling" cape-dict)
      ("k" "Keyword" cape-keyword)
      ("l" "Line" cape-line)]
     [
      ("f" "File" cape-file)
      ("h" "History" cape-history)
      ("a" "Abbrev" cape-abbrev)
      ("q" "Quit" transient-quit-one)]
     [
      ("e" "Elisp Symbol" cape-elisp-symbol)
      ("E" "Elisp Block" cape-elisp-block)
      ("t" "Tags" complete-tag)
      ("y" "yasnippet" yasnippet-capf)
      ("v" "verilog" verilog-ext-capf)
      ]])
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
         ("C-c p r" . cape-rfc1345)
         ("C-c /" . sn/cape)
         ("s-q" . sn/cape)
         )
  ;; :config
  ;;   (defun my/cape-in-code ()
  ;;       (cape-wrap-nonexclusive
  ;;         (cape-capf-inside-code
  ;;       	(cape-capf-super
  ;;       	  #'eglot-completion-at-point
  ;;       	  #'yasnippet-capf
  ;;       	  #'cape-dabbrev))))
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
   (lambda () (add-hook 'completion-at-point-functions #'cape-tex nil t)))
  ;; quit completion
  (add-hook 'completion-in-region-mode-hook
            (lambda () (setq completion-in-region-mode--predicate #'always)))
)

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  ;; (tab-always-indent 'complete)
  (tab-always-indent nil)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

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
