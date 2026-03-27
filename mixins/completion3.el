;;; completion.el --- Corfu + Cape + YASnippet completion stack -*- lexical-binding: t; -*-

(defun +corfu-smart-ret ()
  "Confirm candidate if selected, otherwise pass RET through."
  (interactive)
    (message "corfu-insert fired, index: %d" corfu--index)
  (if (>= corfu--index 0)
      (corfu-insert))
    (corfu-quit)
    (message "corfu-insert fired, index: %d" corfu--index)
    (call-interactively (or (command-remapping #'newline)
                            (key-binding (kbd "RET")))))

;; Orderless: powerful completion style
(use-package orderless
  :ensure t
  :config
  (setq completion-styles '(orderless)))

;; ─── Corfu ────────────────────────────────────────────────────────────────────

(use-package corfu
  :straight (:files (:defaults "extensions/*.el"))
  :custom
  (corfu-auto           t)
  (corfu-auto-delay     0)
  (corfu-auto-prefix    2)
  (corfu-on-exact-match 'quit)
  (corfu-preselect      'prompt)
  (corfu-cycle          t)
  (corfu-quit-at-boundary t)
  (corfu-quit-no-match    t)
  ;; (corfu-quit-no-match    'separator)
  ;; (corfu-separator ?\s)
  ;; (corfu-separator ?_)
  ;; (corfu-separator (string-to-char "⊂"))

  :bind (:map corfu-map
         ("TAB"        . corfu-next)
         ([tab]        . corfu-next)
         ("S-TAB"      . corfu-previous)
         ([backtab]    . corfu-previous)
         ("M-TAB"      . corfu-complete)
         ("M-<tab>"    . corfu-complete)
         ("M-SPC"      . corfu-insert-separator)
         ("S-<return>" . corfu-insert)
         ;; ("RET"        . +corfu-smart-ret)
         ;; ("RET"        . corfu-insert)
         )

  :hook
  ((prog-mode circe-mode)   . corfu-mode)
  (corfu-mode               . corfu-history-mode)
  ((eshell-mode shell-mode) . +corfu-less-intrusive-h)
  (minibuffer-setup         . +corfu-enable-in-minibuffer-h)

  (global-corfu-mode)

  :config
  (when (>= emacs-major-version 30)
    (setq text-mode-ispell-word-completion nil))

  (keymap-set corfu-map "RET"
  `(menu-item "" nil :filter
     ,(lambda (&optional _)
        (and (derived-mode-p 'eshell-mode 'comint-mode
                             'prog-mode 'tex-mode 'latex-mode)
             #'corfu-insert))))

  
  (defun +corfu-enable-in-minibuffer-h ()
    "Enable Corfu in the minibuffer when `completion-at-point' is locally bound."
    (when (local-variable-p 'completion-at-point-functions)
      (setq-local corfu-auto            t
                  corfu-popupinfo-delay nil)
      (corfu-mode 1)))

  (defun +corfu-less-intrusive-h ()
    "A quieter Corfu for shell-like modes."
    (setq-local corfu-quit-at-boundary t
                corfu-quit-no-match    t
                corfu-auto             nil)
    (corfu-mode 1))

  (unless (bound-and-true-p savehist-mode) (savehist-mode 1))
  (add-to-list 'savehist-additional-variables 'corfu-history))


;; ─── Kind-icon ────────────────────────────────────────────────────────────────

(use-package kind-icon
  :ensure t
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default)
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; ─── Cape ─────────────────────────────────────────────────────────────────────

(use-package cape
  :straight t

  :bind
  (("C-c p p" . completion-at-point)
   ("C-c p d" . cape-dabbrev)
   ("C-c p h" . cape-history)
   ("C-c p f" . cape-file)
   ("C-c p e" . cape-elisp-block)
   ("C-c p a" . cape-abbrev)
   ("C-c p w" . cape-dict)
   ("C-c p y" . yasnippet-capf)
   ("C-c p :" . cape-emoji)
   ("C-c /"   . sn/cape)
   ("s-q"     . sn/cape))

  :init
  ;; Global fallback capfs — wrap yasnippet-capf in a cache buster so it
  ;; re-runs on every keystroke and never returns stale/empty results.
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  (add-hook 'completion-at-point-functions #'cape-file 90)
  (add-hook 'completion-at-point-functions #'yasnippet-capf 99)  ; last

  ;; Silence pcomplete noise — important for Corfu in shell modes.
  (advice-add 'pcomplete-completions-at-point :around #'cape-wrap-silent)

  ;; Keep completion-in-region alive unconditionally.
  (add-hook 'completion-in-region-mode-hook
            (lambda () (setq completion-in-region-mode--predicate #'always)))

  (defun +eglot-capf-h ()
  "Setup or teardown the fused eglot capf depending on eglot state."
  (if eglot--managed-mode
      (progn
        (setq +eglot-super-capf
              (cape-capf-nonexclusive
               (cape-capf-super
                ;; #'cape-file
                #'eglot-completion-at-point
                #'yasnippet-capf)))
        (add-to-list 'completion-at-point-functions #'cape-file)
        (add-to-list 'completion-at-point-functions +eglot-super-capf))
    (when +eglot-super-capf
      (setq-local completion-at-point-functions
                  (remove +eglot-super-capf completion-at-point-functions))
      (setq +eglot-super-capf nil))))

  (add-hook 'eglot-managed-mode-hook #'+eglot-capf-h)

  
  ;; Eglot: fuse LSP + yasnippet + file into one non-exclusive capf so Corfu
  ;; doesn't stop after this entry even when it returns candidates.
  ;; (defun +eglot-capf-setup-h ()
  ;;   (setq-local completion-at-point-functions
  ;;               (cons (cape-capf-nonexclusive
  ;;                      (cape-capf-super
  ;;                       #'cape-file
  ;;                       #'eglot-completion-at-point
  ;;                       #'yasnippet-capf
  ;;                       ))
  ;;                     completion-at-point-functions)))
  ;; (add-hook 'eglot-managed-mode-hook #'+eglot-capf-setup-h)

  ;; Elisp modes: fuse symbol + block + file, buffer-local.
  ;; (dolist (hook '(emacs-lisp-mode-hook git-commit-mode-hook))
  ;;   (add-hook hook
  ;;             (lambda ()
  ;;               (add-hook 'completion-at-point-functions
  ;;                         (cape-capf-super #'cape-file
  ;;                                          #'cape-elisp-symbol
  ;;                                          #'cape-elisp-block)
  ;;                         nil t))))

  ;; TeX modes: add TeX capf buffer-locally.
  (dolist (hook '(TeX-mode-hook LaTeX-mode-hook))
    (add-hook hook
              (lambda ()
                (add-hook 'completion-at-point-functions #'cape-tex nil t))))

  :config
  (transient-define-prefix sn/cape ()
    "Explicit completion dispatch."
    [["Source"
      ("d" "Dabbrev"      cape-dabbrev)
      ("s" "Spelling"     cape-dict)
      ("l" "Line"         cape-line)
      ("k" "Keyword"      cape-keyword)]
     ["More"
      ("f" "File"         cape-file)
      ("h" "History"      cape-history)
      ("a" "Abbrev"       cape-abbrev)
      ("e" "Elisp symbol" cape-elisp-symbol)
      ("E" "Elisp block"  cape-elisp-block)
      ("t" "Tags"         complete-tag)
      ("y" "Yasnippet"    yasnippet-capf)
      ("q" "Quit"         transient-quit-one)]]))


;; ─── Emacs built-ins ──────────────────────────────────────────────────────────

(use-package emacs
  :custom
  (tab-always-indent                nil)
  (text-mode-ispell-word-completion nil)
  (read-extended-command-predicate  #'command-completion-default-include-p))


;; ─── YASnippet ────────────────────────────────────────────────────────────────

(use-package yasnippet :ensure t)
;; (use-package yasnippet-snippets         :ensure t)
;; (use-package yasnippet-classic-snippets :ensure t)

; Best snippets
(use-package doom-snippets
  :straight (:host github :repo "hlissner/doom-snippets" :files ("*.el" "*")))

;; (setq yas-snippet-dirs '("~/.emacs.d/snippets")
      ;; yas-indent-line   'fixed)

;; TAB belongs to Corfu — unbind it from yasnippet after the package loads.
;; (with-eval-after-load 'yasnippet
  ;; (define-key yas-minor-mode-map (kbd "<tab>") nil)
  ;; (define-key yas-minor-mode-map (kbd "TAB")   nil))

(yas-recompile-all)
(yas-reload-all)
(yas-global-mode 1)


;; ─── Yasnippet-capf ───────────────────────────────────────────────────────────

(use-package yasnippet-capf
  :straight t
  :hook ((prog-mode text-mode conf-mode) . +yasnippet-capf-setup-h)
  :init
  (defun +yasnippet-capf-setup-h ()
    "Add cache-busted `yasnippet-capf' buffer-locally when yasnippet is active."
    (when (bound-and-true-p yas-minor-mode)
      (add-hook 'completion-at-point-functions +yasnippet-capf nil t))))

;;; completion.el ends here
