;(add-to-list 'load-path "~/.emacs.d/lisp/")

;; Needs to patch
;;(use-package corfu-candidate-overlay
;;  :after corfu
;;  :config
;;  ;; enable corfu-candidate-overlay mode globally
;;  ;; this relies on having corfu-auto set to nil
;;  (corfu-candidate-overlay-mode +1)
;;  ;; bind Ctrl + TAB to trigger the completion popup of corfu
;;  (global-set-key (kbd "C-<tab>") 'completion-at-point)
;;  ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
;;  ;; (keybing <iso-lefttab> may not work for your keyboard model)
;;  (global-set-key (kbd "S-<tab>") 'corfu-candidate-overlay-complete-at-point)
;;  )


;; Popup completion-at-point
(use-package corfu
  :ensure t
  ;:hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  (corfu-preview-current t)
  (corfu-cycle t)
  (corfu-on-exact-match 'show)
  :init
  (global-corfu-mode)
  (corfu-prescient-mode 1)
  (corfu-history-mode)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ("SPC"        . corfu-insert-separator)
              ("M-TAB"      . corfu-next)
              ("TAB"        . corfu-next)
              ([tab]        . corfu-next)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("S-<return>" . corfu-insert)
              ("RET"        . corfu-insert))
;;  :config
;;  ;; Setup lsp to use corfu for lsp completion
;;  (defun kb/corfu-setup-lsp ()
;;    "Use orderless completion style with lsp-capf instead of the
;;default lsp-passthrough."
;;    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
;;          '(orderless)))

  )


;; Optionally use the `orderless' completion style.
(use-package orderless
  :ensure t
  :init
  ;; Configure a custom style dispatcher (see the Consult wiki)
  ;; (setq orderless-style-dispatchers '(+orderless-consult-dispatch orderless-affix-dispatch)
  ;;       orderless-component-separator #'orderless-escapable-split-on-space)
  (setq completion-styles '(orderless basic)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))


;; Part of corfu
(use-package corfu-popupinfo
  :after corfu
  :hook (corfu-mode . corfu-popupinfo-mode)

  :custom
  (corfu-auto nil)
  (corfu-auto-delay 2.1)
  (corfu-auto-prefix 0)
  (corfu-min-width 80)
  (corfu-max-width corfu-min-width)
  (corfu-preselect-first nil)        ; Preselect first candidate?
  ;(corfu-popupinfo-delay '(0.25 . 0.1))
  ;(corfu-popupinfo-hide nil)
  :config
  (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
(use-package corfu-terminal
  :if (not (display-graphic-p))
  :ensure t
  :config
  (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :ensure t
;  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


;; Add extensions
(use-package cape
  :ensure t
  ;; Bind dedicated completion commands
  ;; Alternative prefix keys: C-c p, M-p, M-+, ...
  :config
  ;(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

  :bind (
         ;;("C-c p p" . completion-at-point) ;; capf
;;         ("C-c p t" . complete-tag)        ;; etags
;;("M-TAB" . completion-at-point)
;;         ("C-c p d" . cape-dabbrev)        ;; or dabbrev-completion
;;         ("C-c p h" . cape-history)
;;         ("C-c p f" . cape-file)
;;         ("C-c p k" . cape-keyword)
;;         ("C-c p s" . cape-elisp-symbol)
;;         ("C-c p e" . cape-elisp-block)
;;         ("C-c p a" . cape-abbrev)
;;         ("C-c p l" . cape-line)
;;         ("C-c p w" . cape-dict)
;;         ("C-c p \\" . cape-tex)
;;         ("C-c p _" . cape-tex)
;;         ("C-c p ^" . cape-tex)
;;         ("C-c p &" . cape-sgml)
;;         ("C-c p r" . cape-rfc1345)
)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;;(add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;;(defun check-expansion ()
;;  (save-excursion
;;    (if (looking-at "\\_>")
;;        t
;;      (backward-char 1)
;;      (if (looking-at "\\.")
;;          t
;;        (backward-char 1)
;;        (if (looking-at "::")
;;            t
;;          nil)))))
;;
;;
;;(defun tab-indent-or-complete ()
;;  (interactive)
;;  (if (minibufferp)
;;      (minibuffer-complete)
;;    (if (or (not yas/minor-mode) (null (do-yas-expand)))
;;        (if (check-expansion)
;;            ;(company-complete-common)
;;            (completion-at-point)
;;          (indent-for-tab-command)))))

;(defun local/clang-capf-init ()
;  "Add `clang-capf' to `completion-at-point-functions'."
;  (add-hook 'completion-at-point-functions #'clang-capf nil t))
;
;(add-hook 'c-mode-hook #'local/clang-capf-init)

(add-hook 'eshell-mode-hook #'capf-autosuggest-mode)

(use-package yasnippet
  :ensure t
  :hook
  (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  )


;; (use-package yasnippet-classic-snippets
;;   :ensure t
;;   )

;; (use-package consult-yasnippet
;;   :ensure t
;;   )

(define-key yas-minor-mode-map [(tab)] nil)
(define-key yas-minor-mode-map (kbd "TAB") nil)

(use-package corfu-prescient
  :ensure t
  )

(use-package yasnippet-capf
  :ensure t
  :after cape
 ; :config
;  (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )

(setq yasnippet-capf-lookup-by 'name) ;; Prefer the name of the snippet instead

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'yasnippet-capf
                     #'tempel-expand
                     #'cape-file))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


;; Configure Tempel
(use-package tempel
  ;; Require trigger prefix before template name when completing.
  ;; :custom
  ;; (tempel-trigger-prefix "<")

  :bind (("M-+" . tempel-complete) ;; Alternative tempel-expand
         ("M-*" . tempel-insert))

  :init

  ;; Setup completion at point
  (defun tempel-setup-capf ()
    ;; Add the Tempel Capf to `completion-at-point-functions'.
    ;; `tempel-expand' only triggers on exact matches. Alternatively use
    ;; `tempel-complete' if you want to see all matches, but then you
    ;; should also configure `tempel-trigger-prefix', such that Tempel
    ;; does not trigger too often when you don't expect it. NOTE: We add
    ;; `tempel-expand' *before* the main programming mode Capf, such
    ;; that it will be tried first.
    (setq-local completion-at-point-functions
                (cons #'tempel-expand
                      completion-at-point-functions)))

  (add-hook 'conf-mode-hook 'tempel-setup-capf)
  (add-hook 'prog-mode-hook 'tempel-setup-capf)
  (add-hook 'text-mode-hook 'tempel-setup-capf)

  ;; Optionally make the Tempel templates available to Abbrev,
  ;; either locally or globally. `expand-abbrev' is bound to C-x '.
  ;; (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
  ;; (global-tempel-abbrev-mode)
)

;; Optional: Add tempel-collection.
;; The package is young and doesn't have comprehensive coverage.
(use-package tempel-collection)
