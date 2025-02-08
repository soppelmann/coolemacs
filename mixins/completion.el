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

;; unbind M-TAB globally
;; (global-unset-key (kbd "M-TAB"))

;; Popup completion-at-point
(use-package corfu
  :straight
  (:type git
         :host github
         :repo "minad/corfu")
  :ensure t
  ;; :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  ;; (corfu-preselect 'prompt) ;; Always preselect the prompt
 ; (corfu-preview-current t)
  (corfu-cycle t)
  (corfu-on-exact-match 'show)
  :init
  (global-corfu-mode)
  ;; (corfu-prescient-mode 1)
  (corfu-history-mode)

  ;; Optionally use TAB for cycling, default is `corfu-complete'.
  :bind (:map corfu-map
              ("M-SPC"      . corfu-insert-separator)
              ;; ("SPC"        . corfu-insert-separator)
              ("M-TAB"      . corfu-next)
              ("M-<tab>"      . corfu-next)
              ("M-TAB"      . corfu-next)
              ("M-<tab>"      . corfu-next)
              ("TAB"        . corfu-complete)
              ([tab]        . corfu-complete)
              ("S-TAB"      . corfu-previous)
              ([backtab]    . corfu-previous)
              ("<backtab>"  . corfu-previous)
              ("S-<return>" . corfu-complete)
              ("RET"        . corfu-complete))
  :config
  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
   ;; "Use orderless completion style with lsp-capf instead of the
;; default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))
  )

(use-package
 yasnippet
 :demand t
 :init
 (load "yasnippet.el") ; get rid of weird invalid function issue
 )
(use-package
 yasnippet-snippets
 :demand t
 :straight
 '(yasnippet-snippets
   :type git
   :host github
   :repo "jsigman/yasnippet-snippets"))

;; A few more useful configurations...
(use-package emacs
  :custom
  ;; TAB cycle if there are only few candidates
  ;; (completion-cycle-threshold 3)

  ;; Enable indentation+completion using the TAB key.
  ;; `completion-at-point' is often bound to M-TAB.
  (tab-always-indent 'complete)

  ;; Emacs 30 and newer: Disable Ispell completion function.
  ;; Try `cape-dict' as an alternative.
  (text-mode-ispell-word-completion nil)

  ;; Hide commands in M-x which do not apply to the current mode.  Corfu
  ;; commands are hidden, since they are not used via M-x. This setting is
  ;; useful beyond Corfu.
  (read-extended-command-predicate #'command-completion-default-include-p))

;; to enable yasnippet-capf everywhere (optional) (add-to-list 'completion-at-point-functions #'yasnippet-capf)

;; to integrate yasnippet-capf with eglot completion
;; https://github.com/minad/corfu/wiki#making-a-cape-super-capf-for-eglot

;; (fset 'non-greedy-capf (cape-capf-properties #'cape-file :exclusive 'no))

;; (defun mi/eglot-capf-with-yasnippet () (setq-local completion-at-point-functions (list (cape-capf-super #'yasnippet-capf #'eglot-completion-at-point)))) (with-eval-after-load 'eglot (add-hook 'eglot-managed-mode-hook #'mi/eglot-capf-with-yasnippet)) 

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'cape-file
                     #'eglot-completion-at-point
                     #'verilog-ext-capf
                     #'yasnippet-capf
                     ))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;; (defun my/eglot-capf ()
;;   (setq-local completion-at-point-functions
;;               (list (cape-capf-super
;;                      #'eglot-completion-at-point
;;                      #'yasnippet-capf
;;                      ;; #'yas-expand
;;                      #'cape-dabbrev
;;                      #'cape-file))))

;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)
(setq yasnippet-capf-lookup-by 'name) ;; Prefer the name of the snippet instead
(setq yas-indent-line 'fixed)
(add-hook 'eglot-managed-mode-hook #'yas-minor-mode)

;; ;; Strangely, just redefining one of the variations below won't work.
;; ;; All rebinds seem to be needed.

(define-key yas-minor-mode-map [(tab)]        nil)
(define-key yas-minor-mode-map (kbd "TAB")    nil)
(define-key yas-minor-mode-map (kbd "<tab>")  nil)


  ;; (use-package corfu-candidate-overlay
  ;;   :straight (:type git
  ;;              :repo "https://code.bsdgeek.org/adam/corfu-candidate-overlay"
  ;;              :files (:defaults "*.el"))
  ;;   :after corfu
  ;;   :config
  ;;   ;; enable corfu-candidate-overlay mode globally
  ;;   ;; this relies on having corfu-auto set to nil
  ;;   (corfu-candidate-overlay-mode +1)
  ;;   ;; bind Ctrl + Shift + Tab to trigger completion of the first candidate
  ;;   ;; (keybing <iso-lefttab> may not work for your keyboard model)
  ;;   (global-set-key (kbd "C-<tab>") 'corfu-candidate-overlay-complete-at-point)
  ;;   )

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

(defun orderless-fast-dispatch (word index total)
  (and (= index 0) (= total 1) (length< word 4)
       (cons 'orderless-literal-prefix word)))

(orderless-define-completion-style orderless-fast
  (orderless-style-dispatchers '(orderless-fast-dispatch))
  (orderless-matching-styles '(orderless-literal orderless-regexp)))

;; (add-hook 'corfu-mode-hook
;;           (lambda ()
;;             (setq-local completion-styles '(orderless-fast basic)
;;                         completion-category-overrides nil
;;                         completion-category-defaults nil)))

(setq corfu-auto        t
      corfu-auto-delay  0.3  ;; TOO SMALL IS NOT RECOMMENDED!
      corfu-auto-prefix 2
      corfu-quit-no-match t)
      ;; corfu-quit-no-match 'separator)

(add-hook 'corfu-mode-hook
          (lambda ()
            ;; Settings only for Corfu
            (setq-local completion-styles '(basic)
                        completion-category-overrides nil
                        completion-category-defaults nil)))

(add-hook 'eshell-mode-hook (lambda ()
                              ;; (setq-local corfu-auto nil)
                              (corfu-mode)))
;; Part of corfu
;; (use-package corfu-popupinfo
;;   :after corfu
;;   :ensure nil
;;   :hook (corfu-mode . corfu-popupinfo-mode)

;;   :custom
;;   (corfu-auto t)
;;   (corfu-auto-delay 2.1)
;;   (corfu-auto-prefix 0)
;;   (corfu-min-width 80)
;;   (corfu-max-width corfu-min-width)
;;   ;(corfu-preselect-first t)        ; Preselect first candidate?
;; ;  (corfu-preselect-first nil)        ; Preselect first candidate?
;;   ;(corfu-popupinfo-delay '(0.25 . 0.1))
;;   ;(corfu-popupinfo-hide nil)
;;   :config
;;   (corfu-popupinfo-mode))

;; Make corfu popup come up in terminal overlay
;; (use-package corfu-terminal
  ;; :if (not (display-graphic-p))
  ;; :ensure t
  ;; :config
  ;; (corfu-terminal-mode))

;; Pretty icons for corfu
(use-package kind-icon
  :ensure t
;  :if (display-graphic-p)
  :after corfu
  :custom
  (kind-icon-default-face 'corfu-default) ; to compute blended backgrounds correctly
  :config
  (add-to-list 'corfu-margin-formatters #'kind-icon-margin-formatter))


(use-package cape
  :ensure t
  ;; :hook
  ;; (eglot-managed-mode . (lambda ()
  ;;                         (setq-local completion-at-point-functions
  ;;                                     (list (cape-capf-super
  ;;                                            #'cape-file
  ;;                                            #'eglot-completion-at-point
  ;;                                            #'yasnippet-capf
  ;;                                            ;; #'tempel-complete
  ;;                                            ;; (cape-company-to-capf #'company-yasnippet)
  ;;                                            )
  ;;                                           t))))
  ;; :config
  ;; (add-to-list 'completion-at-point-functions
               ;; (cape-capf-super
                ;; #'cape-file
                ;; (cape-capf-prefix-length #'cape-dabbrev 3)))
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)
  ;; (advice-add 'eglot-completion-at-point :around #'cape-wrap-noninterruptible)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  ;; (add-to-list 'completion-at-point-functions #'cape-dabbrev)
  (add-to-list 'completion-at-point-functions #'cape-file)
  ;; (add-to-list 'completion-at-point-functions #'verilog-ext-capf)
  ;; (add-to-list 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-to-list 'completion-at-point-functions #'yasnippet-capf)  
  ;; (add-to-list 'completion-at-point-functions #'tempel-complete)
  ;; (add-to-list 'completion-at-point-functions #'cape-abbrev)
  ;; (add-to-list 'completion-at-point-functions #'cape-keyword)
  ;;(add-to-list 'completion-at-point-functions #'cape-history)
  ;;(add-to-list 'completion-at-point-functions #'cape-tex)
  ;;(add-to-list 'completion-at-point-functions #'cape-sgml)
  ;;(add-to-list 'completion-at-point-functions #'cape-rfc1345)
  ;;(add-to-list 'completion-at-point-functions #'cape-dict)
  ;;(add-to-list 'completion-at-point-functions #'cape-elisp-symbol)
  ;;(add-to-list 'completion-at-point-functions #'cape-line)
)

;; (defun my/eglot-capf ()
;; (setq-local completion-at-point-functions
;; (list (cape-capf-super
;; #'eglot-completion-at-point
;;        ;; #'yasnippet-capf
;; #'cape-dabbrev
;; ;; (cape-company-to-capf #'company-yasnippet)
;; ;; #'tempel-expand
;; #'cape-file

;; ))))
;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;(defun local/clang-capf-init ()
;  "Add `clang-capf' to `completion-at-point-functions'."
;  (add-hook 'completion-at-point-functions #'clang-capf nil t))
;
;(add-hook 'c-mode-hook #'local/clang-capf-init)

;; (add-hook 'eshell-mode-hook #'capf-autosuggest-mode)

(use-package yasnippet
  :ensure t
  ;; :hook
  ;; (prog-mode . yas-minor-mode)
  :config
  (yas-reload-all))

(use-package yasnippet-snippets
  :ensure t
  )

;; (use-package yasnippet-classic-snippets
  ;; :ensure t
  ;; )

;; (use-package consult-yasnippet
;;   :ensure t
;;   )


;; Strangely, just redefining one of the variations below won't work.
;; All rebinds seem to be needed.
;; (define-key yas-minor-mode-map [(tab)]        nil)
;; (define-key yas-minor-mode-map (kbd "TAB")    nil)
;; (define-key yas-minor-mode-map (kbd "<tab>")  nil)

;; (use-package corfu-prescient
;;   :ensure t
;;   )

(use-package yasnippet-capf
  :ensure t
  :after cape
  ;; :config
;;   (add-to-list 'completion-at-point-functions #'yasnippet-capf)
  )


;; (defun my/eglot-capf ()
  ;; (setq-local completion-at-point-functions
              ;; (list (cape-capf-super
                     ;; #'eglot-completion-at-point
                     ;; #'yasnippet-capf
                     ;; #'tempel-expand
                     ;; #'cape-dabbrev
                     ;; #'cape-file))))

;; (add-hook 'eglot-managed-mode-hook #'my/eglot-capf)


;; Configure Tempel

;; (use-package tempel
;;   ;; Require trigger prefix before template name when completing.
;;   ;; :custom
;;   ;; (tempel-trigger-prefix "<")

;;   :init
;;   ;; Setup completion at point
;;   (defun tempel-setup-capf ()
;;     ;; Add the Tempel Capf to `completion-at-point-functions'.
;;     ;; `tempel-expand' only triggers on exact matches. Alternatively use
;;     ;; `tempel-complete' if you want to see all matches, but then you
;;     ;; should also configure `tempel-trigger-prefix', such that Tempel
;;     ;; does not trigger too often when you don't expect it. NOTE: We add
;;     ;; `tempel-expand' *before* the main programming mode Capf, such
;;     ;; that it will be tried first.
;;     (setq-local completion-at-point-functions
;;                 (cons #'tempel-expand
;;                       completion-at-point-functions)))

;;   (add-hook 'conf-mode-hook 'tempel-setup-capf)
;;   (add-hook 'prog-mode-hook 'tempel-setup-capf)
;;   (add-hook 'text-mode-hook 'tempel-setup-capf)

;;   ;; Optionally make the Tempel templates available to Abbrev,
;;   ;; either locally or globally. `expand-abbrev' is bound to C-x '.
;;   (add-hook 'prog-mode-hook #'tempel-abbrev-mode)
;;   (global-tempel-abbrev-mode)
;; )

;; ;; Optional: Add tempel-collection.
;; ;; The package is young and doesn't have comprehensive coverage.
;; (use-package tempel-collection)

;; (use-package eglot-tempel
;;   :preface (eglot-tempel-mode)
;;   :init
;;   (eglot-tempel-mode t))
