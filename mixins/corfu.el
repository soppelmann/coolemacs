;; Popup completion-at-point
(use-package corfu
  :ensure t
  :hook (lsp-completion-mode . kb/corfu-setup-lsp) ; Use corfu for lsp completion
  :custom
  (corfu-preselect 'prompt) ;; Always preselect the prompt
 ; (corfu-preview-current t)
  (corfu-cycle t)
  ;; (corfu-on-exact-match 'quit)
  :init
  (global-corfu-mode)
  ;(corfu-prescient-mode 1)
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
              ("<backtab>"  . corfu-insert)
              ("RET"        . corfu-insert))
  :config
  ;; Setup lsp to use corfu for lsp completion
  (defun kb/corfu-setup-lsp ()
;;    "Use orderless completion style with lsp-capf instead of the
;;default lsp-passthrough."
    (setf (alist-get 'styles (alist-get 'lsp-capf completion-category-defaults))
          '(orderless)))

  )



(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)


;; Option 2: Undo the Eglot modification of completion-category-defaults
(with-eval-after-load 'eglot
   (setq completion-category-defaults nil))

;; Enable cache busting, depending on if your server returns
;; sufficiently many candidates in the first place.
(advice-add 'eglot-completion-at-point :around #'cape-wrap-buster)

(defun my/eglot-capf ()
  (setq-local completion-at-point-functions
              (list (cape-capf-super
                     #'eglot-completion-at-point
                     #'tempel-expand
                     #'cape-file))))

(add-hook 'eglot-managed-mode-hook #'my/eglot-capf)

;; SPC as separator
(setq corfu-separator 32)

;; highly recommanded to use corfu-separator with "32" (space)
(define-key corfu-map (kbd "SPC")
  (lambda ()
    (interactive)
    (if current-prefix-arg
        ;;we suppose that we want leave the word like that, so do a space
        (progn
          (corfu-quit)
          (insert " "))
      (if (and (= (char-before) corfu-separator)
               (or
                ;; check if space, return or nothing after
                (not (char-after))
                (= (char-after) ?\s)
                (= (char-after) ?\n)))
          (progn
            (corfu-insert)
            (insert " "))
        (corfu-insert-separator)))))

