(use-package treesit-auto
  :custom
  (treesit-auto-install 'prompt)
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

    (add-to-list 'major-mode-remap-alist '(c-mode . c-ts-mode))
    (add-to-list 'major-mode-remap-alist '(c++-mode . c++-ts-mode))
    (add-to-list 'major-mode-remap-alist
                 '(c-or-c++-mode . c-or-c++-ts-mode))

(setq treesit-available (and (fboundp 'treesit-available-p) (treesit-available-p)))

;; Automatically manage `treesit' grammars
(use-package treesit-auto
  :straight (:host github :repo "renzmann/treesit-auto")
  :init
  ;; (treesit-auto-install-all)
  (global-treesit-auto-mode)
  :custom
  (treesit-auto-install 'prompt)
  :config

  ;; Ensure that installed tree-sitter languages have their corresponding `x-ts-mode' added to `auto-mode-alist'
  (treesit-auto-add-to-auto-mode-alist 'all)

  (defvar +treesit-auto-create-parser-modes-deny '(org-mode))

  ;; Create `treesit' parsers when they are available even in non-treesit modes.
  ;; This is useful for packages like `virtual-format', `treesit-fold', `expreg'
  ;; and `ts-movement'.
  (defun +treesit-auto-create-parser-in-buffer (&optional buffer)
    "Create `treesit' in BUFF-NAME, even if the mode isn't a ts-mode."
    (interactive (list (when current-prefix-arg (get-buffer (read-buffer "Create treesit parser in buffer: ")))))
    (let ((buffer (or buffer (current-buffer)))
          (interact-p (called-interactively-p 'interactive)))
      (if (treesit-available-p)
          (when (or (not (derived-mode-p +treesit-auto-create-parser-modes-deny))
                    (and interact-p (y-or-n-p "Creating parsers for `%S' is blacklisted in `+treesit-auto-create-parser-modes-deny', continue?")))
            (with-current-buffer buffer
              (if-let* ((lang-recipe (cl-find-if (lambda (recipe) (eq major-mode (treesit-auto-recipe-remap recipe)))
                                                 treesit-auto-recipe-list))
                        (lang (treesit-auto-recipe-lang lang-recipe))
                        (lang (and (treesit-language-available-p lang) lang)))
                  (when (or (not (treesit-parser-list buffer lang))
                            (and interact-p (y-or-n-p (format "The %S buffer already have a %S language parser, continue?" buffer lang))))
                    (treesit-parser-create lang)
                    (when interact-p (message "Created a %S language parser in %S" lang buffer)))
                (when interact-p (user-error "No installed tree-sitter grammar for mode `%s'" major-mode)))))
        (when interact-p (user-error "Tree-sitter isn't available in this Emacs build")))))

  (add-hook 'after-change-major-mode-hook '+treesit-auto-create-parser-in-buffer))

;; Move and edit code blocks based on tree-sitter AST
(use-package ts-movement
  :straight (:host github :repo "psibi/ts-movement")
  :hook ((prog-mode conf-mode) . +ts-movement-maybe)
  :init
  (defun +ts-movement-maybe ()
    "Enable `ts-movement-mode' when if `major-mode' is a trees-sitter mode."
    (run-with-timer 1.0 nil (lambda () (when (treesit-parser-list) (ts-movement-mode 1)))))
  :config
  (with-eval-after-load 'transient
    (transient-define-prefix +ts-movement-transient ()
      "Transient for ts-movement."
      [[("d" "delete-overlay-at-point" tsm/delete-overlay-at-point :transient t)
        ("D" "clear-overlays-of-type" tsm/clear-overlays-of-type :transient t)
        ("C-b" "backward-overlay" tsm/backward-overlay :transient t)
        ("C-f" "forward-overlay" tsm/forward-overlay :transient t)
        ("c" "tsm/mc/mark-all-overlays" tsm/mc/mark-all-overlays :transient t)]
       [("a" "node-start" tsm/node-start :transient t)
        ("e" "node-end" tsm/node-end :transient t)
        ("b" "node-prev" tsm/node-prev :transient t)
        ("f" "node-next" tsm/node-next :transient t)]
       [("p" "node-parent" tsm/node-parent :transient t)
        ("n" "node-child" tsm/node-child :transient t)
        ("N" "node-children" tsm/node-children :transient t)
        ("s" "node-children-of-type" tsm/node-children-of-type :transient t)
        ("m" "node-mark" tsm/node-mark :transient t)]]
      [("Q" "Quit" ignore :transient t)])))


;; Tree-sitter based code folding
(use-package treesit-fold
  :straight (:host github :repo "emacs-tree-sitter/treesit-fold")
  )
