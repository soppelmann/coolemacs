;(straight-use-package '(far :type git :repo "https://github.com/soppelmann/far.el"))
(use-package swiper
  :ensure t)

(use-package far
  :straight (far :type git :host github :repo "soppelmann/far.el")
  :after meow
  :ensure t
)

(use-package evil-matchit
  :ensure t)


;; (define-key input-decode-map 
    ;; (kbd "C-[") 
    ;; [control-bracketleft])

(defun local/conditional-quit ()
  "Quit buffer if not in Eat-Eshell minor mode, otherwise do nothing."
  (interactive)
  (unless (bound-and-true-p eat-eshell-mode)
    (meow-quit)))

(use-package meow
  :ensure t
  :init
  ;; This allows 'a' key to work as in vim.
  (setq meow-use-cursor-position-hack t))
(require 'meow)

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("y" . meow-save)
   '("<escape>" . ignore)
   )
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("=" . avy-goto-word-1)
   '("+" . swiper)
   '("`" . far-fill-paragraph)
   '("%" . evilmi-jump-items-native)
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
   ;; '("M-]" . xref-find-definitions)
   ;; '("M-[" . xref-go-back)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("x" . meow-delete)
   ;; '("d" . meow-kill)
   '("d" . kill-region)
   '("D" . meow-kill-whole-line)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . local/conditional-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("s" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '(":" . meow-goto-line)
   '("/" . phi-search)
   '("C-r" . undo-redo)
   '("<escape>" . ignore)
   ))

;; meow-use-clipboard
(setq meow-use-clipboard t)

(require 'meow)
(meow-setup)
(meow-global-mode 1)

;; Make C-w kill word backward and rebind kill-region to C-x C-k
(global-set-key "\C-w" 'backward-kill-word)
(global-set-key "\C-x\C-k" 'kill-region)
(global-set-key "\C-c\C-k" 'kill-region)


;; swap windows
(global-set-key (kbd "C-x w") 'ace-swap-window)

;; use meow-tree-sitter
(use-package meow-tree-sitter
  :ensure t)
(meow-tree-sitter-register-defaults)


(with-eval-after-load 'meow
  (add-hook 'meow-insert-mode-hook (lambda () (setq delete-active-region t)))
  (add-hook 'meow-insert-exit-hook (lambda () (setq delete-active-region nil)))
)

