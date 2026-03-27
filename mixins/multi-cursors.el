;;; multi-cursors.el --- Programming stuff -*- lexical-binding: t; -*-

;; Modify multiple occurrences simultaneously
(use-package iedit
  :straight t
  :bind ("C-;" . iedit-mode))

;; (use-package mc-extras
  ;; :ensure t)

;; Multiple cursors implementation for Emacs
(use-package multiple-cursors
 :straight t
 :bind (("C->"           . mc/mark-next-like-this)
        ("C-<"           . mc/mark-previous-like-this)
        ("C-M->"         . mc/skip-to-next-like-this)
        ("C-M-<"         . mc/skip-to-previous-like-this)
        ("C-c C-<"       . mc/mark-all-like-this)
        ("C-s-c x"       . mc/mark-more-like-this-extended)
        ("C-s-c a"       . mc/mark-all-dwim)
        ("C-s-c s"       . mc/mark-next-symbol-like-this)
        ("C-s-c S"       . mc/mark-previous-like-this-symbol)
        ("C-s-c C-s"     . mc/mark-all-symbols-like-this)
        ("C-s-c C-S-c"   . mc/edit-lines)
        ("C-s-c C-e"     . mc/edit-ends-of-lines)
        ("C-s-c C-a"     . mc/edit-beginnings-of-lines)
        ("C-s-<mouse-1>" . mc/add-cursor-on-click)
        ("C-s-c SPC"     . +mc/transient))
 :commands (+mc/transient)
 :config
 ;; Add some extra commands to be run on all cursors
 (cl-callf append mc--default-cmds-to-run-for-all
   '(;; Some extra Emacs commands
     beginning-of-visual-line end-of-visual-line kill-region forward-sexp backward-sexp
     tab-to-tab-stop indent-for-tab-command transient-noop comment-line comment-dwim
     ;; MinEmacs' commands
     +kill-whitespace-or-word +kill-region-or-backward-word +backward-kill-whitespace-or-word
     ;; `avy'
     avy-goto-char avy-goto-char-timer avy-goto-char-in-line avy-goto-char-2
     ;; `avy-zap'
     avy-zap-to-char avy-zap-up-to-char avy-zap-to-char-dwim avy-zap-up-to-char-dwim
     ;; `crux'
     crux-smart-kill-line crux-smart-open-line crux-smart-open-line-above
     ;; `expreg'
     expreg-expand expreg-contract
     ;; Org specific commands
     org-delete-char org-self-insert-command))

 (cl-callf append mc--default-cmds-to-run-once
   '(pixel-scroll-precision +mc/mark-all-symbol-overlays))

 (with-eval-after-load 'transient
   (transient-define-prefix +mc/transient ()
     "Multiple-cursors transient menu."
     [["Up"
       ("p" "prev" mc/mark-previous-like-this :transient t)
       ("P" "skip" mc/skip-to-previous-like-this :transient t)
       ("M-p" "unmark" mc/unmark-previous-like-this :transient t)
       ("|" "align with input CHAR" mc/vertical-align :transient t)]
      ["Down"
       ("n" "next" mc/mark-next-like-this :transient t)
       ("N" "skip" mc/skip-to-next-like-this :transient t)
       ("M-n" "unmark" mc/unmark-next-like-this :transient t)]
      ["Misc"
       ("l" "edit lines" mc/edit-lines)
       ("a" "mark all" mc/mark-all-like-this)
       ("s" "search" mc/mark-all-in-region-regexp)
       ("<mouse-1>" "click" mc/add-cursor-on-click :transient t)]
      ["Insert"
       ("0" "insert numbers" mc/insert-numbers)
       ("A" "insert letters" mc/insert-letters)]]))

 ;; Integrate with `symbol-overlay'
 (with-eval-after-load 'symbol-overlay
   ;; https://lmno.lol/alvaro/its-all-up-for-grabs-and-it-compounds
   (defun +mc/mark-all-symbol-overlays (&optional discard)
     "Mark all symbol overlays using multiple cursors.
When DISCARD is non-nil, discard the current cursors before creating the
new ones."
     (interactive "P")
     (when discard (mc/remove-fake-cursors))
     (when-let* ((overlays (symbol-overlay-get-list 0))
                 (point (point))
                 (point-overlay (seq-find
                                 (lambda (overlay)
                                   (and (<= (overlay-start overlay) point)
                                        (<= point (overlay-end overlay))))
                                 overlays))
                 (offset (- point (overlay-start point-overlay))))
       (setq deactivate-mark t)
       (mapc (lambda (overlay)
               (unless (eq overlay point-overlay)
                 (mc/save-excursion
                  (goto-char (+ (overlay-start overlay) offset))
                  (mc/create-fake-cursor-at-point))))
             overlays)
       (mc/maybe-multiple-cursors-mode)))

   (with-eval-after-load 'transient
     ;; Add to the transient menu after the "s"
     (transient-append-suffix '+mc/transient "s" '("S" "symbol overlays" +mc/mark-all-symbol-overlays)))

   (with-eval-after-load 'casual-symbol-overlay
     (transient-append-suffix 'casual-symbol-overlay-tmenu '(-2)
       ["Multiple cursors" ("c" "Mark all" +mc/mark-all-symbol-overlays)]))))

;; Emacs text actions using LSP symbol information
(use-package gambol
 :straight (:host codeberg :repo "woolsweater/gambol.el")
 :hook (eglot-managed-mode . gambol-mode)
 :bind
 (("M-g ," . gambol:go-to-previous)
  ("M-g ." . gambol:go-to-next)
  ([remap mc/mark-all-dwim] . gambol:edit-all)
  ([remap occur] . +gambol:occur-dwim)
  :map gambol-repeat-map
  ("," . gambol:go-to-previous)
  ("." . gambol:go-to-next)
  ("e" . gambol:edit-all)
  ("o" . gambol:occur))
 :init
 (with-eval-after-load 'embark (gambol:install-embark-integration)) ; Integrate with `embark'
 (defun +gambol:occur-dwim ()
   "Call `gambol:occur' if in an Eglot managed buffer, fallback to `occur'."
   (interactive)
   (unless (and (featurep 'eglot) (eglot-managed-p) (ignore-errors (gambol:occur) t))
     (call-interactively #'occur))))
