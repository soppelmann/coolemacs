;;; Emacs Bedrock
;;;
;;; Mixin: Researcher

;;; Usage: Append or require this file from init.el for research helps. If you
;;; write papers in LaTeX and need to manage your citations or keep track of
;;; notes, this package is for you.
;;;
;;; Highly recommended to enable this mixin with the UI enhancements in
;;; `base.el', as citar works best with the Vertico completing-read interface.
;;; Also recommended is the `writer.el' mixin, which adds some nice features for
;;; spell-checking etc.

;;; Contents:
;;;
;;;  - Citation Management
;;;  - Authoring
;;;  - Note Taking: Org-Roam
;;;  - Note Taking: Denote

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Critical variables
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; These variables must be set for citar to work properly!

(setq citar-bibliography '("~/Nextcloud/Zotero.bib")) ; paths to your bibtex files

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Citation Management
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package bibtex-utils
  :ensure t)

(use-package bibtex-completion
  :ensure t)

(use-package bibtex-capf
  :ensure t)

(use-package citar
  :ensure t
  :bind (("C-c b" . citar-insert-citation)
         :map minibuffer-local-map
         ("M-b" . citar-insert-preset))
  :custom
  ;; Allows you to customize what citar-open does
  (citar-file-open-functions '(("html" . citar-file-open-external)
                               ;; ("pdf" . citar-file-open-external)
                               (t . find-file))))

;; Optional: if you have the embark package installed, enable the ability to act
;; on citations with citar by invoking `embark-act'.
(use-package citar-embark
 :after citar embark
 :diminish ""
 :no-require
 :config (citar-embark-mode))

;; (use-package citar-org-roam
;;   :diminish ""
;;   ;; To get this to work both citar *and* org-roam have to have been used
;;   :after citar org-roam
;;   :no-require
;;   :config
;;   (citar-org-roam-mode)
;;   (setq citar-org-roam-note-title-template "${author} - ${title}\n#+filetags: ${tags}"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Authoring
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Org-Roam
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; (use-package org-roam
;;   :ensure t
;;   :config
;;   ;; Make sure the backlinks buffer always shows up in a side window
;;   (add-to-list 'display-buffer-alist
;;                '("\\*org-roam\\*"
;;                  (display-buffer-in-side-window)
;;                  (side . right)
;;                  (window-width . 0.4)
;;                  (window-height . fit-window-to-buffer)))

;;   (org-roam-db-autosync-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Note Taking: Denote
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; TODO

(use-package auctex
  :ensure t
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master nil))

(use-package tex
  :ensure auctex
  :mode ("\\.tex\\$" . latex-mode)
  :custom
  (TeX-source-correlate-mode t)
  (TeX-source-correlate-method 'synctax)
  (TeX-auto-save t)
  (TeX-parse-self t)
  (TeX-electric-math (cons "$" "$"))
  (LaTeX-electric-left-right-brace t)
  (reftex-plug-into-AUCTeX t)
  (TeX-view-program-selection '((output-pdf "PDF Tools")))
  (TeX-source-correlate-start-server t)
  (TeX-master nil)
  :config
  ;; (defun save-and-compile ()
    ;; (interactive)
    ;; (let (TeX-save-query) (TeX-save-document (TeX-master-file)))
    ;; (TeX-command-run-all nil))
  ;; (bind-key "<f5>" 'save-and-compile)
  (progn
    ;; (pdf-loader-install)
    ;; Update PDF buffers after successful LaTeX runs
    (add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)
    (yas-reload-all)
    (add-hook 'LaTeX-mode-hook
          (lambda ()
        (reftex-mode t)
        (flyspell-mode t)
        (corfu-mode t)
        (yas-minor-mode t)
        (LaTeX-math-mode t)
        (tex-fold-mode 1)
        ;; Set Latexmk to be the default compiler
        (setq TeX-command-default "LatexMk")))))

;; (use-package reftex
  ;; :ensure t
  ;; :custom
  ;; (reftex-cite-prompt-optional-args t)) ; Prompt for empty optional arguments in cite

(setq LaTeX-biblatex-use-Biber t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; (setq TeX-auto-save t 
      ;; TeX-parse-self t)

;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package pdf-tools
  :ensure t
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :hook
  (pdf-view-mode . (lambda() (setq display-line-numbers-mode nil))))
