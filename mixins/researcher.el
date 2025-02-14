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
  (TeX-source-correlate-method 'synctex)
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
    (pdf-loader-install)
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

;; (setq LaTeX-command "latex --synctex=1") ;; optional: enable synctex
(setq reftex-extra-bindings t)
(use-package reftex
  :ensure t
  :custom
  (reftex-cite-prompt-optional-args t)) ; Prompt for empty optional arguments in cite

(setq LaTeX-biblatex-use-Biber t)
(add-hook 'doc-view-mode-hook 'auto-revert-mode)

;; (setq TeX-auto-save t 
      ;; TeX-parse-self t)

;; (add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)

(use-package pdf-tools
  :ensure t
  :mode (("\\.pdf\\'" . pdf-view-mode))
  :custom
  (pdf-view-display-size 'fit-width)
  (pdf-annot-activate-created-annotations t "automatically annotate highlights")
  :config
  (setq TeX-source-correlate-mode t)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  :hook
  (pdf-view-mode . (lambda() (setq display-line-numbers-mode nil)))
  (pdf-view-mode . (lambda() (auto-revert-mode 1)))
  (pdf-view-mode . (lambda() (pdf-view-themed-minor-mode)))
  :bind (:map pdf-view-mode-map
            ("<left>" . pdf-view-previous-page-command)
            ("<right>" . pdf-view-next-page-command))
  )

(add-to-list 'org-file-apps '("\\.pdf\\'" . emacs))

(pdf-loader-install)

(use-package latex-preview-pane
  :ensure t
  :config
  (latex-preview-pane-enable))
(add-hook 'LaTeX-mode-hook 'latex-preview-pane-mode)

;; (add-to-list 'revert-without-query ".+\\.pdf$")

(setq org-export-in-background nil)

;; Use zotero bib for org mode always
;; Use latexmk for PDF export
(setq reftex-default-bibliography '("/Users/getz/Nextcloud/Zotero.bib"))
(setq org-latex-to-pdf-process (list "latexmk -pdf -bibtex %f"))

;; (require 'ox-bibtex)
(setq org-latex-pdf-process '("latexmk -pdf --synctex=1 -outdir=%o %f"))

(setq reftex-plug-into-AUCTeX t)
(use-package consult-reftex
  :straight (consult-reftex :type git :host github :repo "karthink/consult-reftex")
  :ensure t)


(use-package org
  :hook ((org-mode . visual-line-mode)  ; wrap lines at word breaks
         (org-mode . org-modern-mode)    ; Look nice
         (org-mode . flyspell-mode))    ; Look nice

  :bind (:map global-map
              ("C-c l s" . org-store-link)          ; Mnemonic: link → store
              ("C-c l i" . org-insert-link-global)) ; Mnemonic: link → insert
  :config
  (require 'oc-csl)                     ; citation support
  ;; (add-to-list 'org-export-backends 'md)

  ;; Make org-open-at-point follow file links in the same window
  (setf (cdr (assoc 'file org-link-frame-setup)) 'find-file)

  ;; Make exporting quotes better
  (setq org-export-with-smart-quotes t)

  ;; Support shift select
  (setq org-support-shift-select t)
  )

;; use flyspell
;; (add-hook 'text-mode-hook #'flyspell-mode)
;; (add-hook 'flyspell-mode-hook #'flyspell-local-vars)
;; (defun flyspell-local-vars ()
  ;; (add-hook 'hack-local-variables-hook #'flyspell-buffer))
  ;; (add-hook 'hack-local-variables #'flyspell-buffer nil 'local))

(add-hook 'flyspell-mode-hook #'flyspell-buffer)
