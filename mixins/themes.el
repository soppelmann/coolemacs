
;; (use-package nerd-icons
;;   :ensure t
;;   )

;(use-package
; modus-themes
; :ensure t
; :config (define-key global-map (kbd "<f5>") #'modus-themes-toggle))

(use-package
 ef-themes
 :ensure t
 :config (define-key global-map (kbd "<f5>") #'ef-themes-toggle))

(setq
 ef-themes-custom-auto-reload nil
 ef-themes-mixed-fonts t
 ef-themes-variable-pitch-ui nil
 ef-themes-italic-constructs t
 ef-themes-bold-constructs nil
 ef-themes-org-blocks nil
 ef-themes-completions '((t . (extrabold)))
 ef-themes-prompts nil
 ef-themes-headings
 '((agenda-structure . (variable-pitch light 2.2))
   (agenda-date . (variable-pitch regular 1.3))
   (t . (regular 1.15))))

(setq
 ef-themes-to-toggle '(ef-tritanopia-dark ef-arbutus)
)

;;(setq
;; modus-themes-custom-auto-reload nil
;;modus-themes-to-toggle '(modus-operandi-tinted modus-vivendi-deuteranopia)
;; modus-themes-mixed-fonts t
;; modus-themes-variable-pitch-ui nil
;; modus-themes-italic-constructs t
;; modus-themes-bold-constructs nil
;; modus-themes-org-blocks nil
;; modus-themes-completions '((t . (extrabold)))
;; modus-themes-prompts nil
;; modus-themes-headings
;; '((agenda-structure . (variable-pitch light 2.2))
;;   (agenda-date . (variable-pitch regular 1.3))
;;   (t . (regular 1.15))))

;;(setq ef-themes-common-palette-overrides
;;;;(setq modus-themes-common-palette-overrides
;;      '((cursor magenta-cooler)
;;        ;; Make the fringe invisible.
;;        (fringe unspecified)
;;
;;        ;; Line numbers same as BG
;;        (bg-line-number-active unspecified)
;;        (bg-line-number-inactive unspecified)
;;
;;        ;; Make line numbers less intense and add a shade of cyan
;;        ;; for the current line number.
;;        ;(fg-line-number-inactive "gray50")
;;        ;(fg-line-number-active cyan-cooler)
;;        ;(bg-line-number-inactive unspecified)
;;        ;(bg-line-number-active unspecified)
;;        ;; Make the current line of `hl-line-mode' a fine shade of
;;        ;; gray (though also see my `lin' package).
;;        (bg-hl-line bg-dim)
;;        ;; Make the region have a cyan-green background with no
;;        ;; specific foreground (use foreground of underlying text).
;;        ;; "bg-sage" refers to Salvia officinalis, else the common
;;        ;; sage.
;;        (bg-region bg-sage)
;;        (fg-region unspecified)
;;        ;; Make matching parentheses a shade of magenta.  It
;;        ;; complements the region nicely.
;;        (bg-paren-match bg-magenta-intense)
;;        ;; Make email citations faint and neutral, reducing the
;;        ;; default four colors to two; make mail headers cyan-blue.
;;        (mail-cite-0 fg-dim)
;;        (mail-cite-1 blue-faint)
;;        (mail-cite-2 fg-dim)
;;        (mail-cite-3 blue-faint)
;;        (mail-part cyan-warmer)
;;        (mail-recipient blue-warmer)
;;        (mail-subject magenta-cooler)
;;        (mail-other cyan-warmer)
;;        ;; Change dates to a set of more subtle combinations.
;;        (date-deadline magenta-cooler)
;;        (date-scheduled magenta)
;;        (date-weekday fg-main)
;;        (date-event fg-dim)
;;        (date-now blue-faint)
;;        ;; Make tags (Org) less colorful and tables look the same as
;;        ;; the default foreground.
;;        (prose-done cyan-cooler)
;;        (prose-tag fg-dim)
;;        (prose-table fg-main)
;;        ;; Make headings less colorful (though I never use deeply
;;        ;; nested headings).
;;        (fg-heading-2 blue-faint)
;;        (fg-heading-3 magenta-faint)
;;        (fg-heading-4 blue-faint)
;;        (fg-heading-5 magenta-faint)
;;        (fg-heading-6 blue-faint)
;;        (fg-heading-7 magenta-faint)
;;        (fg-heading-8 blue-faint)
;;        ;; Make the active mode line a fine shade of lavender
;;        ;; (purple) and tone down the gray of the inactive mode
;;        ;; lines.
;;        ;(bg-mode-line-active bg-lavender)
;;        ;(border-mode-line-active bg-lavender)
;;
;;        ;(bg-mode-line-inactive bg-dim)
;;        ;(border-mode-line-inactive bg-inactive)
;;        ;; Make the prompts a shade of magenta, to fit in nicely with
;;        ;; the overall blue-cyan-purple style of the other overrides.
;;        ;; Add a nuanced background as well.
;;        ;(bg-prompt bg-magenta-nuanced)
;;        ;(fg-prompt magenta-cooler)
;;        ;; Tweak some more constructs for stylistic constistency.
;;        ;(name blue-warmer)
;;        ;(identifier magenta-faint)
;;        ;(keybind magenta-cooler)
;;        ;(accent-0 magenta-cooler)
;;        ;(accent-1 cyan-cooler)
;;        ;(accent-2 blue-warmer)
;;        ;(accent-3 red-cooler)
;;        ))

;; Make the active mode line have a pseudo 3D effect (this assumes
;; you are using the default mode line and not an extra package).
;; (custom-set-faces
;;  '(mode-line ((t :box (:style released-button)))))


;(defun my-modus-themes-custom-faces ()
;  (set-face-attribute 'fill-column-indicator nil :background (face-attribute 'default :background)))
;
;(add-hook 'modus-themes-after-load-theme-hook #'my-modus-themes-custom-faces)
(add-hook 'prog-mode-hook 'display-fill-column-indicator-mode)


;; All configs that need to apply for
;; each new emacsclient window
;; need to be wrapped inside this hook!
(add-hook
 'after-make-frame-functions
 (lambda (frame)
   ;(fontaine-set-preset 'large)
   (read-only-mode)

   (scroll-bar-mode 0)
   (ef-themes-select 'ef-tritanopia-dark)
   ;(ef-themes-select 'ef-maris-dark)
   ;;(modus-themes-select 'modus-operandi)
   ;(modus-themes-select 'modus-vivendi-deuteranopia)
   ;(load-theme 'grb256)
   ;'(restore-desktop)
   ))

;;(load-theme 'modus-vivendi)
(ef-themes-select 'ef-tritanopia-dark)
;(ef-themes-select 'ef-maris-dark)
;;(modus-themes-select 'modus-operandi)
;(modus-themes-select 'modus-vivendi-deuteranopia)
;;(load-theme 'grb256)
