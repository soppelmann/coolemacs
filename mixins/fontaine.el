(use-package fontaine :ensure t)

(setq fontaine-presets
      '((small
         :default-family "Hack"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Noto Sans"
         :variable-pitch-weight normal
         :variable-pitch-height 1.0
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (font3270
         :default-family "3270 Nerd Font Mono"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Iosevka
         :default-family "Iosevka Nerd Font Mono"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (regular
         :default-family "Iosevka Comfy"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (medium
         :default-family "Source Code Pro"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Source Sans Pro"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight semibold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (large
         :default-family "Iosevka Comfy"
         :default-weight semilight
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (IBM
         :default-family "Ac437 IBM MDA"
         :default-weight normal
         :default-height 220
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Ac427 IBM MDA"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Gallant
         :default-family "gallant12x22"
         :default-weight normal
         :default-height 200
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight normal ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "gallant12x22"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Julia
         :default-family "JuliaMono"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight normal ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "JuliaMono"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (SFMono
         :default-family "SFMono Nerd Font"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight normal ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "SFMono Nerd Font"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Plex
         :default-family "IBM Plex Mono"
         :default-weight normal
         :default-height 144
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight normal ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "IBM Plex Mono"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Comfy
         :default-family "Px437 IBM PS/55 re." ;Mx437 DOS/V re. JPN30 Is also nice
         :default-weight semilight
         :default-height 175
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Px437 IBM PS/55 re."
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (OpenBSD
         :default-family "Spleen 32x64"
         :default-weight normal
         :default-height 200
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Spleen 32x64"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Cozette
         :default-family "CozetteVector"
         :default-weight medium
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "CozetteVector"
         :variable-pitch-weight medium
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Fantasque
         :default-family "FantasqueSansM Nerd Font Mono"
         :default-weight medium
         :default-height 145
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FantasqueSansM Nerd Font Mono"
         :variable-pitch-weight medium
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Berkeley
         :default-family "Berkeley Mono"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Berkeley Mono"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Pragmata
         :default-family "Pragmata Pro"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Pragmata Pro"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (XanhMono
         :default-family "Xanh Mono"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Xanh Mono"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (MajorMono
         :default-family "Major Mono Display"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Major Mono Display"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (BerkeleyVariable
         :default-family "Berkeley Mono Variable"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Berkeley Mono Variable"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight normal
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (Comic
         :default-family "Comic Code Ligatures"
         :default-weight normal
         :default-height 140
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "Comic Code Ligatures"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)
        (presentation
         :default-family "Iosevka Comfy"
         :default-weight semilight
         :default-height 170
         :fixed-pitch-family nil ; falls back to :default-family
         :fixed-pitch-weight nil ; falls back to :default-weight
         :fixed-pitch-height 1.0
         :variable-pitch-family "FiraGO"
         :variable-pitch-weight normal
         :variable-pitch-height 1.05
         :bold-family nil ; use whatever the underlying face has
         :bold-weight bold
         :italic-family nil
         :italic-slant italic
         :line-spacing nil)))


;; Set last preset or fall back to desired style from `fontaine-presets'.
(fontaine-set-preset (or (fontaine-restore-latest-preset) 'regular))

;; The other side of `fontaine-restore-latest-preset'.
(add-hook 'kill-emacs-hook #'fontaine-store-latest-preset)

;; Persist font configurations while switching themes (doing it with
;; my `modus-themes' and `ef-themes' via the hooks they provide).
(dolist (hook '(modus-themes-after-load-theme-hook ef-themes-post-load-hook))
  (add-hook hook #'fontaine-apply-current-preset))

;;;; Show Font (preview fonts)
;; Read the manual: <https://protesilaos.com/emacs/show-font>
(use-package show-font
  :ensure t
  :commands (show-font-select-preview show-font-list)
  :config
  ;; These are the defaults, but I keep them here for easier access.
  (setq show-font-pangram 'prot)
  (setq show-font-character-sample
        "
ABCDEFGHIJKLMNOPQRSTUVWXYZ
abcdefghijklmnopqrstuvwxyz
0123456789   !@#$¢%^&*~|
`'\"‘’“”.,;:  ()[]{}—-_+=<>

()[]{}<>«»‹› 6bB8&0ODdoa 1tiIlL|\/
!ij c¢ 5$Ss 7Z2z 9gqp nmMNNMW uvvwWuuw
x×X .,·°;:¡!¿?`'‘’   ÄAÃÀ TODO
"))
