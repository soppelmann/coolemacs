;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq load-prefer-newer t)

(add-to-list 'default-frame-alist '(undecorated . t))
;(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Startup speed, annoyance suppression
;; Maximize GC threshold during startup (will be reset in init.el)
(setq gc-cons-threshold most-positive-fixnum)
(setq gc-cons-percentage 0.6)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)

(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway
(menu-bar-mode -1)                      ; All these tools are in the menu-bar anyway

(setenv "LSP_USE_PLISTS" "true")


(setq default-frame-alist '(
    ;(fullscreen . maximized)
(setenv "LSP_USE_PLISTS" "true")

    ;; You can turn off scroll bars by uncommenting these lines:
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (undecorated . t)
    ;; Setting the face in here prevents flashes of
    ;; color as the theme gets activated
   (background-color . "#000000")
   (ns-appearance . dark)
    (ns-transparent-titlebar . t)
    )
)
