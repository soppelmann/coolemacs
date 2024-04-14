;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Basic settings for quick startup and convenience
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;(add-to-list 'default-frame-alist '(undecorated . t))
;(add-to-list 'default-frame-alist '(undecorated-round . t))

;; Startup speed, annoyance suppression
(setq gc-cons-threshold 10000000)
(setq byte-compile-warnings '(not obsolete))
(setq warning-suppress-log-types '((comp) (bytecomp)))

;; Silence stupid startup message
(setq inhibit-startup-echo-area-message (user-login-name))

;; Default frame configuration: full screen, good-looking title bar on macOS
(setq frame-resize-pixelwise t)

(tool-bar-mode -1)                      ; All these tools are in the menu-bar anyway

(setq default-frame-alist '(
    ;(fullscreen . maximized)

    ;; You can turn off scroll bars by uncommenting these lines:
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (undecorated-round . t)
    ;; Setting the face in here prevents flashes of
    ;; color as the theme gets activated
    (background-color . "#000000")
    (ns-appearance . dark)
    (ns-transparent-titlebar . t)
    )
)
