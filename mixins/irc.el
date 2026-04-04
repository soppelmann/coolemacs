(use-package circe
  :ensure t
  :straight (circe
             :repo "emacs-circe/circe"
             :type git
             :host github
             :files (:defaults "circe-pingmon.el"))
  :config
  (load "~/.emacs.d/mixins/circe-stable-colors.el")
  (require 'circe-pingmon)
  (circe-pingmon-mode))

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(defun my/pass-get (entry)
  (auth-source-pass-get 'secret entry))

(setq epg-pinentry-mode 'loopback)

(setq circe-default-nick "getz"
      circe-default-user "getz"
      circe-default-realname "getz")

(setq circe-network-options
      `(("ZNC/LIBERA"
         :tls t
         :host "iblis.dflund.se"
         :server-buffer-name "⇄ Libera"
         :port 1026
         :user "getz/LIBERA"
         :pass ,(my/pass-get "znc/password"))
        ("ZNC/EFNET"
         :tls t
         :host "iblis.dflund.se"
         :server-buffer-name "⇄ EFNET"
         :port 1026
         :user "getz/EFNET"
         :pass ,(my/pass-get "znc/password"))
        ("ZNC/dataswamp"
         :tls t
         :host "iblis.dflund.se"
         :server-buffer-name "⇄ Dataswamp"
         :port 1026
         :user "getz/dataswamp"
         :pass ,(my/pass-get "znc/password"))
        ("ZNC/IRCnet"
         :tls t
         :host "iblis.dflund.se"
         :server-buffer-name "⇄ IRCnet"
         :port 1026
         :user "getz/IRCnet"
         :pass ,(my/pass-get "znc/password"))))

(defconst irc-left-padding 8)

(setq circe-format-say         (format "{nick:+%ss} │ {body}" irc-left-padding)
      circe-format-self-say    circe-format-say
      circe-format-action      (format "{nick:+%ss} * {body}" irc-left-padding)
      circe-format-self-action circe-format-action
      circe-color-nicks-everywhere t
      circe-reduce-lurker-spam t
      )

(enable-circe-color-nicks)

(setq lui-fill-type nil)
(setq lui-time-stamp-only-when-changed-p nil)
;; (setq lui-fill-column                     80)

(defvar irc-truncate-nick-char ?…)
(defun irc-circe-truncate-nicks ()
  (when-let ((beg (text-property-any (point-min) (point-max) 'lui-format-argument 'nick)))
    (goto-char beg)
    (let ((end (next-single-property-change beg 'lui-format-argument))
          (nick (plist-get (plist-get (text-properties-at beg) 'lui-keywords) :nick)))
      (when (> (length nick) irc-left-padding)
        (compose-region (+ beg irc-left-padding -1) end irc-truncate-nick-char)))))
(add-hook 'lui-pre-output-hook 'irc-circe-truncate-nicks)

(defun irc-init-lui-margins ()
  (setq lui-time-stamp-position 'left
        lui-time-stamp-format "[%H:%M] "
        lui-time-stamp-only-when-changed-p nil
        ;; left-margin-width (length (format-time-string "[%H:%M] "))
        ))
(add-hook 'lui-mode-hook 'irc-init-lui-margins)

(defun irc-init-lui-wrapping ()
  (setq fringes-outside-margins t
        word-wrap t
        wrap-prefix (concat (make-string (length (format-time-string "[%H:%M] ")) ? )
                            (make-string (+ irc-left-padding 1) ? )
                            "│ ")))
(add-hook 'lui-mode-hook 'irc-init-lui-wrapping)

(use-package circe-menu
  :straight (circe-menu
             :repo "KarimAziev/circe-menu"
             :type git
             :host github)
  :bind ("<f10>" . circe-menu))

(setq tracking-faces-priorities '(circe-highlight-nick-face
                                   circe-query-face))

;; (defun tpanum/enable-circe-notifications ()
;;   "Turn on notifications."
;;   (interactive)
;;   (run-at-time "5sec" nil 'enable-circe-notifications))

;; (defun circe-notifications-notify (nick body channel)
;;   (if (and (not (string-match "^\[[0-9]+:[0-9]+\]" body))
;;            (not (string-match "^\\\*\\\*\\\*$" nick))
;;            (not (string-match "^/\\(PART\\|JOIN\\)" body)))
;;       (alert
;;        (concat "<b>" nick "</b>: " body)
;;        :severity circe-notifications-alert-severity
;;        :title channel
;;        :category "chat"
;;        :style circe-notifications-alert-style)))

;; (use-package circe-notifications
;;   :ensure t
;;   :config
;;   (setq circe-notifications-wait-for 2
;;         circe-notifications-JOIN nil
;;         circe-notifications-PART nil)
;;   (add-to-list 'circe-notifications-watch-strings "getz")
;;   (add-hook 'circe-server-connected-hook 'tpanum/enable-circe-notifications))
