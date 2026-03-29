(use-package circe
  :ensure t
  :straight (circe
             :repo "emacs-circe/circe"
             :type git
             :host github
  :files (:defaults "circe-pingmon.el"))
  :config
  (require 'circe-pingmon)
  (circe-pingmon-mode)
  )

(use-package auth-source-pass
  :config
  (auth-source-pass-enable))

(defun my/pass-get (entry)
  (auth-source-pass-get 'secret entry))

(setq epg-pinentry-mode 'loopback)

(setq circe-default-nick "getz"
      circe-default-user "getz"
      circe-default-realname "getz"
      )

(setq circe-new-buffer-behavior 'ignore)

(setq circe-network-options
      (let ((server-passwd
	     (lambda (server-name)
	       (read-passwd (format "Password for server: %s? " server-name)))))
	`(
	 ("ZNC/LIBERA"
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
          :pass ,(my/pass-get "znc/password"))
	 )))

  ;; (setq lui-fill-column                     80
  ;;       lui-time-stamp-position             'right
  ;;       lui-time-stamp-only-when-changed-p  t
  ;;       lui-time-stamp-format               "[%H:%M]"
  ;;       lui-fill-type                       "                "
  ;;       circe-server-max-reconnect-attempts 2
  ;;       circe-format-server-topic           "{new-topic}"
  ;;       circe-format-say                    "{nick:-16s}{body}"
  ;;       circe-format-self-say               circe-format-say
  ;;       circe-default-part-message          nil
  ;;       circe-default-quit-message          nil
  ;;       circe-network-defaults              nil
  ;;       lui-logging-file-format             "{buffer}/%Y-%m-%d.txt")

(setq circe-format-say "{nick:-9s} {body}")
(setq circe-color-nicks-everywhere t)
(enable-circe-color-nicks)
(setq circe-reduce-lurker-spam t)
(setq erc-server-auto-reconnect nil)


(setq
   lui-fill-type "        >"
   lui-time-stamp-position 'right-margin
   lui-time-stamp-format "%H:%M")
(setq lui-fill-column 90)
(setq lui-fill-type nil)

;; (setq lui-fill-type "    ")
;; (setq lui-time-stamp-position 'left)
;; (setq lui-time-stamp-format "[%H:%M]  ")

(add-hook 'lui-mode-hook 'my-circe-set-margin)
(defun my-circe-set-margin ()
  (setq right-margin-width 5))


(use-package circe-menu
  :straight (circe-menu
             :repo "KarimAziev/circe-menu"
             :type git
             :host github)
  :bind ("<f10>" . circe-menu))

(setq tracking-faces-priorities '(circe-highlight-nick-face
                                   circe-query-face))
