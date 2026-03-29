;;; circe-self-message.el --- IRCv3 self-message support for Circe -*- lexical-binding: t -*-

;; Author: see circe.el
;; Keywords: IRC, chat, comm

;; This file is part of Circe.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;;; Commentary:

;; Adds IRCv3 self-message support to Circe.
;;
;; When a bouncer (e.g. soju, ZNC, Quassel) replays a message that
;; WE sent from another connected client, or when a server that
;; negotiated the echo-message capability reflects our own outgoing
;; PRIVMSG/NOTICE back to us, the wire format looks like:
;;
;;   :yournick!~user@host PRIVMSG someone_else :Hello world!
;;
;; The sender nick equals OUR nick, and the target is the person or
;; channel we were talking to.  Without this extension Circe silently
;; drops these messages, because the original display handlers only
;; handle (a) messages where WE are the target and (b) messages to
;; channels — they have no path for "we are the sender".
;;
;; This extension adds that third path.  Every other code path in the
;; original handlers is preserved character-for-character, so regular
;; messages are unaffected.
;;
;; Installation:
;;
;;   (require 'circe-self-message)
;;   (circe-self-message-enable)
;;
;; echo-message capability and duplicate prevention:
;;
;;   Calling `circe-self-message-enable' also arranges for the
;;   "echo-message" IRCv3 capability to be requested on every new
;;   connection.  When the server grants echo-message it reflects
;;   every outgoing PRIVMSG/NOTICE back to us.  Circe's
;;   `circe-command-SAY' and `circe-command-ME' already display the
;;   message locally before sending, so without intervention you
;;   would see every message twice.
;;
;;   This extension solves that by suppressing the local display in
;;   SAY and ME when echo-message is active — the server reflection
;;   becomes the one and only display.  When echo-message was NOT
;;   granted by the server, SAY and ME display normally as before.
;;
;; CTCP / ACTION:
;;
;;   irc.el extracts CTCP messages (including /me actions) from
;;   PRIVMSGs and fires them as "irc.ctcp.ACTION" events *before*
;;   they reach the "irc.message" handler.  The existing
;;   `circe-display-ctcp-action' handler already handles the channel
;;   and query cases correctly, so self-message /me replays work
;;   automatically with no extra code here.

;;; Code:

(require 'circe)
(require 'cl-lib)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; User options
;;;; ─────────────────────────────────────────────────────────────────────────

(defcustom circe-self-message-show-echo t
  "When non-nil, display self-messages received from the server.

A self-message is a PRIVMSG or NOTICE where the sender nick is
our own nick.  This happens in two situations:

  1. A bouncer (soju, ZNC, …) replays a message sent by another
     one of our connected clients.
  2. A server that granted the echo-message capability reflects
     every outgoing PRIVMSG/NOTICE back to us.

When non-nil (the default) these are shown in the appropriate
buffer using `circe-format-self-say' / `circe-format-self-message',
matching the behaviour of HexChat, mIRC, WeeChat, and other
clients in the IRCv3 self-message compatibility table.

Set to nil to silently suppress them — useful if your bouncer
deduplicates echo and you would otherwise see every message twice."
  :type 'boolean
  :group 'circe)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Internal helpers
;;;; ─────────────────────────────────────────────────────────────────────────

(defun circe-self-message--channel-p (target)
  "Return non-nil when TARGET is a channel name on the current server.

Consults the CHANTYPES ISUPPORT parameter so that networks using
non-standard prefixes (&, +, !) are handled correctly.  Falls
back to the common set \"#&+!\" before ISUPPORT has arrived."
  (when (> (length target) 0)
    (let* ((proc (ignore-errors (circe-server-process)))
           ;; CHANTYPES is a string of valid channel-prefix characters.
           ;; Default per RFC 1459 is "#&"; IRC servers commonly add "+!"
           (chantypes (if proc
                          (or (irc-isupport proc "CHANTYPES") "#&+!")
                        "#&+!")))
      ;; Check whether the first character of TARGET appears in CHANTYPES.
      ;; seq-contains-p compares with equal, which works on characters.
      (seq-contains-p chantypes (aref target 0) #'eq))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Duplicate prevention: suppress local display when echo-message is active
;;;; ─────────────────────────────────────────────────────────────────────────

;; `circe-command-SAY' and `circe-command-ME' call `circe-display' to
;; show the message locally *before* sending it to the server.  When
;; echo-message is active the server immediately reflects the message
;; back, which our display handler shows — causing every message to
;; appear twice.
;;
;; Fix: advise SAY and ME to skip their local `circe-display' call when
;; echo-message was granted on this connection.  The server reflection
;; then becomes the sole display, exactly as in HexChat/WeeChat/mIRC.
;;
;; We track whether echo-message is active ourselves by listening for
;; the "CAP" ACK event, rather than calling a function that may or may
;; not exist in the installed version of irc.el.

(defvar-local circe-self-message--echo-active nil
  "Non-nil in a server buffer when echo-message cap is active.")

(defun circe-self-message--cap-handler (_conn _event _sender _star subcommand caps)
  "Watch for CAP ACK/NAK to track whether echo-message is active.
Runs in the server buffer."
  (let ((cap-list (split-string (or caps "") " " t)))
    (cond
     ((string= subcommand "ACK")
      (when (member "echo-message" cap-list)
        (setq circe-self-message--echo-active t)))
     ((string= subcommand "NAK")
      (when (member "echo-message" cap-list)
        (setq circe-self-message--echo-active nil))))))

(defun circe-self-message--echo-active-p ()
  "Return non-nil if echo-message cap is active on this connection."
  (with-circe-server-buffer
    circe-self-message--echo-active))

(defun circe-self-message--say-around (orig-fn line)
  "Suppress local display in `circe-command-SAY' when echo-message is active."
  (if (not (ignore-errors (circe-self-message--echo-active-p)))
      (funcall orig-fn line)
    ;; echo-message is active: send but skip the local circe-display call.
    ;; Replicate only the send side of circe-command-SAY.
    (when circe-chat-target
      (dolist (l (circe--split-line line))
        (irc-send-PRIVMSG (circe-server-process)
                          circe-chat-target
                          (if (string= l "") " " l))))))

(defun circe-self-message--me-around (orig-fn line)
  "Suppress local display in `circe-command-ME' when echo-message is active."
  (if (not (ignore-errors (circe-self-message--echo-active-p)))
      (funcall orig-fn line)
    (when circe-chat-target
      (irc-send-ctcp (circe-server-process)
                     circe-chat-target
                     "ACTION" line))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Display handler: irc.message  (plain PRIVMSG, non-CTCP)
;;;; ─────────────────────────────────────────────────────────────────────────

;; irc.el fires "irc.message" only for plain PRIVMSGs.  CTCPs (including
;; ACTION) are peeled off earlier and dispatched as "irc.ctcp.*" events,
;; so we never see them here.  The existing circe-display-ctcp-action
;; already handles both channel and query cases, so self-message /me
;; replays are covered automatically.

(defun circe-self-message--display-PRIVMSG (nick userhost _command target text)
  "Like `circe-display-PRIVMSG' but handles self-messages.

A self-message arrives when NICK equals our own nick: the server
(or bouncer) is telling us about a message WE sent.  TARGET is
the recipient — a channel or another user.  We display it using
the self-say format, matching standard client behaviour.

Every other case is handled identically to the original handler
to ensure regular messages are unaffected."
  (cond

   ;; ── Case 1: self-message — WE are the sender, TARGET is the recipient ───
   ;;
   ;; Wire format:  :yournick!~user@host PRIVMSG target :text
   ;;
   ;; This cannot collide with a normal incoming message because IRC
   ;; servers enforce nick uniqueness: no other user can have our nick.
   ((and (circe-server-my-nick-p nick)
         circe-self-message-show-echo)
    (cond
     ;; Self-message to a channel
     ((circe-self-message--channel-p target)
      (with-current-buffer
          (circe-server-get-or-create-chat-buffer target 'circe-channel-mode)
        (circe-display 'circe-format-self-say
                       :body text
                       :nick nick)))
     ;; Self-message to another nick — show in their query buffer
     (t
      (with-current-buffer
          (circe-server-get-or-create-chat-buffer target 'circe-query-mode)
        (circe-display 'circe-format-self-say
                       :body text
                       :nick nick)))))

   ;; ── Case 2 & 3: normal incoming message ─────────────────────────────────
   ;; Copied verbatim from circe-display-PRIVMSG in circe.el.

   ;; Message sent directly to us
   ((circe-server-my-nick-p target)
    (let ((buf (circe-query-auto-query-buffer nick)))
      (if buf
          (with-current-buffer buf
            (circe-display 'circe-format-say
                           :nick nick
                           :userhost (or userhost "server")
                           :body text))
        (with-current-buffer (circe-server-last-active-buffer)
          (circe-display 'circe-format-message
                         :nick nick
                         :userhost (or userhost "server")
                         :body text)))))

   ;; Message to a channel (or any other non-us target)
   (t
    (with-current-buffer
        (circe-server-get-or-create-chat-buffer target 'circe-channel-mode)
      (circe-lurker-display-active nick userhost)
      (circe-display 'circe-format-say
                     :nick nick
                     :userhost (or userhost "server")
                     :body text)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Display handler: irc.notice
;;;; ─────────────────────────────────────────────────────────────────────────

(defun circe-self-message--display-NOTICE (nick userhost _command target text)
  "Like `circe-display-NOTICE' but handles self-messages.

When NICK equals our own nick the notice was sent BY us.  Show it
using `circe-format-self-message' in the appropriate buffer.

Every other case is handled identically to the original handler."
  (cond

   ;; ── Case 1: self-notice — WE sent this notice ───────────────────────────
   ((and (circe-server-my-nick-p nick)
         circe-self-message-show-echo)
    (cond
     ;; Self-notice to a channel
     ((circe-self-message--channel-p target)
      (with-current-buffer
          (circe-server-get-or-create-chat-buffer target 'circe-channel-mode)
        (circe-display 'circe-format-self-message
                       :target target
                       :body text)))
     ;; Self-notice to a nick
     (t
      (with-current-buffer
          (or (circe-server-get-chat-buffer target)
              (circe-server-last-active-buffer))
        (circe-display 'circe-format-self-message
                       :target target
                       :body text)))))

   ;; ── Cases 2-4: normal incoming notice ────────────────────────────────────
   ;; Copied verbatim from circe-display-NOTICE in circe.el.

   ;; Server notice: no userhost means it came from the server itself
   ((not userhost)
    (with-current-buffer (circe-server-last-active-buffer)
      (circe-display 'circe-format-server-notice
                     :server nick
                     :body text)))

   ;; Notice addressed directly to us
   ((circe-server-my-nick-p target)
    (with-current-buffer (or (circe-server-get-chat-buffer nick)
                             (circe-server-last-active-buffer))
      (circe-display 'circe-format-notice
                     :nick nick
                     :userhost (or userhost "server")
                     :body text)))

   ;; Notice to a channel or other target
   (t
    (with-current-buffer (or (circe-server-get-chat-buffer target)
                             (circe-server-last-active-buffer))
      (circe-display 'circe-format-notice
                     :nick nick
                     :userhost (or userhost "server")
                     :body text)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; echo-message capability injection
;;;; ─────────────────────────────────────────────────────────────────────────

;; `irc-connect' is called from `circe-reconnect--internal' with all
;; connection parameters as a flat plist.  We need to append
;; "echo-message" to the :cap-req list without touching circe.el itself.
;;
;; We use a :filter-args advice on `irc-connect' that is only active
;; during the execution of `circe-reconnect--internal' (via an :around
;; advice on that function).  This avoids leaving a permanent hook on
;; `irc-connect' that could affect unrelated callers.

(defun circe-self-message--inject-cap (args)
  "filter-args advice: splice \"echo-message\" into ARGS for `irc-connect'.
ARGS is the flat plist argument list."
  (let ((cap-req (plist-get args :cap-req)))
    (unless (member "echo-message" cap-req)
      (plist-put args :cap-req (append cap-req (list "echo-message"))))
    args))

(defun circe-self-message--reconnect-around (orig-fn &rest args)
  ":around advice on `circe-reconnect--internal': scope the cap injection."
  (advice-add 'irc-connect :filter-args #'circe-self-message--inject-cap)
  (unwind-protect
      (apply orig-fn args)
    (advice-remove 'irc-connect #'circe-self-message--inject-cap)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Enable / disable
;;;; ─────────────────────────────────────────────────────────────────────────

(defvar circe-self-message--saved-message-handler nil
  "Saved \"irc.message\" handler, restored by `circe-self-message-disable'.")

(defvar circe-self-message--saved-notice-handler nil
  "Saved \"irc.notice\" handler, restored by `circe-self-message-disable'.")

;;;###autoload
(defun circe-self-message-enable ()
  "Enable IRCv3 self-message support in Circe.

Installs enhanced \"irc.message\" and \"irc.notice\" display
handlers that detect when the sender is our own nick and display
those messages using the self-say / self-message format strings,
exactly matching the behaviour described in the IRCv3 self-message
compatibility table (HexChat, mIRC, WeeChat, XChat column: ✓).

Also arranges for the \"echo-message\" capability to be requested
on every new connection.  Reconnect (\\[circe-reconnect]) to
activate echo-message on an existing session.

Safe to call multiple times — calling it again when already
enabled is a no-op.

Call `circe-self-message-disable' to undo everything."
  (interactive)
  ;; Guard against double-registration: if our handler is already in
  ;; place, do nothing.  Without this guard, a second call (e.g. from
  ;; re-evaluating the file in *scratch*) would save OUR handler as
  ;; "the original", making disable restore the wrong thing.
  (when (eq (circe-get-display-handler "irc.message")
            #'circe-self-message--display-PRIVMSG)
    (when (called-interactively-p 'any)
      (message "circe-self-message: already enabled."))
    (cl-return-from circe-self-message-enable))
  ;; Save whatever is currently registered so we can restore it exactly.
  ;; nil is a valid saved value — it means «no custom handler registered».
  (setq circe-self-message--saved-message-handler
        (circe-get-display-handler "irc.message")
        circe-self-message--saved-notice-handler
        (circe-get-display-handler "irc.notice"))
  (circe-set-display-handler "irc.message"
                             #'circe-self-message--display-PRIVMSG)
  (circe-set-display-handler "irc.notice"
                             #'circe-self-message--display-NOTICE)
  ;; Watch CAP ACK/NAK to know when echo-message is actually active.
  ;; We add this to the global handler table so it fires on every connection.
  (irc-handler-add (circe-irc-handler-table)
                   "CAP" #'circe-self-message--cap-handler)
  ;; Reset the echo-active flag when the connection drops, so that on
  ;; reconnect we start in the "not active" state until ACK arrives.
  (irc-handler-add (circe-irc-handler-table)
                   "conn.disconnected"
                   #'circe-self-message--disconnected-handler)
  (advice-add 'circe-command-SAY :around #'circe-self-message--say-around)
  (advice-add 'circe-command-ME  :around #'circe-self-message--me-around)
  (advice-add 'circe-reconnect--internal :around
              #'circe-self-message--reconnect-around)
  (message "circe-self-message: enabled. Reconnect to request echo-message."))

(defun circe-self-message--disconnected-handler (conn _event)
  "Reset echo-active flag in the server buffer when disconnected."
  (with-current-buffer (irc-connection-get conn :server-buffer)
    (setq circe-self-message--echo-active nil)))

;;;###autoload
(defun circe-self-message-disable ()
  "Disable IRCv3 self-message support and restore original handlers."
  (interactive)
  (circe-set-display-handler "irc.message"
                             circe-self-message--saved-message-handler)
  (circe-set-display-handler "irc.notice"
                             circe-self-message--saved-notice-handler)
  (setq circe-self-message--saved-message-handler nil
        circe-self-message--saved-notice-handler nil)
  (advice-remove 'circe-command-SAY #'circe-self-message--say-around)
  (advice-remove 'circe-command-ME  #'circe-self-message--me-around)
  (advice-remove 'circe-reconnect--internal
                 #'circe-self-message--reconnect-around)
  ;; Note: irc-handler-table has no remove function in irc.el, so the
  ;; CAP and conn.disconnected handlers linger but are harmless (they
  ;; just set/clear a buffer-local variable that is no longer consulted).
  (message "circe-self-message: disabled."))

(provide 'circe-self-message)

;; Auto-enable when the file is loaded or eval'd, so that
;; M-x eval-buffer in *scratch* is all you need.
(circe-self-message-enable)

;;; circe-self-message.el ends here
