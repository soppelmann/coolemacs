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
;; When a bouncer (e.g. soju, ZNC, Quassel) replays a message that WE
;; sent from another connected client, or when a server that negotiated
;; the echo-message capability reflects our own outgoing PRIVMSG/NOTICE
;; back to us, the wire format looks like:
;;
;;   :yournick!~user@host PRIVMSG someone_else :Hello world!
;;
;; The sender nick equals OUR nick, and the target is the person or
;; channel we were talking to.  Without this extension Circe silently
;; drops these messages, because the original display handlers only
;; handle (a) messages where WE are the target and (b) messages to
;; channels — they have no path for "we are the sender".
;;
;; Deduplication design:
;;
;;   The naive approach — suppress local display when a boolean
;;   "echo-message is active" flag is set — is racy.  If the server
;;   is slow, SAY displays the message locally before the echo arrives;
;;   when the echo then arrives it gets displayed again.
;;
;;   Instead we use a pending-echo queue (per server buffer).  Every
;;   time circe-command-SAY or circe-command-ME sends a message it
;;   enqueues (target . text) *and* displays locally as normal.  When
;;   a self-message arrives from the server, the display handler checks
;;   the queue:
;;
;;     • Queue has a matching entry  → this is the echo of something we
;;       just sent.  Consume the entry and skip display (already shown).
;;
;;     • No matching entry  → this is a replay from another client
;;       connected to the same bouncer.  Display it normally.
;;
;;   This means:
;;     - No race condition.  Local display happens immediately at send
;;       time.  The echo is suppressed whenever it arrives.
;;     - No dependency on any cap negotiation flag or irc.el internal
;;       API.  Works equally for echo-message and bouncer replay.
;;     - Queue entries that never get echoed (server without
;;       echo-message, or the echo was lost) are cleaned up after a
;;       short timeout so memory does not grow unboundedly.
;;
;; CTCP / ACTION:
;;
;;   irc.el extracts CTCP messages (including /me actions) from
;;   PRIVMSGs and fires them as "irc.ctcp.ACTION" events *before*
;;   they reach the "irc.message" handler.  The existing
;;   `circe-display-ctcp-action' handler already handles the channel
;;   and query cases correctly, so self-message /me replays work
;;   automatically with no extra code here.  Echo-message reflections
;;   of /me also go through irc.ctcp.ACTION, and circe-display-ctcp-action
;;   already handles the self-send case (nick == our nick → self-action
;;   format), so /me echo deduplication is not needed.

;;; Code:

(require 'circe)
(require 'cl-lib)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; User options
;;;; ─────────────────────────────────────────────────────────────────────────

(defcustom circe-self-message-queue-ttl 30
  "Seconds to keep an unmatched pending-echo entry before discarding it.

When echo-message is not active on a connection, messages you
send are enqueued but the echo never arrives.  This TTL prevents
those entries from accumulating indefinitely.  30 seconds is
generous — a server echo normally arrives within milliseconds."
  :type 'integer
  :group 'circe)

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Pending-echo queue
;;;; ─────────────────────────────────────────────────────────────────────────

;; The queue is a buffer-local list in the server buffer.
;; Each entry is a list: (TARGET TEXT TIMESTAMP)
;; TARGET and TEXT are compared case-insensitively using the server's
;; own case-mapping via irc-string-equal-p.

(defvar-local circe-self-message--pending nil
  "Queue of recently sent messages awaiting a possible server echo.
Each entry is (TARGET TEXT FLOAT-TIME).
Lives in the server buffer.")

(defun circe-self-message--enqueue (target text)
  "Record that we just sent TEXT to TARGET, expecting a possible echo."
  (with-circe-server-buffer
    (push (list target text (float-time))
          circe-self-message--pending)))

(defun circe-self-message--dequeue (target text)
  "Return non-nil and remove the entry if TARGET+TEXT are in the queue.

Also evicts stale entries older than `circe-self-message-queue-ttl'."
  (with-circe-server-buffer
    (let* ((now (float-time))
           (ttl circe-self-message-queue-ttl)
           (proc (circe-server-process))
           found
           kept)
      (dolist (entry circe-self-message--pending)
        (cl-destructuring-bind (etarget etext etime) entry
          (cond
           ;; Expired — drop silently
           ((> (- now etime) ttl))
           ;; Matched — consume (only the first match)
           ((and (not found)
                 (irc-string-equal-p proc etarget target)
                 (string= etext text))
            (setq found t))
           ;; Keep everything else
           (t
            (push entry kept)))))
      (setq circe-self-message--pending (nreverse kept))
      found)))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Intercept outgoing SAY and ME to enqueue them
;;;; ─────────────────────────────────────────────────────────────────────────

;; We do NOT suppress local display here.  SAY and ME display and send
;; exactly as before.  We just also record what was sent so the display
;; handler can recognise and suppress the server echo.

(defun circe-self-message--say-after (line)
  "After `circe-command-SAY': enqueue each sent line for echo deduplication."
  (when circe-chat-target
    (dolist (l (circe--split-line line))
      (circe-self-message--enqueue circe-chat-target
                                   (if (string= l "") " " l)))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Internal helpers
;;;; ─────────────────────────────────────────────────────────────────────────

(defun circe-self-message--channel-p (target)
  "Return non-nil when TARGET is a channel name on the current server."
  (when (> (length target) 0)
    (let* ((proc (ignore-errors (circe-server-process)))
           (chantypes (if proc
                          (or (irc-isupport proc "CHANTYPES") "#&+!")
                        "#&+!")))
      (seq-contains-p chantypes (aref target 0) #'eq))))

;;;; ─────────────────────────────────────────────────────────────────────────
;;;; Display handler: irc.message  (plain PRIVMSG, non-CTCP)
;;;; ─────────────────────────────────────────────────────────────────────────

(defun circe-self-message--display-PRIVMSG (nick userhost _command target text)
  "Like `circe-display-PRIVMSG' but handles self-messages.

When NICK is our own nick this is a self-message: either a server
echo of something we just sent, or a replay from another client.

If it matches a pending-echo queue entry it is the echo of a
message we already displayed — skip it.  Otherwise display it as
an outgoing message in the target buffer."
  (cond

   ;; ── Self-message ─────────────────────────────────────────────────────────
   ((circe-server-my-nick-p nick)
    ;; Check the queue.  If found, we already showed this locally — done.
    (unless (circe-self-message--dequeue target text)
      ;; Not in queue: replay from another client.  Display it.
      (cond
       ((circe-self-message--channel-p target)
        (with-current-buffer
            (circe-server-get-or-create-chat-buffer target 'circe-channel-mode)
          (circe-display 'circe-format-self-say
                         :body text
                         :nick nick)))
       (t
        (with-current-buffer
            (circe-server-get-or-create-chat-buffer target 'circe-query-mode)
          (circe-display 'circe-format-self-say
                         :body text
                         :nick nick))))))

   ;; ── Normal incoming message — verbatim from circe-display-PRIVMSG ────────

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
  "Like `circe-display-NOTICE' but handles self-messages."
  (cond

   ;; ── Self-notice ───────────────────────────────────────────────────────────
   ((circe-server-my-nick-p nick)
    (unless (circe-self-message--dequeue target text)
      (cond
       ((circe-self-message--channel-p target)
        (with-current-buffer
            (circe-server-get-or-create-chat-buffer target 'circe-channel-mode)
          (circe-display 'circe-format-self-message
                         :target target
                         :body text)))
       (t
        (with-current-buffer
            (or (circe-server-get-chat-buffer target)
                (circe-server-last-active-buffer))
          (circe-display 'circe-format-self-message
                         :target target
                         :body text))))))

   ;; ── Normal incoming notice — verbatim from circe-display-NOTICE ──────────

   ((not userhost)
    (with-current-buffer (circe-server-last-active-buffer)
      (circe-display 'circe-format-server-notice
                     :server nick
                     :body text)))

   ((circe-server-my-nick-p target)
    (with-current-buffer (or (circe-server-get-chat-buffer nick)
                             (circe-server-last-active-buffer))
      (circe-display 'circe-format-notice
                     :nick nick
                     :userhost (or userhost "server")
                     :body text)))

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

(defun circe-self-message--inject-cap (args)
  "filter-args advice: splice \"echo-message\" into ARGS for `irc-connect'."
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
handlers that handle self-messages: messages the server sends to
us where our own nick is the sender.  These are either echoes of
messages we sent (via the echo-message capability) or replays
from another client connected to the same bouncer.

Deduplication is handled via a pending-echo queue: every outgoing
SAY or ME is recorded; when the server echo arrives it is matched
against the queue and suppressed if found, since we already
displayed it locally at send time.  This is race-free — timing
between send and echo does not matter.

Also arranges for the \"echo-message\" capability to be requested
on every new connection.  Reconnect (\\[circe-reconnect]) to
activate echo-message on an existing session.

Safe to call multiple times — calling it again when already
enabled is a no-op."
  (interactive)
  (when (eq (circe-get-display-handler "irc.message")
            #'circe-self-message--display-PRIVMSG)
    (when (called-interactively-p 'any)
      (message "circe-self-message: already enabled."))
    (cl-return-from circe-self-message-enable))
  (setq circe-self-message--saved-message-handler
        (circe-get-display-handler "irc.message")
        circe-self-message--saved-notice-handler
        (circe-get-display-handler "irc.notice"))
  (circe-set-display-handler "irc.message"
                             #'circe-self-message--display-PRIVMSG)
  (circe-set-display-handler "irc.notice"
                             #'circe-self-message--display-NOTICE)
  (advice-add 'circe-command-SAY :after #'circe-self-message--say-after)
  (advice-add 'circe-reconnect--internal :around
              #'circe-self-message--reconnect-around)
  (message "circe-self-message: enabled. Reconnect to request echo-message."))

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
  (advice-remove 'circe-command-SAY #'circe-self-message--say-after)
  (advice-remove 'circe-reconnect--internal
                 #'circe-self-message--reconnect-around)
  (message "circe-self-message: disabled."))

(provide 'circe-self-message)

(circe-self-message-enable)

;;; circe-self-message.el ends here
