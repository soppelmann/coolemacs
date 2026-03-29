;;; consult-circe.el --- consult circe buffer management. -*- lexical-binding: t -*-

;; Package-Requires: ((emacs "27.1") (consult "0.34") (circe "0.0") (cl-lib "0.5"))
;; Keywords: consult circe

;;; Commentary:

;; Jump to Circe buffers easily with Consult.

;; A call to `consult-circe` will show a list of circe channel and server
;; buffers organised into narrow groups, allowing you to switch channels
;; easily or kill circe buffers.
;;
;; Available commands:
;;   `consult-circe'            - channels + queries + servers in one session
;;   `consult-circe-channels'   - channels only
;;   `consult-circe-servers'    - servers only
;;   `consult-circe-queries'    - queries only
;;   `consult-circe-new-activity' - buffers with recent tracking activity
;;   `consult-circe-by-server'  - channels grouped under their server
;;
;; Largely based on helm-circe by Les Harris
;; https://github.com/lesharris/helm-circe

;;; Code:

(require 'cl-lib)
(require 'consult)
(require 'circe)

;;;; Internal helpers — buffer collection

(defun consult-circe--channel-buffers ()
  "Return a list of circe channel buffer names."
  (cl-loop for buf in (buffer-list)
           when (eq 'circe-channel-mode
                    (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)))

(defun consult-circe--server-buffers ()
  "Return a list of circe server buffer names."
  (cl-loop for buf in (buffer-list)
           when (eq 'circe-server-mode
                    (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)))

(defun consult-circe--query-buffers ()
  "Return a list of circe query buffer names."
  (cl-loop for buf in (buffer-list)
           when (eq 'circe-query-mode
                    (buffer-local-value 'major-mode buf))
           collect (buffer-name buf)))

(defun consult-circe--recent-buffers ()
  "Return a list of circe buffer names that have unread activity."
  tracking-buffers)

;;;; Annotation helper

(defun consult-circe--annotate (candidate)
  "Return a brief annotation string for buffer CANDIDATE."
  (when-let* ((buf (get-buffer candidate)))
    (with-current-buffer buf
      (concat
       (propertize " " 'display '(space :align-to 40))
       (propertize (symbol-name major-mode) 'face 'completions-annotations)))))

;;;; Consult source constructors

(defun consult-circe--make-source (name buffers-fn narrow-key)
  "Build a `consult' source plist.

NAME is the group heading string, BUFFERS-FN is a zero-argument
function that returns a list of candidate buffer names, and
NARROW-KEY is the character used to narrow to this group."
  `(:name     ,name
    :narrow   ,narrow-key
    :category buffer
    :annotate consult-circe--annotate
    :state    ,#'consult--buffer-state
    :action   ,(lambda (candidate) (switch-to-buffer candidate))
    :items    ,buffers-fn))

(defvar consult-circe--source-channels
  (consult-circe--make-source "Channels"
                               #'consult-circe--channel-buffers
                               ?c)
  "Consult source for circe channel buffers.")

(defvar consult-circe--source-servers
  (consult-circe--make-source "Servers"
                               #'consult-circe--server-buffers
                               ?s)
  "Consult source for circe server buffers.")

(defvar consult-circe--source-queries
  (consult-circe--make-source "Queries"
                               #'consult-circe--query-buffers
                               ?q)
  "Consult source for circe query buffers.")

(defvar consult-circe--source-activity
  (consult-circe--make-source "New Activity"
                               #'consult-circe--recent-buffers
                               ?a)
  "Consult source for circe buffers with recent tracking activity.")

;;;; Kill / part action

(defun consult-circe--kill-buffer (candidate)
  "Kill the buffer named CANDIDATE (part channel / disconnect server)."
  (when (get-buffer candidate)
    (kill-buffer candidate)))

;;;; Connection guard

(defun consult-circe--connected-p ()
  "Return non-nil if there is at least one live circe buffer of any kind."
  (or (consult-circe--server-buffers)
      (consult-circe--channel-buffers)
      (consult-circe--query-buffers)))

(defun consult-circe--ensure-connected ()
  "Return non-nil if circe is active, otherwise offer to connect.

When no circe buffers exist the user is prompted to connect via
`circe'.  Returns nil so callers can abort their own flow."
  (or (consult-circe--connected-p)
      (when (yes-or-no-p "No active Circe connections.  Connect now? ")
        (call-interactively #'circe)
        nil)))

;;;; Generic selection runner

(defun consult-circe--read (prompt sources)
  "Run `consult--multi' with PROMPT over SOURCES.

If no circe connections are active, offer to run `circe' first."
  (when (consult-circe--ensure-connected)
    (consult--multi sources
                    :prompt prompt
                    :require-match t
                    :sort nil)))

;;;; Public commands

;;;###autoload
(defun consult-circe ()
  "Jump to a circe channel, query, or server buffer.

Candidates are grouped into three narrow sections:
  c — Channels
  q — Queries
  s — Servers

Use \\[consult-circe-new-activity] to filter to buffers with
unread activity, or \\[consult-circe-by-server] to browse
channels grouped under each server."
  (interactive)
  (consult-circe--read
   "Circe: "
   (list consult-circe--source-channels
         consult-circe--source-queries
         consult-circe--source-servers)))

;;;###autoload
(defun consult-circe-new-activity ()
  "Jump to a circe buffer that has unread activity."
  (interactive)
  (consult-circe--read "Circe activity: "
                        (list consult-circe--source-activity)))

;;;###autoload
(defun consult-circe-channels ()
  "Jump to a circe channel buffer."
  (interactive)
  (consult-circe--read "Circe channel: "
                        (list consult-circe--source-channels)))

;;;###autoload
(defun consult-circe-servers ()
  "Jump to a circe server buffer."
  (interactive)
  (consult-circe--read "Circe server: "
                        (list consult-circe--source-servers)))

;;;###autoload
(defun consult-circe-queries ()
  "Jump to a circe query buffer."
  (interactive)
  (consult-circe--read "Circe query: "
                        (list consult-circe--source-queries)))

;;;###autoload
(defun consult-circe-by-server ()
  "Jump to a circe channel, with candidates grouped by server.

Each server becomes its own narrow group using the first character
of the server buffer name as the narrow key.  Selecting a candidate
switches to that channel buffer."
  (interactive)
  (let* ((curbuf  (current-buffer))
         (servers (consult-circe--server-buffers))
         (sources
          (save-excursion
            (cl-loop
             for server in servers
             for idx    from 0
             ;; Use a window of printable ASCII chars as narrow keys.
             ;; 49 = ?1, so servers get keys 1, 2, 3 … up to ~75 servers.
             for key = (+ 49 idx)
             do (switch-to-buffer server)
             collect
             (let ((chat-bufs (mapcar #'buffer-name
                                      (circe-server-chat-buffers))))
               `(:name     ,server
                 :narrow   ,key
                 :category buffer
                 :annotate consult-circe--annotate
                 :state    ,#'consult--buffer-state
                 :action   ,(lambda (c) (switch-to-buffer c))
                 :items    ,(lambda () (or chat-bufs (list "")))))))))
    (switch-to-buffer curbuf)
    (if sources
        (consult--multi sources
                        :prompt "Circe (by server): "
                        :require-match t
                        :sort nil)
      (consult-circe--ensure-connected))))

;;;; Embark integration (optional)
;;
;; If you use Embark, add these actions to `embark-buffer-map' so you can
;; act on circe buffers from any consult session:
;;
;;   (with-eval-after-load 'embark
;;     (define-key embark-buffer-map (kbd "P")
;;       #'consult-circe-embark-part-buffer))
;;
;;;###autoload
(defun consult-circe-embark-part-buffer (buf)
  "Part from / disconnect BUF (an Embark buffer target)."
  (interactive "bPart from buffer: ")
  (consult-circe--kill-buffer buf))

;;;; Keep circe buffers out of the normal buffer list

(defun consult-circe--bury ()
  "Bury the current buffer so it is hidden from \\[switch-to-buffer]."
  (bury-buffer (current-buffer)))

(dolist (hook '(circe-server-mode-hook
                circe-channel-mode-hook
                circe-query-mode-hook))
  (add-hook hook #'consult-circe--bury))

(provide 'consult-circe)

;;; consult-circe.el ends here
