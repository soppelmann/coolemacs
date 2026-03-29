;;; circe-stable-colors.el --- Stable nick colors for Circe -*- lexical-binding: t; -*-

(require 'circe-color-nicks)

(defun circe-nick-hash (nick)
  "Return a stable integer hash for NICK."
  (let ((hash 0))
    (dolist (char (string-to-list nick))
      (setq hash (+ (* hash 31) char)))
    (abs hash)))

(defun circe-nick-color-for-nick--stable (nick)
  "Determine a stable color for NICK based on a hash of the nick string.
This replaces the default behavior where nicks get colors from a
sequential pool, which changes on every restart."
  (let ((color (gethash nick circe-nick-color-mapping)))
    (when (not color)
      (let* ((pool (circe-nick-color-generate-pool))
             (index (mod (circe-nick-hash nick) (length pool)))
             (picked (nth index pool)))
        (puthash nick picked circe-nick-color-mapping)
        (puthash picked (float-time) circe-nick-color-timestamps)))
    (gethash nick circe-nick-color-mapping)))

(advice-add 'circe-nick-color-for-nick
            :override
            #'circe-nick-color-for-nick--stable)

(provide 'circe-stable-colors)
;;; circe-stable-colors.el ends here
