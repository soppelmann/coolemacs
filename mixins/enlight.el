;; Highly customizable startup screen for Emacs
  ;; A wrapper around `desktop-read' to select from the list of saved files
(defun +desktop-read-session (base-name)
  (interactive
   (list (completing-read
          "Select a session file to read: "
          (seq-filter (lambda (file)
                        (and (file-regular-p (expand-file-name file (car desktop-path)))
                             (not (equal "lock" (file-name-extension file)))))
                      (directory-files (car desktop-path) nil directory-files-no-dot-files-regexp)))))
  (let ((desktop-base-file-name base-name)
        (desktop-dirname (car desktop-path)))
    (call-interactively #'desktop-read)))

(defvar +desktop-this-session-base-file-name (format-time-string "%F--%H-%m-%S"))

(defun +desktop-save--timestamp-file:around-a (origfn &rest args)
  (let ((dirname (car desktop-path)))
    (let ((desktop-base-file-name +desktop-this-session-base-file-name)
          (desktop-dirname dirname))
      (apply origfn args))
    (copy-file (expand-file-name +desktop-this-session-base-file-name dirname)
               (expand-file-name desktop-base-file-name dirname)
               'overwrite)))


;; recent files
(use-package recentf
  :ensure t
  :config
  (recentf-mode)
  :custom
  ;; (recentf-save-file
   ;; (convert-standard-filename
       ;; (expand-file-name  "emacs/recentf" (xdg-state-home))))
  (recentf-max-menu-items 25)
  (recentf-max-saved-items 25)
  (recentf-exclude '("/autosave$"
                     ".cache")))

(run-at-time nil 600 'recentf-save-list)

(use-package grid
  :straight (:host github :repo "ichernyshovvv/grid.el"))

(use-package enlight
  :straight (:host github :repo "ichernyshovvv/enlight")
  
  :custom

  (enlight-content
   (concat
    ;; (grid-get-box `( :align center :content ,puffy :width 40 :border nil))
    
 ;;    (grid-get-box `( :align center
 ;;                     :width 40
 ;;                     :content
 ;;                     ;; Art generated by
 ;;                     ;; https://www.patorjk.com/software/taag/#p=display&f=Isometric1&t=emacs
 ;;                     ,(propertize
 ;;                       "      ___           ___           ___           ___           ___
 ;;     /\\  \\         /\\__\\         /\\  \\         /\\  \\         /\\  \\
 ;;    /::\\  \\       /::|  |       /::\\  \\       /::\\  \\       /::\\  \\
 ;;   /:/\\:\\  \\     /:|:|  |      /:/\\:\\  \\     /:/\\:\\  \\     /:/\\ \\  \\
 ;;  /::\\~\\:\\  \\   /:/|:|__|__   /::\\~\\:\\  \\   /:/  \\:\\  \\   _\\:\\~\\ \\  \\
 ;; /:/\\:\\ \\:\\__\\ /:/ |::::\\__\\ /:/\\:\\ \\:\\__\\ /:/__/ \\:\\__\\ /\\ \\:\\ \\ \\__\\
 ;; \\:\\~\\:\\ \\/__/ \\/__/~~/:/  / \\/__\\:\\/:/  / \\:\\  \\  \\/__/ \\:\\ \\:\\ \\/__/
 ;;  \\:\\ \\:\\__\\         /:/  /       \\::/  /   \\:\\  \\        \\:\\ \\:\\__\\
 ;;   \\:\\ \\/__/        /:/  /        /:/  /     \\:\\  \\        \\:\\/:/  /
 ;;    \\:\\__\\         /:/  /        /:/  /       \\:\\__\\        \\::/  /
 ;;     \\/__/         \\/__/         \\/__/         \\/__/         \\/__/    "
 ;;                       'face 'modus-themes-fg-yellow-intense)))
    (grid-get-box
     `( :align center
        :width 40
        :content ,(enlight-menu
    '(("Org Mode"
       ("Org-Agenda (today)" (org-agenda nil "a") "a")
       ("Org directory" (dired org-directory) "o"))
      ("Projects"
       ("Switch to project" project-switch-project "p"))
      ("Files"
         ("Recent" (consult-recent-file) "f"))
      ("Desktop / Session"
       ("Restore session" desktop-read "d")
       ("Bufler switch" bufler-switch-buffer "b")
       ("Restore session from file" +desktop-read-session "R"))))))
    "\n"

    (grid-get-box
     `( :align center
        :width 40
        :content ,(with-temp-buffer
     (insert-file-contents "~/.emacs.d/theo")
     (let ((lines (split-string (buffer-string) "\n" t)))
       (nth (random (length lines)) lines))))))


   )
  :init
      (enlight-open)
    (setq initial-buffer-choice #'enlight))

(setq tab-bar-new-tab-choice "*enlight*") ;; buffer to show in new tabs
(setq tab-bar-tab-hints t) ;; show tab numbers
(setq tab-bar-select-tab-modifiers "super")

(setq tab-bar-format
      '(tab-bar-format-menu-bar tab-bar-format-tabs tab-bar-separator))

(global-set-key (kbd "s-{") 'tab-bar-switch-to-prev-tab)
(global-set-key (kbd "s-}") 'tab-bar-switch-to-next-tab)
(global-set-key (kbd "s-t") 'tab-bar-new-tab)
(global-set-key (kbd "s-w") 'tab-bar-close-tab)
(global-set-key (kbd "s-1") 'tab-bar-select-tab)
(global-set-key (kbd "s-2") 'tab-bar-select-tab)
(global-set-key (kbd "s-3") 'tab-bar-select-tab)
(global-set-key (kbd "s-4") 'tab-bar-select-tab)
(global-set-key (kbd "s-5") 'tab-bar-select-tab)
(global-set-key (kbd "s-6") 'tab-bar-select-tab)
(global-set-key (kbd "s-7") 'tab-bar-select-tab)
(global-set-key (kbd "s-8") 'tab-bar-select-tab)
(global-set-key (kbd "s-9") 'tab-bar-select-tab)
(global-set-key (kbd "s-0") 'tab-bar-select-tab)

(add-hook
 'tab-bar-tab-post-open-functions
 (lambda (&rest _) (call-interactively #'tab-bar-rename-tab)))

;(global-unset-key (kbd "s-q"))
;(global-set-key (kbd "s-q") 'kill-current-buffer)
(setq tab-line-separator "") ;; set it to empty

(use-package desktop
  :custom
  (desktop-base-file-name "last-session") ; File name to use when saving desktop
  (desktop-base-lock-name (concat desktop-base-file-name ".lock")) ; File name to use as a lock
  (desktop-restore-eager 50) ; Load 50 buffers immediately, and the remaining buffers lazily
  (desktop-file-checksum t) ; Avoid writing contents unchanged between auto-saves
  (desktop-save-buffer t) ; Save buffer status
  (desktop-save t) ; Always save, the hack below will take care of file names
  (desktop-path (list "~/.emacs.d/desktop/"))
  :commands (+desktop-read-session)
  :init
  (setq desktop-dirname "~/.emacs.d/desktop/"

  :config
  ;; HACK: When saving the session, we set the file name to the timestamp. Then
  ;; we copy it back to `desktop-base-file-name'. This ensures `desktop-read'
  ;; will read the last session when called while keeping the previous one.

  (advice-add 'desktop-save :around #'+desktop-save--timestamp-file:around-a)
  ))


(desktop-save-mode)
