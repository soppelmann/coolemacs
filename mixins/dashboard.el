;; Config

;; To save frameset to register run
;; C-x r f <register>

(use-package dashboard
  :ensure t)

(dashboard-setup-startup-hook)

(setq dashboard-footer-messages
      (with-temp-buffer
        (insert-file-contents "~/.emacs.d/theo")
        (split-string (buffer-string) "\n" t)))

(setq dashboard-item-shortcuts
      '((recents . "r")
        (bookmarks . "m")
        (projects . "p")
        (agenda . "a")
        (registers . "e")
        (commands . "c")))

(if (< (length command-line-args) 2)
  (setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*"))))
;(setq initial-buffer-choice (lambda () (get-buffer-create "*dashboard*")))
(setq inhibit-startup-screen t)

(setq dashboard-center-content t)
;(split-window-config)
(defun dashboard-insert-custom (list-size)
  (dashboard-insert-heading
   "Commands:"
   "c") ; Optional heading for the widget
  (insert "\n")
  (dashboard-insert-shortcut 'commands "c" "Commands:")

  (let ((button-text "    Layout\n"))
    (put-text-property 0 (length button-text) 'mouse-face 'highlight
                       button-text)
    (put-text-property 0 (length button-text) 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key map (kbd "RET") 'split-window-config)
                         map)
                       button-text)
    (insert button-text))
  (let ((button-text "    Refresh dashboard\n"))
    (put-text-property 0 (length button-text) 'mouse-face 'highlight
                       button-text)
    (put-text-property 0 (length button-text) 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key
                          map (kbd "RET") 'dashboard-refresh-buffer)
                         map)
                       button-text)
    (insert button-text))
  (let ((button-text "    Ranger"))
    (put-text-property 0 (length button-text) 'mouse-face 'highlight
                       button-text)
    (put-text-property 0 (length button-text) 'keymap
                       (let ((map (make-sparse-keymap)))
                         (define-key
                          map (kbd "RET") 'ranger)
                         map)
                       button-text)
    (insert button-text))
  )


(add-to-list 'dashboard-item-generators '(commands . dashboard-insert-custom))
(add-to-list 'dashboard-items '(commands) t)
(dashboard-insert-shortcut (dashboard-get-shortcut 'commands) "c" "Commands:")

(setq dashboard-items
      '((commands . 5)
        (recents . 5)
        (bookmarks . 5)
        (registers . 5)))

(setq dashboard-set-navigator t)

(setq tab-bar-new-tab-choice "*dashboard*") ;; buffer to show in new tabs
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

(global-unset-key (kbd "s-q"))
(global-set-key (kbd "s-q") 'kill-current-buffer)
(setq tab-line-separator "") ;; set it to empty
