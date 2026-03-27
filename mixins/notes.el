;; ─── Notes configuration ─────────────────────────────────────────────────────
(defvar my/notes-directory (expand-file-name "/Users/getz/Nextcloud/Notebooks/notes")
  "Local directory where Denote notes are stored.")

(defvar my/notes-remote-host "df"
  "SSH host for publishing notes.")

(defvar my/notes-remote-path "~/public_html/text/"
  "Remote path to publish notes to.")

;; ─── Denote ──────────────────────────────────────────────────────────────────
(use-package denote
  :ensure t
  :hook (dired-mode . denote-dired-mode)
  :bind (("C-c n n" . denote)
         ("C-c n s" . denote-subdirectory)
         ("C-c n r" . denote-rename-file)
         ("C-c n R" . denote-rename-file-using-front-matter)
         ("C-c n l" . denote-link)
         ("C-c n c" . denote-link-after-creating)
         ("C-c n b" . denote-backlinks)
         ("C-c n d" . denote-dired)
         ("C-c n p" . my/publish-note-to-server))
  :config
  (setq denote-directory my/notes-directory)
  (setq denote-file-type 'markdown-yaml)
  (setq denote-prompts '(title keywords subdirectory))
  (denote-rename-buffer-mode 1))

;; ─── Consult-Denote ──────────────────────────────────────────────────────────
(use-package consult-denote
  :ensure t
  :after (consult denote)
  :bind (("C-c n f" . consult-denote-find)   ; find by filename with preview
         ("C-c n g" . consult-denote-grep))   ; grep note contents with preview
  :config
  (consult-denote-mode 1))

;; ─── Denote-menu ─────────────────────────────────────────────────────────────
(use-package denote-menu
  :ensure t
  :bind (("C-c n m" . denote-menu-list-notes))
  :config
  (denote-menu-bar-mode 1))


;; ─── Citar ───────────────────────────────────────────────────────────────────
;; (use-package citar
;;   :ensure t
;;   :custom
;;   (citar-bibliography (list (concat my/notes-directory "references.bib")))
;;   :hook
;;   (markdown-mode . citar-capf-setup))

;; ─── Citar-Denote ────────────────────────────────────────────────────────────
;; (use-package citar-denote
;;   :ensure t
;;   :after (citar denote)
;;   :custom
;;   (citar-open-always-create-notes t)
;;   (citar-denote-file-type 'markdown-yaml)
;;   (citar-denote-keyword "bib")
;;   :init
;;   (citar-denote-mode)
;;   :bind (("C-c n B c" . citar-create-note)
;;          ("C-c n B o" . citar-denote-open-note)
;;          ("C-c n B a" . citar-denote-add-reference)
;;          ("C-c n B k" . citar-denote-remove-reference)
;;          ("C-c n B x" . citar-denote-nocite)))

;; ─── Deft ────────────────────────────────────────────────────────────────────
(use-package deft
  :ensure t
  :bind ("C-c n F" . deft)
  :config
  (setq deft-directory my/notes-directory
        deft-extensions '("md")
        deft-recursive t
        deft-use-filename-as-title nil))

(defun my/publish-note-to-server ()
  "Browse notes with Consult, then publish selected note and associated files."
  (interactive)
  (let* ((note-file (buffer-file-name (consult-denote-find)))
         (note-id (denote-retrieve-filename-identifier note-file))
         (all-files (directory-files
                     (file-name-directory note-file) t note-id))
         (remote-dest (concat "/sshx:" my/notes-remote-host ":"
                               my/notes-remote-path note-id "/"))
         (index-content "<p>\n#!\nlowdown --html-no-skiphtml --html-no-escapehtml $1/*.md\n#!\n</p>\n")
         (tmp-index (make-temp-file "index" nil ".upphtml" index-content)))
    (make-directory remote-dest t)
    (dolist (f all-files)
      (copy-file f (concat remote-dest (file-name-nondirectory f)) t))
    (copy-file tmp-index (concat remote-dest "index.upphtml") t)
    (delete-file tmp-index)
    (message "Published %d file(s) for %s"
             (length all-files) note-id)))

;; ─── Publish ─────────────────────────────────────────────────────────────────
;; (defun my/publish-note-to-server ()
;;   "Copy current note and all files sharing its Denote ID to remote public_html."
;;   (interactive)
;;   (let* ((note-file (buffer-file-name))
;;          (note-id (denote-retrieve-filename-identifier note-file))
;;          (all-files (directory-files
;;                      (file-name-directory note-file) t note-id))
;;          (remote-dest (concat "/sshx:" my/notes-remote-host ":"
;;                                my/notes-remote-path note-id "/")))
;;     (make-directory remote-dest t)
;;     (dolist (f all-files)
;;       (copy-file f (concat remote-dest (file-name-nondirectory f)) t))
;;     (message "Published %d file(s) for %s" (length all-files) note-id)))


(defun my/denote-attach-file-to-note ()
  "Select a file, copy and rename it to match the current Denote note,
then insert a Markdown image or link at point with prompted alt text."
  (interactive)
  (let* ((note-file (buffer-file-name))
         (_ (unless (and note-file (denote-file-has-identifier-p note-file))
              (user-error "Current buffer is not a Denote note")))
         (note-id       (denote-retrieve-filename-identifier note-file))
         (note-title    (denote-retrieve-filename-title note-file))
         (note-keywords (denote-retrieve-filename-keywords note-file))
         (note-keywords-list (when note-keywords
                               (split-string note-keywords "_")))
         (target   (expand-file-name (read-file-name "Attach file: ")))
         (bare     (file-name-base target))
         (ext      (file-name-extension target t))
         (imagep   (member (downcase (file-name-extension target))
                           '("jpg" "jpeg" "png" "gif" "webp" "svg" "bmp")))
         (dest-dir (file-name-directory note-file))
         ;; Build the new filename ourselves
         (new-title (if (string-empty-p note-title)
                        bare
                      (concat note-title "--" bare)))
         (new-name  (denote-format-file-name
                     dest-dir note-id note-keywords-list
                     (denote-sluggify-title new-title) ext ""))
         (new-base  (file-name-nondirectory new-name)))
    ;; Copy source file into note directory under the new name
    (copy-file target new-name t)
    ;; Prompt for alt text and insert markdown
    (let ((alt-text (read-string "Alt text: " bare)))
      (if imagep
          (insert (format "![%s](./%s)" alt-text new-base))
        (insert (format "[%s](./%s)" alt-text new-base))))))
