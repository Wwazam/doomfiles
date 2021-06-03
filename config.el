;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


(setq byte-compile-warnings '(cl-functions))

;; / ;; Graphic
;; ;; Theme


(setq theme-list '(
                   ;; hc-zenburn
                   darktooth
                   ;; doom-material
                   ;; doom-nord
                   ;; doom-spacegrey
                   ;; doom-tomorrow-night
                   ;; badwolf
                   ;; ayu-dark
                   ))


(defun random-theme ()
  (interactive)
  (random t)  ; randomazing
  (load-theme (nth (random (length theme-list)) theme-list) t))

(random-theme)

;;
;; / ;; Evil
;; ;; jk to Normal Mode
(setq-default evil-escape-unordered-key-sequence "jk")
(setq-default evil-escape-delay 0.5)

;;
;; / ;; Org
;; ;; Directory
(setq org-directory "~/documents/notes/roam")
(setq org-agenda-files '("~/documents/notes/roam"))
(setq org-roam-directory "~/documents/notes/roam/")

(set-display-table-slot standard-display-table
                        'selective-display (string-to-vector " …")) ; or whatever you like
(after! org
  (setq org-log-into-drawer t))

(after! org
  (map! :map 'doom-leader-notes-map "i" #'org-id-store-link))

;; / ;; Roam
(defadvice! doom-modeline--reformat-roam (orig-fun)
  :around #'doom-modeline-buffer-file-name
  (message "Reformat?")
  (message (buffer-file-name))
  (if (s-contains-p org-roam-directory (or buffer-file-name ""))
      (replace-regexp-in-string
       "\\(?:^\\|.*/\\)\\([0-9]\\{4\\}\\)\\([0-9]\\{2\\}\\)\\([0-9]\\{2\\}\\)[0-9]*-"
       "(\\1-\\2-\\3) "
       (funcall orig-fun))
    (funcall orig-fun)))

(setq org-agenda-prefix-format
      '((agenda . " %i %-12:(+org-entry-category)%?-12t% s")
        (todo . " %i %-12:(+org-entry-category)")
        (tags . " %i %-12:(+org-entry-category)")
        (search . " %i %-12:(+org-entry-category)")))

(defun +org-entry-category ()
  "Get category of item at point.

Supports `org-roam' filenames by chopping prefix cookie."
  (+string-chop-prefix-regexp
   "^[0-9]+\\-"
   (or (org-entry-get nil "CATEGORY")
       (if buffer-file-name
           (file-name-sans-extension
            (file-name-nondirectory buffer-file-name))
         ""))))

;; requires s.el
(defun +string-chop-prefix-regexp (prefix s)
  "Remove PREFIX regexp if it is at the start of S."
  (s-chop-prefix (car (s-match prefix s)) s))

;; ;; Babel

(setq org-confirm-babel-evaluate nil)

;; tangle on save

;; (add-hook! 'org-mode-hook
;;   (add-hook! 'after-save-hook (org-babel-tangle))
;;   )

;;
;; ;;
(map!
 (:map override :i "C-k" #'evil-insert-digraph))

(setq doom-font (font-spec :family "hack" :size 14))


;;
;; / ;; SSH Agent
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;;
;; / ;; php
(setq! lsp-clients-php-server-command
       (expand-file-name "~/.config/composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))

;; / ;; yas
(defun +yas/org-src-header-p ()
  "Return non-nil if point is on a org src header, nil otherwise."
  (car
   (member
    (downcase
     (save-excursion
       (goto-char (line-beginning-position))
       (buffer-substring-no-properties
        (point)
        (or (ignore-errors
              (search-forward " " (line-end-position)))
            (1+ (point))))))
    '("#+property:" "#+begin_src" "#+header:"))))

;;
(setq-hook! 'python-mode-hook +format-with-lsp nil)
;; / ;; python

;;
;; / ;; js

(setq-hook! 'js-mode-hook +format-with-lsp nil)

(setq-hook! 'web-mode-hook rainbow-mode nil)

;;
;; / ;; ripgrep

(after! counsel
  (setq counsel-rg-base-command "rg -M 240 --with-filename --no-heading --line-number --color never %s || true"))

;;
;; / ;; mu4e
(setq +mu4e-mu4e-mail-path '~/documents/mail)
(set-email-account! "basile.pracca@gmail.com"
                    '((mu4e-sent-folder       . "/Sent")
                      (mu4e-drafts-folder     . "/Draft")
                      (mu4e-trash-folder      . "/Trash")
                      ;; (mu4e-refile-folder     . "/All")
                      (smtpmail-smtp-user     . "basile.pracca@gmail.com")
                      (mu4e-compose-signature . "---\nBasile PRACCA"))
                    t)
(setq user-mail-address "basile.pracca@gmail.com")

(setq mu4e-bookmarks
      '(( :name  "Unread messages"
          :query "flag:unread AND NOT flag:trashed AND maildir:/INBOX"
          :key ?u)
        ( :name "Today's messages"
          :query "date:today..now and maildir:/INBOX"
          :key ?t)
        ( :name "Last 7 days"
          :query "date:7d..now AND and maildir:/INBOX"
          :hide-unread t
          :key ?w)
        ( :name "Important"
          :query "maildir:/Starred"
          :key ?i)
        ( :name "Git"
          :query "from:noreply@github.com or from:noreplay@gitlab.com"
          :key ?g)
        )
      )
;;
;; / ;; elfeed
(map! :map 'doom-leader-open-map "e" #'elfeed)
(map! :map 'elfeed-search-mode-map :n "r" #'elfeed-search-fetch)
(setq rmh-elfeed-org-files '("~/documents/notes/elfeed.org"))

;; / ;; elfeed dashboard
(setq elfeed-dashboard-file "~/.doom.d/elfeed_dashboard.org")

;; / ;; window transpose
(map! :map 'evil-window-map "t" #'transpose-frame)
