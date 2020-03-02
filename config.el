;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


;;
;; Graphic
;; ;; Theme


(setq theme-list '(
                   hc-zenburn
                   doom-material
                   doom-gruvbox
                   doom-nord
                   doom-spacegrey
                   spacemacs-dark
                   sanityinc-tomorrow-night
                   dorsey
                   junio
                   odersky
                   darktooth
                   jazz
                   subatomic
                   badwolf
                   vscdark-theme
                   ))


(defun random-theme ()
  (interactive)
  (random t)  ; randomazing
  (load-theme (nth (random (length theme-list)) theme-list) t))

(random-theme)
;; ;; Auto fill mode

(setq auto-fill-mode -1)

;;
;; Evil
;; ;; jk to Normal Mode
(setq-default evil-escape-unordered-key-sequence "jk")
(setq-default evil-escape-delay 0.5)

;;
;; ;; Holy mode by default
(add-hook 'org-agenda-mode-hook 'evil-emacs-state)
(add-to-list 'evil-emacs-state-modes 'org-agenda-mode)

;;
;; Navigation
;; ;; avy
(define-key evil-insert-state-map (kbd "C-.") 'avy-goto-word-or-subword-1)
(define-key evil-normal-state-map (kbd "C-.") 'avy-goto-word-or-subword-1)
(define-key evil-visual-state-map (kbd "C-.") 'avy-goto-word-or-subword-1)
(define-key evil-motion-state-map (kbd "C-.") 'avy-goto-word-or-subword-1)
(setq avy-all-windows t)


;; ;; maximize windows

(define-key evil-window-map (kbd "C-o") 'doom/window-maximize-buffer)

;;
;; Org
;; ;; Directory
(setq org-directory "~/documents/notes/orgFiles/")

;; ;; Bullets
(setq org-bullets-fac-name (quote org-bullet-face))
(add-hook 'org-mode-hook (lambda ()(org-bullets-mode 1)))
(setq org-bullets-bullet-list '("·"))

                                        ;(setq org-ellipsis " ▼")
(set-display-table-slot standard-display-table
                        'selective-display (string-to-vector " …")) ; or whatever you like

;; remove auto fill mode

(remove-hook 'text-mode-hook #'auto-fill-mode)
(remove-hook 'org-mode-hook #'auto-fill-mode)
(add-hook 'text-mode-hook #'visual-line-mode)
(add-hook 'org-mode-hook #'visual-line-mode)

;; ;; TO DO keywords
(with-eval-after-load 'org
  (setq org-todo-keywords
        '((sequence "TODO(t)" "WAIT(w@)" "|" "DONE(d!)" )
          (sequence "ISSUE(i@)" "|" "LATER(l)" "CANCELED(a@)")
          ))

  (setq org-todo-keyword-faces
        '(
          ("TODO" .(:foreground "#bc8383" :weight bold))
          ("DONE" .(:foreground "#94bff3"))
          ("WAIT" .(:foreground "#ebe9bf"))
          ("ISSUE" .(:foreground "#dfaf8f"))
          ("CANCELED" .(:foreground "#7f9f7f"))
          ))
  (setq auto-fill-mode -1)
  (setq visual-line-mode 1)
  (setq org-log-into-drawer t))

;; ;; Agenda
(global-set-key (kbd "C-c a") 'org-agenda)
(defun my/org-agenda-skip-without-match (match)
  "Skip current headline unless it matches MATCH.
Return nil if headline containing point matches MATCH (which
should be a match string of the same format used by
`org-tags-view').  If headline does not match, return the
position of the next headline in current buffer.
Intended for use with `org-agenda-skip-function', where this will
skip exactly those headlines that do not match."
  (save-excursion
    (unless (org-at-heading-p) (org-back-to-heading))
    (let ((next-headline (save-excursion
                           (or (outline-next-heading) (point-max)))))
      (if (my/org-match-at-point-p match) nil next-headline))))
(setq org-agenda-custom-commands
      '(("G" . "GTD contexts")
        ("Gw" "Work" tags-todo "work")
        ("Gc" "Computer" tags-todo "computer")
        ("Gm" "Meeting" tags-todo "meeting")
        ("Gh" "Home" tags-todo "home")
        ("Ge" "Errands" tags-todo "errands")
        ("g" "GTD Block Agenda"
         (
          (tags-todo "work")
          (tags-todo "home")
          (tags-todo "computer")
          (tags-todo "errands")
          )
         nil                      ;; i.e., no local settings
         ("~/next-actions.html")) ;; exports block to this file with C-c a e
        ("x" "Missing scheduled date" tags-todo "+DEADLINE=\"\"+SCHEDULED=\"\"/!")

        ("d" . "Day")
        ("dd" "Day" agenda "All events, not filtered"
         ((org-agenda-span 1)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "+0d")
          ))
        ("dw" "Work" agenda "Events tagged 'work' and 'meeting'"
         ((org-agenda-span 1)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "+0d")
          (org-agenda-tag-filter-preset '("+work"))
          ))
        ("dp" "Personnal" agenda "Events tagged 'work', 'errands', 'meeting' and 'computer'"
         ((org-agenda-span 1)
          (org-agenda-start-on-weekday nil)
          (org-agenda-start-day "+0d")
          (org-agenda-tag-filter-preset '("-work"))
          ))
        )
      )
(setq org-agenda-start-day "+0d")

;; ;; Capture templates
(global-set-key (kbd "C-c c") 'org-capture)
(after! org
  (setq org-capture-templates
        '(
          ("h" "Home" entry (file "~/documents/notes/inbox.org")
           "* TODO %?   :home:\n %i\n\n")
          ("w" "Work" entry (file "~/documents/notes/inbox.org")
           "* TODO %?   :work:\n %i\n\n")
          ("e" "Errand" entry (file "~/documents/notes/inbox.org")
           "* TODO %?   :errand:\n %i\n\n")
          ("c" "Computer" entry (file "~/documents/notes/inbox.org")
           "* TODO %?   :computer:\n %i\n\n")
          ("m" "Meeting" entry (file "~/documents/notes/inbox.org")
           "* %?   :meeting:\n %i\n\n")
          ("d" "Diary" entry (file+olp+datetree "~/documents/notes/diary.org")
           "* %?\n")
          ("j" "Journal" entry (file+olp+datetree "~/documents/notes/journal.org")
           "* %?\n")
          )
        ))
(setq org-defaults-notes-file "~/documents/notes/orgFiles/inbox.org")

;; ;; Babel

(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '( (python  . t)
    (plantuml . t)
    (emacs-lisp . t)
    (C . t)
    ))

;;
;; ;; web
(add-to-list 'auto-mode-alist '("\\.djhtml$" . web-mode))
(add-to-list 'auto-mode-alist '("\\.twig$" . web-mode))

;;
;; ;; Expand key

(eval-after-load "evil-maps"
  (dolist (map '(evil-insert-state-map))
    (define-key (eval map) "\C-n" nil)))
(define-key evil-insert-state-map (kbd "C-n") 'hippie-expand)
(setq-default
 hippie-expand-try-functions-list '(
                                    yas-hippie-try-expand
                                    try-complete-file-name-partially
                                    try-complete-file-name
                                    try-expand-all-abbrevs
                                    try-expand-list
                                    ;; try-expand-line
                                    try-expand-dabbrev
                                    try-expand-dabbrev-all-buffers
                                    try-expand-dabbrev-from-kill
                                    try-complete-lisp-symbol-partially
                                    try-complete-lisp-symbol
                                    ))

;;
;; ;;
(map!
 (:map override :i "C-k" #'evil-insert-digraph))

(setq doom-font (font-spec :family "hack" :size 14))

;;
;; Elfeed
;; ;;

;; (map! 'evil-normal-state-map
;;       :localleader
;;       :prefix "f"
;;         :desc "Elfeed" "f" #'elfeed

;; )

;; (!map
;;  :leader
;;    :prefix "Z"
;;      :desc "Kill buffer" "d" #'kill-this-buffer
;;    )

;;
;; SSH Agent
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;;
;; Deft

(setq deft-directory "~/documents/notes/orgFiles/")
(add-hook! 'deft-mode-hook 'evil-normal-state)

;;
;; php
(setq! lsp-clients-php-server-command
       (expand-file-name "~/.config/composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))
