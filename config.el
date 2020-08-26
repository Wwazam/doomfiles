;;; ~/.doom.d/config.el -*- lexical-binding: t; -*-


(setq byte-compile-warnings '(cl-functions))

;; Graphic
;; ;; Theme


(setq theme-list '(
                   hc-zenburn
                   doom-material
                   doom-nord
                   doom-spacegrey
                   sanityinc-tomorrow-night
                   darktooth
                   badwolf
                   ayu-dark
                   ))


(defun random-theme ()
  (interactive)
  (random t)  ; randomazing
  (load-theme (nth (random (length theme-list)) theme-list) t))

(random-theme)

;;
;; Evil
;; ;; jk to Normal Mode
(setq-default evil-escape-unordered-key-sequence "jk")
(setq-default evil-escape-delay 0.5)

;;
;; Org
;; ;; Directory
(setq org-directory "~/documents/notes/orgFiles/")
(setq org-roam-directory "~/documents/notes/roam/")

                                        ;(setq org-ellipsis " ▼")
(set-display-table-slot standard-display-table
                        'selective-display (string-to-vector " …")) ; or whatever you like
(after! org
  (setq org-log-into-drawer t))

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
;; SSH Agent
(exec-path-from-shell-copy-env "SSH_AGENT_PID")
(exec-path-from-shell-copy-env "SSH_AUTH_SOCK")

;;
;; php
(setq! lsp-clients-php-server-command
       (expand-file-name "~/.config/composer/vendor/felixfbecker/language-server/bin/php-language-server.php"))

;; yas
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
