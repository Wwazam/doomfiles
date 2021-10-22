;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

(package! async)

(package! exec-path-from-shell)

(package! hc-zenburn-theme)
(package! darktooth-theme)
(package! badwolf-theme)

(package! transpose-frame)
(package! lsp-pyright)

;; ajc formation
(package! jenkinsfile-mode)
(package! puppet-mode)

;; Org-roam-ui tries to keep up with the latest features of org-roam, which conflicts with Doom Emacs's desire for stability.
;; To make sure nothing breaks, use the latest version of org-roam by unpinning it.
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
