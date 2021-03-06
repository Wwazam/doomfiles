;; -*- no-byte-compile: t; -*-
;;; ~/.doom.d/packages.el

;;; Examples:
;; (package! some-package)
;; (package! another-package :recipe (:fetcher github :repo "username/repo"))
;; (package! builtin-package :disable t)

(package! async)

(package! exec-path-from-shell)

(package! hc-zenburn-theme)
(package! darktooth-theme)
(package! badwolf-theme)

(package! elfeed-dashboard
  :recipe (:host github :repo "Manoj321/elfeed-dashboard"
           :files ("elfeed-dashboard.el")))

(package! vue-mode)

(package! transpose-frame)
(package! lsp-pyright)
