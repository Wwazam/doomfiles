;;; ~/.doom.d/module/language/org/+capture.el -*- lexical-binding: t; -*-

(after! org
  (setq org-capture-templates '(
    setq org-capture-templates '(
        ("h" "Home" entry (file "~/documents/notes/inbox.org")
        "* TODO %?   :home:\n %i\n\n")
        ("w" "Work" entry (file "~/documents/notes/inbox.org")
        "* TODO %?   :work:\n %i\n\n")
        ("e" "Errand" entry (file "~/documents/notes/inbox.org")
        "* TODO %?   :errand:\n %i\n\n")
        ("c" "Computer" entry (file "~/documents/notes/inbox.org")
        "* TODO %?   :computer:\n %i\n\n")
        ("m" "Meeting" entry (file "~/documents/notes/inbox.org")
        "* TODO %?   :meeting:\n %i\n\n")
        ("d" "Diary" entry (file+datetree "~/documents/notes/diary.org")
        "* %?\n")
        ("j" "Journal" entry (file+datetree "~/documents/notes/journal.org")
        "* %?\n")
        )
)))
