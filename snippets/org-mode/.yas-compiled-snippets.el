;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("sprint" "#+TITLE: SPRINT: $1\n#+AUTHOR: Basile PRACCA\n#+EMAIL: bpracca@vivlio.com\n#+DATE:  `(format-time-string \"%Y-%b-%d %a\")`\n\n* Stories\n* Notes for next stories" "sprint" t nil nil "/home/basile/.doom.d/snippets/org-mode/sprint" nil "_my_sprint")
                       ("new-story" "** ${1:Story Name} | ${2:Story Label}\n*** [/] Acceptance Criterias\n$3\n\n*** [/] Checklist\n$4\n\n*** Notes\n$0" "new-story" t nil nil "/home/basile/.doom.d/snippets/org-mode/new-story" nil "_me_new-story")))


;;; Do not edit! File generated at Wed Jul 22 10:10:35 2020
