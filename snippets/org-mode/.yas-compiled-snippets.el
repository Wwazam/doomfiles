;;; Compiled snippets and support files for `org-mode'
;;; Snippet definitions:
;;;
(yas-define-snippets 'org-mode
                     '(("sprint" "#+TITLE: SPRINT: $1\n#+AUTHOR: Basile PRACCA\n#+EMAIL: bpracca@vivlio.com\n#+DATE:  `(format-time-string \"%Y-%b-%d %a\")`\n\n#+TODO: TODO(t) ISSU(i@) WAIT(w@) | SETUP(s) PROB(p) DONE(d) CNCL(c)\n\n* Stories\n* Notes for next stories" "sprint" t nil nil "/home/basile/.doom.d/snippets/org-mode/sprint" nil "_my_sprint")
                       ("new-story" "** TODO ${2: Story Tag} | ${1:Story Name}\n*** [/] Acceptance Criterias\n$3\n*** [/] Notes\n$0" "new-story" t nil nil "/home/basile/.doom.d/snippets/org-mode/new-story" nil "_me_new-story")))


;;; Do not edit! File generated at Mon Aug 10 14:38:28 2020
