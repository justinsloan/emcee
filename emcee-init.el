;; -------------------------------------------------------------------------
;; EMCEE - Emacs Menu Configuration for Elightened Editing
;; Vision: Mneumonic menu-driven AI assisted editor
;;
;; To use Emcee, add the following to your init.el file:
;; (add-to-list 'load-path (locate-user-emacs-file "emcee/"))
;; (require 'emcee-init)
;; -------------------------------------------------------------------------
(add-to-list 'load-path ".")
(setq initial-major-mode 'org-mode)

;; Load Emcee Configs
;; ------------------
(require 'emcee-keybindings)
(require 'emcee-theme)
(require 'emcee-packages)
(require 'emcee-functions)


;; ORG CAPTURE MENU CONFIG
;; -----------------------
(use-package org
  :hook (text-mode . visual-line-mode)
  :config
  ;;(visual-line-mode t)
  (display-line-numbers-mode nil)
  (setq org-export-publishing-directory "/tmp")
  (setq org-agenda-files '("~/org"))
  (setq org-capture-templates
    '(("t" "💡 Task" entry
      (file+datetree "~/org/journal.org")
       "* TODO %U %^{Task}"
       :immediate-finish t)
      ("e" "🗓️ Event" entry
       (file+datetree "~/org/journal.org")
       "* 🗓️ %U  %^{Event} :event:%^{Type meeting|project}:\n%?")
      ("n" "✏️ Note" entry
       (file+datetree "~/org/journal.org")
       "* ✏️ %U %^{Note} :note:"
       :immediate-finish t)
      ("x" "💳 Expense" entry
       (file+datetree "~/org/journal.org")
       "* 💳 %U  %^{Company AFF|AFI|ATW|DCL|DTG|RAM|RHM}  $%^{Amount}  %^{Description} :expense:%^{GL}:"
       :immediate-finish t)
     )
  )
)


(provide 'emcee-init)
