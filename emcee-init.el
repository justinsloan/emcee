;; -------------------------------------------------------------------------
;;  _____                         
;; | ____|_ __ ___   ___ ___  ___ 
;; |  _| | '_ ` _ \ / __/ _ \/ _ \
;; | |___| | | | | | (_|  __/  __/
;; |_____|_| |_| |_|\___\___|\___|
;; 
;; EMCEE - Emacs Mneumonic Configuration for Efficient Editing
;; Vision: Mneumonic, menu-driven, AI assisted, productivity workflow
;;
;; To use Emcee, add the following to your init.el file:
;; (add-to-list 'load-path (locate-user-emacs-file "emcee/"))
;; (require 'emcee-init)
;; -------------------------------------------------------------------------
(add-to-list 'load-path ".")

;; Declutter auto-save
;; -------------------
(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "auto-save")))
      delete-old-versions t
      version-control t)

;; Load Emcee Configs
;; ------------------
(require 'emcee-keybindings)
(require 'emcee-theme)
(require 'emcee-packages)
(require 'emcee-functions)

;; Load Scratch Buffer Text
;; ------------------------
(setq initial-major-mode 'org-mode)
(let ((filename (concat user-emacs-directory "emcee/scratch-buffer.txt")))
  (when (file-exists-p filename)
    (let ((scratch-buffer (get-buffer "*scratch*")))
      (when scratch-buffer
        (with-current-buffer scratch-buffer
          (erase-buffer)
          (insert-file-contents filename)
	  (goto-char (point-max)))))))


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
