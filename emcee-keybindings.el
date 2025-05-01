;;  _____                         
;; | ____|_ __ ___   ___ ___  ___ 
;; |  _| | '_ ` _ \ / __/ _ \/ _ \
;; | |___| | | | | | (_|  __/  __/
;; |_____|_| |_| |_|\___\___|\___|

(require 'transient)


;; Set Menu Keybindings
;; --------------------
;; Set <menu> as global leader key
(define-key global-map (kbd "\<menu>") 'emcee-menu-main)
(global-set-key (kbd "M-m") (lambda () (interactive) (emcee-menu-main)))

(transient-define-prefix emcee-menu-insert ()
  "Transient insert menu for Emcee."
  ["Insert..." ; Group 1
   ("d" "Date"       emcee-insert-date)
   ("t" "Time"       emcee-insert-time)]
  [:class transient-row
	  ("C-g" "< Go Back"   keyboard-quit)
	  ("q" "Dismiss"       ignore :transient transient--do-exit)])

(transient-define-prefix emcee-menu-tools ()
  "Transient insert menu for Emcee."
  ["Tools" ; Group 1
    ("t" "Terminal"          vterm)
    ("b" "Browser"           eww)
    ("c" "Calculator"        calc)
    ("g" "Git"               magit-status)
    ("d" "Dig (DNS)"         dig)
    ("a" "AI Assistant"      ellama)
    ("!" "Shell Command"     sh-execute-region)
    (":" "Eval Lisp"         eval-last-sexp)]
  [:class transient-row
	  ("C-g" "< Go Back"   keyboard-quit)
	  ("q" "Dismiss"       ignore :transient transient--do-exit)])

(transient-define-prefix emcee-menu-preferences ()
  "Transient preferences menu for Emcee."
  ["Toggle Preferences" ; Group 1
   ("w" "Word Wrap"      visual-line-mode)
   ("l" "Line Numbers"   line-number-mode)
   ("b" "Blink Cursor"   blink-cursor-mode)
   ("t" "Tab Line"       tab-line-mode)]
  [:class transient-row
	  ("C-g" "< Go Back"   keyboard-quit)
	  ("q" "Dismiss"       ignore :transient transient--do-exit)])

(transient-define-prefix emcee-menu-main ()
  "Transient main menu for Emcee."
  [ ; Group 1
   ["File"
    ("fo" "Open/Create New..." counsel-find-file)
    ("fd" "Open with Dired..." dired-jump)
    ("fr" "Open Recent..."     recentf-open-files)
    ("fs" "Save"               save-buffer)
    ("fa" "Save as..."         emcee-write-file-as)
    ("fc" "Close"              kill-current-buffer)
    ;; Print
    ;;("fl" "List Buffers..."    ivy-switch-buffer)
    ("fn" "Next Buffer"        next-buffer)
    ("fp" "Previous Buffer"    previous-buffer)]

   ["Edit"
    ("eu" "Undo"               undo)
    ("ey" "Redo"               undo-redo)
    ("ek" "Cut"                kill-region)
    ("ec" "Copy"               kill-ring-save)
    ("ep" "Paste"              yank)
    ("ef" "Find..."            swiper-isearch)
    ("er" "Replace..."         emcee-query-replace-from-start)
    ("em" "Multiple Cursors"   set-rectangular-region-anchor)
    ;; Select all
    ("i" "Insert »"            emcee-menu-insert)]

   ["View"
     ("\\" "Switch Window"     other-window)
     ("|" "Split Vertically"   split-window-right)
     ("_" "Split Horizontally" split-window-below)
     ("b" "List Buffers..."    ivy-switch-buffer)
     ("c" "Close Window Pane"  delete-window)]

   ["Actions" ; Group 2
    ("aa" "Agenda"             org-agenda)
    ("ac" "Capture"            org-capture)
    ;;("tt" "Create Table"       org-table-create-or-convert-from-region)
    ;;("tc" "Toggle Coord."      org-table-toggle-coordinate-overlays)
    ("ae" "Export"             org-export-dispatch)]]

  [("t" "Tools" emcee-menu-tools)]
  
  [:class transient-row ; Footer Group
    ("p" "Preferences »"       emcee-menu-preferences)
    ("x" "Execute Command..."  execute-extended-command)
    ("q" "Dismiss"             ignore :transient transient--do-exit)

    ("!" "Exit Emacs"          save-buffers-kill-emacs)]
)


(provide 'emcee-keybindings)
