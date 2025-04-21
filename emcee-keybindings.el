

(provide 'emcee-keybindings)


;; Set Menu Keybindings
;; --------------------
;; Set <menu> as global leader key
(defalias 'menu-keymap (make-sparse-keymap))
(defvar menu-key (symbol-function 'menu-keymap) "Menu")
(define-key global-map (kbd "\<menu>") 'menu-keymap)
(define-key global-map (kbd "M-m") 'menu-keymap)

;; Top-level Menu
(which-key-add-key-based-replacements
  "\<menu>f" "File"
  "\<menu>e" "Edit"
  "\<menu>p" "Preferences"
  "\<menu>b" "Buffer"
  "\<menu>w" "Window"
  "\<menu>t" "Tools"
  "\<menu>o" "Org")
(which-key-add-key-based-replacements
  "M-m f" "File"
  "M-m e" "Edit"
  "M-m p" "Preferences"
  "M-m b" "Buffer"
  "M-m w" "Window"
  "M-m t" "Tools"
  "M-m o" "Org")
(define-key menu-key "x" '("Execute"     . execute-extended-command))
(define-key menu-key "\\" '("Other Window" . other-window))
(define-key menu-key "|" '("Split Vertically" . split-window-right))
(define-key menu-key "_" '("Split Horizontally" . split-window-below))
(define-key menu-key (kbd "SPC") '("Buffer List" . ivy-switch-buffer))
(define-key menu-key "h" '("Help"        . help-map))
(define-key menu-key "q" '("Abort"       . keyboard-quit))

;; File Menu
(define-key menu-key "fn" '("New"        . make-empty-file))
(define-key menu-key "fo" '("Open"       . counsel-find-file))
(define-key menu-key "fs" '("Save"       . save-buffer))
;; Save as...
(define-key menu-key "fc" '("Close"      . kill-buffer-and-window))
;; Print
(define-key menu-key "fr" '("Restart"    . restart-emacs))   
(define-key menu-key "fx" '("Exit/Quit"  . emcee-server-shutdown))
(define-key menu-key "fq" '("Abort"      . keyboard-quit))

;; Edit Menu
(define-key menu-key "ez"  '("Undo"       . undo))
(define-key menu-key "er"  '("Redo"       . undo-redo))
(define-key menu-key "ep"  '("Paste"      . yank))
(define-key menu-key "ex"  '("Cut"        . kill-region))
(define-key menu-key "ec"  '("Copy"       . kill-ring-save))
(define-key menu-key "es"  '("Select all" . mark-whole-buffer))
(define-key menu-key "ef"  '("Find"       . swiper-isearch))
(define-key menu-key "er"  '("Replace"    . emcee-query-replace-from-start))
(define-key menu-key "eid" '("Date"       . emcee-insert-date))
(define-key menu-key "eit" '("Time"       . emcee-insert-time))
;; AI check grammar
;; AI review
;; AI summarize
(define-key menu-key "eq" '("Abort"       . keyboard-quit))

;; Buffer Menu
(define-key menu-key "bn" '("Next Buffer"     . next-buffer))
(define-key menu-key "bp" '("Previous Buffer" . previous-buffer))
(define-key menu-key "bk" '("Kill Buffer"     . kill-buffer))
(define-key menu-key "bl" '("List Buffers"    . ivy-switch-buffer))
(define-key menu-key "bq" '("Abort"           . keyboard-quit))

;; Window Menu
(define-key menu-key "wn" '("Next Window"    . other-window))
;;(define-key menu-key "wp" '("Previous"     . kill-buffer))
(define-key menu-key "wc" '("Close Window"   . delete-window))
(define-key menu-key "wq" '("Abort"          . keyboard-quit))

;; Tools Menu
(define-key menu-key "tt" '("Terminal"       . term))
(define-key menu-key "tb" '("Browser"        . eww))
(define-key menu-key "tc" '("Calculator"     . calc))
(define-key menu-key "tg" '("Git"            . magit-status))
(define-key menu-key "td" '("Dig (DNS)"      . dig))
(define-key menu-key "ta" '("AI Assistant"   . ellama))
(define-key menu-key "t." '("Shell Command"  . sh-execute-region))
(define-key menu-key "te" '("Eval Lisp"      . eval-last-sexp))
(define-key menu-key "tq" '("Abort"          . keyboard-quit))

;; Preferences Menu
(define-key menu-key "pw" '("Word Wrap"      . visual-line-mode))
(define-key menu-key "pl" '("Line Numbers"   . line-number-mode))
(define-key menu-key "pb" '("Blink Cursor"   . blink-cursor-mode))
(define-key menu-key "pt" '("Tab Line"       . tab-line-mode))
(define-key menu-key "pq" '("Abort"          . keyboard-quit))

;; Org Menu
(define-key menu-key "oa"  '("Agenda"        . org-agenda))
(define-key menu-key "oc"  '("Capture"       . org-capture))
(define-key menu-key "ott" '("Create Table"  . org-table-create-or-convert-from-region))
(define-key menu-key "otc" '("Toggle Coord." . org-table-toggle-coordinate-overlays))
(define-key menu-key "oe"  '("Export"        . org-export-dispatch))
(define-key menu-key "oq"  '("Abort"         . keyboard-quit))
