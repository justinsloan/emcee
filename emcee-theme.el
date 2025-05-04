;;  _____                         
;; | ____|_ __ ___   ___ ___  ___ 
;; |  _| | '_ ` _ \ / __/ _ \/ _ \
;; | |___| | | | | | (_|  __/  __/
;; |_____|_| |_| |_|\___\___|\___|


;; Set Theme, Font, & Modeline Faces
;; =================================


;; Disable Interface Elements
;; --------------------------
(setq inhibit-startup-screen t)
(menu-bar-mode              -1)
(toggle-scroll-bar          -1)
(tool-bar-mode              -1)
(scroll-bar-mode            -1)


;; Enable Interface Elements
;; -------------------------
(delete-selection-mode 1)
(column-number-mode    1)
(which-key-mode        1)
(ido-mode              1)

(defun new-frame-setup (frame)
  "Set options based on frame type."
  (unless (display-graphic-p frame)
    (xterm-mouse-mode 1)))
(mapc 'new-frame-setup (frame-list)) ; Run for already-existing frames
(add-hook 'after-make-frame-functions 'new-frame-setup) ; Run when a new frame is created


;; Theme
;; -----
;;(when (display-graphics-p)
  (use-package doom-themes
    :ensure t
    :config
    ;; Global settings (defaults)
    (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
          doom-themes-enable-italic t) ; if nil, italics is universally disabled
    (load-theme 'doom-tokyo-night t) ; specify theme name
    (doom-themes-visual-bell-config)) ; enable flashing mode-line on errors

    
;; Font config
;; -----------
(defun emcee-font-faces ()
  "Configure custom font-lock faces."
  (set-face-attribute 'default nil
    :font "LiberationMono"
    :height 160)
  (set-face-attribute 'variable-pitch nil
    :font "LiberationSans"
    :height 160)
  (set-face-attribute 'fixed-pitch nil
    :font "LiberationMono"
    :height 160)
  (setq-default line-spacing 0.12)
  (set-face-attribute 'font-lock-comment-face nil
    :slant 'italic)
  (set-face-attribute 'font-lock-keyword-face nil
    :slant 'italic))
(add-hook 'after-init-hook 'emcee-font-faces)
(add-hook 'server-after-make-frame-hook 'emcee-font-faces)


;; Mode line config
;; ----------------
(setq-default
 mode-line-format
 '(; read-only or modified status
   (:eval
    (cond (buffer-read-only
           (propertize " 🔏 " 'face 'mode-line-read-only-face))
          ((buffer-modified-p)
           (propertize " ** " 'face 'mode-line-modified-face))
          (t " 🖋️ ")))
   ;"    "
   ; mode indicators: major mode, version control, working file, position, font
   "%["
   (:propertize mode-name
                face mode-line-mode-face)
   " ["
   (vc-mode vc-mode)
   " ] "
   ;(:eval (propertize (format-mode-line minor-mode-alist)
   ;                   'face 'mode-line-minor-mode-face))
   (:propertize mode-line-process
                face mode-line-process-face)
   (global-mode-string global-mode-string)
   " %]"
   ; directory and buffer/file name
   (:propertize (:eval (shorten-directory default-directory 30))
                face mode-line-folder-face)
   (:propertize "%b "
                face mode-line-filename-face)
   ; Position, including warning for 80 columns
   ;(:eval (propertize "aaaaa" 'mode-line-format-right-align))
   (:propertize " (%p)  %l:" face mode-line-position-face)
   (:eval (propertize "%c   " 'face
                      (if (>= (current-column) 80)
                          'mode-line-80col-face
                        'mode-line-position-face)))
   ; Font face
   (:propertize (:eval (get-font-name (frame-parameter nil 'font)))
		face mode-line-minor-mode-face)
   ))

;; Modeline Helper functions
(defun shorten-directory (dir max-length)
  "Show up to `max-length' characters of a directory name `dir'."
  (let ((path (reverse (split-string (abbreviate-file-name dir) "/")))
        (output ""))
    (when (and path (equal "" (car path)))
      (setq path (cdr path)))
    (while (and path (< (length output) (- max-length 4)))
      (setq output (concat (car path) "/" output))
      (setq path (cdr path)))
    (when path
      (setq output (concat ".../" output)))
    output))

(defun get-font-name (font-spec)
  "Extract and return the base NAME from FONT-SPEC string."
  ;; Remove leading dashes (-*- or -*) that are often used to denote style attributes.
  (let* ((trimmed-spec (replace-regexp-in-string "-+$" "" font-spec))
         (parts (split-string trimmed-spec "-"))
         (base-name nil))
    ;; Handle X11/XF86 naming scheme
    (if (member "OT1" parts)
        (setq base-name (nth 1 parts))  ; OT1 encoding style, usually "fontname-encoding"
      (if (member "Type1" parts)
          (setq base-name (nth 1 parts))  ; Type1 encoding style
        ;; Handle modern X11 font naming conventions or other formats
        (setq base-name (nth 2 parts))))))

;; Extra mode line faces
(make-face 'mode-line-read-only-face)
(make-face 'mode-line-modified-face)
(make-face 'mode-line-folder-face)
(make-face 'mode-line-filename-face)
(make-face 'mode-line-position-face)
(make-face 'mode-line-mode-face)
(make-face 'mode-line-minor-mode-face)
(make-face 'mode-line-process-face)
(make-face 'mode-line-80col-face)

(set-face-attribute 'mode-line nil
    :foreground "gray80" :background "gray20"
    :inverse-video nil
    :box '(:line-width 10 :color "#ffffff" :style nil))
(set-face-attribute 'mode-line-inactive nil
    :foreground "#000111" :background "#000000"
    :inverse-video nil
    :box '(:line-width 1)); :color "#000111" :style nil))
(set-face-attribute 'mode-line-read-only-face nil
    :inherit 'mode-line-face
    :foreground "#4271ae")
    ;;:box '(:line-width 2 :color "#4271ae"))
(set-face-attribute 'mode-line-modified-face nil
    :inherit 'mode-line-face
    :foreground "#c82829"
    ;;:background "#ffffff"
    ;;:box '(:line-width 2 :color "#c82829")
    )
(set-face-attribute 'mode-line-folder-face nil
    :inherit 'mode-line-face
    :foreground "gray60")
(set-face-attribute 'mode-line-filename-face nil
    :inherit 'mode-line-face
    :foreground "#eab700" ; yellow
    :weight 'bold)
(set-face-attribute 'mode-line-position-face nil
    :inherit 'mode-line-face
    :family "Menlo" :height 100)
(set-face-attribute 'mode-line-mode-face nil
    :inherit 'mode-line-face
    :foreground "gray80")
(set-face-attribute 'mode-line-minor-mode-face nil
    :inherit 'mode-line-mode-face
    :foreground "gray40"
    :height 110)
(set-face-attribute 'mode-line-process-face nil
    :inherit 'mode-line-face
    :foreground "#718c00")
(set-face-attribute 'mode-line-80col-face nil
    :inherit 'mode-line-position-face
    :foreground "black" :background "#eab700")

;; Move mode line to the top
(setq-default header-line-format mode-line-format)
(setq-default mode-line-format nil)


(provide 'emcee-theme)
