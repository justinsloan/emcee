;;  _____                         
;; | ____|_ __ ___   ___ ___  ___ 
;; |  _| | '_ ` _ \ / __/ _ \/ _ \
;; | |___| | | | | | (_|  __/  __/
;; |_____|_| |_| |_|\___\___|\___|


;; Custom shutdown function
(defun emcee-server-shutdown () 
  "Save buffers, Quit, and Shutdown (kill) server" 
  (interactive) 
  (save-some-buffers) 
  (kill-emacs))

(defun emcee-save-check-parens ()
  "Run check-parens on save if not org-mode."
  (interactive)
  (unless (eq major-mode 'org-mode)
    (check-parens)))

(defun emcee-insert-time ()
  "Insert string for the current time."
  (interactive) ; permit invocation in minibuffer
  ;(insert (format-time-string "%D %-I:%M %p"))) ; formatted like '9:34 AM'
  (insert (format-time-string "%H:%M"))) ; formatted like '09:34'

(defun emcee-insert-date ()
  "Insert string for today's date nicely formatted in American style,
  e.g. Sunday, September 17, 2000."
  (interactive)
  (insert (format-time-string "%A, %B %e, %Y")))

(defun emcee-insert-ordinal-date ()
  "Insert string for today's ordinal date,
  e.g. 25.365 for year 2025 and day number 365."
  (interactive)
  (insert (format-time-string "%y.%j")))

(defun emcee-query-replace-from-start ()
  "Move the cursor to the start of the buffer and perform a query replace."
  (interactive)
  ;; Ensure we are in a buffer that supports `query-replace`
  (if (buffer-modified-p)
      (error "Buffer is modified, save or discard changes before replacing"))
  
  (let ((original-point (point)))       ; Store current point
    (goto-char (point-min))             ; Move the cursor to the top of the buffer
    (call-interactively 'query-replace) ; permit invocation in minibuffer
    (goto-char original-point)))        ; Return cursor to starting point

(defun emcee-execute-shell-command ()
  "Execute highlighted shell command or prompt for command."
  (if (not (mark))
    (shell-command) ; THEN
  ;; ELSE
  ( 
  (interactive)
  (shell-command-on-region
   ;; beginning and end of buffer
   (point)
   (mark)
   ;; command and parameters
   "/bin/bash"
   ;; output buffer
   (current-buffer)
   ;; replace contents?
   t
   ;; name of the error buffer
   "*Bash Error Buffer*"
   ;; show error buffer?
   t))))

(defun emcee-write-file-as ()
  "Write a copy of the current buffer or selected region to a new file."
  (interactive)
  (let* ((curr (buffer-file-name))
         (new (read-file-name
               "New file name: " nil nil nil
               (and curr (file-name-nondirectory curr))))
         (mustbenew (if (and curr (file-equal-p new curr)) 'excl t)))
    (if (use-region-p)
        (write-region (region-beginning) (region-end) new nil nil nil mustbenew)
      (save-restriction
        (widen)
        (write-region (point-min) (point-max) new nil nil nil mustbenew)))))


;;(defun emcee-reinstall-package (pkg)
;;  (interactive (list (intern (completing-read "Reinstall package: " (mapcar #'car package-alist)))))
;;  (unload-feature pkg)
;;  (package-reinstall pkg)
;;  (require pkg))



(provide 'emcee-functions)
