

(provide 'emcee-functions)

;; Custom shutdown function
(defun emcee-server-shutdown () 
  "Save buffers, Quit, and Shutdown (kill) server" 
  (interactive) 
  (save-some-buffers) 
  (kill-emacs))

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
