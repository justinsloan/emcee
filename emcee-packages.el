


;; Package Manager
;; ---------------

;; Add sources
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install packages
(use-package all-the-icons      :ensure t)
(use-package dashboard          :ensure t)
(use-package nerd-icons         :ensure t)
(use-package which-key-posframe :ensure t)
(use-package vertico            :ensure t)
(use-package vertico-posframe   :ensure t)
(use-package magit              :ensure t)

(use-package dired
  :ensure nil
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package nov
  :ensure t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode)))

(use-package spacious-padding
  :ensure t
  :config
  (spacious-padding-mode))

(use-package golden-ratio
  :ensure t
  :config
  (golden-ratio-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B" . ivy-switch-buffer-other-window))
  :custom
  (ivy-count-format "(%d/%d) ")
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode))

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode))

(use-package nerd-icons-ivy-rich
  :ensure t
  :after ivy-rich)

(use-package counsel
  :ensure t
  :after ivy
  :config
  (counsel-mode))

(use-package ivy-posframe
  :ensure t
  :after ivy
  :config
  ;; Different command can use different display function.
  (setq ivy-posframe-display-functions-alist
        '((swiper          . ivy-posframe-display-at-point)
          (complete-symbol . ivy-posframe-display-at-point)
          (counsel-M-x     . ivy-posframe-display-at-window-bottom-left)
          (t               . ivy-posframe-display)))
  (vertico-posframe-mode 1))

(use-package popper
  :ensure t ; or :straight t
  :bind (("C-`"   . popper-toggle)
         ("M-`"   . popper-cycle)
         ("C-M-`" . popper-toggle-type))
  :init
  (setq popper-reference-buffers
        '("\\*Messages\\*"
          "Output\\*$"
          "\\*Async Shell Command\\*"
	  "^\\*term.*\\*$"   term-mode   ;term as a popup
          help-mode
          compilation-mode))
  (popper-mode +1)
  (popper-echo-mode +1)) ; For echo area hints

(use-package org-bullets
  :ensure t
  :init
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (add-hook 'org-mode-hook (lambda () (line-number-mode -1)))
  (add-hook 'org-mode-hook (lambda () (org-indent-mode))))

(use-package lisp
  :hook
  (after-save . check-parens))

(use-package elisp-mode
  :hook
  (emacs-lisp-mode . display-line-numbers-mode)
  :bind
  (:map emacs-lisp-mode-map
        ("C-c C-d C-f" . describe-function)))

(use-package highlight-defined
  :ensure t
  :custom
  (highlight-defined-face-use-itself t)
  :hook
  (help-mode . highlight-defined-mode)
  (emacs-lisp-mode . highlight-defined-mode))

(use-package highlight-quoted
  :ensure t
  :hook
  (emacs-lisp-mode . highlight-quoted-mode))

(use-package eros
  :ensure t
  :hook
  (emacs-lisp-mode . eros-mode))

(use-package suggest
  :ensure t
  :defer t)

(use-package ipretty
  :defer t
  :ensure t
  :config
  (ipretty-mode 1))

(use-package nameless
  :ensure t
  :hook
  (emacs-lisp-mode .  nameless-mode)
  :custom
  (nameless-global-aliases '())
  (nameless-private-prefix t))

;;(use-package erefactor
;;  :ensure t
;;  :defer t)

;;(use-package flycheck-package
;;  :ensure t
;;  :hook
;;  (emacs-lisp-mode . flycheck-package-setup))

;;(use-package elsa
;;  :defer t
;;  :ensure t)

;;(use-package flycheck-elsa
;;  :ensure t
;;  :hook
;;  (emacs-lisp-mode . flycheck-elsa-setup))

;; Ellama Config
;; -------------
(use-package ellama
  :ensure t
  ;; send last message in chat buffer with C-c C-c
  :hook (org-ctrl-c-ctrl-c-final . ellama-chat-send-last-message)
  :init
  (setopt ellama-keymap-prefix "C-x a i")
  (setopt ellama-language "English")
  (setopt ellama-assistant-nick "Emcee")
  (setopt ellama-auto-scroll t)
  (require 'llm-ollama)
  (setopt ellama-provider
    (make-llm-ollama
      :chat-model "gemma3:1b"
      :embedding-model "nomic-embed-text"
      ))
  :config
  (setq ellama-sessions-directory "~/.emacs.d/.local/cache/ellama-sessions"))

;; Auto-update Packages on Launch
;; ------------------------------
(use-package auto-package-update
  :ensure t
  :config
  ;;(package-refresh-contents)
  (setq auto-package-update-delete-old-versions t)
  (setq auto-package-update-hide-results t)
  (auto-package-update-maybe))


(provide 'emcee-packages)
