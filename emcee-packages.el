;;  _____                         
;; | ____|_ __ ___   ___ ___  ___ 
;; |  _| | '_ ` _ \ / __/ _ \/ _ \
;; | |___| | | | | | (_|  __/  __/
;; |_____|_| |_| |_|\___\___|\___|


;; Package Manager
;; ---------------

;; Add sources
(require 'package)
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Install packages
(use-package all-the-icons            :ensure t)
(use-package all-the-icons-dired      :ensure t)
(use-package all-the-icons-nerd-fonts :ensure t)
(use-package all-the-icons-ivy-rich   :ensure t)
(use-package all-the-icons-ivy        :ensure t)
(use-package nerd-icons               :ensure t)
(use-package which-key-posframe       :ensure t)
(use-package vertico                  :ensure t)
(use-package vertico-posframe         :ensure t)
(use-package magit                    :ensure t)
(use-package vterm                    :ensure t)

(use-package dired
  :ensure nil
  :hook
  (dired-mode . all-the-icons-dired-mode)
  :init
  (add-hook 'dired-mode-hook
    ;; Auto-refresh dired on file change
    (auto-revert-mode)))
              
(use-package multiple-cursors
  :ensure t
  :init
  (multiple-cursors-mode 1))

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

;;(use-package golden-ratio
;;  :ensure t
;;  :config
;;  (golden-ratio-mode))

(use-package ivy
  :defer 0.1
  :diminish
  :bind (("C-c C-r" . ivy-resume)
         ("C-x B"   . ivy-switch-buffer-other-window))
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

(use-package org
  :ensure nil
  :hook (org-indent-mode)
  :config
  (setq org-hide-emphasis-markers t)
  (line-number-mode -1))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (let* ((base-font-color     (face-foreground 'default nil 'default))
         (headline           `(:inherit default :weight bold :foreground ,base-font-color)))

    (custom-theme-set-faces
     'user
     `(org-level-8 ((t (,@headline))))
     `(org-level-7 ((t (,@headline))))
     `(org-level-6 ((t (,@headline))))
     `(org-level-5 ((t (,@headline))))
     `(org-level-4 ((t (,@headline))))
     `(org-level-3 ((t (,@headline, :height 1.1))))
     `(org-level-2 ((t (,@headline, :height 1.2))))
     `(org-level-1 ((t (,@headline, :height 1.3))))
     `(org-document-title ((t (,@headline, :height 1.4 :underline nil)))))))


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

(use-package elsa
  :defer t
  :ensure t)

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
