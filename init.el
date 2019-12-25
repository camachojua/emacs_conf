;;; Package -- summary
;;; Commentary: Package configuration.
;;; Code:
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'bind-key)
(use-package diminish
  :ensure t)

;; User information
(setq user-full-name "Juan Alberto Camacho Bolaños")
(setq user-mail-address "juancamacho@ciencias.unam.mx")
(setq user-gitlab-user "camachojua")
(setq user-github-user "camachojua")

;; General settings
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(set-frame-font "FiraCode 11" nil t)
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))
;; (add-hook 'after-focus-change-function #'garbage-collect)
(windmove-default-keybindings)
(global-set-key (kbd "C-c b")  'windmove-left)
(global-set-key (kbd "C-c f") 'windmove-right)
(global-set-key (kbd "C-c p")    'windmove-up)
(global-set-key (kbd "C-c n")  'windmove-down)
(setq windmove-wrap-around t)

;; Org-mode setting
(setq org-startup-indented t)
(setq org-directory "~/org")
(setq org-startup-folded "showall")

;; Org Mode Rest client support
(use-package ob-restclient
  :ensure t
  :defer t
  :after org-mode)

;; Asynchronous execution
(use-package ob-async
  :ensure t
  :defer t
  :after org-mode)

;; Org-babel settings
(org-babel-do-load-languages 'org-babel-load-languages
                             '((awk . t)
                               (C . t)
                               (clojure . t)
                               (ditaa . t)
                               (emacs-lisp . t)
                               (gnuplot . t)
                               (haskell . t)
                               (java . t)
                               (js . t)
                               (latex . t)
                               (org . t)
                               (plantuml . t)
                               (python . t)
                               (restclient . t)
                               (sass . t)
                               (sql . t)
                               (sqlite . t)
                               (shell . t)))

(setq org-ditaa-jar-path "/usr/bin/ditaa")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

(use-package org-bullets
  :ensure t
  :after org-mode
  :defer t
  :config
  (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸"))
  :hook
  (org-mode . org-bullets-mode))

;; Establish the TODO keywords
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "#6272a4" :weight bold))
	("DOING" . (:foreground "#f1fa8c" :weight bold))
	("DONE" . (:foreground "#6272a4" :background "#373844" :weight bold))))

;; Configuration inside org files
(setq org-link '((:foreground "#ebe087" :underline t)))
(setq org-list-dt '((:foreground "#bd93f9")))
(setq org-special-keyword '((:foreground "#6272a4")))
(setq org-todo '((:background "#272934" :foreground "#51fa7b" :weight bold)))
(setq org-document-title '((:foreground "#f1fa8c" :weight bold)))
(setq org-done '((:background "#373844" :foreground "#215933" :strike-trough nil :weight bold)))
(setq org-footnote '((:foreground "#76e0f3")))

;; Don't ask for confirmation while evaluating a block
(setq org-confirm-babel-evaluate nil
      org-src-fontify-natively t
      org-src-tab-acts-natively t)
(add-hook 'org-babel-after-execute-hook 'org-display-inline-images)

;; Org tables and lists on message mode
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; Org-mu4e
(require 'org-mu4e)
(setq org-mu4e-link-query-in-headers-mode nil)
(setq org-capture-templates
      '(("t" "todo" entry
         (file+headline "~/todo.org" "Tasks")
         "* TODO [#A] %?\nSCHEDULED: %(org-insert-time-stamp (org-read-date nil t \"+0d\"))\n%a\n")))

;; Visual lines for indentation
(use-package highlight-indent-guides
  :ensure t
  :defer t
  :diminish
  :hook
  ((elisp-mode dockerfile-mode js-mode prog-mode yaml-mode json-mode) . highlight-indent-guides-mode)
  :custom
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

;; Eshell goodies
(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")
(setq eshell-prompt-function
      (lambda nil
        (concat
         (if (string= (eshell/pwd) (getenv "HOME"))
             (propertize "~" 'face `(:foreground "#a099ab"))
           (replace-regexp-in-string
            (getenv "HOME")
            (propertize "~" 'face `(:foreground "#98cbff"))
            (propertize (eshell/pwd) 'face `(:foreground "#908bff"))))
         (if (= (user-uid) 0)
             (propertize " α " 'face `(:foreground "#ffaf06"))
         (propertize " λ " 'face `(:foreground "#aabf2e"))))))
(setq eshell-highlight-prompt nil)
(defalias 'open 'find-file-other-window)
(defalias 'clean 'eshell/clear-scrollback)
(defun eshell/sudo-open (filename)
  "Open a file (FILENAME) as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

;; Text style
(use-package whitespace
  :ensure t
  :defer t
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face-lines-tail))
  :hook
  (org-mode . whitespace-mode)
  (prog-mode . whitespace-mode)
  (arduino-mode . whitespace-mode)
  (json-mode . whitespace-mode)
  (yaml-mode . whitespace-mode))

;; Electric pair
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))
(electric-pair-mode 1)

(use-package rainbow-mode
  :ensure t
  :after prog-mode
  :defer t
  :hook
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode)
  (yaml-mode . rainbow-mode)
  (json-mode . rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :after prog-mode
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  (yaml-mode . rainbow-delimiters-mode)
  (json-mode . rainbow-delimiters-mode))

;; Emoji support
(use-package emojify
  :ensure t
  :defer t
  :hook
  (after-init . global-emojify-mode))

(use-package company-emoji
  :ensure t
  :defer t
  :after company)

;; Programming projects
(use-package ivy
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d)")
  :config
  (ivy-mode t))

(use-package counsel
  :ensure t
  :after ivy
  :defer t
  :config (counsel-mode t))

(use-package swiper
  :after ivy
  :defer t
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package projectile
  :ensure t
  :after ivy
  :defer t
  :init
  (setq projectile-completion-system 'ivy)
  :bind
  ("C-c p" . 'projectile-command-map)
  ("C-c p f" . 'projectile-find-file)
  :config
  (projectile-mode t))

(use-package company
  :ensure t
  :defer t
  :diminish
  :config
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-emoji)
  (setq company-idle-delay t)

  (use-package company-go
    :ensure t
    :after company
    :defer t
    :config
    (add-to-list 'company-backends 'company-go))
  :hook
  ((prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(use-package company-emoji
  :ensure t
  :after company
  :defer t
  :hook
  ((markdown-mode . company-mode)
   (git-commit-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package emojify
  :ensure t
  :defer t
  :hook
  ((markdown-mode . emojify-mode)
   (git-commit-mode . emojify-mode)
   (magit-status-mode . emojify-mode)
   (magit-log-mode . emojify-mode)))

;; YASnippet
(if (not (file-directory-p (expand-file-name "snippets" user-emacs-directory)))
    (mkdir (expand-file-name "snippets" user-emacs-directory)))
(use-package yasnippet
  :ensure t
  :after ivy
  :defer t
  :hook
  (after-init . yas-global-mode)
  (prog-mode-hook . yas-minor-mode)
  :config
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode t))

;; YAML mode
(use-package yaml-mode
  :ensure t)

;; ReactJS Settings
(use-package react-snippets
  :ensure t
  :defer t
  :after yasnippet)

;; Icons for various modes
(use-package all-the-icons :ensure t :defer 0.5)

(use-package all-the-icons-ivy
  :ensure t
  :defer t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Doom mode-line
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name 'truncate-with-project)
  :ensure t
  :init
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-major-mode-or-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-lsp t)
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-env-version t)
  (setq doom-modeline-mu4e t)
  :hook
  (after-init . doom-modeline-mode))

;; Docker support
(use-package docker
  :ensure t
  :defer t
  :bind("C-c d" . docker))

;; Docker tramp
(use-package docker-tramp
  :ensure t
  :defer t)

;; Language Server Protocol
;; (use-package lsp-mode
;;   :ensure t
;;   :hook
;;   (js-mode . lsp-deferred)
;;   (docker-mode . lsp-deferred)
;;   (company-mode . lsp-deferred)
;;   :commands (lsp lsp-deferred)
;;   :custom
;;   (lsp-print-io nil)
;;   (lsp-trace nil)
;;   (lsp-print-performance nil)
;;   (lsp-auto-guess-root t)
;;   (lsp-document-sync-method 'incremental)
;;   (lsp-response-timeout 5)
;;   (lsp-enable-completion-at-point nil))

;; (use-package company-lsp
;;   :ensure t
;;   :after lsp-mode
;;   :custom
;;   (company-lsp-cache-candidates t)
;;   (company-lsp-async t)
;;   (company-lsp-enable-recompletion nil )
;;   :config
;;   (push 'company-lsp company-backends))

;; (use-package lsp-ui
;;   :ensure t
;;   :defer t
;;   :after lsp-mode
;;   :hook
;;   (lsp-mode . lsp-ui-mode)
;;   (js-mode . flycheck-mode)
;;   :commands lsp-ui-mode)

(defun js-config-hooks ()
  (add-hook 'js-mode-hook (lambda () (setq indent-line-function 'insert-tab)))
  (add-hook 'js-mode-hook (lambda () (setq indent-tabs-mode nil)))
  (add-hook 'js-mode-hook (lambda () (setq tab-width 2)))
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))
(add-hook 'js-mode-hook #'js-config-hooks)

;; Git
(use-package magit
  :ensure t
  :after ivy
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :bind
  (("C-x g" . 'magit-status)
   ("C-x M-g" . 'magit-dispatch)))
;; Github specific configuration
(auth-source-forget-all-cached)
(use-package forge
  :ensure t
  :defer t
  :config
  (setq ghub-use-workaround-for-emacs-bug nil)
  (setq gitlab.user "camachojua")
  (setq git.fciencias.unam.mx/api/v4.user "camachojua")
  (add-to-list 'forge-alist
               '("git.fciencias.unam.mx"
                 "git.fciencias.unam.mx/api/v4"
                 "git.fciencias.unam.mx"
                 forge-gitlab-repository))
  (defun forge-create-secret-auth ()
    "Prompts for and creates the git forge secret. Mostly for gitlab"
    (interactive)
    (let*
	((repo (forge-get-repository 'full))
	 (host (oref repo apihost))
	 (username (ghub--username host 'gitlab))
	 (user (concat username "^forge"))
	 token)
      (setq token (read-passwd (format "Enter your token for %s @ %s: " username host)))
      (ghub-clear-caches)
      (auth-source-forget-all-cached)
      (secrets-create-item
       "Login" (format "%s @ %s" user host)
       token
       :host host
       :user user))))
;; Ediff settings
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))

;; AUCTeX
(use-package tex
  :ensure auctex
  :defer t
  :init
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq-default TeX-master nil)
  (setq reftex-plug-into-AUCTEX t)
  (setq TeX-parse-self t)
  (setq TeX-source-correlate-method 'synctex)
  (setq TeX-source-correlate-start-server t)
  (setq preview-gs-command "PDF Tools")
  :hook
  (doc-view-mode . auto-revert-mode)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  :config
  (add-hook 'TeX-after-compilation-finished-functions
            #'TeX-revert-document-buffer)
  (TeX-source-correlate-mode t)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-command-list
               '("Index" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                 TeX-run-index nil t
                 :help "Run makeindex to create index file")))
;; NASM mode
(use-package nasm-mode
  :ensure t
  :defer t
  :hook
  (asm-mode . nasm-mode))

;; Email Settings
(use-package w3m
  :ensure t
  :defer t
  :after mu4e)

;; IMAP Settings
(when (not (featurep 'mu4e))
  (add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e/"))

(use-package mu4e
  :defer t
  :if window-system
  :load-path "/usr/local/share/emacs/site-lisp/mu4e/"
  :ensure org-mime
  :ensure htmlize
  :ensure mu4e-alert
  :init
  (require 'org-mu4e)
  :config
  (setq mu4e-html2text-command "w3m -T text/html")
  (setq mail-user-agent 'mu4e-user-agent)
  (setq mu4e-drafts-folder "/[Gmail].Borradores")
  (setq mu4e-sent-folder "/[Gmail].Enviados")
  (setq mu4e-trash-folder "/[Gmail].Papelera")
  (setq mu4e-sent-messages-behavior 'delete)
  (setq mu4e-user-mail-address-list (quote ("juancamacho@ciencias.unam.mx")))
  (setq mu4e-view-show-images t)
  (when (fboundp 'imagemagick-register-types)
    (imagemagick-register-types))
  (setq mu4e-maildir-shortcuts
        '(("/INBOX" . ?i)
          ("/[Gmail].Enviados" . ?s)
          ("/[Gmail].Trash" . ?t)
          ("/[Gmail].Todos" . ?a)))
  (setq mu4e-get-mail-command "offlineimap")
  (setq
   mu4e-compose-signature
   (concat "¡Saludos!\n"
           user-full-name)))

  ;; SMTP Settings
(use-package smtpmail
  :ensure t
  :after mu4e
  :defer t
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials
        '(("smtp.gmail.com" 587 "juancamacho@ciencias.unam.mx" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))
;; Email notifications
(use-package mu4e-alert
  :ensure t
  :defer t
  :hook
  (after-init . mu4e-alert-enable-mode-line-display)
  :init
  (setq mu4e-alert-interesting-mail-query
        (concat
         "flag:unread"
         " AND NOT flag:trashed"
         " AND maildir:/[Gmail]/INBOX"))
  (mu4e-alert-enable-mode-line-display))

;; Org tables and lists on message mode
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; HTTP requests
(use-package request
  :ensure t
  :defer t)

;; Alerts
(use-package alert
  :ensure t
  :defer t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; Circe IRC client
(use-package circe
  :ensure t
  :defer t)

;; Easy HTTP requests
(use-package request
  :ensure t
  :defer t)

;; Websocket
(use-package websocket
  :ensure t
  :defer t)

;; Document rendering
(use-package pdf-tools
  :ensure t
  :defer t
  :init
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (setq pdf-view-resize-factor 1.1)
  :config
  (pdf-tools-install)
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward))

(defun my-nov-font-setup ()
  "Font for epubs."
  (face-remap-add-relative 'variable-pitch :family "Liberation Serif"
                                           :height 1.0))
(use-package nov
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode . mv-nov-font-setup)
  :config
  (add-hook 'doc-view-mode-hook (lambda () (linum-mode -1))))

;; Dashboard
(use-package dashboard
  :ensure t
  :functions (all-the-icons-faicon
                all-the-icons-material
                open-custom-file
                persp-get-buffer-or-null
                persp-load-state-from-file
                persp-switch-to-buffer
                winner-undo
                widget-forward)
  :init
  (setq dashboard-banner-logo-title "")
  (setq dashboard-startup-banner "~/Images/gnu.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)
			  (agenda . 5)))
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-init-info t
        dashboard-set-file-icons t
        dashboard-set-heading-icons t
        dashboard-heading-icons '((recents   . "file-text")
                                  (bookmarks . "bookmark")
                                  (agenda    . "calendar")
                                  (projects  . "file-directory")
                                  (registers . "database")))
  (add-hook 'dashboard-mode-hook (lambda() (linum-mode -1))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming languages ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; YAML mode
(use-package yaml-mode
  :ensure t
  :defer t)

;; Pug mode
(use-package pug-mode
  :ensure t
  :defer t)

;; Clojure mode settings
(use-package clojure-mode
  :ensure t
  :defer t
  :hook
  (coljure-mode . agrresive-indent-mode))

;; Haskell mode settings
(use-package haskell-mode
  :ensure t
  :defer t)

;; Scss mode settings
(use-package scss-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode)))

;; HTTP requests
(use-package request
  :ensure t
  :defer t)

;; ReactJS Settings
(use-package react-snippets
  :ensure t
  :after yasnippet
  :defer t)

;; JSON support
(use-package json-mode
  :ensure t
  :defer t)

;; Dockerfile mode
(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'"
  :defer t)

;; Flycheck
(use-package flycheck
  :ensure t
  :defer t
  :hook
  (after-init . global-flycheck-mode)
  :config
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t))))
;; js2-refactor
(use-package js2-refactor
  :ensure t
  :after js-mode
  :defer t
  :config
  (js2r-add-keybindings-with-prefix "C-c C-m")
  :hook
  (js-mode . js2-refactor-mode))

(use-package prettier-js
  :ensure t
  :after js-mode
  :defer t
  :config
  (setq prettier-js-args
	'("--trailing-comma" "es7"
          "--single-quote" "true"
          "--print-width" "100"))
  :hook
  (js-mode . prettier-js-mode))

;; Jest mode
(use-package jest
  :ensure t
  :after js-mode
  :defer t
  :bind
  (("C-x j" . jest-popup)))

;; REST-client
(use-package restclient
  :ensure t
  :defer t)

;; Tema
(use-package doom-themes
  :ensure t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (doom-themes-visual-bell-config)
  (load-theme 'doom-dracula t)
  (doom-themes-neotree-config)
  (doom-themes-org-config))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
 '(company-lsp-async t)
 '(company-lsp-cache-candidates t)
 '(company-lsp-enable-recompletion nil)
 '(doom-modeline-buffer-file-name 'truncate-with-project t)
 '(doom-themes-enable-bold t)
 '(doom-themes-enable-italic t)
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
 '(highlight-indent-guides-auto-enabled t)
 '(highlight-indent-guides-method 'character)
 '(highlight-indent-guides-responsive t)
 '(ivy-use-virtual-buffers t)
 '(lsp-auto-guess-root t)
 '(lsp-document-sync-method 'incremental)
 '(lsp-enable-completion-at-point nil)
 '(lsp-log-io nil)
 '(lsp-print-io nil)
 '(lsp-print-performance nil)
 '(lsp-response-timeout 5)
 '(lsp-trace nil t)
 '(org-bullets-bullet-list
   '(":diamonds:"
     (\, ":one:")
     (\, ":two:")
     (\, ":three:")
     (\, ":four:")
     (\, ":five:")))
 '(package-selected-packages
   '(lsp-ui highlight-indent-guides doom-themes company-lsp lsp-mode ob-restclient jade-mode org-pretty-table org-bullets xah-elisp-mode htmlize org-mime emacs-emojify jest company-php prettier-js company-go org-mu4e rjsx-mode js2-refactor flycheck dockerfile-mode json-mode scss-mode haskell-mode clojure-mode pug-mode dashboard websocket circe request mu4e-alert react-snippets yaml-mode yasnippet emojify company-emoji company projectile doom-modeline all-the-icons-dired all-the-icons-ivy all-the-icons rainbow-delimiters rainbow-mode autopair use-package ob-async diminish)))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4")))))
