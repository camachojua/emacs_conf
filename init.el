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

;; General settings
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(set-frame-font "Cascadia Code 10" nil t)
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))
;; (add-hook 'after-focus-change-function #'garbage-collect)

;; Enable full screen
(if (eq window-system 'ns)
    (toggle-frame-maximized)
  (toggle-frame-fullscreen))

;; Org-mode setting
(setq org-startup-indented t)
(setq org-directory "~/org")
(setq org-startup-folded "showall")

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
                               (sass . t)
                               (sql . t)
                               (sqlite . t)
                               (shell . t)))

(setq org-ditaa-jar-path "/usr/bin/ditaa")
(setq org-plantuml-jar-path "/usr/share/plantuml/plantuml.jar")

;; Establish the TODO keywords
(setq org-todo-keywords
      '((sequence "REPORT" "BUG" "KNOWCAUSE" "|" "FIXED")
        (sequence "TODO" "|" "DOING" "DONE" "CANCELED")))
(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "cyan" :weight bold))
        ("BUG" . (:foreground "purple" :weight bold))
        ("REPORT" . (:foreground "white" :weight bold))
        ("FIXED" . (:foreground "green" :weight bold))
        ("DOING" . (:foreground "orange" :weight bold))
        (("DONE") . (:foreground "green" :weight bold))
        ("CANCELED" . (:foreground "yellow" :weight bold))))

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
  :init
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face-lines-tail))
  :config
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'arduino-mode-hook 'whitespace-mode))

;; Electric pair
(setq electric-pair-pairs
      '(
        (?\" . ?\")
        (?\{ . ?\})))
(electric-pair-mode 1)

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; Emoji support
(use-package emojify
  :ensure t
  :init
  (add-hook 'after-init-hook #'global-emojify-mode))

(use-package company-emoji
  :ensure t
  :defer t
  :after company)

;; Programming projects
(use-package ivy
  :ensure t
  :init
  (setq ivy-use-virtual-buffers t)
  (setq ivy-count-format "(%d/%d) ")
  :custom
  (ivy-use-virtual-buffers t)
  :config
  (ivy-mode t))

(use-package counsel
  :ensure t
  :after ivy
  :config (counsel-mode t))

(use-package swiper
  :after ivy
  :bind (("C-s" . swiper)
         ("C-r" . swiper)))

(use-package projectile
  :ensure t
  :after ivy
  :init
  (setq projectile-completion-system 'ivy)
  :bind
  (("C-c p" . 'projectile-command-map))
  :config
  (projectile-mode t))

(use-package company
  :ensure t
  :diminish
  :config
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (add-to-list 'company-backends 'company-emoji)
  (setq company-idle-delay t)

  (use-package company-go
    :ensure t
    :config
    (add-to-list 'company-backends 'company-go))
  :hook
  ((prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)))

(use-package company-emoji
  :ensure t
  :after company
  :hook
  ((markdown-mode . company-mode)
   (git-commit-mode . company-mode))
  :config
  (add-to-list 'company-backends 'company-emoji))

(use-package emojify
  :ensure t
  :hook
  ((markdown-mode . emojify-mode)
   (git-commit-mode . emojify-mode)
   (magit-status-mode . emojify-mode)
   (magit-log-mode . emojify-mode)))

;; YASnippet
(use-package yasnippet
  :ensure t
  :after ivy
  :defer t
  :hook (after-init . yas-global-mode)
  :config
  (yas-load-directory "~/.emacs.d/snippets/")
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode t))
;; YAML mode
(use-package yaml-mode
  :ensure t)

;; ReactJS Settings
(use-package react-snippets
  :ensure t
  :after yasnippet)

;; Icons for various modes
(use-package all-the-icons :ensure t :defer 0.5)

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

;; Doom mode-line
(use-package doom-modeline
  :ensure t
  :init
  (add-hook 'after-init-hook #'mu4e-alert-enable-mode-line-display)
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
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
  :config
  (doom-modeline-mode t))

;; Git
(use-package magit
  :ensure t
  :after ivy
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . 'magit-status)
   ("C-x M-g" . 'magit-dispatch)))
;; Github specific configuration
(use-package forge
  :ensure t
  :defer t
  :after magit
  :config
  (setq gitlab.user "username")
  (setq gitlab.git.private.domain.com/api/v4.user "username")
  (add-to-list 'forge-alist
               '("gitlab.git.privatedomain.com"
                 "gitlab.git.privatedomain.com/api/v4"
                 "gitlab.git.privatedomain.com"
                 forge-gitlab-repository)))
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
  :config
  (add-hook 'doc-view-mode-hook 'auto-revert-mode)
  (add-hook 'LaTeX-mode-hook 'visual-line-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
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
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))

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
   user-mail-address "juancamacho@ciencias.unam.mx"
   user-full-name "Juan Alberto Camacho Bolaños"
   mu4e-compose-signature
   (concat "¡Saludos!\n"
           "Juan Alberto Camacho Bolaños")))

  ;; SMTP Settings
(use-package smtpmail
  :ensure t
  :after mu4e
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
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query
        "flag:unread maildir:/[Gmail]/INBOX"))
(mu4e-alert-enable-mode-line-display)
(defun refresh-mu4e-mode-line ()
  "Para mu4e."
  (interactive)
  (mu4e~proc-kill)
  (mu4e-alert-enable-mode-line-display))
(run-with-timer 0 60 'refresh-mu4e-mode-line)

;; Org tables and lists on message mode
(add-hook 'message-mode-hook 'turn-on-orgtbl)
(add-hook 'message-mode-hook 'turn-on-orgstruct++)

;; HTTP requests
(use-package request
  :ensure t
  :defer t)

;; NASM mode
(use-package nasm-mode
  :ensure t
  :defer t
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))

;; Alerts
(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; Circe IRC client
(use-package circe
  :ensure t)

;; Easy HTTP requests
(use-package request
  :ensure t)

;; Websocket
(use-package websocket
  :ensure t)


;; Document rendering
(use-package pdf-tools
  :ensure t
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
  :config
  (add-hook 'nov-mode-hook 'my-nov-font-setup)
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
  (setq dashboard-startup-banner "/home/juan/Images/gnu.png")
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
  :ensure t)

;; Pug mode
(use-package pug-mode
  :ensure t)

;; Clojure mode settings
(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook #'aggressive-indent-mode))

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

;; NASM mode
(use-package nasm-mode
  :ensure t
  :defer t
  :config
  (add-hook 'asm-mode-hook 'nasm-mode))

;; HTTP requests
(use-package request
  :ensure t
  :defer t)

;; Alerts
(use-package alert
  :ensure t
  :commands (alert)
  :init
  (setq alert-default-style 'notifier))

;; Circe IRC client
(use-package circe
  :ensure t)

;; Websocket
(use-package websocket
  :ensure t)

;; ReactJS Settings
(use-package react-snippets
  :ensure t
  :after yasnippet)

;; JSON support
(use-package json-mode
  :ensure t)

;; Dockerfile mode
(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")

;; Web-mode settings
(use-package web-mode
  :ensure t
  :defer t
  :init
  (setq web-mode-code-indent-offset 4)
  (setq web-mode-indent-style 4)
  (setq web-mode-enable-current-column-highlight t)
  (setq web-mode-ac-sources-alist
	'(("css" . (ac-source-css-property))
	  ("html" . (ac-source-words-in-buffer ac-source-abbrev))))
  (setq web-mode-engines-alist
	'(("php" . "\\.phtml\\'")
	  ("blade" . "\\.blade\\.")
          ("json" . "\\.api\\'")
          ("xml"  . "\\.api\\'")
          ("jsx"  . "\\.js[x]?\\'")))
  (add-to-list 'auto-mode-alist '("\\.api\\'" . web-mode))
  (setq-default indent-tabs-mode nil)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.json$" . json-mode))
  (defadvice web-mode-highlight-part (around tweak-jsx activate)
    (if (equal web-mode-content-type "jsx")
        (let ((web-mode-enable-part-face nil))
          ad-do-it)
      ad-do-it))
  :config
  (define-key web-mode-map (kbd "C-t t") 'phpunit-current-test)
  (define-key web-mode-map (kbd "C-t c") 'phpunit-current-class)
  (define-key web-mode-map (kbd "C-t p") 'phpunit-current-project))

(use-package phpunit
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.php$'" . phpunit-mode)))

(defun php-company-hook()
  "Company-mode hook for php."

  (use-package company-php
    :ensure t
    :defer t
    :init
    (add-to-list 'company-backends 'company-ac-php-backend)
    :config
    (company-mode t)
    (ac-php-core-eldoc-setup)
    (make-local-variable 'company-backends)))

(use-package php-mode
  :ensure t
  :defer t
  :bind
  (("M-." . ac-php-find-symbol-at-point)
   ("M-," . ac-php-location-stack-back))
  :config
  (add-hook 'php-mode-hook 'php-company-hook))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'after-init-hook #'global-flycheck-mode)
  (eval-after-load 'flycheck
    '(custom-set-variables
      '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))))
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t))))

(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" source)
  :error-patterns
  ((error line-start
          (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

;; rjsx mode
(use-package rjsx-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
  (add-hook 'rjsx-mode-hook
            (lambda ()
              (setq indent-tabs-mode nil)
              (setq js-indent-level 2)
              (setq js2-strict-missing-semi-warning nil))))

(defun js-jsx-indent-line-align-closing-bracket ()
  "Workaround 'sgml-mode' and align closing bracket with opening bracket."
  (save-excursion
    (beginning-of-line)
    (when (looking-at-p "^ +\/?> *$")
      (delete-char sgml-basic-offset))))
(advice-add #'js-jsx-indent-line
            :after
            #'js-jsx-indent-line-align-closing-bracket)

(use-package js2-refactor
    :ensure t
    :config
    (js2r-add-keybindings-with-prefix "C-c C-m")
    (add-hook 'js2-mode-hook 'js2-refactor-mode))

(use-package prettier-js
  :ensure t
  :config
  (setq prettier-js-args '(
                           "--trailing-comma" "es6"
                           "--single-quote" "true"
                           "--print-width" "100"
                          ))
  (add-hook 'js2-mode-hook 'prettier-js-mode)
  (add-hook 'rjsx-mode-hook 'prettier-js-mode))

;; Jest mode
(use-package jest
  :ensure t
  :after web-mode)

;; Tema
(use-package chocolate-theme
  :ensure t
  :defer t
  :init
  (load-theme 'chocolate t))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
 '(flycheck-disabled-checkers '(javascript-jshint javascript-jscs))
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
   '(htmlize org-mime emacs-emojify jest company-php prettier-js company-go org-mu4e rjsx-mode js2-refactor flycheck dockerfile-mode json-mode scss-mode haskell-mode clojure-mode pug-mode dashboard websocket circe request mu4e-alert react-snippets yaml-mode yasnippet emojify company-emoji company projectile doom-modeline all-the-icons-dired all-the-icons-ivy all-the-icons rainbow-delimiters rainbow-mode autopair use-package ob-async diminish)))
;;; init.el ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
