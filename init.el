;;; package --- Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el bootstraping ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el"
			 user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
(setq package-enable-at-startup nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Interoperability between straight and use-package ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(straight-use-package 'use-package)
(use-package diminish :straight t)
(setq straight-check-for-modifications nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; General compilation options ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq comp-deferred-compilation t)
(setq load-prefer-newer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Init.el configurations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-file()
  "This function opens the principal configuration file."
  (find-file (expand-file-name "init.el"
			       user-emacs-directory)))

(global-set-key (kbd "<f6>") (lambda() (interactive) (config-file)))
(global-set-key (kbd "<f7>") 'emacs-lisp-byte-compile-and-load)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Custom UI settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(electric-pair-mode +1)
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(global-linum-mode +1)
(setq-default linum-highlight-current-line t)
(set-frame-font "Cascadia Mono 12" nil t)
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))

;;;;;;;;;;;;;;;;;;;;
;; TRAMP settings ;;
;;;;;;;;;;;;;;;;;;;;
(setq tramp-default-method "ssh")

;;;;;;;;;;;;;;;;;;
;; GPG settings ;;
;;;;;;;;;;;;;;;;;;
(setq epg-gpg-program "gpg2")
(setq auth-sources '((:source "~/.authinfo.gpg")))
(setq epa-pinentry-mode 'loopback)

;;;;;;;;;;;;;;;;;;
;; Text styling ;;
;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :straight t
  :defer t
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face-lines-tail))
  :hook
  (org-mode . whitespace-mode)
  (prog-mode . whitespace-mode)
  (json-mode . whitespace-mode)
  (yaml-mode . whitespace-mode))

(setq electric-pair-pairs
      '((?\" . ?\")
	(?\{ . ?\})
	(?\[ . ?\])))

(use-package rainbow-mode
  :straight t
  :after prog-mode
  :defer t
  :hook
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode)
  (yaml-mode . rainbow-mode)
  (json-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :after prog-mode
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (elisp-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  (yaml-mode . rainbow-delimiters-mode)
  (json-mode . rainbow-delimiters-mode))

(use-package yafolding
  :straight t
  :defer t
  :hook
  (prog-mode . yafolding-mode)
  (elisp-mode . yafolding-mode)
  (js-mode . yafolding-mode))

(use-package highlight-indent-guides
  :straight t
  :defer t
  :diminish
  :config
  (setq highlight-indent-guides-character ?\|)
  :hook
  (prog-mode . highlight-indent-guides-mode)
  (org-mode . highlight-indent-guides-mode)
  (yaml-mode . highlight-indent-guides-mode)
  (json-mode . highlight-indent-guides-mode)
  :custom
  (setq highlight-indent-guides-auto-enabled t)
  (setq highlight-indent-guides-responsive t)
  (setq highlight-indent-guides-method 'character))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package winum
  :straight t
  :config
  (set-face-attribute 'winum-face nil :weight 'bold)
  (setq winum-mode +1))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Project management ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package bind-key
  :straight t
  :config
  (add-to-list 'same-window-buffer-names "*Personal Keybindings*"))

(use-package ivy
  :straight t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d)")
  :config
  (ivy-mode +1))

(use-package counsel
  :straight t
  :after ivy
  :defer t
  :config
  (counsel-mode t))

(use-package swiper
  :straight t
  :after ivy
  :defer t
  :bind
  ("C-s" . swiper)
  ("C-s" . swiper))

(use-package company
  :straight t
  :defer t
  :diminish company-mode
  :custom
  (company-tooltip-align-annotation t)
  :config
  (setq company-idle-delay t)

  (use-package company-go
    :straight t
    :after company
    :defer t
    :config
    (add-to-list 'company-backends 'company-go))

  :hook
  (after-init . global-company-mode)
  (prog-mode . company-mode)
  (LaTeX-mode . company-mode)
  (org-mode . company-mode)
  (terraform-mode . company-mode))

(use-package projectile
  :straight t
  :after ivy
  :defer t
  :init
  (setq projectile-completion-system 'ivy)
  :bind
  ("C-c p" . 'projectile-command-map))
(projectile-mode +1)

(use-package counsel-projectile
  :straight t
  :after ivy
  :defer t)

(use-package projectile-rails
  :straight t
  :defer t)

(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode t))

(use-package magit
  :straight t
  :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  ("C-x g" . 'magit-status)
  ("C-x M-g" . 'magit-dispatch)
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends)))

(use-package forge
  :straight t
  :defer t
  :config
  (setq ghub-use-workaround-for-emacs-bug nil)
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

(use-package magit-gitflow
  :straight t
  :defer t
  :config
  (setq magit-gitflow-popup "C-F")
  :hook
  (magit-mode-hook . 'turn-on-magit-gitflow)
  :commands (magit-gitlfow))

(use-package treemacs
  :straight t
  :defer t
  :hook
  (treemacs-mode . (lambda() (linum-mode -1)))
  :config
  (setq treemacs-follow-mode t)
  (setq treemacs-filewatch-mode t)
  (setq treemacs-fringe-indicator-mode t))

(use-package treemacs-projectile
  :straight t
  :defer t
  :bind
  ("C-x t a" . treemacs-projectile)
  :after treemacs projectile)

;;;;;;;;;;;;;;;;;;;;;;
;; Terminal support ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :straight t
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-always-compile-module t))

(use-package multi-vterm
  :straight t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Postframe settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-ivy
  :straight t
  :after (all-the-icons ivy)
  :custom
  (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands
	       '(counsel-find-file
		 counsel-file-jump
		 counsel-recentf
		 counsel-projectile-find-file
		 counsel-projectile-find-dir))
  (all-the-icons-ivy-setup)
  :hook
  (after-init . all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :straight t
  :after dired
  :defer t
  :hook
  (dired-mode . all-the-icons-dired-mode))

(use-package ivy-posframe
  :straight t
  :defer t
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist
	'((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
	'((left-fringe . 20)
	  (right-fringe . 20)))
  (setq ivy-posframe-height-alist '((t . 15)))
  (ivy-posframe-mode +1)
  :hook
  (after-init . ivy-posframe-mode))

;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck support ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :straight t
  :defer t
  :hook
  (prog-mode . flycheck-mode)
  (org-mode . flycheck-mode)
  (json-mode . flycheck-mode)
  (yaml-mode . flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "/usr/bin/eslint")
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

;;;;;;;;;;;;;;;
;; Dashboard ;;
;;;;;;;;;;;;;;;
(use-package doom-modeline
  :straight t
  :custom
  (doom-modeline-buffer-file-name 'truncate-with-project)
  :init
  (setq vc-handled-backends nil)
  (setq doom-modeline-height 25)
  (setq doom-modeline-bar-width 3)
  (setq doom-modeline-project-detection 'project)
  (setq doom-modeline-buffer-file-name-style 'truncate-upto-project)
  (setq doom-modeline-icon t)
  (setq doom-modeline-major-mode-icon t)
  (setq doom-modeline-buffer-state-icon t)
  (setq doom-modeline-buffer-modification-icon t)
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-enable-word-count nil)
  (setq doom-modeline-checker-simple-format t)
  (setq doom-modeline-vcs-max-length 12)
  (setq doom-modeline-persp-name t)
  (setq doom-modeline-env-version t)
  (setq doom-modeline-env-load-string "...")
  :config
  (doom-themes-org-config)
  (setq doom-modeline-major-mode-icon t)
  :hook
  (after-init . doom-modeline-mode))

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;
(use-package doom-themes
  :straight t
  :custom
  (doom-themes-enable-italic t)
  (doom-themes-enable-bold t)
  :custom-face
  (doom-modeline-bar
   ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-sourcerer t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cl-lib :straight t)

(use-package which-key
  :straight t
  :config
  (which-key-mode))

(use-package js2-mode
  :straight t
  :hook
  (js2-mode . company-mode))

(use-package rjsx-mode
  :straight t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package apheleia
  :straight (apheleia :host github :repo "raxod502/apheleia")
  :config
  (setf (alist-get 'prettier apheleia-formatters)
	'(npx "prettier"
	      "--trailing-coma" "es5"
	      "--tab-width" "2"
	      "--use-tabs" "false"
	      "--bracket-spacing" "true"
	      "--single-quote" "true"
	      "--semi" "true"
	      "--jsx-single-quote" "true"
	      "--jsx-bracket-same-line" "true"
	      "--arrow-parens" "always"
	      file))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-mode. prettier))
  (apheleia-global-mode t))

(use-package json-mode
  :straight t
  :defer t)

(use-package yaml-mode
  :straight t
  :defer t)

(use-package web-mode
  :straight t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Language Server Protocol ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :straight t
  :hook
  (prog-mode . lsp-deferred)
  (lsp-mode . lsp-enable-which-key-integration)
  (lsp-mode . lsp-headerline-breadcrumb-mode)
  :init
  (setq lsp-auto-guess-root t)
  :config
  (add-hook 'js-mode-hook (lambda () (setq js-indent-level 2)))
  (add-hook 'js-mode-hook (lambda () (setq tab-width 2)))
  (setq warning-suppress-log-types t)
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 4096))
  (setq lsp-log-io nil)
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.node-modules\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.log\\'")
  (setq lsp-keymap-prefix "C-c l")
  :commands (lsp lsp-deferred))

(use-package company-lsp
  :straight t
  :commands company-lsp)

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

;;;;;;;;;;;;;;;;
;; Yassnippet ;;
;;;;;;;;;;;;;;;;
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :after ivy
  :defer t
  :hook
  (after-init . yas-global-mode)
  (prog-mode . yas-minor-mode))

;;;;;;;;;;;;;;
;; ORG MODE ;;
;;;;;;;;;;;;;;
(if (not (file-directory-p (expand-file-name "org" (getenv "HOME"))))
    (mkdir (expand-file-name "org" (getenv "HOME"))))

(if (not (file-exists-p (expand-file-name "org/agenda.org" (getenv "HOME"))))
    (write-region "" nil (expand-file-name "org/agenda.org" (getenv "HOME"))))
(if (not (file-exists-p (expand-file-name "org/todo.org" (getenv "HOME"))))
    (write-region "" nil (expand-file-name "org/todo.org" (getenv "HOME"))))
(if (not (file-exists-p (expand-file-name "org/journal.org" (getenv "HOME"))))
    (write-region "" nil (expand-file-name "org/journal.org" (getenv "HOME"))))

(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-c c") 'org-capture)
(global-set-key (kbd "C-c l") 'org-store-link)

(use-package org
  :straight t
  :config
  (setq org-hide-emphasis-markers t)
  (setq org-display-inline-images t)
  (setq org-log-done t)
  (setq org-image-actual-width 800)
  (setq org-startup-indented t)
  (setq org-directory "~/org") ;; establece el directorio principal de org
  (setq org-agenda-files (list "~/org/agenda.org"))
  (setq org-startup-folded "showall")
  (setq org-ditaa-jar-path (expand-file-name "ditaa"
					   (file-name-as-directory "/usr/bin")))
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar"
					      (file-name-as-directory "/usr/share/plantuml")))

  (setq org-default-notes-file (concat org-directory "/notas.org"))

  (setq org-todo-keywords
	'((sequence "TODO(t)" "DOING(d)" "|" "DONE" "CANCELED")
	  (sequence "REPORT(r)" "BUG(b)" "KNOWCAUSE(k)" "|" "FIXED(f)")))
  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "#76f0f3" :weight bold))
          ("BUG" . (:foreground "purple" :weight bold))
          ("REPORT" . (:foreground "white" :weight bold))
          ("FIXED" . (:foreground "#51ee7b" :weight bold))
          ("DOING" . (:foreground "orange" :weight bold))
          (("DONE") . (:foreground "green" :weight bold))
          ("CANCELED" . (:foreground "yellow" :weight bold))))
  (setq org-link '((:foreground "#ebe087" :underline t)))
  (setq org-list-dt '((:foreground "#bd93f9")))
  (setq org-special-keyword '((:foreground "#6272a4")))
  (setq org-todo '((:background "#272934" :foreground "#51fa7b" :weight bold)))
  (setq org-document-title '((:foreground "#f1fa8c" :weight bold)))
  (setq org-done '((:background "#373844" :foreground "#215933" :strike-trough nil :weight bold)))
  (setq org-footnote '((:foreground "#76e0f3")))
  (setq org-confirm-babel-evaluate nil
	org-src-fontify-natively t
	org-src-tab-acts-natively t)
  :hook
  (org-babel-after-execute . org-display-inline-images)
  (message-mode . turn-on-orgtbl)
  (message-mode . turn-on-orgstruct++))

(use-package ob-async
  :straight t
  :after org-mode
  :defer t)

(use-package ob-restclient
  :straight t
  :after org-mode
  :defer t)

(use-package ob-http
  :straight t
  :defer t
  :after org-mode)

(org-babel-do-load-languages
 'org-babel-load-languages
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
   (http . t)
   (sass . t)
   (sql . t)
   (sqlite . t)
   (shell . t)))

(use-package org-tree-slide
  :straight t
  :after org-mode-abbrev-table
  :hook
  (org-mode . org-tree-slide-mode)
  :defer t)

(use-package org-bullets
  :straight t
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))
