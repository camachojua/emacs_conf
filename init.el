;;; package --- Summary
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el bootstraping ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq native-comp-async-report-warnings-errors nil)
(setq straight-repository-branch "develop")
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 6))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

;; Pixel scrolling
(pixel-scroll-precision-mode)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gagbage collection settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq gc-cons-thresold (* 100 1000 1000))
(add-hook 'focus-out 'garbage-collect)
(run-with-idle-timer 5 t 'garbage-collect)

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
(global-display-line-numbers-mode t)
(setq-default linum-highlight-current-line t)
;; (set-frame-font "Cascadia Mono 9" nil t)
(setq default-frame-alist '((font . "Cascadia Mono 9")))
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))
(setq-default fill-column 80)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)
;; (global-subword-mode t)
;; (global-superword-mode t)

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
(use-package pinentry
  :defer nil
  :straight t
  :config (pinentry-start))

;; Better close encrypted buffers after one minute of innactivity
(defun kill-gpg-buffers ()
  "Kill GPG buffers."
  (interactive)
  (let ((buffers-killed 0))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (string-match ".*\.gpg$" (buffer-name buffer))
          (message "Auto killing .gpg buffer '%s'" (buffer-name buffer))
          (when (buffer-modified-p buffer)
            (save-buffer))
          (kill-buffer buffer)
          (setq buffers-killed (+ buffers-killed 1)))))
    (unless (zerop buffers-killed)
      ;; Kill gpg-agent.
      (shell-command "gpgconf --kill gpg-agent")
      (message "%s .gpg buffers have been autosaved and killed" buffers-killed))))

(run-with-idle-timer 60 t 'kill-gpg-buffers)

;;;;;;;;;;;;;;;;;;
;; Text styling ;;
;;;;;;;;;;;;;;;;;;
(use-package whitespace
  :straight t
  :defer t
  :config
  (setq whitespace-line-column 88)
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

(global-so-long-mode 1)

(use-package vlf
  :straight t)

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
  (setq winum-mode +1)
  :hook
  (after-init . winum-mode))

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
  :init
  (setq company-tooltip-minimum-width 15
	company-idle-delay 0.1)
  :custom
  (company-tooltip-align-annotation t)
  :config
  (setq company-idle-delay t)
  (setq company-dabbrev-downcase nil)

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



(use-package diff-hl
  :straight t
  :init
  (global-diff-hl-mode t))

(use-package hydra
  :straight t)

(use-package magit
  :straight t
  :defer t
  :config
  (setq ghub-use-workaround-for-emacs-bug t)
  (setq ghub-use-workaround-for-emacs-bug-54989 t)
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

(use-package code-review
  :straight t
  :defer t
  :config
  (define-key forge-topic-mode-map (kbd "C-c C-r") 'code-review-forge-pr-at-point)
  (setq code-review-fill-column 80)
  (setq code-review-auth-login-marker 'forge)
  :hook
  (code-review-mode-hook . 'emojify-mode))

(require 'hydra)

(use-package smerge-mode
  :straight t
  :config
  (defhydra unpackaged/smerge-hydra
    (:color pink :hint nil :post (smerge-auto-leave))
    "
^Move^       ^Keep^               ^Diff^                 ^Other^
^^-----------^^-------------------^^---------------------^^-------
_n_ext       _b_ase               _<_: upper/base        _C_ombine
_p_rev       _u_pper              _=_: upper/lower       _r_esolve
^^           _l_ower              _>_: base/lower        _k_ill current
^^           _a_ll                _R_efine
^^           _RET_: current       _E_diff
"
    ("n" smerge-next)
    ("p" smerge-prev)
    ("b" smerge-keep-base)
    ("u" smerge-keep-upper)
    ("l" smerge-keep-lower)
    ("a" smerge-keep-all)
    ("RET" smerge-keep-current)
    ("\C-m" smerge-keep-current)
    ("<" smerge-diff-base-upper)
    ("=" smerge-diff-upper-lower)
    (">" smerge-diff-base-lower)
    ("R" smerge-refine)
    ("E" smerge-ediff)
    ("C" smerge-combine-with-next)
    ("r" smerge-resolve)
    ("k" smerge-kill-current)
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :color blue)
    ("q" nil "cancel" :color blue))
  :hook (magit-diff-visit-file . (lambda ()
                                   (when smerge-mode
                                     (unpackaged/smerge-hydra/body)))))

(use-package blamer
  :straight t
  :defer t
  :bind (("s-i" . blamer-show-commit-info))
  :custom
  (blamer-idle-time 0.3)
  (blamen-min-offset 20)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
		   :background "#001111"
		   :height 100
		   :italic t)))
  :config
  (global-blamer-mode 1))

;; (use-package code-review
;;   :straight t)

(use-package treemacs
  :straight t
  :defer t
  :hook
  (treemacs-mode . (lambda() (display-line-numbers-mode -1)))
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

(use-package dizzee
  :straight (dizzee :host github :repo "camachojua/dizzee")
  :defer t
  :init
  (setq garbage-collection-messages t)
  :hook
  (comint-mode . (lambda() (display-line-numbers-mode -1)))
  :config
  (dz-defservice buk-backend "./bin/rails"
               :args ("s")
               :cd "~/Src/buk-webapp")
  (dz-defservice buk-frontend "./bin/webpack-dev-server"
		 :cd "~/Src/buk-webapp")
  (dz-defservice-group buk (buk-backend buk-frontend)))

;;;;;;;;;;;;;;;;;;;;;;
;; Terminal support ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :straight t
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t)
  (setq vterm-always-compile-module t)
  :hook
  (vterm-mode . (lambda() (display-line-numbers-mode -1))))

(use-package vterm-toggle
  :straight t
  :bind
  ("C-c t" . vterm-toggle))

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
  :config
  (doom-themes-visual-bell-config)
  (doom-themes-treemacs-config)
  (doom-themes-org-config)
  (load-theme 'doom-dracula t))

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
  :interpreter (("node" . js2-mode))
  :mode "\\.\\(js\\|json\\)$"
  :config
  (setq js2-basic-offset 2
	js2-highlight-level 3)
  (setq js2-global-externs
	'("module" "require" "buster" "sinon" "assert" "refute" "setTimeout"
	  "clearTimeout" "setInterval" "clearInterval" "location" "__dirname"
	  "console" "JSON" "PTL" "$" "exports" "resolve" "reject" "process"
	  "localStorage" "DOMPurify"))
  :mode ("\\.js\\'" "\\.mjs\\'"))

(use-package web-beautify
  :straight t
  :defer t
  :config
  (bind-key "C-c C-b" 'web-beautify-js js2-mode-map))

(use-package dockerfile-mode
  :straight t)

(use-package clojure-mode
  :straight t
  :mode ("\\.clj\\'"))

(use-package tide
  :straight t
  :defer t
  :bind
  (("C-c r" . 'tide-rename-symbol)
   ("C-c f" . 'tide-refactor)
   ("C-c h" . 'tide-documentation-at-point))
  :hook
  (
   (typescript-mode . tide-setup)
   (typescript-mode . tide-mode)
   (typescript-mode . tide-hl-identifier-mode)
   (typescript-mode . eldoc-mode)
   (js-mode . tide-setup)
   (js-mode . tide-hl-identifier-mode)
   (js-mode . eldoc-mode)
   (js-mode . tide-mode)))

(use-package typescript-mode
  :straight t
  :mode ("\\.ts\\'" "\\.js'\\'")
  :config
  (setq typescrypt-indent-level 2))


(use-package rjsx-mode
  :straight t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package gcmh
  :straight (gcmh :host github :repo "emacsmirror/gcmh")
  :config
  (setq garbage-collection-messages t)
  (gcmh-mode 1))

(use-package apheleia
  :straight t
  :config
  (setf (alist-get 'prettier apheleia-formatters)
	'(npx "prettier"
	      "--trailing-coma"
	      "--tab-width" "2"
	      "--use-tabs" "false"
	      "--bracket-spacing" "true"
	      "--single-quote" "true"
	      "--semi" "true"
	      "--jsx-single-quote" "true"
	      "--arrow-parens" "always"
	      file))
  (add-to-list 'apheleia-mode-alist '(rjsx-mode . prettier))
  (add-to-list 'apheleia-mode-alist '(js-mode . prettier))
  :hook
  (js-mode . apheleia-mode))

(use-package json-mode
  :straight t
  :defer t)

(use-package yaml-mode
  :straight t
  :defer t)

(use-package haml-mode
  :straight t
  :defer t)

(use-package slim-mode
  :straight t
  :defer t)


(use-package web-mode
  :straight t
  :defer t
  :mode ("\\.html\\'"
         "\\.html\\.erb\\'"
         "\\.php\\'"
	 "\\.erb\\'"
         "\\.jinja\\'"
         "\\.j2\\'")
  :init
  ;; fix paren matching web-mode conflict for jinja-like templates
  (add-hook
   'web-mode-hook
   (lambda ()
     (setq-local electric-pair-inhibit-predicate
                 (lambda (c)
                   (if (char-equal c ?{) t (electric-pair-default-inhibit c))))))
  :config
  (setq web-mode-code-indent-offset 2
        web-mode-css-indent-offset 2
	web-mode-markup-indent-offset 2
        web-mode-markup-indent-offset 2
	web-mode-enable-auto-closing t
	web-mode-enable-current-element-highlighting t
 	web-mode-enamle-current-column-highlighting t))

(use-package ruby-mode
  :straight t
  :interpreter "ruby"
  :config
  (defun my-ruby-mode ()
    (custom-set-variables
     '(ruby-insert-encoding-magic-comment nil))
    (flycheck-mode t))
  (add-hook 'ruby-mode-hook 'my-ruby-mode))

(use-package ruby-end
  :straight t
  :config
  (setq ruby-end-mode t)
  :hook
  (ruby-mode-hook . ruby-end-mode))

(use-package robe
  :straight t
  :defer t
  :config
  (robe-start)
  :hook
  (ruby-mode . robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-to-list 'auto-mode-alist '("\\.rake\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Guardfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.cap\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.rabl\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.jbuilder\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Podfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.podspec\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Puppetfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Berksfile\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("Appraisals\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(use-package projectile-rails
  :straight t
  :hook
  (after-init . projectile-rails-global-mode)
  (after-init . inf-ruby-switch-setup)
  :config
  (setq projectile-rails-spring-command "~/Src/buk-webapp/bin/spring"
	projectile-rails-vanilla-command "~/Src/buk-webapp/bin/rails")
  (define-key projectile-rails-mode-map (kbd "C-c r") 'projectile-rails-command-map))

(use-package rubocop
  :straight t
  :defer t
  :config
  (setq rubocop-check-command "~/Src/buk-webapp/bin/rubocop --format emacs")
  (setq rubocop-format-command "~/Src/buk-webapp/bin/rubocop --format emacs")
  (setq rubocop-autocorrect-command "~/Src/Buk/buk-webapp/bin/rubocop --format emacs -a")
  ;; (setq rubocop-autocorrect-on-save t)
  :hook
  (ruby-mode . rubocop-mode))

(use-package inf-ruby
  :straight t
  :defer t
  :after ruby-mode)

(use-package minitest
  :straight t
  :defer t
  :config
  (setq minitest-use-rails t)
  (setq compilation-scroll-output t)
  :hook
  (ruby-mode . minitest-mode))

;;;;;;;;;;;;;;;;
;; Yassnippet ;;
;;;;;;;;;;;;;;;;
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :after ivy
  :defer t
  :config
  (setq yas-indent-line 'auto)
  (setq yas-also-auto-indent-first-line t)
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
  (setq org-startup-with-inline-images t)
  (setq org-log-done t)
  (setq org-image-actual-width 600)
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
   (shell . t)
   ))

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

(use-package org-roam
  :straight t
  :init
  (setq org-roam-v2-ack t)
  :custom
  (org-roam-directory "~/RoamNotes")
  :bind
  (
   ("C-c n l" . org-roam-buffer-toggle)
   ("C-c n f" . org-roam-node-find)
   ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-setup))

(use-package olivetti
  :straight t
  :config
  (setq olivetti-set-width 94)
  (setq olivetti-body-width 80)
  :hook
  (text-mode . olivetti-mode))

;;;;;;;;;;;;;;;;;;;;;
;; Gopher explorer ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package elpher
  :straight t)
(put 'set-goal-column 'disabled nil)

;; Diff two regions
;; Step 1: Select a region and `M-x diff-region-tag-selected-as-a'
;; Step 2: Select another region and `M-x diff-region-compare-with-b'
;; Press "q" in evil-mode or "C-c C-c" to exit the diff output buffer
(defun diff-region-format-region-boundary (b e)
  "Make sure lines are selected and B is less than E"
  (let (tmp rlt)
    ;; swap b e, make sure b < e
    (when (> b e)
      (setq tmp b)
      (setq b e)
      (set e tmp))

    ;; select lines
    (save-excursion
      ;; Another workaround for evil-visual-line bug:
      ;; In evil-mode, if we use hotkey V or `M-x evil-visual-line` to select line,
      ;; the (line-beginning-position) of the line which is after the last selected
      ;; line is always (region-end)! Don't know why.
      (if (and (> e b)
               (save-excursion (goto-char e) (= e (line-beginning-position)))
               (boundp 'evil-state) (eq evil-state 'visual))
          (setq e (1- e)))
      (goto-char b)
      (setq b (line-beginning-position))
      (goto-char e)
      (setq e (line-end-position)))
    (setq rlt (list b e))
    rlt))

(defun diff-region-exit ()
  (interactive)
  (bury-buffer "*Diff-region-output*")
  (winner-undo))

(defun diff-region-tag-selected-as-a ()
  "Select a region to compare"
  (interactive)
  (when (region-active-p)
    (let (tmp buf)
      ;; select lines
      (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
      (setq buf (get-buffer-create "*Diff-regionA*"))
      (save-current-buffer
        (set-buffer buf)
        (erase-buffer))
      (append-to-buffer buf (car tmp) (cadr tmp))))
  (message "Now select other region to compare and run `diff-region-compare-with-b`"))

(defun diff-region-compare-with-b ()
  "Compare current region with region selected by `diff-region-tag-selected-as-a' "
  (interactive)
  (if (region-active-p)
      (let (rlt-buf
            diff-output
            (fa (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory))))
            (fb (make-temp-file (expand-file-name "scor"
                                                  (or small-temporary-file-directory
                                                      temporary-file-directory)))))
        ;;  save current content as file B
        (when fb
          (setq tmp (diff-region-format-region-boundary (region-beginning) (region-end)))
          (write-region (car tmp) (cadr tmp) fb))

        (setq rlt-buf (get-buffer-create "*Diff-region-output*"))
        (when (and fa (file-exists-p fa) fb (file-exists-p fb))
          ;; save region A as file A
          (save-current-buffer
            (set-buffer (get-buffer-create "*Diff-regionA*"))
            (write-region (point-min) (point-max) fa))
          ;; diff NOW!
          (setq diff-output (shell-command-to-string (format "diff -Nabur %s %s" fa fb)))
          ;; show the diff output
          (if (string= diff-output "")
              ;; two regions are same
              (message "Two regions are SAME!")
            ;; show the diff
            (save-current-buffer
              (switch-to-buffer-other-window rlt-buf)
              (set-buffer rlt-buf)
              (erase-buffer)
              (insert diff-output)
              (diff-mode)
              (if (fboundp 'evil-local-set-key)
                           (evil-local-set-key 'normal "q" 'diff-region-exit))
              (local-set-key (kbd "C-c C-c") 'diff-region-exit)
              )))

        ;; clean the temporary files
        (if (and fa (file-exists-p fa))
            (delete-file fa))
        (if (and fb (file-exists-p fb))
            (delete-file fb)))
    (message "Please select region at first!")))

(add-hook 'sql-interactive-mode-hook
          (lambda ()
            (toggle-truncate-lines t)))

;; Make garbage collector pauses faster
(setq gc-const-threshold (* 2 1000 1000))

;;;;;;;;;;;;;;;
;; PDF-tools ;;
;;;;;;;;;;;;;;;
(defun my-nov-font-setup ()
  "Font for epubs."
  (face-remap-add-relative 'variable-pitch
			   :family "Liberation Serif"
			   :height 1.0))

(use-package nov
  :straight t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode . my-nov-font-setup)
  (doc-view-mode . (lambda () (display-line-numbers-mode -1))))

(use-package pdf-tools
  :straight t
  :defer
  :config
  (add-hook 'pdf-view-mode-hook (lambda() (display-line-numbers-mode -1)))
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (line-number-mode -1))

;; We need to save the last viewed page in a pdf file
(use-package pdf-view-restore
  :straight t
  :after pdf-tools
  :hook
  (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))


;;;;;;;;;;;;;;;;;;;;;
;; AUCTeX Settings ;;
;;;;;;;;;;;;;;;;;;;;;
(use-package tex
  :straight auctex
  :defer t
  :config
  (setq-default TeX-master nil)
  (setq TeX-auto-save t
	TeX-parse-self t
	Tex-PDF-mode t
	reftex-plug-into-AUCTEX t
	TeX-source-correlate-method 'synctex
	TeX-source-correlate-start-server t
	preview-gs-command "PDF Tools")
  :hook
  (doc-view-mode . auto-revert-mode)
  (LaTeX-mode . visual-line-mode)
  (LaTeX-mode . flyspell-mode)
  (LaTeX-mode . LaTeX-math-mode)
  (LaTeX-mode . turn-on-reftex)
  (TeX-after-compilation-finished-functions .TeX-revert-document-buffer)
  :config
  (TeX-source-correlate-mode t)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-command-list
               '("Index" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                 TeX-run-index nil t
                 :help "Run makeindex to create index file")))

(use-package company-auctex
  :straight t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Anotaciones la margen ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package marginalia
  :straight t
  :bind
  (("M-A" . marginalia-cycle)
   :map minibuffer-local-map
   ("M-A" . marginalia-cycle))
  :init
  (marginalia-mode))

;;;;;;;;;;;
;; slime ;;
;;;;;;;;;;;
(use-package slime
  :straight t
  :init
  (setq slime-contribs                 '(slime-fancy)
	slime-complete-symbol-function 'slime-fuzzy-complete-symbol
	slime-net-coding-system        'utf-8-unix)
  :config
  (setq inferior-lisp-program "sbcl"))

(load (expand-file-name "~/.roswell/helper.el"))

;;;;;;;;;;;;;;;;;
;; Tree Sitter ;;
;;;;;;;;;;;;;;;;;
(setq treesit-language-source-alist
   '((bash "https://github.com/tree-sitter/tree-sitter-bash")
     (cmake "https://github.com/uyha/tree-sitter-cmake")
     (css "https://github.com/tree-sitter/tree-sitter-css")
     (elisp "https://github.com/Wilfred/tree-sitter-elisp")
     (go "https://github.com/tree-sitter/tree-sitter-go")
     (html "https://github.com/tree-sitter/tree-sitter-html")
     (haskell "https://github.com/tree-sitter/haskell-tree-sitter" "master" "tree-sitter-haskell")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

(setq major-mode-remap-alist
 '((yaml-mode . yaml-ts-mode)
   (bash-mode . bash-ts-mode)
   (js2-mode . js-ts-mode)
   (typescript-mode . typescript-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (python-mode . python-ts-mode)
   (ruby-mode . ruby-ts-mode)
   (json-mode . json-ts-mode)
   (css-mode . css-ts-mode)
   (html-mode . html-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;
;; Ruby development ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package exec-path-from-shell
  :straight t
  :init
  (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path "~/.nodenv/shims//npx")
(add-to-list 'exec-path "~/.nodenv/shims//yarn")

;; rbenv
(use-package rbenv
  :straight t
  :init
  (global-rbenv-mode))

;;;;;;;;;;;;;;;;;;
;; Music player ;;
;;;;;;;;;;;;;;;;;;
(use-package simple-mpc
  :straight t)

;;;;;;;;;;;;;;;;;
;; Environment ;;
;;;;;;;;;;;;;;;;;
(use-package direnv
  :straight t
  :config
  (direnv-mode))

;;;;;;;;;;;;;;;;;
;; Vue Support ;;
;;;;;;;;;;;;;;;;;
(use-package vue-mode
  :straight t
  :mode "\\.vue\\'"
  :hook
  (vue-mode . prettier-js-mode)
  (vue-mode . web-mode)
  :config
  (setq prettier-js-args '("--parser vue")))

;;;;;;;;;;;;;;;;;
;; LSP support ;;
;;;;;;;;;;;;;;;;;
(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-auto-gess-mode t)
  (setq lsp-solargraph-symbols nil)
  (setq lsp-solargraph-folding nil)
  :config
  (setq lsp-ui-sideline-show-code-actions t)
  :hook ((python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
         (lsp-mode . lsp-enable-which-key-integration))
  :commands (lsp lsp-deferred))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-ivy
  :straight t
  :commands lsp-ivy-workspace-symbol)

(use-package lsp-treemacs
  :straight t
  :commands lsp-treemacs-errors-list)

(use-package company-lsp
  :straight t
  :commands company-lsp)

;;;;;;;;;;;;;;;;;
;; DAP support ;;
;;;;;;;;;;;;;;;;;
(straight-use-package 'dap-mode)
(use-package dap-mode
  ;; Uncomment the config below if you want all UI panes to be hidden by default!
  ;; :custom
  ;; (lsp-enable-dap-auto-configure nil)
  ;; :config
  ;; (dap-ui-mode 1)
  :commands dap-debug
  :config
  ;; Set up Node debugging
  (require 'dap-node)
  (setq dap-netcore-install-dir (executable-find "netcoredbg"))
  (dap-node-setup) ;; Automatically installs Node debug adapter if needed
  (require 'dap-hydra)
  (require 'dap-gdb-lldb)
  (dap-gdb-lldb-setup)

  ;; Bind `C-c l d` to `dap-hydra` for easy access
  (general-define-key
    :keymaps 'lsp-mode-map
    :prefix lsp-keymap-prefix
    "d" '(dap-hydra t :wk "debugger")))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Process monitoring ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package proced
  :straight t
  :defer t
  :custom
  (proced-auto-update-flag t))

;;;;;;;;;;;;;;;;;
;; Tree sitter ;;
;;;;;;;;;;;;;;;;;
(use-package tree-sitter
  :straight
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter
  :config
  (global-tree-sitter-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A very nice Dired cenfig ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dirvish
  :straight t
  :init
  ;; Let Dirvish take over Dired globally
  (dirvish-override-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Link preview for org-mode ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package link-preview
  :straight (link-preview :host github :repo "aviaviavi/link-preview.el"))

;;;;;;;;;
;; EAF ;;
;;;;;;;;;
(use-package eaf
  :straight (eaf
	     :type git
	     :host github
	     :repo "emacs-eaf/emacs-application-framework"
             :files ("*.el" "*.py" "core" "app" "*.json")
	     :includes (eaf-browser)
             :pre-build (("python3" "install-eaf.py" "--install" "browser" "--ignore-sys-deps")))
  :config
  (defalias 'browse-web #'eaf-open-browser)) ;; unbind, see more in the Wiki

(use-package eaf-browser
  :custom
  (eaf-browser-continue-where-left-off t)
  (eaf-browser-enable-adblocker t)
  (browse-url-browser-function 'eaf-open-browser))

;;;;;;;;;;;;;;
;; Chat GPT ;;
;;;;;;;;;;;;;;
(use-package chatgpt
  :straight (chatgpt
	     :type git
	     :host github
	     :repo "joshcho/ChatGPT.el"
	     :files ("dist" "*.el"))
  :init
  (require 'python)
  (setq chatgpt-python-interpreter "/usr/bin/python3")
  (setq chatgpt-repo-path "~/.emacs.d/straight/repos/ChatGPT.el/")
  :bind ("C-c q" . chatgpt-query))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(treesit-font-lock-level 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
