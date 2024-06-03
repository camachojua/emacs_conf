;;; package --- Summary
;;; Commentary:
;;; This is my personal Emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Personal config variables ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'personal-config (expand-file-name "personal-config.el" user-emacs-directory))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el bootsraping ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code:
(setq package-enable-at-startup nil) ;;; Disable built-in emacs package manager
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name
        "straight/repos/straight.el/bootstrap.el"
        (or (bound-and-true-p straight-base-dir)
            user-emacs-directory)))
      (bootstrap-version 7))
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

;; Para copiar texto desde WSL hacia linux
(defun copy-selected-text (start end)
  "Copy-selected-text from wsl to windows, you need to mark a START position and an END one."
  (interactive "r")
  (if (use-region-p)
      (let ((text (buffer-substring-no-properties start end)))
        (shell-command (concat "echo '" text "' | clip.exe")))))

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

(use-package org
  :straight t)

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
(scroll-bar-mode -1)
(toggle-scroll-bar -1)
(global-display-line-numbers-mode t)
(setq-default linum-highlight-current-line t)
(global-hl-line-mode t)
(setq default-frame-alist '((font . "Cascadia Mono 11")))
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))
(setq-default fill-column 80)
(global-auto-revert-mode 1)
(setq global-auto-revert-non-file-buffers t)

;; Transparent background
(set-frame-parameter nil 'alpha-background 92)
(add-to-list 'default-frame-alist '(alpha-background . 92))

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
  :straight t
  :config (pinentry-start))

(defun kill-gpg-buffers ()
  "Kill GPG buffers after a period of innactivity."
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
      (message "%s .gpg buffers have been autosaved and killed." buffers-killed))))

(run-with-idle-timer 60 t 'kill-gpg-buffers)

;;;;;;;;;;;;;;;;;;
;; Text styling ;;
;;;;;;;;;;;;;;;;;;
:; No usar  tabs
(setq-default indent-tabs-mode nil)
(setq tab-always-indent 'complete)

;; Espaciado para shell scripts
(setq-default sh-basic-offset 2
	      sh-indentation 2)

;; Use 2 spaces for tab
(defun die-tabs ()
  "Die tabs use spaces instead of tabs."
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(use-package whitespace
  :straight t
  :defer t
  :config
  (setq whitespace-line-column 80
	whitespace-style '(face-lines-tail))
  :hook
  (org-mode . whitespace-mode)
  (prog-mode . whitespace-mode)
  (json-mode . whitespace-mode)
  (yaml-mode . whitespace-mode))

(setq electric-pair-pairs
      '((?\" . ?\")
	(?\{ . ?\})
	(?\[ . ?\])
	(?\( . ?\))))

(global-so-long-mode 1)

(use-package vlf
  :straight t)
(use-package rainbow-mode
  :straight t
  :defer t
  :hook
  (prog-mode . rainbow-mode)
  (org-mode . rainbow-mode)
  (elisp-mode . yafolding-mode)
  (yaml-mode . rainbow-mode)
  (json-mode . rainbow-mode)
  (ruby-mode . rainbow-made))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  (elisp-mode . rainbow-delimiters-mode)
  (yaml-mode . rainbow-delimiters-mode)
  (json-mode . rainbow-delimiters-mode)
  (ruby-mode . rainbow-delimiters-mode))

(use-package yafolding
  :straight t
  :defer t
  :hook
  (prog-mode . yafolding-mode)
  (yaml-mode . yafolding-mode)
  (json-mode . yafolding-mode)
  (elisp-mode . yafolding-mode))

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
  (elisp-mode . highlight-indent-guides-mode)
  (json-mode . highlight-indent-guides-mode)
  (ruby-mode . highligiht-indent-guides-mode)
  :custom
  (setq highlight-indent-guides-auto-enabled t
	highlight-indent-guides-responsive t
	highlight-indent-guides-method 'character))

(global-prettify-symbols-mode t)
(defun my-pretty-symbols ()
  "Make some words display as Unicode symbols."
  (setq prettify-symbols-alist
        '(
          ("lambda" . 955) ; λ
          ("->" . 8594)    ; →
          ("=>" . 8658)    ; ⇒
          ("map" . 8614)   ; ↦
          )
        ))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Frame default size ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(when window-system
  (set-frame-position (selected-frame) 340 200)
  (set-frame-size (selected-frame) 120 44))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer's windows navigations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  :hook
  (after-init . ivy-mode))

(use-package counsel
  :straight t
  :after ivy
  :defer t
  :bind
  (("M-x" . counsel-M-x))
  :hook
  (after-init. counsel-mode))

(use-package ivy-hydra
  :straight t)

(use-package swiper
  :straight t
  :after ivy
  :defer t
  :config
  (ivy-mode 1)
  :bind
  ("C-s" . swiper)
  ("M-s" . counsel-grep-or-swiper)
  ("C-c C-r" . ivi-resume)
  ("M-x" . counsel-M-x)
  ("C-x C-f" . counsel-find-file))

(use-package company
  :straight t
  :init
  (setq company-tooltip-minimum-weight 15
	company-idle-delay 0.1)
  :custom
  (company-tooltip-align-annotation t)
  :config
  (setq company-idle-delay t
	company-dabbrev-downcase nil)
  :hook
  (after-init . global-company-mode)
  (cider-mode . company-mode))

(use-package company-go
  :straight t
  :after company
  :defer t
  :config
  (add-to-list 'company-backends 'company-go))

(use-package projectile
  :straight t
  :after ivy
  :defer t
  :init
  (setq projectile-completion-system 'ivy)
  (setq projectile-project-search-path '("~/Src/"))
  :config
  (setq projectile-enable-caching t)
  (add-to-list 'projectile-globally-ignored-directories "/vendor")
  (add-to-list 'projectile-globally-ignored-directories "/node_modules")
  (add-to-list 'projectile-globally-ignored-directories "/log")
  (add-to-list 'projectile-globally-ignored-directories "/logs")
  (add-to-list 'projectile-globally-ignored-directories "/public")
  (add-to-list 'projectile-globally-ignored-directories "/.git")
  (add-to-list 'projectile-globally-ignored-directories "/.github")
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :hook
  (after-init . projectile-mode))

(use-package ag
  :straight t)

(use-package rg
  :straight t)

(use-package diff-hl
  :straight t
  :hook
  (after-init . global-diff-hl-mode))

(use-package hydra
  :straight t)

(use-package magit
  :straight t
  :defer t
  :config
  (setq ghub-use-workaround-for-emacs-bug t
	ghub-use-workaround-for-emacs-bug-54989 t
	magit-completing-read-function 'ivy-completing-read)
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

(use-package code-review
  :straight t
  :defer t
  :bind
  (:map forge-topic-mode-map
	("C-c C-r" . 'code-review-forge-pr-at-point))
  :config
  (setq code-review-fill-column 80
	code-review-auth-login-marker 'forge)
  :hook
  (code-review-hook . 'emojify-mode))

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

;; Ediff customizations
(setq ediff-split-window-function 'split-window-horizontally)
(setq ediff-window-setup-function 'ediff-setup-windows-plain)

(use-package blamer
  :straight t
  :defer t
  :bind
  ("s-i" . blamer-show-commit-info)
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 20)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
		   :background "#001111"
		   :height 100
		   :italic t)))
  :hook
  (after-init . global-blamer-mode))

(use-package treemacs
  :straight t
  :defer t
  :hook
  (treemacs-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (setq treemacs-follow-mode t
	treemacs-filewatch-mode t
	treemacs-fringe-indicator-mode t))

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
  (comint-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (dz-defservice buk-backend "./bin/rails"
		 :args ("s")
		 :cd buk-webapp-dir)
  (dz-defservice buk-frontend "./bin/webpack-dev-server"
		 :cd buk-webapp-dir)
  (dz-defservice-group buk (buk-backend buk-frontend)))

;;;;;;;;;;;;;;;;;;;;;;
;; Terminal support ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package vterm
  :straight t
  :defer t
  :config
  (setq vterm-kill-buffer-on-exit t
	vterm-always-compile-module t)
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode -1)))
  :bind
  (:map vterm-mode-map
        ("C-y" . vterm-yank)
        ("C-q" . vterm-send-next-key)))

(use-package multi-vterm
  :straight t
  :defer)

(use-package vterm-toggle
  :straight t
  :bind
  ("C-c t" . vterm-toggle))

;;;;;;;;;;;;;;;;;;;;;;;;
;; Postframe settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons
  :straight t)

(use-package all-the-icons-ivy
  :straight t
  :after (all-the-icons ivy)
  :config
  (add-to-list 'all-the-icons-ivy-file-commands
	       '(counsel-find-file
		 counsel-file-jump
		 counsel-recentf
		 counsel-projectile-find-file
		 counsel-projectile-find-dir))
  (all-the-icons-ivy-setup)
  (ivy-mode 1)
  :hook
  (after-init . all-the-icons-ivy-setup))

(use-package all-the-icons-dired
  :straight t
  :after dired
  :defer t
  :hook
  (dired-mode . all-the-icons-dired-mode))

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
  (after-init . ivy-posframe-mode)
  (prog-mode . ivy-posframe-mode))

(use-package counsel-projectile
  :straight t
  :after projectile
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :config
  (counsel-projectile-mode 1))

(use-package fzf
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck support ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package flycheck
  :straight t
  :hook
  (prog-mode . flycheck-mode)
  (org-mode . flycheck-mode)
  :config
  (setq flycheck-javascript-eslint-executable "/usr/bin/eslint"
	flycheck-check-syntax-automatically '(save mode-enabled))
  :custom
  (flycheck-emacs-lisp-load-path 'inherit))

;;;;;;;;;;;;
;; Themes ;;
;;;;;;;;;;;;
(load-theme 'modus-operandi t)
(use-package emojify)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package which-key
  :straight t
  :hook
  (after-init . which-key-mode))

(use-package js2-mode
  :straight t
  :defer t
  :interpreter
  (("node" . js2-mode))
  :config
  (setq js2-basic-offset 2
	js2-highlight-level 4)
  (setq js2-global-externs
	'("module" "require" "buster" "sinon" "assert" "refute" "setTimeout"
	  "clearTimeout" "setInterval" "clearInterval" "location" "__dirname"
	  "console" "JSON" "PTL" "$" "exports" "resolve" "reject" "process"
	  "localStorage" "DOMPurify"))
  :mode ("\\.js\\'" "\\.mjs\\'"))

(use-package web-beautify
  :straight t
  :defer t
  :bind-keymap
  ("C-c C-b" . js2-mode-map))

(use-package dockerfile-mode
  :straight t)

(use-package docker
  :straight t
  :bind
  ("C-c d" . docker))

(use-package tide
  :straight t
  :defer t
  :bind
  (("C-c r" . 'tide-rename-symbol)
   ("C-c f" . 'tide-refactor)
   ("C-c h" . 'tide-documentation-at-point))
  :hook
  ((typescript-ts-mode . tide-setup)
   (typescript-ts-mode . tide-mode)
   (typescript-ts-mode . tide-hl-identifier-mode)
   (typescript-ts-mode . eldoc-mode)
   (js-mode . tide-setup)
   (js-mode . tide-hl-identifier-mode)
   (js-mode . eldoc-mode)
   (js-mode . tide-mode)))

(use-package subword
  :straight t
  :hook
  ((js-mode . subword-mode)
  (html-mode . subword-mode)
  (coffee-mode . subword-mode)))

(setq-default js-indent-level 2)

(use-package rjsx-mode
  :straight t
  :defer t
  :config
  (add-to-list 'auto-mode-alist
	       '("components\\/.*\\.js\\'" . rjsx-mode)))

(use-package vue-mode
  :straight t)

(use-package vue-ts-mode
  :straight (vue-ts-mode
	     :host github
	     :repo "8uff3r/vue-ts-mode")
  :hook (vue-ts-mode . vue-mode))

(use-package gcmh
  :straight (gcmh
	     :host github
	     :repo "emacsmirror/gcmh")
  :config
  (setq garbage-collection-messages t)
  (gcmh-mode 1))

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
  :hook
  (web-mode . (lambda ()
		(setq-local electric-pair-inhibit-predicate
			    (lambda (c)
			      (if (char-equal c ?{)
				  t
				(electric-pair-default-inhibit c))))))
  :config
  (setq web-mode-code-indent-offset 2
	web-mode-css-indent-offset 2
	web-mode-markup-indent-offset 2
	web-mode-markup-indent-offset 2
	web-mode-enable-auto-closing t
	web-mode-enable-current-element-highlighting t
	web-mode-enable-current-column-highlighting t))

(use-package php-mode
  :straight t
  :mode
  ("\\.php\\'" . php-mode))

(use-package phpunit
 :straight t)

(provide 'lang-php)

;;;;;;;;;;;;;;;;;;;;;;
;; Ruby development ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package ruby-mode
  :straight t
  :interpreter "ruby"
  :defer t
  :config
  (defun my-ruby-mode ()
       "Stablish encoding."
       (custom-set-variables
	'(ruby-insert-encoding-magic-comment nil))
       (flycheck-mode t))
  :mode
  ("\\.rb\\'" . ruby-mode)
  ("\\.erb\\'" . ruby-mode)
  ("\\.pryrc\\'" . ruby-mode)
  ("\\.rake\\'" . ruby-mode)
  ("Rakefile" . ruby-mode)
  ("\\.gemspec\\'" . ruby-mode)
  ("\\.ru\\'" . ruby-mode)
  ("Gemfile" . ruby-mode)
  ("Guardfile" . ruby-mode)
  ("Capfile\\'" . ruby-mode)
  ("\\.cap\\'" . ruby-mode)
  ("\\.thor\\'" . ruby-mode)
  ("\\.rabl\\'" . ruby-mode)
  ("Thorfile\\'" . ruby-mode)
  ("Vagrantfile\\'" . ruby-mode)
  ("\\.jbuilder\\'" . ruby-mode)
  ("Podfile\\'" . ruby-mode)
  ("\\.podspec\\'" . ruby-mode)
  ("Puppetfile\\'" . ruby-mode)
  ("Berksfile\\'" . ruby-mode)
  ("Appraisals\\'" . ruby-mode)
  :hook
  (ruby-ts-mode . ruby-mode)
  (ruby-ts-mode . my-ruby-mode))

(use-package ruby-end
  :straight t
  :config
  (setq ruby-end-mode t)
  :hook
  (ruby-ts-mode . ruby-end-mode)
  (ruby-mode . ruby-end-mode))

(use-package robe
  :straight t
  :defer t
  :init
  (setq inf-ruby-console-rails buk-webapp-dir)
  :config
  (robe-start)
  :hook
  (ruby-mode . robe-mode)
  (ruby-ts-mode . robe-mode))

(eval-after-load 'company
  '(push 'company-robe company-backends))

(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

(use-package projectile-rails
  :straight t
  :hook
  (after-init . projectile-rails-global-mode)
  (after-init . inf-ruby-switch-setup)
  :config
  (setq projectile-rails-spring-command (expand-file-name "bin/spring" buk-webapp-dir)
	projectile-rails-vanilla-command (expand-file-name "bin/rails" buk-webapp-dir))
  (projectile-rails-global-mode)
  :bind-keymap
  ("C-c r" . projectile-rails-command-map))

(use-package rubocop
  :straight t
  :defer t
  :config
  (setq rubocop-check-command "~/.rbenv/shims/rubocop --lint --format emacs"
	rubocop-format-command "~/.rbenv/shims/rubocop --format emacs"
	rubocop-autocorrect-command "~/.rbenv/shims/rubocop -A --format emacs")
  (setq rubocop-autocorrect-on-save t)
  :hook
  (ruby-ts-mode . rubocop-mode))

(use-package inf-ruby
  :straight t
  :after ruby-mode
  :hook
  (after-init . inf-ruby-switch-setup)
  (compilation-filter . inf-ruby-auto-enter-and-focus)
  (ruby-base-mode . inf-ruby-minor-mode)
  :config
  (setq show-trailing-whitespace nil)
  (setq inf-ruby-default-implementation "pry")
  (linum-mode 0)
  (inf-ruby-enable-auto-breakpoint))

(use-package minitest
  :straight t
  :defer t
  :config
  (setq minitest-use-rails t
	compilation-scroll-output t)
  :hook
  (ruby-mode . minitest-mode)
  (ruby-ts-mode . minitest-mode))

(use-package exec-path-from-shell
  :straight t
  :config
  (exec-path-from-shell-copy-env "PATH")
  (exec-path-from-shell-copy-env "GEM_PATH")
  :init
  (exec-path-from-shell-initialize))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

(when (daemonp)
  (exec-path-from-shell-initialize))

(add-to-list 'exec-path "~/.nodenv/shims//npx")
(add-to-list 'exec-path "~/.nodenv/shims//yarn")
(add-to-list 'exec-path "~/.rbenv/shims")

(use-package rbenv
  :straight t
  :init
  (global-rbenv-mode))

(use-package bundler
  :straight (bundler
	     :type git
	     :host github
	     :repo "endofunky/bundler.el"))

(use-package ruby-interpolation
  :straight t
  :hook
  (ruby-ts-mode . ruby-interpolation-mode))

(use-package yaml-mode
  :straight t
  :hook
  (yaml-mode . yaml-ts-mode))

(use-package direnv
  :straight t
  :config
  (direnv-mode 1)
  :hook
  (after-init . direnv-mode))

;;;;;;;;;;;;;;;;
;; Yassnippet ;;
;;;;;;;;;;;;;;;;
(use-package yasnippet
  :straight t
  :diminish yas-minor-mode
  :after ivy
  :defer t
  :config
  (setq yas-indent-line 'auto
	yas-also-auto-indent-first-line t)
  :hook
  (after-init . yas-global-mode)
  (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :straight t)

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

(use-package doc-view
  :straight t
  :custom
  (doc-view-resolution 300)
  (doc-view-mupdf-use-svg t)
  (large-file-warning-threshold (* 50 (expt 2 20))))

(use-package pdf-tools
  :straight t
  :defer t
  :hook
  (pdf-view-mode . (lambda () (display-line-numbers-mode -1)))
  :config
  (pdf-loader-install)
  (setq-default pdf-vie-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (line-number-mode -1))

(use-package pdf-view-restore
  :straight t
  :defer t
  :after pdf-tools
  :hook
  (pdf-view-mode . pdf-view-restore-mode)
  :config
  (setq pdf-view-restore-filename "~/.emacs.d/.pdf-view-restore"))

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
     ;; (haskell "https://github.com/tree-sitter/haskell-tree-sitter" "master" "tree-sitter-haskell")
     (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
     (json "https://github.com/tree-sitter/tree-sitter-json")
     (make "https://github.com/alemuller/tree-sitter-make")
     (markdown "https://github.com/ikatyang/tree-sitter-markdown")
     (python "https://github.com/tree-sitter/tree-sitter-python")
     (toml "https://github.com/tree-sitter/tree-sitter-toml")
     (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
     (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
     (ruby "https://github.com/tree-sitter/tree-sitter-ruby")
     (vue "https://github.com/ikatyang/tree-sitter-vue")
     (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

;; (mapc #'treesit-install-language-grammar (mapcar #'car treesit-language-source-alist))

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
   (elisp-mode . elisp-ts-mode)
   (html-mode . html-ts-mode)))

(use-package tree-sitter
  :straight t
  :init
  (setq treesit-font-lock-level 4)
  :config
  (global-tree-sitter-mode))

(use-package tree-sitter-langs
  :straight t
  :after tree-sitter)

;;;;;;;;;;;;;;;;;
;; LSP support ;;
;;;;;;;;;;;;;;;;;
(setenv "LSP_USE_PLISTS" "true")

(use-package lsp-mode
  :straight t
  :init
  (setq lsp-keymap-prefix "C-c l")
  (setq lsp-auto-gess-mode t)
  (setq lsp-solargraph-symbols nil)
  (setq lsp-solargraph-folding nil)
  (setq company-minimum-prefix-length 1)
  (setq company-idle-delay 0.0)
  :config
  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-file-watch-ignored-directories ["\\node_modules\\'"])
  (setq lsp-log-io nil)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-use-plists t)
  :hook ((python-mode . lsp-deferred)
         (go-mode . lsp-deferred)
         (rust-mode . lsp-deferred)
         (typescript-mode . lsp-deferred)
	 (js-ts-mode . lsp-deferred)
	 (php-mode . lsp)
         (lsp-mode . lsp-enable-which-key-integration)))

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
  :commands company-lsp
  :config
  (push 'company-lsp company-backends))

(with-eval-after-load 'eglot
 (add-to-list 'eglot-server-programs '((ruby-mode ruby-ts-mode) "ruby-lsp")))

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

(use-package walkman
  :config
  (setq walkman-keep-headers t)
  :straight (walkman
	     :type git
	     :host github
	     :repo "abrochard/walkman"))

(use-package ob-go
  :straight t)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((awk . t)
   (C . t)
   (clojure . t)
   (ditaa . t)
   (emacs-lisp . t)
   (gnuplot . t)
   (go . t)
   (haskell . t)
   (java . t)
   (julia . t)
   (js . t)
   (latex . t)
   (org . t)
   (plantuml . t)
   (python . t)
   (restclient . t)
   (http . t)
   (ruby . t)
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

;; Establece color de fondo para bloques de código de org-babel
(custom-set-faces
 '(org-block-begin-line
   ((t (:underline "#A7A6AA" :foreground "#018ED1" :background "#EAEAF0" :extend t))))
 '(org-block
   ((t (:background "#EFF0F1" :extend t))))
 '(org-block-end-line
   ((t (:overline "#A7A6AA" :foreground "#018ED1" :background "#EAEAF0" :extend t))))
 )

(use-package org-capture
  :config
  (setq org-capture-templates
	'(("n" "Plantilla para notas generales" entry
	   (file+headline "todo.org" "Tareas por hacer")
	   "* [%] %?por hacer")))
  :bind
  ("C-c c" . org-capture))

(use-package olivetti
  :straight t
  :config
  (setq olivetti-set-width 110)
  (setq olivetti-body-width 120)
  :hook
  (text-mode . olivetti-mode))

;;;;;;;;;;;;;;;;;;
;; Video player ;;
;;;;;;;;;;;;;;;;;;
(use-package empv
  :straight (empv
	     :host github
	     :repo "isamert/empv.el"))

;;;;;;;;;;;;
;; Eshell ;;
;;;;;;;;;;;;
(use-package eshell-prompt-extras
  :straight t
  :config
  (autoload 'epe-theme-lambda "eshell-prompt-extras")
  (setq eshell-highlight-prompt nil
	eshell-prompt-function 'epe-theme-lambda)
  :hook
  (eshell-mode . (lambda () (display-line-numbers-mode -1))))

(require 'em-term)
(add-to-list 'eshell-visual-commands "htop nano vim")
(setq eshell-destroy-buffer-when-process-dies t)
(ansi-color-for-comint-mode-on)
(defalias 'open' 'find-file-other-window)
(defalias 'clean' 'eshell/clear-scrollback)
(setq eshell-prompt-regexp "^[^αλ\n]*[αλ] ")

(defun eshell/sudo-open (filename)
  "Open a file (FILENAME) as root in Eshell."
  (let ((qual-filename (if (string-match "^/" filename)
                           filename
                         (concat (expand-file-name (eshell/pwd)) "/" filename))))
    (switch-to-buffer
     (find-file-noselect
      (concat "/sudo::" qual-filename)))))

(defun eshell-exec-visual (&rest args)
   "Run the specified PROGRAM in a terminal emulation buffer.
ARGS are passed to the program.  At the moment, no piping of input is
allowed."
   (let* (eshell-interpreter-alist
	   (original-args args)
	   (interp (eshell-find-interpreter (car args) (cdr args)))
	   (in-ssh-tramp (and (tramp-tramp-file-p default-directory)
			      (equal (tramp-file-name-method
				      (tramp-dissect-file-name default-directory))
				     "ssh")))
	   (program (if in-ssh-tramp
			"ssh"
		      (car interp)))
	   (args (if in-ssh-tramp
		     (let ((dir-name (tramp-dissect-file-name default-directory)))
		       (eshell-flatten-list
			(list
			 "-t"
			 (tramp-file-name-host dir-name)
			 (format
			  "export TERM=xterm-256color; cd %s; exec %s"
			  (tramp-file-name-localname dir-name)
			  (string-join
			   (append
			    (list (tramp-file-name-localname (tramp-dissect-file-name (car interp))))
			    (cdr args))
			   " ")))))
		   (eshell-flatten-list
		    (eshell-stringify-list (append (cdr interp)
						   (cdr args))))))
	   (term-buf
	    (generate-new-buffer
	     (concat "*"
		     (if in-ssh-tramp
			 (format "%s %s" default-directory (string-join original-args " "))
			 (file-name-nondirectory program))
		     "*")))
	   (eshell-buf (current-buffer)))
     (save-current-buffer
	(switch-to-buffer term-buf)
	(term-mode)
	(set (make-local-variable 'term-term-name) eshell-term-name)
	(make-local-variable 'eshell-parent-buffer)
	(setq eshell-parent-buffer eshell-buf)
	(term-exec term-buf program program nil args)
	(let ((proc (get-buffer-process term-buf)))
	  (if (and proc (eq 'run (process-status proc)))
	      (set-process-sentinel proc 'eshell-term-sentinel)
	    (error "Failed to invoke visual command")))
	(term-char-mode)
	(if eshell-escape-control-x
	    (term-set-escape-char ?\C-x))))
   nil)

(setq eshell-history-size 1000000)

;; cache file-name forever
(setq remote-file-name-inhibit-cache nil)

;; make sure vc stuff is not making tramp slower
(setq vc-ignore-dir-regexp
      (format "%s\\|%s"
	      vc-ignore-dir-regexp
	      tramp-file-name-regexp))

(setq tramp-verbose 1)
(setq projectile-mode-line "Projectile")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A cozy fireplace for emacs ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package fireplace :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; My own custom functions ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun show-file-name ()
  "Show the full path file name into the minibuffer."
  (interactive)
  (message (buffer-file-name)))

(defun copy-file-name ()
  "Copies buffer's full path to the kill ring."
  (interactive)
  (kill-new (file-truename buffer-file-name)))

(global-set-key (kbd "C-c v") 'copy-file-name)

;;;;;;;;;;;;;;;;;;;;;;
;; Teleport support ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package teleport
  :straight t
  :init
  (teleport-tramp-add-method)
  (add-to-list 'tramp-default-user-alist
	       '("\\`tsh\\'" nil "root"))
  :bind
  (:map teleport-list-nodes-mode-map
	("v" . vterm)
	("t" . term)
	("d" . dired)))

;; Agregamos la integración con vterm
(with-eval-after-load 'vterm
    (add-to-list 'vterm-tramp-shells `(,teleport-tramp-method "/bin/bash")))

;; Administración de buffers creados a partir del comando
;; teleport-list-nodes-mode--do-shell-command
(setq display-buffer-alist
      '(("^\\*Teleport Shell Command Output: .*" display-buffer-pop-up-frame)))

;;;;;;;;;;;;;;;;;;;;;;
;; Lisp development ;;
;;;;;;;;;;;;;;;;;;;;;;
(use-package cl-lib
  :straight t)

(use-package clojure-mode
  :straight t
  :mode
  ("\\.clj\\'" "\\.cljs.*$" "\\.boot$")
  :hook
  (subword-mode . clojure-mode))

(use-package cider
  :straight t
  :defer t
  :after clojure-mode
  :hook
  (clojure-mode . cider-mode)
  :config
  (defun cider-repl-unicorn (namespace)
    "Return a promp string that mentions NAMESPACE."
    (format "%s 🦄 " (cider-abbreviate-ns namespace)))
  (setq cider-repl-result-prefix ";; =>"
        cider-eval-result-prefix ""
        cider-repl-prompt-function #'cider-repl-unicorn
        cider-connection-message-fn nil ; we omit the giant message
        cider-repl-display-help-banner nil))

(use-package cider-hydra
  :straight t
  :hook
  (clojure-mode . cider-hydra-mode))

(use-package clj-refactor
  :straight t
  :hook
  (clojure-mode . clj-refactor-mode))

(use-package tagedit
  :straight t)

(use-package geiser-mit
  :straight t)

(use-package racket-mode
  :straight t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Revert buffer when file has changed ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package autorevert
  :straight t
  :diminish
  :hook
  (after-init . global-auto-revert-mode))

(provide 'init.el)
;;; init.el ends here
