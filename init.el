;;; package --- Summary
;;; Commentary:
;;; This is my personal Emacs configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Straight.el bootsraping ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; code:
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
  (json-mode . rainbow-mode))

(use-package rainbow-delimiters
  :straight t
  :defer t
  :hook
  (prog-mode . rainbow-delimiters-mode)
  (org-mode . rainbow-delimiters-mode)
  (elisp-mode . rainbow-delimiters-mode)
  (yaml-mode . rainbow-delimiters-mode)
  (json-mode . rainbow-delimiters-mode))

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
  :hook
  (after-init. counsel-mode))

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
  (setq company-tooltip-minimum-weight 15
	company-idle-delay 0.1)
  :custom
  (company-tooltip-align-annotation t)
  :config
  (setq company-idle-delay t
	company-dabbrev-downcase nil)
  :hook
  (after-init . global-company-mode))

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
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :hook
  (after-init . projectile-mode))

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
  (setq vterm-kill-buffer-on-exit t
	vterm-always-compile-module t)
  :hook
  (vterm-mode . (lambda () (display-line-numbers-mode -1))))

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
  :config
  (counsel-projectile-mode 1))

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

(provide 'init.el)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming utilities ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package cl-lib
  :straight t)

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

(use-package clojure-mode
  :straight t
  :mode ("\\.clj\\'"))

(use-package dockerfile-mode
  :straight t)

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

(use-package rjsx-mode
  :straight t
  :defer t
  :config
  (add-to-list 'auto-mode-alist
	       '("components\\/.*\\.js\\'" . rjsx-mode)))

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
  (web-mode-hook . (lambda ()
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

(use-package ruby-mode
  :straight t
  :interpreter "ruby"
  :config
  (def my-ruby-mode ()
       "Stablish encoding."
       (custom-set-variables
	'(ruby-insert-encoding-magic-comment nil))
       (flycheck-mode t))
  :hook
  (ruby-mode . my-ruby-mode))
;;; init.el ends here
