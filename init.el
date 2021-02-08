;;; package -- Summary
;;; Commentary:
;;; Package configuration.
;;; Code:
(setq comp-deferred-compilation t)
(setq load-prefer-newer t)
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

;;;;;;;;;;;;;;;;;;;;;;
;; User information ;;
;;;;;;;;;;;;;;;;;;;;;;
(add-to-list 'load-path "./personal-config.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Window Size Configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(if (display-graphic-p)
    (progn
      (setq initial-frame-alist
	    '(
	      (width . 100)
	      (height . 80)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Eshell configuration ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(setq insert-directory-program (executable-find "gls"))
(add-to-list 'exec-path "/usr/local/opt/libpq/bin:/usr/local/bin:/opt/local/bin/:/usr/local/sbin:/Library/Apple/usr/bin:/Library/TeX/texbin:")
(setenv "PATH"
        (concat "/usr/local/opt/libpq/bin:/usr/local/bin:/opt/local/bin/:/usr/local/sbin:/Library/Apple/usr/bin:/Library/TeX/texbin:"
                (getenv "PATH")))
(eval-after-load 'esh-util
  '(progn
     (setq eshell-path-env (concat "/usr/local/opt/libpq/bin:/usr/local/bin:/opt/local/bin/:/usr/local/sbin:/Library/Apple/usr/bin:/Library/TeX/texbin:" eshell-path-env)
           )))

(use-package exec-path-from-shell
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration file shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-file ()
  "This function opens the principal configuration file."
  (find-file (expand-file-name "init.el"
			       user-emacs-directory)))

(defun personal-file ()
  "This function opens the principal configuration file."
  (find-file (expand-file-name "personal-config.el"
			       user-emacs-directory)))

(global-set-key (kbd "<f6>") (lambda () (interactive) (config-file)))
(global-set-key (kbd "<f5>") 'emacs-lisp-byte-compile-and-load)

;; General settings
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(setq-default linum-highlight-current-line t)
(if (eq system-type 'darwin) (setq mac-option-key-is-meta nil))
(if (eq system-type 'darwin) (setq mac-command-key-is-meta t))
(if (eq system-type 'darwin) (setq mac-command-modifier 'meta))
(if (eq system-type 'darwin) (setq mac-option-modifier nil))
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))
(setq tramp-default-method "ssh")

;; GPG settings
(if (eq system-type 'darwin) (setq epg-gpg-program "/usr/local/bin/gpg"))

;; Authinfo
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

;;;;;;;;;;;;;;;;;;
;; Text styling ;;
;;;;;;;;;;;;;;;;;;
(setq custom-tab-width 2)
(use-package whitespace
  :ensure t
  :defer t
  :config
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
        (?\{ . ?\})
	(?\[ . ?\])))
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

(use-package yafolding
  :ensure t
  :defer t
  :hook
  (prog-mode . yafolding-mode)
  (elisp-mode . yafolding-mode)
  (javascript-mode . yafolding-mode)
  :config
  (define-key yafolding-mode-map (kbd "<C-S-space>") 'yafolding-toggle-all)
  (define-key yafolding-mode-map (kbd "<C-M-return>") 'yafolding-hide-parent-element)
  (define-key yafolding-mode-map (kbd "<C-return>") 'yafolding-toggle-element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better Window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package winum
  :ensure t
  :config
  (set-face-attribute 'winum-face nil :weight 'bold)
  (winum-mode))

(use-package highlight-indent-guides
  :ensure t
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
  (highlight-indent-guides-auto-enabled t)
  (highlight-indent-guides-responsive t)
  (highlight-indent-guides-method 'character))

(use-package treemacs
  :ensure t
  :defer t
  :hook
  (treemacs-mode . (lambda() (linum-mode -1)))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs (if treemacs-python-executable 3 0)
	  treemacs-deferred-git-apply-delay 0.5
	  treemacs-display-in-side-window t
	  treemacs-directory-name-transformer #'identity
	  treemacs-eldoc-display t
	  treemacs-file-event-delay 5000
	  treemacs-file-extension-regex treemacs-last-period-regex-value
	  treemacs-file-follow-delay 0.2
	  treemacs-file-name-transformer #'identity
	  treemacs-follow-after-init t
	  treemacs-git-command-pipe ""
	  treemacs-goto-tag-strategy 'refetch-index
	  treemacs-indentation 2
	  treemacs-indentation-string " "
	  treemacs-is-never-other-window nil
	  treemacs-max-git-entries 5000
	  treemacs-missing-project-action 'ask
	  treemacs-no-png-images nil
	  treemacs-no-delete-other-windows t
	  treemacs-project-follow-cleanup nil
	  treemacs-persist-file (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
	  treemacs-position 'left
	  treemacs-recenter-distance 0.1
	  treemacs-recenter-after-file-follow nil
	  treemacs-recenter-after-tag-follow nil
	  treemacs-recenter-after-project-jump 'always
	  treemacs-recenter-after-project-expand 'on-distance
	  treemacs-show-cursor nil
	  treemacs-show-hidden-files t
	  treemacs-silent-filewatch nil
	  treemacs-silent-refresh nil
	  treemacs-sorting 'alphabetic-asc
	  treemacs-space-between-root-nodes t
	  treemacs-tag-follow-cleanup t
	  treemacs-tag-follow-delay 1.5
	  treemacs-width 35)
    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
		 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
	("M-0" . treemacs-select-window)
	("C-x T 1" . treemacs-delete-other-window)
	("C-x T t" . treemacs)
	("C-x T n" . treemacs-add-project-to-workspace)
	("C-x T B" . treemacs-bookmark)
	("C-x T C-t" . treemacs-find-file)
	("C-x T M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :ensure t
  :defer t
  :bind
  (:map global-map
	("C-x t a" . treemacs-projectile))
  :after treemacs projectile)

(use-package treemacs-magit
  :ensure t
  :defer t
  :after treemacs magit)

;;;;;;;;;;;
;; Theme ;;
;;;;;;;;;;;
(use-package doom-themes
  :ensure t
  :custom-face
  (doom-modeline-bar ((t (:background "#6272a4"))))
  :config
  (load-theme 'doom-tomorrow-night t))

;;;;;;;;;;;;;;;;;;;;
;; Magit settings ;;
;;;;;;;;;;;;;;;;;;;;

(use-package diff-hl
  :ensure t
  :init
  (global-diff-hl-mode))

(use-package magit
  :ensure t
  :defer t
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  :init
  (setq vc-handled-backends (delq 'Git vc-handled-backends))
  :bind
  (("C-x g" . 'magit-status)
   ("C-x M-g" . 'magit-dispatch)))

(auth-source-forget-all-cached)
'(ediff-split-window-function (quote split-window-horizontally))
'(ediff-window-setup-function (quote ediff-setup-windows-plain))

(use-package forge
  :ensure t
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
       :user user)))
)

(use-package magit-gitflow
  :ensure t
  :defer t
  :config
  (setq magit-gitflow-popup-key "C-F")
  :hook
  (magit-mode-hook . 'turn-on-magit-gitflow)
  :commands (magit-gitflow))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better search engine ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy
  :ensure t
  :custom
  (ivy-use-virtual-buffers t)
  (ivy-count-format "(%d/%d)")
  :config
  (ivy-mode))

(use-package counsel
  :ensure t
  :after ivy
  :defer t
  :config (counsel-mode t))

(use-package swiper
  :ensure t
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
  :config
  (projectile-mode t))

(use-package counsel-projectile
  :ensure t
  :after ivy
  :defer t
  :bind
  ("C-." . counsel-projectile)
  ("C-c s g" . counsel-projectile-grep))

(use-package projectile-rails
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;;;
;; Snippet support ;;
;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p (expand-file-name "snippets" user-emacs-directory)))
    (mkdir (expand-file-name "snippets" user-emacs-directory)))

(use-package yasnippet
  :ensure t
  :diminish yas-minor-mode
  :after ivy
  :defer t
  :hook
  (after-init . yas-global-mode)
  (prog-mode . yas-minor-mode)
  :config
  (yas-load-directory (expand-file-name "snippets" user-emacs-directory))
  (yas-global-mode t))

(use-package react-snippets
  :ensure t
  :defer t
  :after yasnippet)

;;;;;;;;;;;;;;;;;;;;;;;;
;; Dashboard settings ;;
;;;;;;;;;;;;;;;;;;;;;;;;
(use-package dashboard
  :ensure t
  :functions (all-the-icons-faicon
	      all-the-icons-material
	      open-custom-file
	      persp-get-buffer-or-nil
	      persp-load-state-from-file
	      persp-switch-to-buffer
	      winner-undo
	      widget-forward)
  :init
  (setq dahboard-banner-logo-title "")
  (setq dashboard-startup-banner (expand-file-name
				  "st_ignucius.png"
				  user-emacs-directory))
  :hook
  (dashboard-mode . (lambda () (linum-mode -1)))
  :config
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)
			  (agenda . 5)))
  (setq dashboard-set-init-info t
	dashboard-set-file-icons t
	dashboard-set-heading-icons t
	dashboard-heading-icons '((recents . "file-text")
				  (bookmarks . "bookmark")
				  (agenda . "calendar")
				  (projects . "file-directory")
				  (registers . "database"))
	dashboard-set-navigator t
	dashboard-navigator-buttons
	`(((,(when (display-graphic-p)
               (all-the-icons-octicon "tools" :height 1.0 :v-adjust 0.0))
            "Settings" "Opens settings file"
            (lambda (&rest _) (config-file)))
           (,(when (display-graphic-p)
               (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            "Update" "Update Emacs Configuration to the latest version"
            (lambda (&rest _) (update-config)))
	   (,(when (display-graphic-p)
               (all-the-icons-material "info" :height 1.35 :v-adjust -0.24))
            "Personal File" "Opens the personal config file"
            (lambda (&rest _) (personal-file))))))
  (dashboard-setup-startup-hook))

(defun update-config ()
  "Pulls the latest configuration file from github."
  (interactive)
  (let ((dir (expand-file-name user-emacs-directory)))
    (if (file-exists-p dir)
	(progn
	  (message "Updating configuration ... ")
	  (cd dir)
	  (shell-command "git pull")
	  (message "Updating configuration ... done."))
      (message "\"%s\" doesn't exists." dir))))

;;;;;;;;;;;;;;;;;;;
;; Icons support ;;
;;;;;;;;;;;;;;;;;;;
(use-package all-the-icons :ensure t)

(use-package treemacs-icons-dired
  :ensure t
  :defer t
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package all-the-icons-ivy
  :ensure t
  :after (all-the-icons ivy)
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
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
  :ensure t
  :after dired
  :defer t
  :hook (dired-mode . all-the-icons-dired-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Modeline modifications ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package doom-modeline
  :custom
  (doom-modeline-buffer-file-name 'truncate-with-project)
  :ensure t
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
  :hook
  (after-init . doom-modeline-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; posframe settings ;;
;;;;;;;;;;;;;;;;;;;;;;;

(use-package ivy-posframe
  :ensure t
  :after ivy
  :config
  (setq ivy-posframe-display-functions-alist '((t . ivy-posframe-display-at-frame-top-center)))
  (setq ivy-posframe-parameters
	'((left-fringe . 20)
	  (right-fringe . 20)))
  (setq ivy-posframe-height-alist '((t . 15)))
  :hook
  (after-init . ivy-posframe-mode))

(use-package ivy-rich
  :ensure t
  :defer t
  :after ivy
  :config
  (setq ivy-rich--display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
	     (:width (lambda (x)
		       (ivy-rich-switch-buffer-shorten-path
			x
			(ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand)))
          counsel-M-x
          (:columns
           ((counsel-M-x-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((counsel-describe-function-transformer (:width 40))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((counsel-describe-variable-transformer (:width 40))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-recentf
          (:columns
           ((ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))))
        ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev))

;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;
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
  :ensure t
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
  :ensure t
  :after org-mode
  :defer t)

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
;;   (restclient . t)
   (sass . t)
   (sql . t)
   (sqlite . t)
   (shell . t)))

(use-package org-tree-slide
  :ensure t
  :after org-mode-abbrev-table
  :hook
  (org-mode . org-tree-slide-mode)
  :defer t)

(use-package org-bullets
  :ensure t
  :after org
  :hook
  (org-mode . org-bullets-mode)
  :config (setq org-bullets-bullet-list '("◉" "⁑" "⁂" "❖" "✮" "✱" "✸")))

;;;;;;;;;;;;;;;;;;;;;
;; Company support ;;
;;;;;;;;;;;;;;;;;;;;;
(global-ede-mode 1)
(semantic-mode 1)
;; (srecode-minor-mode 1)
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :custom
  (company-tooltip-align-annotations t)
  :config
  (add-to-list 'company-backends 'company-emoji)
  (setq company-idle-delay t)

  (use-package company-go
    :ensure t
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

;;;;;;;;;;;;;;;;;;;;;;
;; Flycheck support ;;
;;;;;;;;;;;;;;;;;;;;;;

(use-package flycheck
  :ensure t
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

;;;;;;;;;;;;;;;;;;;;;;
;; Document reading ;;
;;;;;;;;;;;;;;;;;;;;;;

(defun my-nov-font-setup ()
  "Font for epubs."
  (face-remap-add-relative 'variable-pitch
			   :family "Liberation Serif"
			   :height 1.0))

(use-package nov
  :ensure t
  :defer t
  :init
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  :hook
  (nov-mode . my-nov-font-setup)
  (doc-view-mode . (lambda () (linum-mode -1))))

(use-package pdf-tools
  :ensure t
  :defer
  :config
  (add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
  (pdf-loader-install)
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (line-number-mode -1))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Programming modes utilities				   ;;
;; 							   ;;
;; This section is big, but I'm going to put preference to ;;
;; javascript, json and yaml				   ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package tide
  :ensure t
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
   (typescript-mdoe . eldoc-mode)
   (js-mode . tide-setup)
   (js-mode . tide-hl-identifier-mode)
   (js-mode . eldoc-mode)
   (js-mode . tide-mode)))

(use-package prettier-js
  :ensure t
  :hook
  (js2-mode . prettier-js-mode)
  (web-mode . prettier-js-mode)
  (js-mode . prettier-js-mode)
  :config
  (setq prettier-js-args
	'("--trailing-comma" "none"
	  "--tab-width" "2"
	  "--use-tabs" "false"
	  "--bracket-spacing" "true"
	  "--single-quote" "true"
	  "--semi" "true"
	  "--jsx-single-quote" "true"
	  "--jsx-bracket-same-line" "true"
	  "--arrow-parens" "always")))

(use-package jest
  :ensure t
  :defer t
  :config
  (setq jest-executable "npm run test --")
  :bind
  ("C-S-t" . jest-file)
  ("C-S-f" . jest))

(use-package add-node-modules-path
  :ensure t)

(use-package rjsx-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))
;; End of javascript configuration

(use-package swift-mode
  :ensure t
  :defer t)

(use-package json-mode
  :ensure t
  :defer t)

(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'"
  :defer t)

(use-package python-pytest
  :ensure t
  :defer t)

(use-package nasm-mode
  :ensure t
  :defer t
  :hook
  (asm-mode . nasm-mode))

(use-package request
  :ensure t
  :defer t)

(use-package websocket
  :ensure t
  :defer t)

(use-package yaml-mode
  :ensure t
  :defer t)

(use-package php-mode
  :ensure t
  :defer t
  :mode
  ("\\.php\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.php$" . php-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Emacs screen recorder ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package camcorder
  :ensure t
  :defer t)

;;;;;;;;;;;;;;;;;;;
;; LaTeX Support ;;
;;;;;;;;;;;;;;;;;;;
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
  (TeX-after-compilation-finished-functions .TeX-revert-document-buffer)
  :config
  (TeX-source-correlate-mode t)
  (add-to-list 'TeX-view-program-selection
               '(output-pdf "PDF Tools"))
  (add-to-list 'TeX-command-list
               '("Index" "makeindex %s.nlo -s nomencl.ist -o %s.nls"
                 TeX-run-index nil t
                 :help "Run makeindex to create index file")))

(provide 'init)
;;; init.el ends here
