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

;;;;;;;;;;;;;;;;;;;;;;
;; User information ;;
;;;;;;;;;;;;;;;;;;;;;;
(setq user-full-name "Juan Alberto Camacho Bolaños")
(setq user-mail-address "juancamacho@ciencias.unam.mx")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuration file shortcuts ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun config-file ()
  "This function opens the principal configuration file."
  (find-file (expand-file-name "init.el"
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
(set-frame-font "Cascadia Mono 11" nil t)
(add-hook 'write-file-functions
	  (lambda() (delete-trailing-whitespace) nil))

;; GPG settings
(setq epg-gpg-program "gpg2")

;; Authinfo
(setq auth-sources
      '((:source "~/.authinfo.gpg")))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better Window navigation ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package winum
  :ensure t
  :config
  (winum-mode))

(use-package highlight-indent-guides
  :ensure t
  :defer t
  :diminish
  :config
  (setq highlight-indent-guides-character ?\| )
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
	("C-x t 1" . treemacs-delete-other-window)
	("C-x t t" . treemacs)
	("C-x t n" . treemacs-add-project-to-workspace)
	("C-x t B" . treemacs-bookmark)
	("C-x t C-t" . treemacs-find-file)
	("C-x t M-t" . treemacs-find-tag)))

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

;;;;;;;;;;;;;;;;;;;;
;; Magit settings ;;
;;;;;;;;;;;;;;;;;;;;

(use-package magit
  :ensure t
  :defer t
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
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
       :user user))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Better search engine ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;
(use-package ivy
  :ensure t
  :defer t
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
  :ensure t
  :defer t
  :bind (("C-s" . swiper)
	 ("C-r" . swiper)))

(use-package projectile
  :ensure t
  :after ivy
  :defer t
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
  ("C-." . counsel-projectile))

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
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents . 5)
			  (projects . 5)
			  (agenda . 5)))
  :hook
  (dashboard-mode . (lambda () (linum-mode -1)))
  :config
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
            "Settings" "Open settings file"
            (lambda (&rest _) (config-file)))
           (,(when (display-graphic-p)
               (all-the-icons-material "update" :height 1.35 :v-adjust -0.24))
            "Update" "Update Emacs Configuration to the latest version"
            (lambda (&rest _) (update-config))))))
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
  :after all-the-icons ivy
  :defer t
  :custom (all-the-icons-ivy-buffer-commands '(ivy-switch-buffer-other-window))
  :config
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-dired-jump)
  (add-to-list 'all-the-icons-ivy-file-commands 'counsel-find-library)
  (all-the-icons-ivy-setup))

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
  (setq doom-modeline-github nil)
  (setq doom-modeline-github-interval (* 30 60))
  (setq doom-modeline-env-version t)
  (setq doom-modeline-mu4e t)
  :hook
  (after-init . doom-modeline-mode))

;;;;;;;;;;;;;;;;;;;;;;;
;; Org Mode Settings ;;
;;;;;;;;;;;;;;;;;;;;;;;
(if (not (file-directory-p (expand-file-name "org" (getenv "HOME"))))
    (mkdir (expand-file-name "org" (getenv "HOME"))))

(use-package org
  :ensure t
  :config
  (setq org-startup-indented t)
  (setq org-directory "~/org")
  (setq org-startup-folded "showall")
  (setq org-ditaa-jar-path (expand-file-name "ditaa"
					   (file-name-as-directory "/usr/bin")))
  (setq org-plantuml-jar-path (expand-file-name "plantuml.jar"
					      (file-name-as-directory "/usr/share/plantuml")))
  (setq org-todo-keywords
	'((sequence "REPORT(r)" "BUG(b)" "KNOWCAUSE(k)" "|" "FIXED(f)")
          (sequence "TODO(t)" "|" "DOING(d)" "DONE" "CANCELED")))
  (setq org-todo-keyword-faces
	'(("TODO" . (:foreground "cyan" :weight bold))
          ("BUG" . (:foreground "purple" :weight bold))
          ("REPORT" . (:foreground "white" :weight bold))
          ("FIXED" . (:foreground "green" :weight bold))
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
   (restclient . t)
   (sass . t)
   (sql . t)
   (sqlite . t)
   (shell . t)))

;;;;;;;;;;;;;;;;;;;;
;; Eshell goodies ;;
;;;;;;;;;;;;;;;;;;;;

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
