;;; package -- summary
;;; Commentary: My own Emacs configuration file
(require 'package)
(setq package-enable-at-startup nil)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)
;; Defer packages
;; (setq use-package-always-defer t)

;; General settings
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(set-frame-font "FiraCode 8" nil t)
(add-hook 'local-write-file-hooks
	  (lambda() (delete-trailing-whitespace) nil))
(add-hook 'focus-out-hook #'garbage-collect)

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
                               (cpp . t)
                               (clojure . t)
                               (ditaa . t)
                               (emacs-lisp)
                               (gnuplot . t)
                               (haskell . t)
                               (java . t)
                               (js . t)
                               (latex . t)
                               (python . t)
                               (sass . t)
                               (sql . t)
                               (sqlite . t)
                               (shell . t)))

(setq org-ditaa-jar-path "/usr/bin/ditaa")

;; Don't ask for confirmation while evaluating a block
(setq org-confirm-babel-evaluate nil)

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
  "Open a file as root in Eshell."
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

(use-package autopair
  :ensure t
  :config
  (autopair-global-mode t))

(use-package rainbow-mode
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;; JSON support
(use-package json-mode
  :ensure t)

;; Dockerfile mode
(use-package dockerfile-mode
  :ensure t
  :mode "\\Dockerfile\\'")

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

;; Theme
(use-package chocolate-theme
  :ensure t
  :defer t
  :init
  (load-theme 'chocolate t))

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

(use-package ivy-rich
  :ensure t
  :defines (all-the-icons-icon-alist
            all-the-icons-dir-icon-alist
            bookmark-alist)
  :functions (all-the-icons-icon-for-file
              all-the-icons-icon-for-mode
              all-the-icons-icon-family
              all-the-icons-match-to-alist
              all-the-icons-faicon
              all-the-icons-octicon
              all-the-icons-dir-is-submodule)
  :preface
  (defun ivy-rich-bookmark-name (candidate)
    (car (assoc candidate bookmark-alist)))

  (defun ivy-rich-buffer-icon (candidate)
    "Display buffer icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((buffer (get-buffer candidate))
             (buffer-file-name (buffer-file-name buffer))
             (major-mode (buffer-local-value 'major-mode buffer))
             (icon (if
                       (and buffer-file-name
                            (all-the-icons-auto-mode-match?))
                       (all-the-icons-icon-for-file
                        (file-name-nondirectory buffer-file-name) :v-adjust -0.05)
                     (all-the-icons-icon-for-mode major-mode :v-adjust -0.05))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o"
                                  :face 'all-the-icons-dsilver
                                  :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-file-icon (candidate)
    "Display file icons in `ivy-rich'."
    (when (display-graphic-p)
      (let* ((path (file-local-name (concat ivy--directory candidate)))
             (file (file-name-nondirectory path))
             (icon (cond
                    ((file-directory-p path)
                     (cond
                      ((and (fboundp 'tramp-tramp-file-p)
                            (tramp-tramp-file-p default-directory))
                       (all-the-icons-octicon "file-directory"
                                              :height 1.0 :v-adjust 0.01))
                      ((file-symlink-p path)
                       (all-the-icons-octicon "file-symlink-directory"
                                              :height 1.0 :v-adjust 0.01))
                      ((all-the-icons-dir-is-submodule path)
                       (all-the-icons-octicon "file-submodule"
                                              :height 1.0 :v-adjust 0.01))
                      ((file-exists-p (format "%s/.git" path))
                       (all-the-icons-octicon "repo" :height 1.1 :v-adjust 0.01))
                      (t (let ((matcher
                                (all-the-icons-match-to-alist
                                 path all-the-icons-dir-icon-alist)))
                           (apply (car matcher) (list (cadr matcher)
                                                      :v-adjust 0.01))))))
                    ((string-match "^/.*:$" path)
                     (all-the-icons-material "settings_remote"
                                             :height 1.0 :v-adjust -0.2))
                    ((not (string-empty-p file))
                     (all-the-icons-icon-for-file file :v-adjust -0.05)))))
        (if (symbolp icon)
            (all-the-icons-faicon "file-o"
                                  :face 'all-the-icons-dsilver
                                  :height 0.8 :v-adjust 0.0)
          icon))))

  (defun ivy-rich-dir-icon (candidate)
    "Display directory icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "file-directory" :height 1.0 :v-adjust 0.01)))

  (defun ivy-rich-function-icon (_candidate)
    "Display function icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "cube" :height 0.9
                            :v-adjust -0.05 :face 'all-the-icons-purple)))

  (defun ivy-rich-variable-icon (_candidate)
    "Display variable icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "tag" :height 0.9
                            :v-adjust -0.05 :face 'all-the-icons-lblue)))

  (defun ivy-rich-symbol-icon (_candidate)
    "Display symbol icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-octicon "gear" :height 0.9 :v-adjust -0.05)))

  (defun ivy-rich-theme-icon (_candidate)
    "Display theme icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "palette" :height 1.0
                              :v-adjust -0.2 :face 'all-the-icons-lblue)))

  (defun ivy-rich-keybinding-icon (_candidate)
    "Display keybindings icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "keyboard" :height 1.0 :v-adjust -0.2)))

  (defun ivy-rich-library-icon (_candidate)
    "Display library icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-material "view_module"
                              :height 1.0 :v-adjust -0.2
                              :face 'all-the-icons-lblue)))

  (defun ivy-rich-package-icon (_candidate)
    "Display package icons in `ivy-rich'."
    (when (display-graphic-p)
      (all-the-icons-faicon "archive" :height 0.9
                            :v-adjust 0.0 :face 'all-the-icons-silver)))

  (when (display-graphic-p)
    (defun ivy-rich-bookmark-type-plus (candidate)
      (let ((filename (file-local-name (ivy-rich-bookmark-filename candidate))))
        (cond ((null filename)
               (all-the-icons-material "block" :v-adjust -0.2 :face 'warning))
              ((file-remote-p filename)
               (all-the-icons-material "wifi_tethering"
                                       :v-adjust -0.2 :face 'mode-line-buffer-id))
              ((not (file-exists-p filename))
               (all-the-icons-material "block" :v-adjust -0.2 :face 'error))
              ((file-directory-p filename)
               (all-the-icons-octicon "file-directory"
                                      :height 0.9 :v-adjust -0.05))
              (t (all-the-icons-icon-for-file (file-name-nondirectory filename)
                                              :height 0.9 :v-adjust -0.05)))))
    (advice-add #'ivy-rich-bookmark-type :override #'ivy-rich-bookmark-type-plus))
  :hook ((ivy-mode . ivy-rich-mode)
         (ivy-rich-mode . (lambda ()
                            (setq ivy-virtual-abbreviate
                                  (or (and ivy-rich-mode 'abbreviate) 'name)))))
  :init
  ;; For better performance
  (setq ivy-rich-parse-remote-buffer nil)

  ;; Setting tab size to 1, to insert tabs as delimiters
  (add-hook 'minibuffer-setup-hook
            (lambda ()
              (setq tab-width 1)))

  (setq ivy-rich-display-transformers-list
        '(ivy-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          ivy-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-switch-buffer-other-window
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          persp-switch-to-buffer
          (:columns
           ((ivy-rich-buffer-icon)
            (ivy-rich-candidate (:width 30))
            (ivy-rich-switch-buffer-size (:width 7))
            (ivy-rich-switch-buffer-indicators
             (:width 4 :face error :align right))
            (ivy-rich-switch-buffer-major-mode (:width 12 :face warning))
            (ivy-rich-switch-buffer-project (:width 15 :face success))
            (ivy-rich-switch-buffer-path
             (:width (lambda (x)
                       (ivy-rich-switch-buffer-shorten-path
                        x (ivy-rich-minibuffer-width 0.3))))))
           :predicate
           (lambda (cand) (get-buffer cand))
           :delimiter "\t")
          counsel-M-x
          (:columns
           ((ivy-rich-function-icon)
            (counsel-M-x-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-function
          (:columns
           ((ivy-rich-function-icon)
            (counsel-describe-function-transformer (:width 50))
            (ivy-rich-counsel-function-docstring (:face font-lock-doc-face))))
          counsel-describe-variable
          (:columns
           ((ivy-rich-variable-icon)
            (counsel-describe-variable-transformer (:width 50))
            (ivy-rich-counsel-variable-docstring (:face font-lock-doc-face))))
          counsel-apropos
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-info-lookup-symbol
          (:columns
           ((ivy-rich-symbol-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-descbinds
          (:columns
           ((ivy-rich-keybinding-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-file
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-file-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-dired
          (:columns
           ((ivy-rich-file-icon)
            (ivy-read-file-transformer))
           :delimiter "\t")
          counsel-dired-jump
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-fzf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-git
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-recentf
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate (:width 0.8))
            (ivy-rich-file-last-modified-time (:face font-lock-comment-face)))
           :delimiter "\t")
          counsel-bookmark
          (:columns
           ((ivy-rich-bookmark-type)
            (ivy-rich-bookmark-name (:width 40))
            (ivy-rich-bookmark-info))
           :delimiter "\t")
          counsel-package
          (:columns
           ((ivy-rich-package-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-find-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-library
          (:columns
           ((ivy-rich-library-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-load-theme
          (:columns
           ((ivy-rich-theme-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-switch-project
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t")
          counsel-projectile-find-file
          (:columns
           ((ivy-rich-file-icon)
            (counsel-projectile-find-file-transformer))
           :delimiter "\t")
          counsel-projectile-find-dir
          (:columns
           ((ivy-rich-dir-icon)
            (counsel-projectile-find-dir-transformer))
           :delimiter "\t")
          treemacs-projectile
          (:columns
           ((ivy-rich-file-icon)
            (ivy-rich-candidate))
           :delimiter "\t"))))

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
  :defer t
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

;; Pug mode
(use-package pug-mode
  :ensure t)

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
  :after magit)

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
  (add-to-list 'auto-mode-alist '("\\.scss\\'" . scss-mode))

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
	  ("blade" . "\\.blade\\.")))
  (setq-default indent-tabs-mode nil)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . web-mode))
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
(require 'mu4e)
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
         "Juan Alberto Camacho Bolaños"))
  ;; SMTP Settings
(use-package smtpmail
  :ensure t
  :after mu4e
  :config
  (setq message-send-mail-function 'smtpmail-send-it
        starttls-use-gnutls t
        smtpmail-starttls-credentials '(("smtp.gmail.com" 587 nil nil))
        smtpmail-auth-credentials '(("smtp.gmail.com" 587 "juancamacho@ciencias.unam.mx" nil))
        smtpmail-default-smtp-server "smtp.gmail.com"
        smtpmail-smtp-server "smtp.gmail.com"
        smtpmail-smtp-service 587))
;; Email notifications
(use-package mu4e-alert
  :ensure t
  :after mu4e
  :init
  (setq mu4e-alert-interesting-mail-query "flag:unread maildir:/[Gmail]/INBOX"))
(mu4e-alert-enable-mode-line-display)
(defun refresh-mu4e-mode-line ()
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

;; ReactJS Settings
(use-package react-snippets
  :ensure t
  :after yasnippet)

;; RJSX mode
(use-package rjsx-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode)))

;; JSX mode
(use-package jsx-mode
  :ensure t
  :defer t
  :config
  (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
  (autoload 'jsx-mode "jsx-mode" "JSX mode" t))

;; Flycheck
(use-package flycheck
  :ensure t
  :config
  (add-hook 'js-mode-hook
            (lambda () (flycheck-mode t))))

;; Flymake
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."
  :command ("jsxhint" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (equal web-mode-content-type "jsx")
              ;; enable flycheck
              (flycheck-select-checker 'jsxhint-checker)
              (flycheck-mode))))

(use-package js2-refactor
  :ensure t
  :after web-mode)

(use-package tern
  :ensure t
  :after web-mode
  :config
  (add-hook 'js-mode-hook (lambda () (tern-mode 1))))

(use-package tern-auto-complete
  :ensure t
  :after tern
  :config
  (tern-ac-setup))

(defun delete-tern-process ()
  (interactive)
    (delete-process "Tern"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(all-the-icons-ivy-buffer-commands (quote (ivy-switch-buffer-other-window)))
 '(ivy-use-virtual-buffers t)
 '(package-selected-packages
   (quote
    (react-snippets tern-auto-complete tern js2-refactor websocket circe flycheck-irony company-irony irony ob-async pug-mode aggressive-indent indent-guide aggresive-indent ansible-doc org-bullets w3m emojify company-emoji json-mode dockerfile-mode yaml-mode forge ivy-rich autumn-light-theme composer all-the-icons-ivy request company-php phpunit web-mode yasnippet rainbow-mode mu4e-alert use-package rainbow-delimiters projectile pdf-tools nov nasm-mode magit flymd doom-modeline diminish dashboard counsel company chocolate-theme autopair auctex all-the-icons-dired))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
