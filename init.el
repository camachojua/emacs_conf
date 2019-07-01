(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/"))
(package-initialize)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(eval-when-compile
  (require 'use-package))
(require 'diminish)
(require 'bind-key)

;; General settings
(setq inhibit-startup-screen t)
(setq backup-inhibited t)
(setq auto-save-default nil)
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)
(global-linum-mode 1)
(set-frame-font "FiraCode 9" nil t)
(add-hook 'local-write-file-hooks
	  (lambda() (delete-trailing-whitespace) nil))

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

;; Icons for various modes
(use-package all-the-icons
  :ensure t
  :defer t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

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
  :config
  (doom-modeline-mode t))

;; Dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-banner-logo-title "")
  (setq dashboard-startup-banner "/home/juan/Images/gnu.png")
  (setq dashboard-center-content t)
  (setq dashboard-items '((recents  . 5)
			  (projects . 5)
			  (agenda . 5)))
  :config
  (dashboard-setup-startup-hook)
)

;; Theme
(use-package spacemacs-theme
  :ensure t
  :defer t
  :init
  (load-theme 'spacemacs-dark t))

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
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
                          ivy-rich-switch-buffer-align-virtual-buffer t
                          ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer))

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

(use-package ivy-rich
  :ensure t
  :after ivy
  :custom
  (ivy-virtual-abbreviate 'full
			  ivy-rich-switch-buffer-align-virtual-buffer t
			  ivy-rich-path-style 'abbrev)
  :config
  (ivy-set-display-transformer 'ivy-switch-buffer
			       'ivy-rich-switch-buffer-transformer))

(use-package company
  :ensure t
  :defer t
  :config
  (add-hook 'prog-mode-hook 'company-mode)
  (add-hook 'LaTeX-mode-hook 'company-mode))

(use-package yasnippet
  :ensure t
  :after ivy
  :defer t
  :config
  (yas-load-directory "~/.emacs.d/snippets/")
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (yas-global-mode t))

(use-package magit
  :ensure t
  :after ivy
  :init
  (setq magit-completing-read-function 'ivy-completing-read)
  :bind
  (("C-x g" . 'magit-status)
   ("C-x M-g" . 'magit-dispatch)))

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

(custom-set-variables
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088"
     default))) 
 '(ivy-virtual-abbreviate (quote full))
 '(package-selected-packages
   (quote
    (counsel auctex tex nov company-php magit web-mode yasnippet
             spacemacs-theme dashboard all-the-icons ivy-rich ivy
             pdf-tools rainnbow-delimiters autopair diminish
             use-package))))

