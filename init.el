;;; package --- summary
;;; Commentary:
;;; Code:
(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))
;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(yafolding winum websocket w3m vlf vdiff-magit use-package unicode-fonts treemacs-projectile treemacs-magit treemacs-icons-dired tide rjsx-mode react-snippets rainbow-mode rainbow-delimiters python-pytest pug-mode projectile-rails prettier-js plantuml-mode pdf-tools org-mime ob-async nov nasm-mode multi-vterm mu4e-alert magit-todos magit-tbdiff magit-org-todos magit-gitflow magit-filenotify magit-delta jest ivy-rich ivy-posframe htmlize highlight-indent-guides haskell-mode forge emojify edit-indirect doom-themes doom-modeline dockerfile-mode docker-compose-mode docker diminish diff-hl dashboard counsel-projectile company-terraform company-go company-emoji clojure-mode circe-notifications chocolate-theme camcorder btc-ticker bitlbee birds-of-paradise-plus-theme auctex all-the-icons-ivy all-the-icons-dired add-node-modules-path)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(doom-modeline-bar ((t (:background "#6272a4")))))
