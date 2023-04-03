(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

;; I am using https://github.com/rougier/nano-emacs for all of my theming/fonts/etc.
(add-to-list 'load-path "~/.emacs.d/nano")
(require 'nano)

(setq my--modules (expand-file-name "mymodules" user-emacs-directory))
(defun my/load-module (module-el)
    (load (expand-file-name module-el my--modules)))

;; Basic emacs tweaks and theme modifications. 
(my/load-module "config.el")

;; git and general project help packages
(my/load-module "project-management.el")

;; I am using vertico et al as my completion engine
(my/load-module "completion-engine.el")

(my/load-module "lsp.el")

(my/load-module "rust.el")

(my/load-module "haskell.el")

(my/load-module "python.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(haskell-mode company rustic rust-mode eglot embark-consult consult embark marginalia orderless vertico vterm magit which-key undo-tree use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
