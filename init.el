(package-initialize)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)

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

;; I am using https://github.com/rougier/nano-emacs for all of my
;; theming/fonts/etc.

;; It might be nice to take a look at this theme, which is inspired by
;; -- but seemingly less opinionated than -- Rougier's NΛNO:
;; https://github.com/mclear-tools/bespoke-themes
;; (add-to-list 'load-path "~/.emacs.d/nano")
;; (require 'nano)

;; tessahsayshi

;; TODO: Using `(setq pop-up-windows nil)` causes some unwanted
;; side-effects. In particualar, if you edit an occur buffer --
;; usually the buffer will be in a split window, but because the
;; buffer can't open a new window, it instead opens a new frame. This
;; seems to be corroborated here by the magit maintainer:
;; https://github.com/magit/magit/issues/2172#issuecomment-134667516
(setq pop-up-windows t)

(setq my--modules (expand-file-name "mymodules" user-emacs-directory))
(defun my/load-module (module-el)
    (load (expand-file-name module-el my--modules)))

(when (eq system-type 'darwin)
  (my/load-module "macos.el"))

;; (my/load-module "cjl-dark-theme.el")
(my/load-module "theme.el")

;; Basic emacs tweaks and theme modifications. 
(my/load-module "config.el")

;; What should be saved between sessions
(my/load-module "session.el")

;; git and general project help packages
(my/load-module "project-management.el")

;; I am using vertico et al as my completion engine
(my/load-module "completion-engine.el")

(my/load-module "lsp.el")

(my/load-module "rust.el")

(my/load-module "zig.el")

;; (my/load-module "haskell.el")

(my/load-module "python.el")

(my/load-module "julia.el")

(my/load-module "lisp.el")

(my/load-module "scheme.el")

(my/load-module "docs.el")

(my/load-module "org.el")


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "82d2cac368ccdec2fcc7573f24c3f79654b78bf133096f9b40c20d97ec1d8016" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "628278136f88aa1a151bb2d6c8a86bf2b7631fbea5f0f76cba2a0079cd910f7d" "bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" "8c585f8c4ed85ffb626ce99b0c7b3f64a22059454ac7ca66e020180ea34fb4a7" "91899d8aea49850d7f7e0fe9162197abe1f09be58d2710f55d05a491db606cd4" "d43860349c9f7a5b96a090ecf5f698ff23a8eb49cd1e5c8a83bb2068f24ea563" "1b623b81f373d49bcf057315fe404b30c500c3b5a387cf86c699d83f2f5763f4" "5ec088e25ddfcfe37b6ae7712c9cb37fd283ea5df7ac609d007cafa27dab6c64" "0f220ea77c6355c411508e71225680ecb3e308b4858ef6c8326089d9ea94b86f" "709518dd57a66363f1e7a4b2175c08f08c0e69ce86789d1fefcfaf71fcaca781" default))
 '(package-selected-packages
   '(paredit rainbow-delimiters sly-quicklisp sly haskell-mode company rustic rust-mode eglot embark-consult consult embark marginalia orderless vertico vterm magit which-key undo-tree use-package)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :extend nil :stipple nil :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 200 :width normal :foundry "nil" :family "Fira Code"))))
 '(variable-pitch ((t (:family "ETBembo")))))
(put 'upcase-region 'disabled nil)
