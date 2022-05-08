;; Add to package archive list
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(unless package-archive-contents
  (package-refresh-contents))

;; Default configuration
(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(visual-line-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq backup-directory-alist `((".*" . "~/.emacs.d/backups/")))

(defalias 'yes-or-no-p 'y-or-n-p)

(setq inhibit-startup-screen t)

(setq x-select-enable-clipboard t)

(setenv "PATH" (concat (getenv "PATH") ":/home/lucec/.local/bin"))
(setq exec-path (append exec-path '("/home/lucec/.local/bin")))


;; Misc. Hooks
(add-hook 'prog-mode-hook 'electric-pair-local-mode)

;; Install use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'use-package)
;; ensure installs a package if it doesn't exist
(setq use-package-always-ensure t)

;; use-package calls
(use-package magit
  :bind ("C-x g" . magit-status))

;; I might not want to set the face-attribute per theme, but we will
;; see
(use-package gruvbox-theme
  :config
  (load-theme 'gruvbox-dark-hard t)
  ;; The value is in 1/10pt, so 140 will give you 14pt, etc.
  (set-face-attribute 'default nil :height 140))

;; (use-package nano-modeline
;;   :config
;;   (nano-modeline-mode)
;;   (setq nano-modeline-position 'bottom))

;; Run ~all-the-icons-install-fonts~ Haven't found a way to do this
;; automatically without reinstalling each time
(use-package doom-modeline
  :init (doom-modeline-mode 1))

;; Installing counsel also installs ivy and swiper
(use-package counsel
  :init (ivy-mode 1)
  :diminish ivy-mode
  :config
  (setq ivy-use-virtual-buffers t
        ivy-count-format "%d/%d ")
  (global-set-key "\C-s" 'swiper)
  (global-set-key (kbd "C-c C-r") 'ivy-resume)
  (global-set-key (kbd "<f6>") 'ivy-resume)
  (global-set-key (kbd "M-x") 'counsel-M-x)
  (global-set-key (kbd "C-x C-f") 'counsel-find-file)
  (global-set-key (kbd "<f1> f") 'counsel-describe-function)
  (global-set-key (kbd "<f1> v") 'counsel-describe-variable)
  (global-set-key (kbd "<f1> o") 'counsel-describe-symbol)
  (global-set-key (kbd "<f1> l") 'counsel-find-library)
  (global-set-key (kbd "<f2> i") 'counsel-info-lookup-symbol)
  (global-set-key (kbd "<f2> u") 'counsel-unicode-char)
  (global-set-key (kbd "C-c g") 'counsel-git)
  (global-set-key (kbd "C-c j") 'counsel-git-grep)
  (global-set-key (kbd "C-c k") 'counsel-ag)
  (global-set-key (kbd "C-x l") 'counsel-locate)
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-minibuffer-history))

(use-package which-key
  :init (which-key-mode 1)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package dashboard
  :config
    (dashboard-setup-startup-hook)
    (setq dashboard-startup-banner "~/Downloads/emacs-logo-3.png")    
    (setq dashboard-items '((recents  . 5)))
    (setq dashboard-banner-logo-title ""))

(if (< (length command-line-args) 2)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/"))))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

;; (defun company-ess-setup ()
;;   (setq-local company-backends
;; 	      (append '((company-dabbrev-code company-R-args company-R-objects))
;; 		      company-backends)))

;; (use-package ess
;;   :init (require 'ess-site)
;;   :mode (("\\.[rR]\\'" . R-mode))
;;   :config
;;   (setq ess-tab-complete-in-script 1)
;;   :hook ((ess-mode-hook company-ess-setup)))

;; (use-package company
;;   :hook ((R-mode . company-mode))
;;   :bind (("C-M-/" . company-complete))
;;   :config
;;   (setq ess-use-company nil))


(use-package ess
  :init (require 'ess-site)
  :mode (("\\.[rR]\\'" . R-mode))
  :config
  (setq ess-tab-complete-in-script 1)
  (setq ess-use-company nil))

;; (use-package company
;;   :hook ((R-mode . company-mode))
;;   :bind (("C-M-/" . company-complete))
;;   :config
;;   (setq ess-use-company t))


;; (require 'company)
;; (setq tab-always-indent 'complete)
;; (setq company-idle-delay 0.1)
;; (global-company-mode)
;; (ess-toggle-underscore nil)
;; (with-eval-after-load 'ess
;;   (setq ess-use-company t))

(use-package hl-todo
  :hook (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
	hl-todo-keyword-faces
	`(("TODO"        warning bold)
	  ("FIXME"       error bold)
	  ("HACK"        font-lock-constant-face bold)
	  ("REVIEW"      font-lock-keyword-face bold)
	  ("NOTE"        success bold)
	  ("DEPRICATED"  font-lock-doc-face bold))))

(use-package ess-view-data
  :config
  (setq ess-view-data-current-backend 'data\.table+magrittr)
  (setq ess-view-data-current-save-backend 'data\.table::fwrite))
(require 'ess-view-data)


(use-package lsp-mode
  ;; :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  (lsp-register-custom-settings
   '(("pyls.plugins.pyls_mypy.enabled" t t)
     ("pyls.plugins.pyls_mypy.live_mode" nil t)
     ("pyls.plugins.pyls_black.enabled" t t)
     ("pyls.plugins.pyls_isort.enabled" t t)

     ;; Disable these as they're duplicated by flake8
     ("pyls.plugins.pycodestyle.enabled" nil t)
     ("pyls.plugins.mccabe.enabled" nil t)
     ("pyls.plugins.pyflakes.enabled" nil t)))
  :hook (;; replace XXX-mode with concrete major-mode(e. g. python-mode)
         (R-mode . lsp)
	 (ess-r-mode . lsp)
         ;; if you want which-key integration
         (lsp-mode . lsp-enable-which-key-integration)
	 (js-mode . lsp)
	 (python-mode . lsp))
  :commands lsp)

(use-package lsp-ui
  :config
  (setq lsp-ui-sideline-show-hover t
                lsp-ui-sideline-delay 0.5
                lsp-ui-doc-delay 5
                lsp-ui-sideline-ignore-duplicates t
                lsp-ui-doc-position 'bottom
                lsp-ui-doc-alignment 'frame
                lsp-ui-doc-header nil
                lsp-ui-doc-include-signature t
                lsp-ui-doc-use-childframe t)
  :commands lsp-ui-mode)
  ;; Probably want to map these...
  ;; :bind (:map evil-normal-state-map
  ;;             ("gd" . lsp-ui-peek-find-definitions)
  ;;             ("gr" . lsp-ui-peek-find-references)
  ;;             :map md/leader-map
  ;;             ("Ni" . lsp-ui-imenu))

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)
	 (scheme-mode . paredit-mode)))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (define-key js-mode-map (kbd "C-M-/") 'company-complete)
  (define-key company-mode-map (kbd "C-M-/") 'company-complete)
  (setq company-selection-wrap-around t))

;; (use-package company-lsp
;;   :config
;;   (push 'company-lsp company-backends))




(use-package json-mode)

(use-package fish-mode)

;; (use-package telega
;;   :ensure nil
;;   :config
;;   (setq telega-server-libs-prefix "/usr/include"))


;; Add support for mitscheme

(load "~/.emacs.d/ob-mitscheme")


(use-package web-mode
  :mode ("\\.html$" . web-mode))
(use-package emmet-mode
  :hook ((sgml-mode-hook . emmet-mode)
	 (css-mode-hook . emmet-mode)
	 (web-mode-hook . emmet-mode)))



;; Customize org-mode
(setq org-src-window-setup 'split-window-below)
(setq browse-url-browser-function 'browse-url-chrome)

;; When exporting org-mode source blocks, don't ask whether to compile
;; each source block when exporting
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)
   (scheme . t)
   (mitscheme . t)))

(defun ess-org-edit-special-set-R-process ()
  "Automatically sets the R process when entering an org-mode
source block for autocompletion purposes. If there are multiple
active R processes, then the user will be asked to select an R
process."
  (when (string-match-p "^\\*Org Src .*\\[ R \\]\\*$"
			(buffer-name))
    (ess-force-buffer-current "Select R Process: " t nil nil)))

(add-hook 'ess-mode-hook 'ess-org-edit-special-set-R-process)




;; Global Keyboard Definitions
(global-set-key (kbd "C-.") 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-view-data-current-backend 'data\.table+magrittr)
 '(ess-view-data-current-save-backend 'data\.table::fwrite)
 '(package-selected-packages
   '(lsp-ui fish-mode emmet-mode web-mode json-mode rainbow-delimiters paredit lsp-mode ess-view-data hl-todo company ess pdf-tools undo-tree dashboard which-key counsel use-package nano-modeline magit gruvbox-theme doom-modeline)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
