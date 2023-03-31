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
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

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
  ;;(global-set-key "\C-s" 'swiper)
  (global-set-key "\C-s" 'swiper-isearch)
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
    (setq dashboard-startup-banner "~/.emacs.d/images/emacs-logo-3.png")    
    (setq dashboard-items '((recents  . 5)))
    (setq dashboard-banner-logo-title ""))

;; This is for running emacs as a server
(if (< (length command-line-args) 2)
  (setq initial-buffer-choice (lambda () (get-buffer "*dashboard*"))))

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo/"))))

;; You might need to uninstall this and force install the "Melpa"
;; version of the package. The other package doesn't seem to build as
;; of May 22, 2022.
(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (setq-default pdf-view-display-size 'fit-page)
  (setq pdf-annot-activate-created-annotations t)
  (pdf-tools-install :no-query)
  (require 'pdf-occur))

(use-package ess
  :init (require 'ess-site)
  :mode (("\\.[rR]\\'" . R-mode))
  :config
  (setq ess-tab-complete-in-script 1)
  (setq ess-use-company nil))

(use-package rust-mode
  :mode ("\\.rs\\'" . rust-mode))
(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status))
  :config
  ;; uncomment for less flashiness
  ;; (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  ;; (setq lsp-signature-auto-activate nil)

  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'rk/rustic-mode-hook))

(defun rk/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t))
  (add-hook 'before-save-hook 'lsp-format-buffer nil t))


;; (use-package lsp-mode
;;   :ensure
;;   :commands lsp
;;   :custom
;;   ;; what to use when checking on-save. "check" is default, I prefer clippy
;;   (lsp-rust-analyzer-cargo-watch-command "clippy")
;;   (lsp-eldoc-render-all t)
;;   (lsp-idle-delay 0.6)
;;   ;; enable / disable the hints as you prefer:
;;   (lsp-rust-analyzer-server-display-inlay-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
;;   (lsp-rust-analyzer-display-chaining-hints t)
;;   (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
;;   (lsp-rust-analyzer-display-closure-return-type-hints t)
;;   (lsp-rust-analyzer-display-parameter-hints nil)
;;   (lsp-rust-analyzer-display-reborrow-hints nil)
;;   :config
;;   (add-hook 'lsp-mode-hook 'lsp-ui-mode))

;; (use-package lsp-ui
;;   :ensure
;;   :commands lsp-ui-mode
;;   :custom
;;   (lsp-ui-peek-always-show t)
;;   (lsp-ui-sideline-show-hover t)
;;   (lsp-ui-doc-enable nil))

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

(use-package paredit
  :hook ((emacs-lisp-mode . paredit-mode)
	 (lisp-mode . paredit-mode)
	 (scheme-mode . paredit-mode)))

;; (use-package geiser-guile
;;   :bind
;;   (:map geiser-mode-map
;; 	("C-c C-." . geiser-completion--for-module)
;; 	geiser-repl-mode-map
;; 	("C-c C-." . geiser-completion--for-module)))

;; (with-eval-after-load "helm"
;;   (bind-key "M-Y" #'helm-end-of-buffer helm-map))



(use-package geiser-guile)
(use-package geiser-chicken)

(with-eval-after-load "geiser-mode"
  (unbind-key "C-." geiser-mode-map)
  (unbind-key "C-." geiser-repl-mode-map)
  (setq geiser-chicken-binary "chicken-csi"))

;; (unbind-key "C-." geiser-mode-map)
;; (unbind-key "C-." geiser-repl-mode-map)
;; (setq geiser-chicken-binary "chicken-csi")

(use-package slime
  :config
  (setq inferior-lisp-program "sbcl"))

(use-package rainbow-delimiters
  :hook ((prog-mode . rainbow-delimiters-mode)))

(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  ;; (define-key js-mode-map (kbd "C-M-/") 'company-complete)
  (define-key company-mode-map (kbd "C-M-/") 'company-complete)
  (setq company-selection-wrap-around t))

(use-package json-mode)

(use-package fish-mode)

(use-package jupyter
  :commands (jupyter-run-server-repl
             jupyter-run-repl
             jupyter-server-list-kernels)
  :init (eval-after-load 'jupyter-org-extensions ; conflicts with my helm config, I use <f2 #>
          '(unbind-key "C-c h" jupyter-org-interaction-mode-map)))

(use-package ace-window
  :config
  (global-set-key (kbd "C-.") 'ace-window))

(use-package avy
  :config
  (global-set-key (kbd "M-j") 'avy-goto-char-timer))
(use-package avy-zap
  :config
  (global-set-key (kbd "M-z") 'avy-zap-up-to-char))

;; Required for counsel-ag, might add it under ivy or something. Okay
;; it might not actually be required, you just have to install
;; "the_silver_searcher" in the terminal.
(use-package ag)

;; Still unsure about a good configuration for html/css/js stuff
;; (use-package web-mode
;;   :mode ("\\.html$" . web-mode))
;; (use-package emmet-mode
;;   :hook ((sgml-mode-hook . emmet-mode)
;; 	 (css-mode-hook . emmet-mode)
;; 	 (web-mode-hook . emmet-mode)))

;; This allows you to swiper to get results, swiper-occur to pop those
;; results into a buffer, and then edit those results. You can also
;; counsel-ag to get results from multiple files.
(use-package wgrep)

;; This is just a handy package. I use it for changing variable names
;; on the fly.
(use-package iedit)

;; I really only need one of visual-regexp and anzu
(use-package visual-regexp)
(use-package anzu)

(use-package dot-mode
  :bind (:map dot-mode-map
	      ("C-," . dot-mode-execute))
  :config
  (global-dot-mode t))


;; Great talk by abo-abo introducing Hydra
;; https://www.youtube.com/watch?v=ONHEDj3kWrE
;; I'll probably add a few hydra's for, e.g., org mode.
(use-package hydra)

(use-package easy-kill
  :config
  (global-set-key [remap kill-ring-save] 'easy-kill))

(use-package helpful
  :config
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key))



(use-package treemacs
  :init
  :config
  (progn
    (setq treemacs-collapse-dirs                   (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay        0.5
          treemacs-directory-name-transformer      #'identity
          treemacs-display-in-side-window          t
          treemacs-eldoc-display                   'simple
          treemacs-file-event-delay                5000
          treemacs-file-extension-regex            treemacs-last-period-regex-value
          treemacs-file-follow-delay               0.2
          treemacs-file-name-transformer           #'identity
          treemacs-follow-after-init               t
          treemacs-expand-after-init               t
          treemacs-find-workspace-method           'find-for-file-or-pick-first
          treemacs-git-command-pipe                ""
          treemacs-goto-tag-strategy               'refetch-index
          treemacs-header-scroll-indicators        '(nil . "^^^^^^")'
          treemacs-hide-dot-git-directory          t
          treemacs-indentation                     2
          treemacs-indentation-string              " "
          treemacs-is-never-other-window           nil
          treemacs-max-git-entries                 5000
          treemacs-missing-project-action          'ask
          treemacs-move-forward-on-expand          nil
          treemacs-no-png-images                   nil
          treemacs-no-delete-other-windows         t
          treemacs-project-follow-cleanup          nil
          treemacs-persist-file                    (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                        'left
          treemacs-read-string-input               'from-child-frame
          treemacs-recenter-distance               0.1
          treemacs-recenter-after-file-follow      nil
          treemacs-recenter-after-tag-follow       nil
          treemacs-recenter-after-project-jump     'always
          treemacs-recenter-after-project-expand   'on-distance
          treemacs-litter-directories              '("/node_modules" "/.venv" "/.cask")
          treemacs-show-cursor                     nil
          treemacs-show-hidden-files               t
          treemacs-silent-filewatch                nil
          treemacs-silent-refresh                  nil
          treemacs-sorting                         'alphabetic-asc
          treemacs-select-when-already-in-treemacs 'move-back
          treemacs-space-between-root-nodes        t
          treemacs-tag-follow-cleanup              t
          treemacs-tag-follow-delay                1.5
          treemacs-text-scale                      nil
          treemacs-user-mode-line-format           nil
          treemacs-user-header-line-format         nil
          treemacs-wide-toggle-width               70
          treemacs-width                           35
          treemacs-width-increment                 1
          treemacs-width-is-initially-locked       t
          treemacs-workspace-switch-cleanup        nil)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode 'always)

    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))

    (treemacs-hide-gitignored-files-mode nil))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t d"   . treemacs-select-directory)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))


(use-package marginalia
  :config
  (marginalia-mode))

(use-package embark
  :ensure t
  :bind
  (("C->" . embark-act)         ;; pick some comfortable binding
   ("M-." . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'
  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)
  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

(use-package julia-mode
  :ensure t)
(use-package vterm
    :ensure t)
(use-package julia-repl
  :ensure t
  :hook (julia-mode . julia-repl-mode)

  :init
  (setenv "JULIA_NUM_THREADS" "8")

  :config
  ;; Set the terminal backend
  (julia-repl-set-terminal-backend 'vterm)
  
  ;; Keybindings for quickly sending code to the REPL
  (define-key julia-repl-mode-map (kbd "<C-RET>") 'my/julia-repl-send-cell)
  (define-key julia-repl-mode-map (kbd "<M-RET>") 'julia-repl-send-line)
  (define-key julia-repl-mode-map (kbd "<S-return>") 'julia-repl-send-buffer))
(use-package quelpa)
(quelpa '(lsp-julia :fetcher github
                    :repo "non-Jedi/lsp-julia"
                    :files (:defaults "languageserver")))
(use-package lsp-julia
  :config
  (setq lsp-julia-default-environment "~/.julia/environments/v1.7"))
(add-hook 'julia-mode-hook #'lsp-mode)

(use-package julia-snail
  :hook (julia-mode . julia-snail-mode)
  :config
  (setq julia-snail-multimedia-enable t)
  ;; (setq julia-snail-repl-display-eval-results t)
  )


(use-package lsp-mode
  ;; :init
  ;; set prefix for lsp-command-keymap (few alternatives - "C-l", "C-c l")
  ;; (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-idle-delay 0.5
        lsp-enable-symbol-highlighting t
        lsp-enable-snippet nil  ;; Not supported by company capf, which is the recommended company backend
        lsp-pyls-plugins-flake8-enabled t)
  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
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
	 (python-mode . lsp)
	 (julia-mode . lsp))
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  (lsp-eldoc-render-all t)
  (lsp-idle-delay 0.6)
  ;; enable / disable the hints as you prefer:
  (lsp-rust-analyzer-server-display-inlay-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-enable "skip_trivial")
  (lsp-rust-analyzer-display-chaining-hints t)
  (lsp-rust-analyzer-display-lifetime-elision-hints-use-parameter-names nil)
  (lsp-rust-analyzer-display-closure-return-type-hints t)
  (lsp-rust-analyzer-display-parameter-hints nil)
  (lsp-rust-analyzer-display-reborrow-hints nil)
  )

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



;; Customize org-mode
(setq org-src-window-setup 'split-window-below)
(setq browse-url-browser-function 'browse-url-chrome)

;; When exporting org-mode source blocks, don't ask whether to compile
;; each source block when exporting
(setq org-confirm-babel-evaluate nil)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((R . t)))

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
;; Commenting this one out since I am now using ace-window
;; (global-set-key (kbd "C-.") 'other-window)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ess-view-data-current-backend 'data\.table+magrittr)
 '(ess-view-data-current-save-backend 'data\.table::fwrite)
 '(package-selected-packages
   '(rust-mode geiser-chicken slime geiser-guile julia-snail quelpa lsp-julia vterm julia-repl julia-mode embark marginalia treemacs helpful easy-kill hydra avy-zap pdf-tools dot-mode anzu visual-regexp iedit wgrep ag ace-window jupyter emacs-jupyter lsp-ui fish-mode emmet-mode web-mode json-mode rainbow-delimiters paredit lsp-mode ess-view-data hl-todo company ess undo-tree dashboard which-key counsel use-package nano-modeline magit gruvbox-theme doom-modeline))
 '(ring-bell-function 'ignore))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
