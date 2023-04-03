;; Get rid of the menu bar at the top of the screen
(menu-bar-mode -1)

;; Allow navigation of wrapped lines
(visual-line-mode 1)

;; Default the nano theme to be dark
(nano-theme-set-dark)
(nano-refresh-theme)

;; Start emacs with a maximized window
(add-to-list 'default-frame-alist '(fullscreen . maximized))
(add-to-list 'initial-frame-alist '(fullscreen . maximized))

;; Remap the "yes or no" question to just be "y or n"
(defalias 'yes-or-no-p 'y-or-n-p)

;; Save/read from system keyboard
(setq x-select-enable-clipboard t)

(global-set-key (kbd "M-o") 'other-window)

(use-package which-key
  :init (which-key-mode 1)
  :config
  (which-key-setup-side-window-bottom)
  (setq which-key-sort-order 'which-key-key-order-alpha
    which-key-side-window-max-width 0.33
    which-key-idle-delay 0.05)
  :diminish which-key-mode)

(use-package undo-tree
  :diminish
  :config
  (global-undo-tree-mode 1)
  (setq undo-tree-history-directory-alist `(("." . ,(expand-file-name "undo" user-emacs-directory)))))

(use-package vterm
    :ensure t)


;; Change the size of the window for ispell corrections. My nano theme
;; seems to make the window just a little too small. The default
;; otherwise was 2.
(setq ispell-choices-win-default-height 3)
