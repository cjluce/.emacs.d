(use-package company
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  (define-key company-mode-map (kbd "C-M-/") 'company-complete)
  (setq company-selection-wrap-around t))

(use-package eglot)
