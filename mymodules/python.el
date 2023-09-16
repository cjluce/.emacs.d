;; (setenv "PATH" (concat (getenv "PATH") ":/home/lucec/.local/bin"))
;; (setq exec-path (append exec-path '("/home/lucec/.local/bin")))

;; (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/lib/python3.11/site-packages"))
;; (setq exec-path (append exec-path '("/opt/homebrew/lib/python3.11/site-packages")))

;; (setenv "PATH" (concat (getenv "PATH") ":/opt/homebrew/bin"))
;; (setq exec-path (append exec-path '("/opt/homebrew/bin")))



(use-package python
;;  :interpreter ("/opt/homebrew/bin/python3" . python-mode)
  :config
  (add-hook 'python-hook (lambda ()
                           (setq
                            python-indent-guess-indent-offset-verbose
                            nil)))
  (add-hook 'python-mode-hook 'company-mode)
  ;; (add-hook 'python-mode-hook #'yas-minor-mode)
  (add-hook 'python-mode-hook 'eglot-ensure)
  (add-hook 'python-mode-hook (lambda () (setq eglot-connect-timeout 120)))
  (add-hook 'python-mode-hook (lambda () (setq eglot-autoshutdown t)))
  (setq-default eglot-workspace-configuration
    '((:pyright .
        ((useLibraryCodeForTypes . t))))))
