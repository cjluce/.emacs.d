(use-package geiser-chicken
  :bind
  (:map scheme-mode-map
        ("C-." . nil))
  ;; (:map geiser-mode-map
  ;;       ("C-." . nil))
  )

(add-hook 'geiser-mode-hook
          '(lambda () (define-key geiser-mode-map (kbd "C-.") nil)))
(add-hook 'geiser-repl-mode-hook
          '(lambda () (define-key geiser-repl-mode-map (kbd "C-.") nil)))

;; (use-package geiser-guile)

