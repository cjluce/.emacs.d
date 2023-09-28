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

(with-eval-after-load 'flycheck
  (setq-default flycheck-scheme-chicken-executable "chicken-csc"))


(with-eval-after-load 'geiser
  ;; chicken-install -s srfi-18 apropos chicken-doc
  (setq-default geiser-chicken-binary "chicken-csi")
  (setq-default geiser-active-implementations
                '(chicken)))

(setq scheme-program-name "chicken-csi -:c")

;; (require 'chicken-scheme)
;; (add-hook 'scheme-mode-hook 'setup-chicken-scheme)
;; (define-key scheme-mode-map (kbd "C-?") 'chicken-show-help)


(add-hook 'scheme-mode-hook 'enable-paredit-mode)
(add-hook 'scheme-mode-hook 'rainbow-delimiters-mode-enable)
