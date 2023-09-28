(use-package sly
  :config (setq inferior-lisp-program "sbcl")
  :bind (:map sly-mode-map
              ("C-c C-q" . sly-quit-lisp)
              ("C-c b" . sly-eval-buffer)))

(use-package sly-quicklisp
  :after (sly))

;; (use-package paredit
;;   :after (sly)
;;   :hook
;;   ((scheme-mode geiser-repl-mode lisp-mode sly-mrepl-mode emacs-lisp-mode lisp-interaction-mode) . enable-paredit-mode)
;;   ;; RET is bound to
;;   :hook
;;   ((geiser-repl-mode sly-mrepl-mode) . (lambda () (unbind-key "RET" paredit-mode-map)))
;;   :bind
;;   (:map paredit-mode-map
;;         ("M-s" . nil)
;;         ("C-c s" . paredit-splice-sexp)))

(use-package paredit
  :after (sly)
  :hook
  ((lisp-mode sly-mrepl-mode emacs-lisp-mode lisp-interaction-mode) . enable-paredit-mode)
  ;; RET is bound to
  )

(add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)


(advice-add 'paredit-RET
            :after
            (lambda ()
              (when (string-prefix-p "*sly-mrepl for"
                                     (buffer-name (current-buffer)))
                (sly-mrepl-return))))


;; (use-package paredit
;;   :after (sly)
;;   :hook
;;   ((lisp-mode sly-mrepl-mode emacs-lisp-mode lisp-interaction-mode) . enable-paredit-mode)
;;   ;; RET is bound to
;;   :hook
;;   ((sly-mrepl-mode) . (lambda () (unbind-key "RET" paredit-mode-map)))
;;   :bind
;;   (:map paredit-mode-map
;;         ("M-s" . nil)
;;         ("C-c s" . paredit-splice-sexp)))
