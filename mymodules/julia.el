(use-package julia-mode)

;; requires vterm or Eat
(use-package julia-snail
  :ensure t
  :hook (julia-mode . julia-snail-mode))

(use-package eglot-jl)
