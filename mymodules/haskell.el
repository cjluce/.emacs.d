(setenv "PATH" (concat (getenv "PATH") ":/home/lucec/.ghcup/bin"))
(setq exec-path (append exec-path '("/home/lucec/.ghcup/bin")))

(use-package haskell-mode)
