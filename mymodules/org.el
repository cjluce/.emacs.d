(use-package org-roam
  :custom
  (org-roam-directory (file-truename "~/org/"))
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n g" . org-roam-graph)
         ("C-c n i" . org-roam-node-insert)
         ("C-c n c" . org-roam-capture)
         ;; Dailies
         ("C-c n j" . org-roam-dailies-capture-today))
  :config
  ;; If using org-roam-protocol
  ;; (require 'org-roam-protocol)
  ;; If you're using a vertical completion framework, you might want a more informative completion interface
  (setq org-roam-node-display-template (concat "${title:*} " (propertize "${tags:10}" 'face 'org-tag)))
  (org-roam-db-autosync-mode))


;; Set default, fixed and variabel pitch fonts
;; Use M-x menu-set-font to view available fonts
(use-package mixed-pitch
  :hook
  (org-mode . mixed-pitch-mode)
  :config
  (set-face-attribute 'default nil :font "Fira Code" :height 200)
  (set-face-attribute 'fixed-pitch nil :font "Fira Code")
  (set-face-attribute 'variable-pitch nil :font "ETBembo"))
;; (add-hook 'mixed-pitch-mode-hook #'solaire-mode-reset)
;; Required for proportional font

(use-package company-posframe
  :config
  (company-posframe-mode 1))

;; Improve org mode looks
(setq org-startup-indented t
      org-pretty-entities t
      ;; org-hide-emphasis-markers t
      org-startup-with-inline-images t
      org-image-actual-width '(300))

;; Show hidden emphasis markers
(use-package org-appear
  :hook (org-mode . org-appear-mode))

;; Nice bullets
(use-package org-superstar
  :config
  (setq org-superstar-special-todo-items t)
  (add-hook 'org-mode-hook (lambda ()
                             (org-superstar-mode 1))))

;; Distraction-free screen
(use-package olivetti
  :init
  (setq olivetti-body-width .67)
  :config
  (defun distraction-free ()
    "Distraction-free writing environment"
    (interactive)
    (if (equal olivetti-mode nil)
        (progn
          (window-configuration-to-register 1)
          (delete-other-windows)
          (text-scale-increase 2)
          (olivetti-mode t))
      (progn
        (jump-to-register 1)
        (olivetti-mode 0)
        (text-scale-decrease 2))))
  :bind
  (("<f9>" . distraction-free)))


;; Increase size of LaTeX fragment previews
;; (plist-put org-format-latex-options :scale 2)

;; Increase line spacing
(add-hook 'org-mode-hook '(lambda () (setq-local line-spacing 4)))
