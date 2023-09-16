;; Okay, so I still haven't created a theme. It's a lot of work. But I
;; do have some ideas. I have been really focused on syntax
;; highlighting and its pro's and con's. I seem to be in the camp of
;; "less is more," but reject the notion that syntax highlighting is a
;; useless construct. However, I do think that syntax highlighting
;; should perhaps be distinct from the color theme --
;; package-wise. This way one could load up the color theme that she
;; was interested in, then load the syntax highlighting theme, thus
;; overlaying the two themes, givng you the best of both
;; worlds. Naturally, I'm unsure of how this will work in practice as
;; it is possible that the font themes Et al. are too interconnected.

;; (my/load-module "modeline.el")
(use-package nano-modeline)
;;(nano-modeline-mode)


(use-package all-the-icons
  ; Requires M-x all-the-icons-install-fonts

  ; This only needs to happen once... probably something in
  ; use-package that will allow for this to happen. If not, I can just
  ; check for the fonts, which get installed into some directory, or I
  ; can set a global variable like *all-the-icons-fonts-installed*.
  )
(use-package lambda-line
  :straight (:type git :host github :repo "lambda-emacs/lambda-line") 
  :custom
  (lambda-line-icon-time t) ;; requires ClockFace font (see below)
  (lambda-line-clockface-update-fontset "ClockFaceRect") ;; set clock icon
  (lambda-line-position 'bottom)       ;; Set position of status-line 
  (lambda-line-abbrev t)            ;; abbreviate major modes
  (lambda-line-hspace "  ")         ;; add some cushion
  (lambda-line-prefix t)            ;; use a prefix symbol
  (lambda-line-prefix-padding nil)  ;; no extra space for prefix 
  (lambda-line-status-invert nil)   ;; no invert colors
  (lambda-line-gui-ro-symbol  " ⨂") ;; symbols
  (lambda-line-gui-mod-symbol " ⬤") 
  (lambda-line-gui-rw-symbol  " ◯") 
  (lambda-line-space-top +.50) ;; padding on top and bottom of line
  (lambda-line-space-bottom -.50)
  (lambda-line-symbol-position 0.1) ;; adjust the vertical placement of symbol
  :config
  ;; activate lambda-line 
  (lambda-line-mode) 
  ;; set divider line in footer
  (when (eq lambda-line-position 'top)
    (setq-default mode-line-format (list "%_"))
    (setq mode-line-format (list "%_"))))

(use-package lambda-themes
  :straight (:type git :host github :repo "lambda-emacs/lambda-themes") 
  :custom
  (lambda-themes-set-italic-comments t)
  (lambda-themes-set-italic-keywords t)
  (lambda-themes-set-variable-pitch t))

;; (use-package color-theme-sanityinc-tomorrow
;;   :config
;;   (load-theme 'sanityinc-tomorrow-eighties))

;; (use-package solarized-theme)

;; (load-theme 'solarized-gruvbox-light t)

;; (load-theme 'lambda-dark-faded)
;; (load-theme 'solarized-zenburn t)

;; (load-theme 'lambda-dark-faded t)

;; Start up with my light theme between the hours of 8 and 8,
;; otherwise use my dark theme.

(defun my/update-theme-light-dark ()
  (interactive)
  (if (< 8 (decoded-time-hour (decode-time)) 20)
      (load-theme 'spacemacs-light t)
    (load-theme 'spacemacs-dark t)))
(my/update-theme-light-dark)
