(setq gc-cons-threshold (* 200 1000 1000))

(tool-bar-mode 0)
(menu-bar-mode 0)
(scroll-bar-mode 0)

(setq package-enable-at-startup nil)

(setq frame-inhibit-implied-resize t)

(setq default-frame-alist
      (append
       (list
        '(undecorated . t)
        '(min-height . 1)
        '(height     . 42)
        '(min-width  . 1)
        '(width      . 170)
        '(vertical-scroll-bars . nil)
        '(internal-border-width . 10)
        '(tool-bar-lines . 0)
        '(menu-bar-lines . 0))))

(setq-default left-margin-width 1
              right-margin-width 1)
