(defvar elpaca-installer-version 0.7)
(defvar elpaca-directory (expand-file-name "elpaca/" user-emacs-directory))
(defvar elpaca-builds-directory (expand-file-name "builds/" elpaca-directory))
(defvar elpaca-repos-directory (expand-file-name "repos/" elpaca-directory))
(defvar elpaca-order '(elpaca :repo "https://github.com/progfolio/elpaca.git"
                              :ref nil :depth 1
                              :files (:defaults "elpaca-test.el" (:exclude "extensions"))
                              :build (:not elpaca--activate-package)))
(let* ((repo  (expand-file-name "elpaca/" elpaca-repos-directory))
       (build (expand-file-name "elpaca/" elpaca-builds-directory))
       (order (cdr elpaca-order))
       (default-directory repo))
  (add-to-list 'load-path (if (file-exists-p build) build repo))
  (unless (file-exists-p repo)
    (make-directory repo t)
    (when (< emacs-major-version 28) (require 'subr-x))
    (condition-case-unless-debug err
        (if-let ((buffer (pop-to-buffer-same-window "*elpaca-bootstrap*"))
                 ((zerop (apply #'call-process `("git" nil ,buffer t "clone"
                                                 ,@(when-let ((depth (plist-get order :depth)))
                                                     (list (format "--depth=%d" depth) "--no-single-branch"))
                                                 ,(plist-get order :repo) ,repo))))
                 ((zerop (call-process "git" nil buffer t "checkout"
                                       (or (plist-get order :ref) "--"))))
                 (emacs (concat invocation-directory invocation-name))
                 ((zerop (call-process emacs nil buffer nil "-Q" "-L" "." "--batch"
                                       "--eval" "(byte-recompile-directory \".\" 0 'force)")))
                 ((require 'elpaca))
                 ((elpaca-generate-autoloads "elpaca" repo)))
            (progn (message "%s" (buffer-string)) (kill-buffer buffer))
          (error "%s" (with-current-buffer buffer (buffer-string))))
      ((error) (warn "%s" err) (delete-directory repo 'recursive))))
  (unless (require 'elpaca-autoloads nil t)
    (require 'elpaca)
    (elpaca-generate-autoloads "elpaca" repo)
    (load "./elpaca-autoloads")))
(add-hook 'after-init-hook #'elpaca-process-queues)
(elpaca `(,@elpaca-order))

;; Install use-package support
(elpaca elpaca-use-package
  ;; Enable use-package :ensure support for Elpaca.
  (elpaca-use-package-mode)
  (setq use-package-always-ensure t))

;; Block until current queue processed.
(elpaca-wait)

(use-package org
  :ensure nil
  :init (require 'org)
  :after ox
  :config
  (setq org-startup-indented t         ;indentación de cabeceras
        org-list-allow-alphabetical t  ;listas numeradas alfabéticamente
        org-attach-use-inheritance t 
        org-ellipsis " »"
        org-startup-folded t   ;Inicio todo plegado
        org-hide-emphasis-markers t)   ;Oculta los marcadores de formato
  
  (setq-default org-fold-catch-invisible-edits 'smart)
  (setq org-capture-templates
        '(("o" "TODO vencimiento" entry
           (file+headline "~/Sync/org/inbox.org" "tareas")
           "* TODO %?\nDEADLINE: %^{fecha tope}t")
          ("t" "TODO" entry
           (file+headline "~/Sync/org/inbox.org" "tareas")
           "* TODO %?\n")
          ("n" "Nota" entry
           (file+headline "~/Sync/org/inbox.org" "notas")
           "* %?\n%c")
          ("h" "Nota alerta" entry
           (file+headline "~/proyectos/hugo-sge/content-org/pie.org" "notas de alerta")
           (function org-hugo-new-subtree-post-capture-template)
           :prepend t)
          ("r" "Recordatorio" entry
           (file "~/Sync/org/recuerda.org")
           "* %?\n %^t")
          ("d" "Diario" entry
           (file+olp+datetree "~/Sync/org/diario.org")
           "** %?" :time-prompt t)
          ("R" "Reunion" entry
           (file+headline "~/Sync/org/inbox.org" "reuniones")
           "* %? :reunion:\n%^t %^{lugar}p")
          ("i" "Informe" entry
           (file+headline "~/Sync/org/informes.org" "2019")
           "* TODO %? %^g\n:PROPERTIES:\n
                              :solicitante: %^{solicitante|sge|sgt|vice}\n
                              :f_entrada: %^u\n
                              :f_respuesta: \n:end:\n")))
  (setq org-refile-targets (quote ((nil :maxlevel . 6)
                                   (org-agenda-files :maxlevel . 6))))
  (setq org-archive-location "archivador/%s_archive::")
  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "CANCEL")))
  (setq org-export-default-language "es")
  (setq org-export-date-timestamp-format "%d de %b %y")
  (setq org-export-allow-bind-keywords t)
  (setq org-icalendar-combined-agenda-file "/usr/local/var/www/org.ics")
  (setq org-icalendar-store-uid t)
  (setq org-icalendar-include-todo t)
  (setq org-icalendar-use-deadline '(event-if-todo))
  (setq org-icalendar-use-scheduled '(event-if-todo))
  (setq org-structure-template-alist
        '(("s" . "SRC")
          ("e" . "SRC emacs-lisp")
          ("E" . "EXAMPLE")
          ("q" . "QUOTE")
          ("v" . "VERSE")
          ("V" . "VERBATIM")
          ("c" . "CENTER")
          ("C" . "COMMENT")))
  
  (use-package org-tempo
    :ensure nil
    :init (require 'org-tempo))
  ;; Ejecuta bloques de código sin pedir confirmación
  (setq org-confirm-babel-evaluate nil)
  ;; Ejecuta los bloques de código al salvar el archivo
  (add-hook 'after-save-hook 'org-babel-tangle)
  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (ditaa . t)
     (calc . t)
     (lisp . t)
     (scheme . t)
     (haskell . t)
     (latex . t)
     (js . t)
     (makefile . t)
     (C . t)))
  (add-to-list 'org-file-apps '("\\.odt?\\'" . "xdg-open %s"))
  (add-to-list 'org-file-apps '("\\.ods?\\'" . "xdg-open %s"))
  (add-to-list 'org-file-apps '("\\.docx?\\'" . "xdg-open %s"))
  :bind
  ("C-c a" . org-agenda)
  ("C-c c" . org-capture)
  ("C-c b" . org-switchb)
  ("C-c t" . org-toggle-link-display)
  ("C-c l" . org-store-link)
  :hook
  (org-mode . visual-line-mode))
(elpaca-wait)

(use-package ob-async
  :after org)

(use-package org-mac-link
  :after org)

(add-hook 'org-mode-hook (lambda ()
                           (define-key org-mode-map
                                       (kbd "C-c g") 'org-mac-link-get-link)))

(require 'org-inlinetask)

(use-package org-contrib
  :config
  (require 'ox-extra)
  (ox-extras-activate '(ignore-headlines)))

(setq org-agenda-files '("~/Sync/org/inbox.org"
                         "~/Sync/org/recuerda.org"
                         "~/Sync/org/comisiones.org"
                         "~/Sync/org/inf-obs.org"
                         "~/Sync/org/fci.org"))
;; agenda en la ventana actual
(setq org-agenda-window-setup 'current-window)
;; avisa de tareas deadline en los próximos 7 días
(setq org-deadline-warning-days 7)
;; muestra tareas scheduled o deadline en los próximos 10 días
(setq org-agenda-span 10)
;; no muestra tareas scheduled si ya tienen fecha deadline
(setq org-agenda-skip-deadline-if-deadlline-is-shown t)
;; no colorea tareas con deadline inminente si ya son scheduled
(setq org-agenda-skip-deadline-prewarning-if-scheduled 'pre-scheduled)
;; no muestra tareas con fecha. implica que también ignora tareas
;; scheduled y deadlines
(setq org-agenda-todo-ignore-with-date t)

(setq org-agenda-custom-commands
      '(("h" "Agenda del día"
         ((agenda "" ((org-agenda-format-date "%a, %e %b %Y")))
          (alltodo "")))))

(use-package server
  :ensure nil
  :init
  (server-start))

(use-package exwm
  :config
  ;; Set the initial workspace number.
  (setq exwm-workspace-number 5)

  ;; When a windows class is updated, make class name the buffer name
  (add-hook 'exwm-update-class-hook
            (lambda () (exwm-workspace-rename-buffer exwm-class-name)))
  (add-hook 'exwm-update-title-hook
            (lambda ()
              (when (or (not exwm-instance-name)
                        (string-prefix-p "sum-awt-x11-" exwm-instance-name)
                        (string= "gimp" exwm-instance-name))
                (exwm-workspace-rename-buffer exwm-class-name))))

  ;; Global keybindings.
  (setq exwm-input-global-keys
        '(([?\s-&] . (lambda (command)
                       (interactive (list (read-shell-command "$ ")))
                       (start-process-shell-command command nil command)))

          ;; 's-r': Reset (to line-mode).
          ([?\s-r] . exwm-reset)

          ;; Bind 's-w' to switch to workspace interactively.
          ([?\s-w] . exwm-workspace-switch)

          ;; managing windows
          ([s-left] . windmove-left)
          ([s-right] . windmove-right)
          ([s-up] . windmove-up)
          ([s-down] . windmove-down)

          ;; swaping windows
          ([s-S-left] . windmove-swap-states-left)
          ([s-S-right] . windmove-swap-states-right)
          ([s-S-up] . windmove-swap-states-up)
          ([s-S-down] . windmove-swap-states-down)

          ;; managing window sizes
          ([s-M-left] . enlarge-window-horizontally)
          ([s-M-right] . shrink-window-horizontally)
          ([s-M-up] . shrink-window)
          ([s-M-down] . enlarge-window)

          ;; managing workspaces
          ;; ([?\M-0] . exwm-workspace--switch-map 0)
          ;; ([?\M-1] . exwm-workspace--switch-map 1)
          ;; ([?\M-2] . exwm-workspace--switch-map 2)

          ;; Focus window
          ([?\s- ] . other-window)
          ([?\s-1] . select-window-1)
          ([?\s-2] . select-window-2)
          ([?\s-3] . select-window-3)
          ([?\s-4] . select-window-4)
          ([?\s-5] . select-window-5)
          ([?\s-6] . select-window-6)
          ([?\s-7] . select-window-7)
          ([?\s-8] . select-window-8)))
          ;; ([s-f11] . ammz/go-previous-workspace)
          ;; ([s-f12] . ammz/go-next-workspace)

          ;;             `(,(kbd (format "s-%d" i)) .
          ;;               (lambda ()
          ;;                 (interactive)
          ;;                 (exwm-workspace-switch-create ,i))))
          ;;           (number-sequence 0 9))

          ;; Controles de audio
          ;; ([XF86AudioMute] . (lambda () (interactive) (start-process-shell-command "Mute" nil "amixer -q set Master toggle")))
          ;; ([XF86AudioLowerVolume] . (lambda () (interactive) (start-process-shell-command "Decrease" nil "amixer -q set Master 5%-")))
          ;; ([XF86AudioRaiseVolume] . (lambda () (interactive) (start-process-shell-command "Increase" nil "amixer -q set Master 5%+")))
          ;; ([XF86AudioPlay] . (lambda () (interactive) (start-process-shell-command "Play" nil "playerctl play-pause")))
          ;; ([XF86AudioNext] . (lambda () (interactive) (start-process-shell-command "Next" nil "playerctl next")))
          ;; ([XF86AudioPrev] . (lambda () (interactive) (start-process-shell-command "Previous" nil "playerctl previous")))
          ;; ([XF86AudioStop] . (lambda () (interactive) (start-process-shell-command "Stop" nil "playerctl stop")))

  (exwm-enable))

;; Enable exwm-randr before exwm-init (post-init setup) gets called
(use-package exwm-randr
  :ensure nil
  ;; :if ammz/exwm-enabled
  :after (exwm)
  :config
  (setq exwm-randr-workspace-output-plist '(0 "eDP-1" 1 "DP-1"))
  (add-hook 'exwm-randr-screen-change-hook 'exwm-change-screen-hook)

  (add-hook 'exwm-randr-screen-change-hook
            (lambda ()
              (start-process-shell-command
               "xrandr" nil "xrandr --dpi 108 --output eDP-1 --off --output DP-1 --auto")))

  (defun exwm-change-screen-hook ()
    (let ((xrandr-output-regexp "\n\\([^ ]+\\) connected ")
          default-output)
      (with-temp-buffer
        (call-process "xrandr" nil t nil)
        (goto-char (point-min))
        (re-search-forward xrandr-output-regexp nil 'noerror)
        (setq default-output (match-string 1))
        (forward-line)
        (if (not (re-search-forward xrandr-output-regexp nil 'noerror))
            (call-process "xrandr" nil nil nil "--output" default-output "--auto")
          (call-process
           "xrandr" nil nil nil
           "--dpi 108"
           "--output" (match-string 1) "--primary" "--auto"
           "--output" default-output "--off")
          (setq exwm-randr-workspace-output-plist (list 0 (match-string 1)))))))

  (exwm-randr-enable))

(defun ammz/run-in-background (command)
  (let ((command-parts (split-string command "[ ]+")))
    (apply #'call-process `(,(car command-parts) nil 0 nil ,@(cdr command-parts)))))

(ammz/run-in-background "nm-applet")
;; (ammz/run-in-background "pasystray")
;; (ammz/run-in-background "blueman-applet")

(defvar ammz/polybar-process nil
  "Guarda el proceso de la instancia Polybar que se ejecuta, si existe")

(defun ammz/kill-panel ()
  (interactive)
  (when ammz/polybar-process
    (ignore-errors
      (kill-process ammz/polybar-process)))
  (setq ammz/polybar-process nil))

(defun ammz/start-panel ()
  (interactive)
  (ammz/kill-panel)
  (setq ammz/polybar-process (start-process-shell-command "polybar" nil "polybar panel")))

;; Configura el escritorio en la primera carga
(add-hook 'exwm-init-hook #'ammz/start-panel)

(defun ammz/polybar-exwm-workspace ()
  "Devuelve el icono de un workspace"
  (pcase exwm-workspace-current-index
    (0 "")
    (1 "")
    (2 "")
    (3 "")
    (4 "")))

(defun ammz/send-polybar-hook (module-name hook-index)
  "Envía mensajes a polybar de tipo Hook para los módulos custom/ipc)"
  (start-process-shell-command "polybar-msg" nil (format "polybar-msg hook %s %s" module-name hook-index)))

(defun ammz/send-polybar-exwm-workspace ()
  (ammz/send-polybar-hook "exwm-workspace" 1))

;; Actualiza el indicador del planel cuando cambia el workspace
(add-hook 'exwm-workspacee-switch-hook #'ammz/send-polybar-exwm-workspace)

(use-package startup
  :ensure nil
  :no-require t
  :custom
  (user-mail-address "antonio.moreno.m@juntadeandalucia.es")
  (user-full-name "Antonio Moreno"))

;; Directorio de inicio tras C-x C-f
(setq default-directory "~/")

(push "/Users/ammz/.config/emacs/lisp" load-path)

(use-package files
  :ensure nil
  :config
  (setq backup-directory-alist '(("." . "~/.config/emacs/backups/")))
  (setq create-lockfiles nil))

(use-package emacs
  :ensure nil
  :init
  (setq frame-title-format '("%b -  %I"))
  (setq echo-keystrokes 0.25)           ; Muestra más rápido las combinaciones de teclas
  (setq auto-revert-verbose nil)        ; No muestra mensaje
  (setq ring-bell-function 'ignore)     ; Elimina los avisos sonoros
  (setq use-dialog-box nil)             ; Sin diálogos gráficos emergentes
:config
  (defalias 'yes-or-no-p 'y-or-n-p)     ; Responder solo con y/n
  ;; Se permiten ciertas acciones limitadas por Emacs
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)	; =C-x C-u=
  (put 'downcase-region 'disabled nil)	; =C-x C-l=
  (put 'dired-find-alternate-file 'disabled nil))

(use-package cus-edit
  :ensure nil
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (make-empty-file custom-file))
  (load custom-file 'noerror 'nomessage))

(use-package recentf
  :ensure nil
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-max-menu-items 10)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  :hook (after-init . recentf-mode))

(use-package savehist
  :ensure nil
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-save-minibuffer-history t)
  (setq history-length 25)
  (savehist-mode 1))

(use-package saveplace
  :ensure nil
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-mode 1))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))

(use-package autorevert
  :ensure nil
  :config
  (global-auto-revert-mode 1)
  (setq global-auto-revert-non-file-buffers nil))

(let ((screen-width (x-display-pixel-width))
      (screen-height (x-display-pixel-height)))
  (if (>= screen-width 3840)
      (setq font-size 145)
    (setq font-size 95))
  (set-face-attribute 'default nil :family "Iosevka Comfy" :weight 'normal :height font-size))

(set-face-attribute 'variable-pitch nil :family "Iosevka Comfy Motion Duo" :height 1.0)
(set-face-attribute 'fixed-pitch nil :family "Iosevka Nerd Font" :height 1.0)

(use-package startup
  :ensure nil
  :no-require t
  :config
  (setq inhibit-startup-screen t))

(use-package startup
  :ensure nil
  :no-require t
  :config
  (global-visual-line-mode 1))

(use-package emacs
  :ensure nil
  :config
  (defun prot/toggle-line-numbers ()
    "Toggles the display of line numbers.  Applies to all buffers."
    (interactive)
    (if (bound-and-true-p display-line-numbers-mode)
        (display-line-numbers-mode -1)
      (display-line-numbers-mode)))

  (defun prot/toggle-invisibles ()
    "Toggles the display of indentation and space characters."
    (interactive)
    (if (bound-and-true-p whitespace-mode)
        (whitespace-mode -1)
      (whitespace-mode)))
  :bind (("<f7>" . prot/toggle-line-numbers)
         ("<f8>" . prot/toggle-invisibles)))

(use-package paren
  :ensure nil
  :config
  (show-paren-mode)
  :custom
  (show-paren-style 'mixed))

(use-package emacs
  :ensure nil
  :config
  (setq display-time-24hr-format t)
  (display-time))

(use-package nerd-icons
  :custom
  ;; The Nerd Font you want to use in GUI
  ;; "Symbols Nerd Font Mono" is the default and is recommended
  ;; but you can use any other Nerd Font if you want
  (nerd-icons-font-family "Symbols Nerd Font Mono")
  )

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package emacs
  :ensure nil
  :config
  (setq-default cursor-type '(bar . 4))
  (setq-default cursor-face-highlight-mode 1)
  (setq-default cursor-in-non-selected-windows 'hollow)
  (setq x-stretch-cursor nil))

(use-package window-numbering
  :init
  (setq window-numbering-keymap
    (let ((map (make-sparse-keymap)))
      (define-key map (kbd "M-0") 'select-window-0)
      (define-key map (kbd "M-1") 'select-window-1)
      (define-key map (kbd "M-2") 'select-window-2)
      (define-key map (kbd "M-3") 'select-window-3)
      (define-key map (kbd "M-4") 'select-window-4)
      (define-key map (kbd "M-5") 'select-window-5)
      (define-key map (kbd "M-6") 'select-window-6)
      (define-key map (kbd "M-7") 'select-window-7)
      (define-key map (kbd "M-8") 'select-window-8)
      (define-key map (kbd "M-9") 'select-window-9)
      map))
  :config
  (window-numbering-mode))

(use-package winner
  :ensure nil
  :config (winner-mode))

(use-package help
  :ensure nil
  :custom (help-window-select t))

(use-package olivetti)

(use-package emacs
  :ensure nil
  :init
  (setq modus-themes-mode-line '(accented borderless)
        modus-themes-region '(bg-only)
        modus-themes-fringes nil
        modus-themes-bold-constructs t
        modus-themes-italic-constructs t
        modus-themes-tabs-accented t
        modus-themes-paren-match '(bold intense)
        modus-themes-prompts '(bold intense) 
        modus-themes-org-blocks 'tinted-background
        modus-themes-scale-headings t
        modus-themes-headings
        '((1 . (rainbow overline background 1.05))
          (2 . (rainbow background 1.05))
          (3 . (rainbow bold 1.0))
          (4 . (rainbow 1.0))))
  :config
  (load-theme 'modus-vivendi t)
  :bind ("<f5>" . modus-themes-toggle))

(use-package mu4e 
  :ensure nil
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)

  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.mail/junta")

  (setq mu4e-drafts-folder "/Drafts")
  (setq mu4e-sent-folder   "/Sent")
  (setq mu4e-trash-folder  "/Trash")

  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"     :key ?i)
          (:maildir "/Sent"      :key ?s)
          (:maildir "/Trash"     :key ?t)
          (:maildir "/Drafts"    :key ?d)))

  (setq mu4e-headers-fields
        '((:human-date . 12)
          (:flags . 10)
          (:from-or-to . 72)
          ;; (:mailing-list . 10)
          (:thread-subject)))

   (add-to-list 'mu4e-bookmarks
     ;; add bookmark for not trashed messages
     '( :name "Inbox"
        :key  ?m
        :query "maildir:/inbox AND NOT flag:trashed"))

  (defun ammz-mu4e-headers-function ()
    (visual-line-mode -1)
    (toggle-truncate-lines 1))

  :hook
  (mu4e-headers-mode . ammz-mu4e-headers-function)

  :custom
  (mu4e-use-fancy-chars t))

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers)
  ;; (consult-customize consult--source-buffer :hidden t :default nil)
  ;; (add-to-list 'consult-buffer-sources persp-consult-source)
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p"))
  :init
  (persp-mode t))

(use-package helpful
  :config
  ;; Note that the built-in `describe-function' includes both functions
  ;; and macros. `helpful-function' is functions only, so we provide
  ;; `helpful-callable' as a drop-in replacement.
  (global-set-key (kbd "C-h f") #'helpful-callable)
  (global-set-key (kbd "C-h v") #'helpful-variable)
  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-h x") #'helpful-command)

  ;; Lookup the current symbol at point. C-c C-d is a common keybinding
  ;; for this in lisp modes.
  (global-set-key (kbd "C-c C-d") #'helpful-at-point)
  ;; Look up *F*unctions (excludes macros).
  ;;
  ;; By default, C-h F is bound to `Info-goto-emacs-command-node'. Helpful
  ;; already links to the manual, if a function is referenced there.
  (global-set-key (kbd "C-h F") #'helpful-function))

(use-package pdf-tools
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :magic ("%PDF" . pdf-view-mode))

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq insert-directory-program "/usr/local/bin/gls"))

(use-package dired
  :ensure nil
  :config
  (setq dired-dwim-target t)
  (setq dired-listing-switches "-al --group-directories-first")
  :bind (:map dired-mode-map
              ("b" . dired-up-directory))
  :hook (dired-mode . dired-hide-details-mode))

(use-package dired-subtree
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<S-tab>" . dired-subtree-remove)))

(use-package nerd-icons-dired
  :hook
  (dired-mode . nerd-icons-dired-mode))

(use-package origami
  :bind ("C-c o" . origami-toggle-node)
  :hook (python-mode . origami-mode))

(use-package htmlize)

(use-package dockerfile-mode)

(use-package vertico
  :demand t
  :config (vertico-mode 1))

(use-package orderless
  :after vertico
  :config
  (setq completion-styles '(orderless basic partial-completion)
        completion-category-default nil))

(use-package marginalia
  :config
  ;; (setq marginalia-annotators
  ;;       '(marginalia-annotators-heavy
  ;;         marginalia-annotators-light))
  (marginalia-mode 1))

(use-package consult
  ;; :demand
  :config
  ;; (setq consult-line-numbers-widen t)
  (setq completion-in-region-function #'consult-completion-in-region)
  ;; (setq consult-async-input-debounce 0.5)
  ;; (setq consult-async-input-throttle 0.8)
  ;; (setq consult-narrow-key ">")
  ;; (setq consult-widen-key "<")
  :bind (("M-X" . consult-mode-command)
         ("M-y" . consult-yank-from-kill-ring)
         ("C-x b" . consult-buffer)
         ("M-s m" . consult-mark)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         :map minibuffer-local-completion-map
         ("<tab>" . minibuffer-force-complete)))

(use-package embark
  :bind
  (("C-." . embark-act)         ;; pick some comfortable binding
   ("C-;" . embark-dwim)        ;; good alternative: M-.
   ("C-h B" . embark-bindings)) ;; alternative for `describe-bindings'

  :init
  ;; Optionally replace the key help with a completing-read interface
  (setq prefix-help-command #'embark-prefix-help-command)

  :config
  ;; Hide the mode line of the Embark live/completions buffers
  (add-to-list 'display-buffer-alist
               '("\\`\\*Embark Collect \\(Live\\|Completions\\)\\*"
                 nil
                 (window-parameters (mode-line-format . none)))))

;; Consult users will also want the embark-consult package.
(use-package embark-consult
  :after (embark consult)
  :demand t ; only necessary if you have the hook below
  ;; if you want to have consult previews as you move around an
  ;; auto-updating embark collect buffer
  :hook
  (embark-collect-mode . consult-preview-at-point-mode))

(use-package corfu
 ;; Optional customizations
  :custom
  (corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (corfu-auto t)                 ;; Enable auto completion
  ;; (corfu-separator ?\s)          ;; Orderless field separator
  ;; (corfu-quit-at-boundary nil)   ;; Never quit at completion boundary
  ;; (corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  ;; (corfu-preview-current nil)    ;; Disable current candidate preview
  ;; (corfu-preselect 'prompt)      ;; Preselect the prompt
  ;; (corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (corfu-scroll-margin 5)        ;; Use scroll margin

  ;; Enable Corfu only for certain modes. See also `global-corfu-modes'.
  ;; :hook ((prog-mode . corfu-mode)
  ;;        (shell-mode . corfu-mode)
  ;;        (eshell-mode . corfu-mode))

  ;; Recommended: Enable Corfu globally.  This is recommended since Dabbrev can
  ;; be used globally (M-/).  See also the customization variable
  ;; `global-corfu-modes' to exclude certain modes.
  :init
  (global-corfu-mode))

(use-package cape
  ;; Bind prefix keymap providing all Cape commands under a mnemonic key.
  ;; Press C-c p ? to for help.
  :bind ("C-c p" . cape-prefix-map) ;; Alternative keys: M-p, M-+, ...
  ;; Alternatively bind Cape commands individually.
  ;; :bind (("C-c p d" . cape-dabbrev)
  ;;        ("C-c p h" . cape-history)
  ;;        ("C-c p f" . cape-file)
  ;;        ...)
  :init
  ;; Add to the global default value of `completion-at-point-functions' which is
  ;; used by `completion-at-point'.  The order of the functions matters, the
  ;; first function returning a result wins.  Note that the list of buffer-local
  ;; completion functions takes precedence over the global list.
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-keyword)
  (add-hook 'completion-at-point-functions #'cape-file)
  (add-hook 'completion-at-point-functions #'cape-elisp-block)
  ;; (add-hook 'completion-at-point-functions #'cape-history)
  ;; ...
)

(use-package transient)

(use-package magit
  :bind ("C-x g" . magit-status)
  :after transient)

(use-package edit-server)

(setq treesit-language-source-alist
      '((bash "https://github.com/tree-sitter/tree-sitter-bash")
        (cmake "https://github.com/uyha/tree-sitter-cmake")
        (c "https://github.com/tree-sitter/tree-sitter-c")
        (cpp "https://github.com/tree-sitter/tree-sitter-cpp")
        (css "https://github.com/tree-sitter/tree-sitter-css")
        (elisp "https://github.com/Wilfred/tree-sitter-elisp")
        (go "https://github.com/tree-sitter/tree-sitter-go")
        (html "https://github.com/tree-sitter/tree-sitter-html")
        (javascript "https://github.com/tree-sitter/tree-sitter-javascript" "master" "src")
        (json "https://github.com/tree-sitter/tree-sitter-json")
        (make "https://github.com/alemuller/tree-sitter-make")
        (markdown "https://github.com/ikatyang/tree-sitter-markdown")
        (python "https://github.com/tree-sitter/tree-sitter-python")
        (toml "https://github.com/tree-sitter/tree-sitter-toml")
        (tsx "https://github.com/tree-sitter/tree-sitter-typescript" "master" "tsx/src")
        (typescript "https://github.com/tree-sitter/tree-sitter-typescript" "master" "typescript/src")
        (typst "https://github.com/uben0/tree-sitter-typst")
        (yaml "https://github.com/ikatyang/tree-sitter-yaml")))

(setq major-mode-remap-alist
      '((c-mode . c-ts-mode)
        (c++-mode . c++-ts-mode)))
        ;; (yaml-mode . yaml-ts-mode)
        ;; (bash-mode . bash-ts-mode)
        ;; (c-or-c++-mode . c-or-c++-ts-mode)
        ;; (js2-mode . js-ts-mode)
        ;; (typescript-mode . typescript-ts-mode)
        ;; (json-mode . json-ts-mode)
        ;; (css-mode . css-ts-mode)
        ;; (python-mode . python-ts-mode)))

(use-package flycheck
  :defer t)
(add-hook 'prog-mode-hook 'flycheck-mode)

(use-package projectile
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

(setq org-ditaa-jar-path "/usr/share/java/ditaa/ditaa-0.11.jar")

(use-package haskell-mode)

;; (require 'haskell-interactive-mode)
;; (require 'haskell-process)
;; (setq haskell-process-type 'stack-ghci)
;; (require 'ob-haskell)
;; (add-hook 'haskell-mode-hook 'interactive-haskell-mode)

;; (use-package lsp-haskell)

(setq org-element-use-cache nil)

(use-package ox-hugo
  :config
  (with-eval-after-load 'ox
    (require 'ox-hugo))
  )

(setq js-indent-level 2)
;; (add-hook js-mode-hook #'eglot)

(use-package sly
  :init
  (setq inferior-lisp-program "sbcl")
  (setq sly-default-lisp 'sbcl)
  (setq org-babel-lisp-eval-fn 'sly-eval))

(use-package markdown-mode
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :mode ("\\.md$" . markdown-mode))

(require 'xscheme)

(use-package geiser-guile)

(use-package geiser-mit)

(use-package typst-ts-mode
  :ensure (:host sourcehut :repo "meow_king/typst-ts-mode" :files (:defaults "*.el"))
  :custom
  (typst-ts-mode-watch-options "--open")
  ;; (optional) checking typst grammar version needs it
  (typst-ts-mode-grammar-location (expand-file-name "tree-sitter/libtree-sitter-typst.so" user-emacs-directory)))

(use-package yaml-mode
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package emacs
  :ensure nil
  :config
  (setq system-time-locale "es_ES.UTF-8"))

(use-package parse-time
  :ensure nil
  :defer t
  :config
  (setq parse-time-months 
        (append '(("ene" . 1) ("feb" . 2) ("mar" . 3)
                  ("abr" . 4) ("may" . 5) ("jun" . 6)
                  ("jul" . 7) ("ago" . 8) ("sep" . 9)
                  ("oct" . 10) ("nov" . 11) ("dic" . 12)
                  ("enero" . 1) ("febrero" . 2)
                  ("marzo" . 3) ("abril" . 4) ("junio" . 6)
                  ("julio" . 7) ("agosto" . 8)
                  ("septiembre" . 9) ("octubre" . 10)
                  ("noviembre" . 11) ("diciembre" . 12))
                parse-time-months))
  (setq parse-time-weekdays
        (append '(("dom" . 0) ("lun" . 1) ("mar" . 2)
                  ("mié" . 3) ("jue" . 4) ("vie" . 5)
                  ("sáb" . 6) ("domingo" . 0) ("lunes" . 1)
                  ("martes" . 2) ("miércoles" . 3)
                  ("jueves" . 4) ("viernes" . 5)
                  ("sábado" . 6))
                parse-time-weekdays)))

(use-package calendar
  :ensure nil
  :custom
  (calendar-week-start-day 1)
  (calendar-day-name-array ["Domingo" "Lunes" "Martes" "Miércoles"
                            "Jueves" "Viernes" "Sábado"])
  (calendar-day-abbrev-array ["Dom" "Lun" "Mar" "Mié" "Jue" "Vie" "Sáb"])
  (calendar-day-header-array ["Do" "Lu" "Ma" "Mi" "Ju" "Vi" "Sá"])
  (calendar-month-name-array ["Enero" "Febrero" "Marzo" "Abril" "Mayo"
                              "Junio" "Julio" "Agosto" "Septiembre"
                              "Octubre" "Noviembre" "Diciembre"])
  (calendar-month-abbrev-array ["Ene" "Feb" "Mar" "Abr" "May" "Jun"
                                "Jul" "Ago" "Sep" "Oct" "Nov" "Dic"]))
