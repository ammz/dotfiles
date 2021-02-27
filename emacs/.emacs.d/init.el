;;; init.el --- Emacs main configuration file -*- lexical-binding: t; buffer-read-only: t; no-byte-compile: t -*-
;;;
;;; Comentario:
;;; Configuración de Emacs de Antonio Moreno.
;;; Este fichero ha sido generado automáticamente con `org-babel-tangle'.
;;; No cambie este fichero. Fichero generador: emacs-init.org en `user-emacs-directory'
;;;
;;; Código:

(require 'use-package)

(use-package startup
  :no-require t
  ;; :straight nil
  :custom
  (user-mail-address "ammz@deversorius.net")
  (user-full-name "Antonio Moreno"))

;; Directorio de inicio tras C-x C-f
(setq default-directory "~/")

(push "~/.emacs.d/lisp" load-path)

(use-package files
  ;; :straight nil
  :config
  (setq backup-directory-alist '(("." . "~/.emacs.d/backups/")))
  (setq create-lockfiles nil))

(use-package emacs
  :init
  (setq frame-title-format '("%b -  %I"))
  (setq echo-keystrokes 0.25)				; Muestra más rápido las combinaciones de teclas
  (setq auto-revert-verbose nil)				; No muestra mensaje
  (setq ring-bell-function 'ignore)			; Elimina los avisos sonoros
  :config
  (defalias 'yes-or-no-p 'y-or-n-p)		; Responder solo con y/n
  ;; Se permiten ciertas acciones limitadas por Emacs
  (put 'narrow-to-region 'disabled nil)
  (put 'upcase-region 'disabled nil)	; =C-x C-u=
  (put 'downcase-region 'disabled nil)	; =C-x C-l=
  (put 'dired-find-alternate-file 'disabled nil))

(use-package cus-edit
  ;; :straight nil
  :config
  (setq custom-file (expand-file-name "custom.el" user-emacs-directory))
  (unless (file-exists-p custom-file)
    (make-empty-file custom-file))
  (load custom-file))

(use-package recentf
  :config
  (setq recentf-save-file (expand-file-name "recentf" user-emacs-directory))
  (setq recentf-max-menu-items 10)
  (setq recentf-exclude '(".gz" ".xz" ".zip" "/elpa/" "/ssh:" "/sudo:"))
  :hook (after-init-hook . recentf-mode))

(use-package savehist
  :config
  (setq savehist-file (expand-file-name "savehist" user-emacs-directory))
  (setq savehist-save-minibuffer-history t)
  (savehist-mode 1))

(use-package saveplace
  :config
  (setq save-place-file (expand-file-name "saveplace" user-emacs-directory))
  (save-place-mode 1))

(use-package delsel
  :hook (after-init-hook . delete-selection-mode))

(use-package startup
  :no-require t
  ;; :straight nil
  :config
  (setq inhibit-startup-screen t))

(use-package emacs
  :config
  (set-fringe-mode 10))				; Give some breathing room

(use-package emacs
  :config
  (setq cursor-type 'bar)
  (setq cursor-in-non-selected-windows 'hollow)
  (setq x-stretch-cursor nil))

(use-package emacs
  :config
  ;; (set-frame-font "Hack 12" t t)
  (set-face-attribute 'default nil :font "Hack 12"))

(use-package delight
  :straight t
  :after use-package)

(use-package all-the-icons
  :straight t
  :if (display-graphic-p)
  :commands all-the-icons-install-fonts
  :init
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :disabled
  :straight t
  :if (display-graphic-p)
  :hook (dired-mode-hook . all-the-icons-dired-mode))

(use-package doom-modeline
  :straight t
  :init (doom-modeline-mode 1))

(use-package material-theme
  :straight t)

(load-theme 'material t)

(use-package doom-themes
  :disabled
  :straight t)

(use-package display-line-numbers
  :config
  (column-number-mode)
  (setq global-display-line-numbers-mode t)
  (setq display-line-numbers-grow-only t)
  ;; Deshabilita los números de línea en algunos modos
  (dolist (mode '(org-mode-hook
                  term-mode-hook
				  dired-mode-hook
                  shell-mode-hook
                  eshell-mode-hook))
    (add-hook mode (lambda () (display-line-numbers-mode 0)))))

(when (eq system-type 'darwin)
  (setq ns-pop-up-frames t)
  (setq ns-function-modifier (quote super))
  (setq ns-alternate-modifier (quote meta))
  (setq ns-command-modifier (quote control))
  (setq ns-control-modifier (quote super))
  (setq ns-right-command-modifier (quote meta))
  (setq ns-right-alternate-modifier (quote none)))

(use-package exec-path-from-shell
  :straight t
  :if (memq window-system '(mac ns x))
  :init
  (setq exec-path-from-shell-check-startup-files nil)
  ;; Ampliamos las variables que coge por defecto
  (exec-path-from-shell-copy-envs '("LANG" "WORKON_HOME"))
  :config
  (exec-path-from-shell-initialize))

(use-package winner
    :config (winner-mode))

(use-package help
  :straight nil
  :custom (help-window-select t))

(use-package window-numbering
  :straight t
  :config
  (window-numbering-mode))

(use-package visual-fill-column
  :straight t)

(defun ammz/org-mode-visual-fill ()
  (setq visual-fill-column-width 100
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook
  (org-mode-hook . ammz/org-mode-visual-fill)
  (ConTeXt-mode-hook . ammz/org-mode-visual-fill))

(use-package server
  :hook (after-init-hook . server-start))

(use-package newcomment
  :config
  (setq comment-empty-lines t)
  (setq comment-multi-line t)
  (setq comment-style 'multi-line)
  :bind ("C-;" . comment-dwim)
  )

(use-package origami
  :straight t
  :commands (origami-mode)
  :bind (:map origami-mode-map
              ("C-c o o" . origami-recursively-toggle-node)
              ("C-c o a" . origami-toggle-all-nodes)
              ("C-c o t" . origami-toggle-node)
              ("C-c o :" . origami-show-only-node)
              ("C-c o u" . origami-undo)
              ("C-c o U" . origami-redo)
              ("C-c o C-r" . origami-reset)
              )
  :config
  (setq origami-show-fold-header t)
  ;; The python parser currently doesn't fold if/for/etc. blocks, which is
  ;; something we want. However, the basic indentation parser does support
  ;; this with one caveat: you must toggle the node when your cursor is on
  ;; the line of the if/for/etc. statement you want to collapse. You cannot
  ;; fold the statement by toggling in the body of the if/for/etc.
  (add-to-list 'origami-parser-alist '(python-mode . origami-indent-parser))
  :init
  (add-hook 'prog-mode-hook 'origami-mode)
  )

(when (eq system-type 'darwin)
  (require 'ls-lisp)
  (setq insert-directory-program "/usr/local/bin/gls"))

(use-package dired
  ;; :custom ((dired-listing-switches "-agho --group-directories-first"))
  :bind (:map dired-mode-map
              ("b" . dired-up-directory)))

(use-package dired-subtree
  :straight t
  :after dired
  :bind (:map dired-mode-map
              ("<tab>" . dired-subtree-toggle)
              ("<S-tab>" . dired-subtree-remove)))

(use-package magit
  :straight t
  :bind (("C-c g" . magit-status)))

(use-package git-commit
  :after magit
  :custom
  (git-commit-fill-column 72)
  (git-commit-summary-max-length 50)
  (git-commit-known-pseudo-headers
   '("Signed-off-by"
     "Acked-by"
     "Modified-by"
     "Cc"
     "Suggested-by"
     "Reported-by"
     "Tested-by"
     "Reviewed-by"))
  (git-commit-style-convention-checks
   '(non-empty-second-line
     overlong-summary-line)))

(use-package magit-diff
  :after magit
  :custom
  (magit-diff-refine-hunk t))

(use-package uniquify
  :custom
  (uniquify-buffer-name-style 'post-forward-angle-brackets)
  (uniquify-strip-common-suffix t)
  (uniquify-after-kill-buffer-p t))

(use-package ibuffer
  :custom
  (ibuffer-expert t)
  (ibuffer-display-summary nil)
  (ibuffer-use-other-window nil)
  (ibuffer-show-empty-filter-groups nil)
  (ibuffer-movement-cycle nil)
  (ibuffer-default-sorting-mode 'filename/process)
  ;;;; NOTE built into the Modus themes
  ;; (ibuffer-deletion-face 'dired-flagged)
  ;; (ibuffer-marked-face 'dired-marked)
  (ibuffer-saved-filter-groups
   '(("Main"
      ("Directories" (mode . dired-mode))
      ("Org" (mode . org-mode))
      ("Programming" (or
                      (mode . c-mode)
                      (mode . conf-mode)
                      (mode . css-mode)
                      (mode . emacs-lisp-mode)
                      (mode . html-mode)
                      (mode . mhtml-mode)
                      (mode . python-mode)
                      (mode . ruby-mode)
                      (mode . scss-mode)
                      (mode . shell-script-mode)
                      (mode . yaml-mode)))
      ("Markdown" (mode . markdown-mode))
      ("Magit" (or
                (mode . magit-blame-mode)
                (mode . magit-cherry-mode)
                (mode . magit-diff-mode)
                (mode . magit-log-mode)
                (mode . magit-process-mode)
                (mode . magit-status-mode)))
      ("Apps" (or
                   (mode . bongo-playlist-mode)
                   (mode . mu4e-compose-mode)
                   (mode . mu4e-headers-mode)
                   (mode . mu4e-main-mode)
                   (mode . elfeed-search-mode)
                   (mode . elfeed-show-mode)
                   (mode . mu4e-view-mode)))
       ("Emacs" (or
                 (name . "^\\*Help\\*$")
                 (name . "^\\*Custom.*")
                 (name . "^\\*Org Agenda\\*$")
                 (name . "^\\*info\\*$")
                 (name . "^\\*scratch\\*$")
                 (name . "^\\*Backtrace\\*$")
                 (name . "^\\*Messages\\*$"))))))
  :config
  (defun prot/ibuffer-multi ()
    "Spawn a new instance of `ibuffer' and give it a unique name
based on the directory of the current buffer."
    (interactive)
    (let* ((parent (if (buffer-file-name)
                       (file-name-directory (buffer-file-name))
                     default-directory))
           (name (car (last (split-string parent "/" t)))))
      (split-window-sensibly)
      (other-window 1)
      (ibuffer t "*Ibuffer [new]*")
      (rename-buffer (concat "*Ibuffer: " name "*"))))
  :hook
  (ibuffer-mode-hook . (lambda ()
                     (ibuffer-switch-to-saved-filter-groups "Main")))
  :bind (("C-x C-b" . ibuffer)
         ("C-x C-S-b" . prot/ibuffer-multi) ; EXPERIMENTAL
         ))

(use-package desktop
  :disabled
  :config
  (setq desktop-dirname user-emacs-directory)
  (setq desktop-base-file-name "desktop")
  (setq desktop-missing-file-warning t)
  (setq desktop-restore-eager 3)
  :hook (after-init-hook . (lambda () (desktop-save-mode 1))))

(use-package emacs
    :bind
    (("C-." . repeat)
     ("<C-tab>" . other-window)			; ATENCION: No válido en mac
     ("C-ñ" . other-window)
     ;; ("M-}" . "}")
     ;; ("M-+" . "]")
     ))

(setq ammz/exwm-enabled (and (eq window-system 'x) ; Sólo sistemas con X Window
                           (seq-contains command-line-args "--use-exwm")))

(when ammz/exwm-enabled
  (require 'ammz-exwm))

(use-package orderless
  :straight t
  :demand
  :config
  (setq orderless-component-separator " +")
  ;; (setq orderless-matching-styles prot-orderless-default-styles)
  ;; (setq orderless-style-dispatchers
  ;;       '(prot-orderless-literal-dispatcher
  ;;         prot-orderless-initialism-dispatcher))
  ;; SPC should never complete: use it for `orderless' groups.
  :bind (:map minibuffer-local-completion-map
              ("SPC" . nil)))

(use-package marginalia
  :straight t
  :demand
  :config
  (setq marginalia-annotators
        '(marginalia-annotators-heavy
          marginalia-annotators-light))
  (marginalia-mode 1))

(use-package minibuffer
  :demand
  :config
  (setq completion-styles '(orderless partial-completion))
  (setq completion-category-defaults nil)
  (setq completion-cycle-threshold 3)
  (setq completion-flex-nospace nil)
  (setq completion-pcm-complete-word-inserts-delimiters t)
  (setq completion-pcm-word-delimiters "-_./:| ")
  (setq completion-show-help nil)
  (setq completion-ignore-case t)
  (setq-default case-fold-search t)   ; For general regexp

  (setq completions-format 'vertical)

  (setq read-buffer-completion-ignore-case t)
  (setq read-file-name-completion-ignore-case t)

  (setq enable-recursive-minibuffers t)
  (setq read-answer-short t)
  (setq resize-mini-windows t)
  (setq minibuffer-eldef-shorten-default t)

  (file-name-shadow-mode 1)
  (minibuffer-depth-indicate-mode 1)
  (minibuffer-electric-default-mode 1)

  ;; Defines, among others, aliases for common minibuffer commands to
  ;; Super-KEY.  Normally these should go in individual package
  ;; declarations, but their grouping here makes things easier to
  ;; understand.  Besides, they are related to the minibuffer.
  :bind (("s-b" . switch-to-buffer)
         ("s-B" . switch-to-buffer-other-window)
         ("s-f" . find-file)
         ("s-F" . find-file-other-window)
         ("s-d" . dired)
         ("s-D" . dired-other-window)
         :map completion-list-mode-map
         ("n" . next-line)
         ("p" . previous-line)
         ("f" . next-completion)
         ("b" . previous-completion)))

(use-package embark
  :straight t
  :demand
  :diminish embark-collect-zebra-minor-mode
  :config
  (setq embark-collect-initial-view-alist
        '((file . list)
          (buffer . list)
          (symbol . list)
          (line . list)
          (xref-location . list)
          (kill-ring . zebra)
          (t . list)))
  (setq embark-collect-live-update-delay 0.25)
  (setq embark-collect-live-initial-delay 0.5)

  ;; Please don't read too much into the names of those faces.  Just
  ;; green and yellow.
  (setq embark-action-indicator (propertize "Act" 'face 'success))
  (setq embark-become-indicator (propertize "Become" 'face 'warning))

  ;; ;; NOTE: I keep this around for when I do videos, otherwise I do not
  ;; ;; use it.
  (setq embark-action-indicator
        (lambda (map)
          (which-key--show-keymap "Embark" map nil nil 'no-paging)
          #'which-key--hide-popup-ignore-command)
        embark-become-indicator embark-action-indicator)
  :hook ((minibuffer-setup-hook . embark-collect-completions-after-input)
         (embark-post-action-hook . embark-collect--update-linked)
         (embark-collect-mode-hook . hl-line-mode))
  :bind (("C-," . embark-act)
         :map minibuffer-local-completion-map
         ("C-," . embark-act)
         ("C-." . embark-act-noexit)
         ("C->" . embark-become)
         ("M-q" . embark-collect-toggle-view) ; parallel of `fill-paragraph'
         ;; NOTE: to switch to the live collection buffer, I use
         ;; `prot-minibuffer-focus-mini-or-completions' which is bound
         ;; to "s-v".
         :map embark-collect-mode-map
         ("," . embark-act)
         ("." . embark-act-noexit)
         ("M-o" . embark-export)
         ("C-o" . embark-export)
         ("M-t" . toggle-truncate-lines)
         ("M-q" . embark-collect-toggle-view)
         :map embark-symbol-map
         ("." . embark-find-definition)
         ("k" . describe-keymap)))

(use-package consult
  :straight t
  :demand
  :config
  (setq consult-line-numbers-widen t)
  (setq completion-in-region-function #'consult-completion-in-region)
  (setq consult-async-input-debounce 0.5)
  (setq consult-async-input-throttle 0.8)
  (setq consult-narrow-key ">")
  (setq consult-widen-key "<")
  :bind (("M-X" . consult-mode-command)
         ("M-s m" . consult-mark)
         :map consult-narrow-map
         ("?" . consult-narrow-help)
         :map minibuffer-local-completion-map
         ("<tab>" . minibuffer-force-complete)))

(use-package dabbrev
  :after minibuffer ; read those as well
  :config
  (setq dabbrev-abbrev-char-regexp "\\sw\\|\\s_")
  (setq dabbrev-abbrev-skip-leading-regexp "[$*/=']")
  (setq dabbrev-backward-only nil)
  (setq dabbrev-case-distinction 'case-replace)
  (setq dabbrev-case-fold-search t)
  (setq dabbrev-case-replace 'case-replace)
  (setq dabbrev-check-other-buffers t)
  (setq dabbrev-eliminate-newlines t)
  (setq dabbrev-upcase-means-case-search t)

  ;; FIXME: this is not reliable
  (defun prot/dabbrev-completion ()
    "Expand current phrase or call `dabbrev-completion'."
    (interactive)
    (let* ((abbrev (dabbrev--abbrev-at-point))
           (ignore-case-p (dabbrev--ignore-case-p abbrev))
           (completion-list (dabbrev--find-all-expansions abbrev ignore-case-p)))
      (cond
       ((when (and (eq completion-list nil)
                   (not (eq last-repeatable-command 'mode-exit)))
          (insert " ")
          (dabbrev-expand 1)))
       (t
        (dabbrev-completion)))))

  :bind (("M-/" . dabbrev-expand)
         ("C-M-/" . prot/dabbrev-completion)
         ("s-/" . prot/dabbrev-completion)))

(use-package avy
  :straight t
  :bind
  ("C-c s" . avy-goto-char))

(use-package org
  :straight org-plus-contrib
  :config
  (setq org-startup-indented t)
  (setq org-attach-use-inheritance t)
  (setq org-list-allow-alphabetical t)

  ;; -------------------------------------------------------------------
  ;; agenda
  ;; -------------------------------------------------------------------
  (setq org-agenda-files '("~/Sync/org" "~/Sync/org/etea"))

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

  ;; -------------------------------------------------------------------
  ;; capture, refile, todo
  ;; -------------------------------------------------------------------
  (setq org-capture-templates
   '(("o" "TODO vencimiento" entry
      (file+headline "~/Sync/org/inbox.org" "tareas")
      "* TODO %?\ndeadline: %^{fecha tope}t")
     ("t" "TODO" entry
      (file+headline "~/Sync/org/inbox.org" "tareas")
      "* TODO %?\n")
     ("n" "nota" entry
      (file+headline "~/Sync/org/inbox.org" "notas")
      "* %?\n%c")
     ("h" "nota alerta" entry
      (file+headline "~/proyectos/hugo-sge/content-org/pie.org" "notas de alerta")
      (function org-hugo-new-subtree-post-capture-template)
      :prepend t)
     ("r" "recordatorio" entry
      (file "~/Sync/org/recuerda.org")
      "* %?\n %^t")
     ("d" "diario" entry
      (file+olp+datetree "~/Sync/org/diario.org")
      "** %?" :time-prompt t)
     ("r" "reunion" entry
      (file+headline "~/Sync/org/inbox.org" "reuniones")
      "* TODO %? :reunion:\n%^t %^{lugar}p")
     ("i" "informe" entry
      (file+headline "~/Sync/org/informes.org" "2019")
      "* TODO %? %^g\n:properties:\n
                          :solicitante: %^{solicitante|sge|sgt|vice}\n
                          :f_entrada: %^u\n
                          :f_respuesta: \n:end:\n")))

  ;; Scanea los target de refile en los archivos de la agenda. Aumenta
  ;; los niveles en los que encontrar posibles encabezamientos para
  ;; reasignar tareas. Por defecto son sólo 3.
  (setq org-refile-targets (quote ((nil :maxlevel . 6)
                                   (org-agenda-files :maxlevel . 6))))

  ;; Establece como archivar los ficheros org
  (setq org-archive-location "archivador/%s_archive::")

  (setq org-todo-keywords
        '((sequence "TODO" "NEXT" "WAITING" "|" "DONE" "CANCEL")))

  ;; -------------------------------------------------------------------
  ;; exportar
  ;; -------------------------------------------------------------------
  (setq org-export-default-language "es")
  (setq org-export-date-timestamp-format "%d de %b %y")

  ;; -------------------------------------------------------------------
  ;; icalendar
  ;; -------------------------------------------------------------------
  (setq org-icalendar-combined-agenda-file "/library/webserver/documents/org.ics")
  (setq org-icalendar-store-uid t)
  (setq org-icalendar-include-todo t)
  (setq org-icalendar-use-deadline '(event-if-todo))
  (setq org-icalendar-use-scheduled '(event-if-todo))

  ;; -------------------------------------------------------------------
  ;; latex
  ;; -------------------------------------------------------------------
  (setq org-latex-pdf-process '("latexmk -lualatex"))
  (setq org-latex-packages-alist
   '(("auto" "polyglossia" t ("xelatex" "lualatex"))
     ("" "fontspec" nil ("xelatex" "lualatex"))
     ("" "unicode-math" nil ("xelatex" "lualatex"))))
  (setq org-latex-clasess
   '(("article" "\\documentclass[12pt,a4paper]{article}"
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
      ("\\paragraph{%s}" . "\\paragraph*{%s}")
      ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))
     ("report" "\\documentclass[12pt,a4paper]{report}"
      ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))
     ("book" "\\documentclass[12pt,a4paper]{book}"
      ;; ("\\part{%s}" . "\\part*{%s}")
      ("\\chapter{%s}" . "\\chapter*{%s}")
      ("\\section{%s}" . "\\section*{%s}")
      ("\\subsection{%s}" . "\\subsection*{%s}")
      ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))

  ;; -------------------------------------------------------------------
  ;; codigo
  ;; -------------------------------------------------------------------
  ;; Ejecuta bloques de código sin pedir confirmación
  (setq org-confirm-babel-evaluate nil)
  ;; Ejecuta los bloques de código al salvar el archivo
  (add-hook 'after-save-hook 'org-babel-tangle)

  ;; -------------------------------------------------------------------
  ;; general
  ;; -------------------------------------------------------------------
  (setq org-structure-template-alist
   '(("s" . "SRC")
     ("e" . "SRC emacs-lisp")
     ("E" . "EXAMPLE")
     ("q" . "QUOTE")
     ("v" . "VERSE")
     ("V" . "VERBATIM")
     ("c" . "CENTER")
     ("C" . "COMMENT")))

  (setq org-ellipsis " »")

  (org-babel-do-load-languages
   'org-babel-load-languages
   '((emacs-lisp . t)
     (python . t)
     (shell . t)
     (R . t)
     (calc . t)
     (gnuplot . t)
     (haskell . t)
     (latex . t)
     (org . t)
     (js . t)
     (java . t)
     (makefile . t)
     (C . t)))

  ;; -------------------------------------------------------------------
  ;; org link
  ;; -------------------------------------------------------------------
  (add-to-list 'org-file-apps '("\\.odt?\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.ods?\\'" . "open %s"))
  (add-to-list 'org-file-apps '("\\.docx?\\'" . "open %s"))

  ;; añade un tipo de link para que emacs abra el fichero en Curio
  (defun open-curio (path)
    (shell-command (concat "open -a /Applications/Curio.app '" path "'")))
  (org-add-link-type "curio" 'open-curio)

  :bind
  (("C-c a" . org-agenda)
   ("C-c c" . org-capture)
   ("C-c b" . org-switchb)
   ("C-c t" . org-toggle-link-display)
   ("C-c l" . org-store-link))

  :hook
  ((org-mode-hook . (lambda () (visual-line-mode)))
   (org-mode-hook . (lambda () (scroll-bar-mode -1)))))


(use-package org-mac-link
  :after org
  :hook
  (org-mode-hook . (lambda ()
                (define-key org-mode-map
                  (kbd "C-c g") 'org-mac-grab-link))))

(use-package ob-async
  :straight t)

(use-package org-tempo
  :after org)

(require 'org-inlinetask)

(use-package ox-hugo
  :straight t
  :config
  (with-eval-after-load 'ox
    (require 'ox-hugo))
  )

(defun org-hugo-new-subtree-post-capture-template ()
  "Returns `org-capture' template string for new Hugo post.
See `org-capture-templates' for more information."
  (let* (;; http://www.holgerschurig.de/en/emacs-blog-from-org-to-hugo/
         (date (format-time-string (org-time-stamp-format :long :inactive) (org-current-time)))
         (title (read-from-minibuffer "Post Title: ")) ;Prompt to enter the post title
         (fname (org-hugo-slug title)))
    (mapconcat #'identity
               `(
                 ,(concat "* " title)
                 ":PROPERTIES:"
                 ,(concat ":EXPORT_HUGO_BUNDLE: " fname)
                 ,(concat ":EXPORT_DATE: " date) ;Enter current date and time
                 ":END:"
                 "#+begin_src yaml :front_matter_extra t"
                 "portada: 's'"
                 "resources:"
                 "  - src: "
                 "#+end_src"
                 "%?\n")                ;Place the cursor here finally
               "\n")))

(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

(use-package lsp-mode
  :commands (lsp lsp-deferred)
  :hook (lsp-mode-hook . efs/lsp-mode-setup)
  :init
  (setq lsp-keymap-prefix "C-c l")
  :config
  (setq lsp-enable-which-key-integration t))

(use-package lsp-ui
  :straight t
  :hook (lsp-mode-hook . lsp-ui-mode)
  :custom
  (lsp-ui-doc-position 'bottom))

(use-package lsp-treemacs
  :straight t
  :after lsp)

(use-package dap-mode
  :straight t)

(use-package modern-cpp-font-lock
  :straight t
  :init
  (eval-when-compile
    ;; Silence missing function warnings
    (declare-function modern-c++-font-lock-global-mode
                      "modern-cpp-font-lock.el"))
  :config
  (modern-c++-font-lock-global-mode t)
  )

(defvar my:compile-command "g++ -std=c++11")
(use-package cc-mode
  :init
  (add-to-list 'auto-mode-alist '("\\.tpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.hpp\\'" . c++-mode))
  (add-to-list 'auto-mode-alist '("\\.cpp\\'" . c++-mode))
  :config
  (define-key c++-mode-map (kbd "C-c C-c") 'compile)
  (define-key c++-mode-map (kbd "C-c C-k") 'kill-compilation)
  (setq compile-command my:compile-command)

  (use-package google-c-style
    :straight t
    :config
    ;; This prevents the extra two spaces in a namespace that Emacs
    ;; otherwise wants to put... Gawd!
    (add-hook 'c-mode-common-hook 'google-set-c-style)
    ;; Autoindent using google style guide
    (add-hook 'c-mode-common-hook 'google-make-newline-indent)
    )
  )

(use-package haskell-mode
  :straight t)

(use-package elpy
  :straight t
  :init
  (elpy-enable)
  :hook (python-mode-hook . lsp-deferred))

(use-package web-mode
  :straight t
  :mode ("\\.html$" . web-mode)
  )

(use-package php-mode
  :straight t
  :mode ("\\.php$" . php-mode)
  :init
  (setq php-mode-coding-style (quote psr2))
  (setq php-search-documentation-browser-function 'eww-browse-url)
  (setq php-style-delete-trailing-whitespace 1)
  )

(use-package typoscript-mode
  :straight
  :mode ("\\.typoscript" . typoscript-mode))

(use-package tex
  :straight auctex
  :custom
  (TeX-auto-save t)
  (TeX-parse-self t)
  :config
  (setq-default TeX-master nil)
  (setq-default TeX-engine 'luatex)
  (add-hook 'TeX-mode-hook
        (lambda ()
          (local-set-key (kbd "º") "\\"))) ;muy útil para LaTeX
  )

(setq ConTeXt-Mark-version "IV")

(add-hook 'TeX-mode-hook  #'visual-line-mode)
;; (add-hook 'TeX-mode-hook '(lambda () (switch-theme 'material)))

;;; Useful AUCTeX setup for ConTeXt (for your .emacs)
;;; Sanjoy Mahajan (sanjoy@mrao.cam.ac.uk), 2006-04-20.  No copyright.
;;;
;;; With recent AUCTeX (11.50 or later), editing ConTeXt files should
;;; just work, but I use the following elisp as well.

; the AUCTeX manual recommends these settings (ya configurado más arriba)
;(setq TeX-parse-self t)			; Enable parse on load.
;(setq TeX-auto-save t)			; Enable parse on save.

; for outline views (hide/show sections, chapters, etc.)
(add-hook 'TeX-mode-hook '(lambda () (TeX-fold-mode 1)))
(add-hook 'TeX-mode-hook '(lambda () (outline-minor-mode 1)))
; make PDF by default (can toggle with C-c C-t C-p
(add-hook 'TeX-mode-hook '(lambda () (TeX-PDF-mode 1)))
; these math abbrevs (` as prefix char) are also useful in TeX/ConTeXt files
(require 'latex)			; defines LaTeX-math-mode
(add-hook 'TeX-mode-hook 'LaTeX-math-mode)
; Emacs help for \label, \ref, \cite.  Normally used only with
; LaTeX-mode but also useful with plain TeX + eplain and with ConTeXt, so:
(setq reftex-plug-into-AUCTeX t)
(add-hook 'TeX-mode-hook 'reftex-mode)

;; (defun insert-balanced (left right)
;;   "Insert a left, right delimiter pair and be poised to type inside them."
;;   (interactive)
;;   (insert left)
;;   (save-excursion
;;     (insert right)))

; When star
; t-context-math() is bound to $:
; Typing one $ gets you $$ with the insertion point between them.
; Typing a second $ turns the $$ into ConTeXt's form for displayed math:
;
;   \placeformula\startformula
;   [blank line with insertion point at beginning]
;   \stopformula
;
; Delete the \placeformula if you don't want equations numbered automatically.

;; (defun start-context-math ()
;;   (interactive)
;;   (let* ((start (max (point-min) (- (point) 1)))
;;          (stop  (min (point-max) (+ (point) 1))))
;;     ; if in the middle of a $$, turn inline math into context display math
;;     (if (equal "$$" (buffer-substring-no-properties start stop))
;;         (progn
;;           (delete-region start stop)	;get rid of the $$
;;           ; delete preceding spaces, if any
;;           (while (and (< (point-min) (point))
;;                       (equal (buffer-substring-no-properties (- (point) 1)
;;                                                              (point))
;;                              " "))
;;             (backward-delete-char 1))
;;           ; delete a preceding newline, if any
;;           (if (equal (buffer-substring-no-properties (- (point) 1)
;;                                                      (point))
;;                      "\n")
;;             (backward-delete-char 1))
;;           ; ConTeXt's display math with automatic equation numbering
;;           (insert "\n\\startformula\n")
;;           (save-excursion (insert "\n\\stopformula")))
;;       ; else: just doing inline math
;;       (insert-balanced ?\$ ?\$))))

;; ; automatically insert right delimiter for $, {, [, and ( and be
;; ; poised to type inside them.
;; (add-hook 'TeX-mode-hook
;;           '(lambda ()
;;              (local-set-key "$"
;;                             '(lambda ()
;;                                (interactive)
;;                                (insert-balanced ?\$ ?\$)))
;;              (local-set-key "{"
;;                             '(lambda ()
;;                                (interactive)
;;                                (insert-balanced ?\{ ?\})))
;;              (local-set-key "["
;;                             '(lambda ()
;;                                (interactive)
;;                                (insert-balanced ?\[ ?\])))
;;              (local-set-key "("
;;                             '(lambda ()
;;                                (interactive)
;;                                (insert-balanced ?\( ?\))))))

;; ; For ConTeXt mode, inserting two $ signs needs to behave specially
;; (add-hook 'ConTeXt-mode-hook
;;           '(lambda ()
;;              (local-set-key "$" 'start-context-math)))

;; ; The TeX-format-list from AUCTeX's tex.el (v11.82) with a few more
; ConTeXt-specific patterns.  I've submitted it to the AUCTeX lists,
; so later versions should have them automatically and you won't need
; this regexp mess in your .emacs
;
(setq TeX-format-list
  '(("JLATEX" japanese-latex-mode
     "\\\\\\(documentstyle\\|documentclass\\)[^%\n]*{\\(j[s-]?\\|t\\)\\(article\\|report\\|book\\|slides\\)")
    ("JTEX" japanese-plain-tex-mode
     "-- string likely in Japanese TeX --")
    ("AMSTEX" ams-tex-mode
     "\\\\document\\b")
    ("CONTEXT" context-mode
     "\\(\\\\\\(start\\(text\\|project\\|environment\\|product\\|typing\\|component\\|tekst\\)\\)\\|%.*?interface=\\)")
    ("LATEX" latex-mode
     "\\\\\\(begin\\|section\\|chapter\\|documentstyle\\|documentclass\\)\\b")
    ("TEX" plain-tex-mode ".")))

(defun context-insert-nattab (rows columns)
  ;; Johan Sandblom 2006-01-28
  "Insert a TABLE skeleton"
  (interactive "nNumber of rows: \nnNumber of columns: \n")
  (newline)
  (insert "\\bTABLE\n\\setupTABLE\[\]\n")
  ;; First a TABLE header
  (insert "\\bTABLEhead\n\\bTR\\bTH \\eTH\n")
  (let ((column 1))
    (while (< column (- columns 1))
      (insert "    \\bTH \\eTH\n")
      (setq column (1+ column))))
  (insert "    \\bTH \\eTH\\eTR\n\\eTABLEhead\n\\bTABLEbody\n")
  ;; The rows and columns
  (let ((row 1))
    (while (<= row rows)
      (insert "\\bTR\\bTD \\eTD\n")
      ;; The let expression makes sure that each loop starts at the
      ;; right place
      (let ((column 1))
        (while (< column (- columns 1))
          (insert "    \\bTD \\eTD\n")
          (setq column (1+ column)))
        (insert "    \\bTD \\eTD\\eTR\n")
        (setq row (1+ row))))
    (insert "\\eTABLEbody\n\\eTABLE\n")))

(defun context-insert-nattab-row (columns)
 "Insert a row in a TABLE"
 (interactive "nNumber of columns: \n")
 (newline)
 (insert "\\bTR\\bTD \\eTD\n")
 (let ((column 1))
   (while (< column (- columns 1))
     (insert "    \\bTD \\eTD\n")
     (setq column (1+ column)))
   (insert "    \\bTD \\eTD\\eTR\n")))

(defun context-insert-nattab-column (&optional arg)
 "Insert a column in a TABLE"
 (interactive "P")
 (insert "\\bTD \\eTD")
 (indent-for-tab-command)
 (newline)
 (backward-char 5))



(add-hook 'ConTeXt-mode-hook
         '(lambda ()
            (local-set-key "\C-cnr" 'context-insert-nattab-row)
            (local-set-key "\C-cnc" 'context-insert-nattab-column)
            (local-set-key "\C-cnn" 'context-insert-nattab)))

(use-package markdown-mode
  :straight
  :config
  (setq markdown-fontify-code-blocks-natively t)
  :mode ("\\.md$" . markdown-mode))

(use-package yaml-mode
  :straight
  :mode (("\\.yml$" . yaml-mode)
         ("\\.yaml$" . yaml-mode)))

(use-package css-mode
  :config
  (setq css-fontify-colors nil))

(use-package yasnippet
  :straight t
  :config
  (yas-reload-all)
  (yas-global-mode))

(use-package yasnippet-snippets
  :straight t)

(use-package editorconfig
  :straight t
  :config
  (editorconfig-mode 1))

(use-package hl-line
  :config
  (setq global-hl-line-mode t))

(use-package subword
  :delight
  :hook (prog-mode-hook . subword-mode))

(use-package emacs
  :hook (text-mode-hook . (lambda ()
                       ;; (turn-on-auto-fill)
                       (delight 'auto-fill-function nil t)
                       (setq adaptive-fill-mode t))))

(use-package paren
  :custom
  (show-paren-style 'mixed)
  (show-paren-when-point-in-periphery t)
  (show-paren-when-point-inside-paren t)
  :config
  (show-paren-mode 1))

(use-package electric
  :custom
  (electric-pair-inhibit-predicate 'electric-pair-default-inhibit)
  (electric-pair-skip-self 'electric-pair-default-skip-self)
  (electric-quote-context-sensitive t)
  (electric-quote-paragraph nil)
  (electric-quote-string nil)
  :config
  (electric-indent-mode 1)
  (electric-pair-mode 1))

(use-package emacs
  :init
  (setq-default tab-always-indent 'complete)
  (setq-default tab-width 4))

(use-package emacs
  :custom
  (c-default-style "bsd")
  (c-basic-offset 4))

(use-package emacs
  :custom
  (fill-column 72)
  (sentence-end-double-space t)
  (sentence-end-without-period nil)
  (colon-double-space nil)
  :config
  (column-number-mode 1))

(use-package emacs
  :hook (before-save-hook . delete-trailing-whitespace))

(use-package emacs
  :config
  (setq scroll-preserve-screen-position t)
  (setq scroll-conservatively 1)
  (setq scroll-margin 0))

(use-package emacs
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

(use-package which-key
  :straight t
  :delight
  :config
  (setq which-key-show-early-on-C-h t)
  (setq which-key-idle-delay 10000)
  (setq which-key-idle-secondary-delay 0.05)
  (setq which-key-popup-type 'side-window)
  (setq which-key-show-prefix 'echo)
  (setq which-key-max-display-columns 3)
  (setq which-key-separator " ")
  (setq which-key-special-keys '("SPC" "TAB" "RET" "ESC" "DEL"))
  (which-key-mode 1))

(use-package emacs
  :custom
  (system-time-locale (getenv "LANG")))

(use-package parse-time
  :custom
  (parse-time-months '(("ene" . 1) ("feb" . 2) ("mar" . 3)
                       ("abr" . 4) ("may" . 5) ("jun" . 6)
                       ("jul" . 7) ("ago" . 8) ("sep" . 9)
                       ("oct" . 10) ("nov" . 11) ("dic" . 12)
                       ("enero" . 1) ("febrero" . 2)
                       ("marzo" . 3) ("abril" . 4) ("junio" . 6)
                       ("julio" . 7) ("agosto" . 8)
                       ("septiembre" . 9) ("octubre" . 10)
                       ("noviembre" . 11) ("diciembre" . 12)))
  (parse-time-weekdays '(("dom" . 0) ("lun" . 1) ("mar" . 2)
                         ("mié" . 3) ("jue" . 4) ("vie" . 5)
                         ("sáb" . 6) ("domingo" . 0) ("lunes" . 1)
                         ("martes" . 2) ("miércoles" . 3)
                         ("jueves" . 4) ("viernes" . 5)
                         ("sábado" . 6))))

(use-package calendar
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
