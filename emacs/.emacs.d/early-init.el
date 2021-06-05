;;; early-init.el --- early configuration file -*- lexical-binding: t; buffer-read-only: t; no-byte-compile: t -*-
;;;
;;; Comentario:
;;; Configuración de Emacs de Antonio Moreno.
;;; Este fichero ha sido generado automáticamente con `org-babel-tangle'.
;;; No cambie este fichero. Fichero generador: emacs-init.org en `user-emacs-directory'
;;;
;;; Código:

(defvar ammz/gc-cons-threshold gc-cons-threshold) ;800000
(defvar ammz/gc-cons-percentage gc-cons-percentage) ;0.1

(setq-default gc-cons-threshold (* 50 1000 1000)
              gc-cons-percentage 0.6)

(add-hook 'after-init-hook
          (lambda ()
            (setq gc-cons-threshold ammz/gc-cons-threshold)
            (setq gc-cons-percentage ammz/gc-cons-percentage)))

(unless (eq system-type 'darwin)		; Solo si no es Os X
  (menu-bar-mode -1))					; Deshabilita la menubar
(scroll-bar-mode -1)					; Deshabilita la scrollbar
(tool-bar-mode -1) 					; Deshabilita la toolbar

(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

(straight-use-package 'use-package)

;; Configure `use-package' prior to loading it.
(eval-and-compile
  (setq use-package-always-ensure nil)  ; ESSENTIAL for `straight.el'
  (setq use-package-always-defer nil)
  (setq use-package-always-demand nil)
  (setq use-package-expand-minimally nil)
  (setq use-package-enable-imenu-support t)
  (setq use-package-compute-statistics nil)
  ;; The following is VERY IMPORTANT.  Write hooks using their real name
  ;; instead of a shorter version: after-init ==> `after-init-hook'.
  ;;
  ;; This is to empower help commands with their contextual awareness,
  ;; such as `describe-symbol'.
  (setq use-package-hook-name-suffix nil))

(provide 'early-init)
;;; early-init.el ends here
