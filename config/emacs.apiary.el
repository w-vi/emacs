;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Startup and Behavior Controls
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(setq load-path (cons "~/emacs" load-path))
(setq custom-file "~/emacs/custom.apaiary.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Machine Specific Configuration Section
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; go FULLSCREEN
(defun toggle-fullscreen ()
  (interactive)
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_VERT" 0))
  (x-send-client-message nil 0 nil "_NET_WM_STATE" 32
                         '(2 "_NET_WM_STATE_MAXIMIZED_HORZ" 0))
)

(when (eq window-system 'x)
  (toggle-fullscreen))


;; start server if not runing
(load "server")
(unless (server-running-p) (server-start))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Load the real init
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wvi-init)

;; I want to have same path as in shell
(require 'exec-path-from-shell)
(when (memq window-system '(x))
  (progn
    (exec-path-from-shell-initialize)
    (exec-path-from-shell-copy-env "PYTHONPATH")
    (exec-path-from-shell-copy-env "GOPATH")
    (exec-path-from-shell-copy-env "GOROOT")))

(require 'fill-column-indicator)
(setq-default fci-rule-column 80)
(setq fci-rule-width 5)
(add-hook 'c-mode-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)
(add-hook 'python-mode-hook 'fci-mode)
(add-hook 'makefile-mode-hook 'fci-mode)
(add-hook 'autoconf-mode-hook 'fci-mode)
(add-hook 'js2-mode-hook 'fci-mode)
(add-hook 'rst-mode-hook 'fci-mode)

(defun wvi-cedet-hook ()
  (setq ac-sources (append '(ac-source-semantic) ac-sources))
  (local-set-key (kbd "RET") 'newline-and-indent)
  (semantic-mode 1))
(add-hook 'c-mode-common-hook 'wvi-cedet-hook)

(add-to-list 'load-path "/usr/share/emacs/site-lisp/ecb")
(setq-default ecb-tip-of-the-day nil)
(require 'ecb)



;; TRAMP
(require 'tramp)
(setq tramp-default-method "ssh")

;; GYP
(setq load-path (cons "~/src/gyp/tools/emacs" load-path))
(require 'gyp)

;; Coffee-mode
(defun wvi-coffee-compile-and-open ()
  (interactive)
  (let ((file-name (coffee-compiled-file-name (buffer-file-name))))
    (coffee-compile-file)
    (if (not (find-buffer-visiting file-name))
      (switch-to-buffer (find-file-noselect file-name))
      (switch-to-buffer (find-buffer-visiting file-name)))))

(eval-after-load "coffee-mode"
  '(progn
     (defvar wvi-coffee-other-file-alist
       '(("\\.coffee\\'" (".js"))
         ("\\.js\\'" (".coffee"))))
     (define-key coffee-mode-map (kbd "C-c C-f") 'wvi-coffee-compile-and-open)
     (setq ff-other-file-alist 'wvi-coffee-other-file-alist)))
