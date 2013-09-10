;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Startup and Behavior Controls 
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'cl)

(setq load-path (cons "~/emacs" load-path)) 

(setq custom-file "~/emacs/custom-geewa.el")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Machine Specific Configuration Section
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(set-default-font "Menlo")

;;Bug off with  new frames 
(setq ns-pop-up-frames nil)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Load the real init
;; 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require 'wvi-init)

;; ipython path
(setq python-shell-interpreter "/Library/Frameworks/Python.framework/Versions/2.7/bin/ipython" )

;; Agenda files on this machine
(setq org-agenda-files (append "~/geewa/notification/todo.org"))

;; FLYSPELL ISPELL -- cocoAspell dicts
(add-to-list 'exec-path "/usr/local/bin")
(setq ispell-program-name "aspell")
(setq ispell-list-command "list")
(setq ispell-really-aspell  t)
(setq ispell-dictionary-alist
      '((nil
	 "[A-Za-z]" "[^A-Za-z]" "[']" nil
	 ("-B" "-d" "english" "--dict-dir"
	  "/Library/Application Support/cocoAspell/aspell6-en-6.0-0")
	 nil iso-8859-1)))

(global-set-key (kbd "<f12>") 'ispell-word)
(global-set-key (kbd "C-S-<f12>") 'flyspell-mode)
(global-set-key (kbd "C-M-<f12>") 'flyspell-buffer)
(global-set-key (kbd "C-<f12>") 'flyspell-check-previous-highlighted-word)
(defun flyspell-check-next-highlighted-word ()
  "Custom function to spell check next highlighted word"
  (interactive)
  (flyspell-goto-next-error)
  (ispell-word)
  )
(global-set-key (kbd "M-<f12>") 'flyspell-check-next-highlighted-word)

