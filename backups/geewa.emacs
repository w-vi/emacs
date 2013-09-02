(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(confirm-kill-emacs (quote y-or-n-p))
 '(cua-mode t nil (cua-base))
 '(debug-on-error t)
 '(menu-bar-mode nil)
 '(save-place t nil (saveplace))
 '(show-paren-mode t)
 '(text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))
 '(tool-bar-mode nil)
 '(uniquify-buffer-name-style (quote forward) nil (uniquify))
 '(visual-line-mode t t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(font-lock-builtin-face ((((class color) (min-colors 8)) (:foreground "violet" :weight bold))))
 '(font-lock-function-name-face ((((class color) (min-colors 8)) (:foreground "DarkOrange3" :weight bold))))
 '(link ((((class color) (background light)) (:foreground "blue" :underline t))))
 '(minibuffer-prompt ((t (:foreground "darkgreen")))))

;;; GENERAL SETTINGS

(set-default-font "Menlo")

;;Bug off with  new frames 
(setq ns-pop-up-frames nil)

;; line number in all files, all the time
(global-linum-mode 1)

;; don't show the startup screen
(setq inhibit-startup-screen t)

;; Default Emacs does not scroll pages smoothly with down arrow key.
;; It tries to jump a page-worth.
;; See this for advice on preventing that 
;; http://stackoverflow.com/questions/3631220/fix-to-get-smooth-scrolling-in-emacs
(setq redisplay-dont-pause t)
(setq scroll-conservatively 20)
(setq scroll-margin 2)
(setq scroll-preserve-screen-position 1)

;; programming conveniences:
;;(show-paren-mode t) ; light-up matching parens
(global-font-lock-mode t) ; turn on syntax highlight
(setq text-mode-hook (quote (turn-on-auto-fill text-mode-hook-identify)))


;; FUNCTIONS 

;; Duplicate line
(defun duplicate-line (arg)
  "Duplicate current line, leaving point in lower line."
  (interactive "*p")

  ;; save the point for undo
  (setq buffer-undo-list (cons (point) buffer-undo-list))

  ;; local variables for start and end of line
  (let ((bol (save-excursion (beginning-of-line) (point)))
	eol)
    (save-excursion

      ;; don't use forward-line for this, because you would have
      ;; to check whether you are at the end of the buffer
      (end-of-line)
      (setq eol (point))

      ;; store the line and disable the recording of undo information
      (let ((line (buffer-substring bol eol))
	    (buffer-undo-list t)
	    (count arg))
	;; insert the line arg times
	(while (> count 0)
	  (newline) ;; because there is no newline in 'line'
	  (insert line)
	  (setq count (1- count)))
	)

      ;; create the undo information
      (setq buffer-undo-list (cons (cons eol (point)) buffer-undo-list)))
    ) ; end-of-let

  ;; put the point in the lowest line and return
  (next-line arg))


;;Prefer horizontal splitting
(defun split-window-prefer-horizonally (window)
           "If there's only one window (excluding any possibly active
         minibuffer), then split the current window horizontally."
           (if (and (one-window-p t)
                    (not (active-minibuffer-window)))
               (let ((split-height-threshold nil))
                 (split-window-sensibly window))
            (split-window-sensibly window)))
(setq split-window-preferred-function 'split-window-prefer-horizonally)

;; ALIASES
(defalias 'yes-or-no-p 'y-or-n-p)


;; PROGRAMMING STUFF

;;C style conventions
(setq c-default-style "bsd"
c-basic-offset 4)

;;spaces instead of TAB in C/C++ mode
(setq c-mode-hook
      (function (lambda ()
		  (setq indent-tabs-mode nil)
		  (setq c-indent-level 4))))
(setq objc-mode-hook
      (function (lambda ()
		  (setq indent-tabs-mode nil)
		  (setq c-indent-level 4))))
(setq c++-mode-hook
      (function (lambda ()
		  (setq indent-tabs-mode nil)
		  (c-set-style "ellemtel")
		  (setq c-indent-level 4))))
;;.h are most of the C++ files in my case so use that as default
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; MODS

;; ELPA repos
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")))

;; packages dir
(add-to-list 'load-path "~/.emacs.d/site-lisp/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/yasnippet/")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete-clang-async")
(add-to-list 'load-path "~/.emacs.d/site-lisp/auto-complete")
(add-to-list 'load-path "~/.emacs.d/elpa/autopair-20121123.1829")

;;IDO-MODE
(ido-mode t)
(setq ido-enable-flex-matching t)
(setq ido-everywhere t)

;;ORG-MODE
(setq org-indent-mode t)
(setq org-agenda-files (append "~/geewa/notification/todo.org"))

;;WINNER MODE
(when (fboundp 'winner-mode)
      (winner-mode t))

;;UNDO TREE
(require 'undo-tree)
(global-undo-tree-mode)

;; HIDE/SHOW
(add-hook 'c-mode-common-hook #'(lambda () (hs-minor-mode)))
(add-hook 'python-mode-hook #'(lambda () (hs-minor-mode)))
(add-hook 'lisp-mode-hook #'(lambda () (hs-minor-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (hs-minor-mode)))

;;GLOBAL gtags
(autoload 'gtags-mode "gtags" "" t)
(add-hook 'c-mode-common-hook '(lambda () (gtags-mode 1) ))
(defun my-next-gtag ()
  "Find next matching tag, for GTAGS."
  (interactive)
  (let ((latest-gtags-buffer
         (car (delq nil (mapcar (lambda (x) (and (string-match "GTAGS SELECT" (buffer-name x)) (buffer-name x)) )
                                 (buffer-list))))))
    (cond (latest-gtags-buffer
           (switch-to-buffer latest-gtags-buffer)
           (forward-line)
           (gtags-select-it nil))
          )))


;;FCI - Fill Column mode
(require 'fill-column-indicator)
(setq fci-rule-column 80)
(setq fci-rule-use-dashes t)
(add-hook 'c-mode-common-hook 'fci-mode)
(add-hook 'emacs-lisp-mode-hook 'fci-mode)


;; AUTOPAIR
;; Only in some modes, really just a programming conveniece
(require 'autopair)
(add-hook 'c-mode-common-hook #'(lambda () (autopair-mode)))
(add-hook 'python-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'lisp-mode-hook #'(lambda () (autopair-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (autopair-mode)))

;; HIGHLIGHT CURRENT LINE 
(require 'highlight-current-line)
(if (display-graphic-p) 
    (progn
      (global-hl-line-mode t)
      (setq highlight-current-line-globally t)
      (setq highlight-current-line-high-faces t)
      (setq highlight-current-line-whole-line t)
      (setq hl-line-face (quote highlight)))
  (global-hl-line-mode nil))

;; YASNIPPET
(require 'yasnippet)
(setq yas-trigger-key nil)
(yas-reload-all)
;; This is where your snippets will lie.
(setq yas-root-directory '("~/.emacs.d/site-lisp/yasnippet/snippets"))
(mapc 'yas-load-directory yas-root-directory)
(add-hook 'c-mode-common-hook #'(lambda () (yas-minor-mode)))
(add-hook 'emacs-lisp-mode-hook #'(lambda () (yas-minor-mode)))
(local-set-key "\C-c\C-e" 'yas-expand)


;; AUTOCOMPLETE
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/auto-complete/ac-dict")
(setq-default ac-sources '(ac-source-words-in-same-mode-buffers 
			   ac-source-abbrev ac-source-dictionary 
			   ac-source-gtags ac-source-yasnippet))
(add-hook 'emacs-lisp-mode-hook 'ac-emacs-lisp-mode-setup)
(global-auto-complete-mode t)
(add-to-list 'ac-modes 'objc-mode)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

;;AUTOCOMPLETE CLANG
(require 'auto-complete-clang-async)
(defun ac-cc-mode-setup ()
  (setq ac-clang-complete-executable 
	"~/.emacs.d/site-lisp/auto-complete-clang-async/clang-complete")
  (setq ac-sources '(ac-source-clang-async))
  (ac-clang-launch-completion-process))
(defun my-ac-config ()
  (add-hook 'c-mode-common-hook 'ac-cc-mode-setup)
  (add-hook 'auto-complete-mode-hook 'ac-common-setup)
  (global-auto-complete-mode t))
(my-ac-config)

;;CMAKE MODE
(setq load-path (cons (expand-file-name "~.emacs.d/site-lisp/cmake-mode") load-path))
(require 'cmake-mode)
(setq auto-mode-alist
      (append '(("CMakeLists\\.txt\\'" . cmake-mode)
		("\\.cmake\\'" . cmake-mode))
	      auto-mode-alist))

;; C# MODE
(require 'flymake)
(require 'flymake-cursor)
(autoload 'csharp-mode "csharp-mode" "Major mode for editing C# code." t)
(setq auto-mode-alist
      (append '(("\\.cs$" . csharp-mode)) auto-mode-alist))

;; Custom code to use a default compiler string for all C# files
(defvar my-csharp-default-compiler nil)
(setq my-csharp-default-compiler "mono @@FILE@@")
(defun my-csharp-get-value-from-comments (marker-string line-limit)
  my-csharp-default-compiler)

(add-hook 'csharp-mode-hook (lambda ()
                              (if my-csharp-default-compiler
                                  (progn
                                    (fset 'orig-csharp-get-value-from-comments
                                          (symbol-function 'csharp-get-value-from-comments))
                                    (fset 'csharp-get-value-from-comments
                                          (symbol-function 'my-csharp-get-value-from-comments))))
                              (flymake-mode)))

(defun csharp-repl ()
  "Switch to the CSharpRepl buffer, creating it if necessary."
  (interactive)
  (let ((buf (get-buffer "*csharp*")))
    (if buf
        (pop-to-buffer buf)
        (progn
          (split-window)
          (other-window 1)
          (comint-run "csharp")))))
(define-key global-map [f8] (lambda () (interactive)  (csharp-repl)))


;; KEY BINDINGS
;; C-c C-d for line duplication
(global-set-key "\C-c\C-d" 'duplicate-line)
(global-set-key "\C-cd" 'duplicate-line)
;; C-c k for killing the rest of the line 
(global-set-key "\C-ck" 'kill-line)
;; C-k for killing the whole line
(global-set-key "\C-k" 'kill-whole-line)
;; C-x C-o find the other file, useful for c/c++
(global-set-key "\C-x\C-o" 'ff-find-other-file)
;; C-TAB to swith to other window within frame
(global-set-key (kbd "<C-tab>") 'other-window)
;; C-q go back to mark, ie point where jumped elsewhere
(global-set-key "\C-q" 'pop-global-mark)
;;C-` for autocomplete
(global-set-key (kbd "C-`") 'ac-complete-clang)
;; Override default tags finding
;; M-. finds tag
(global-set-key "\M-." 'gtags-find-tag)
;; C-M-. find all references of tag
(global-set-key (kbd "<C-M-.>") 'gtags-find-rtag)
;; C-M-, find all usages of symbol.
(global-set-key (kbd "<C-M-,>") 'gtags-find-symbol) 
;; hide/show hide block
(global-set-key "\C-cc" 'hs-hide-block) 
;; hide/show show block
(global-set-key "\C-ce" 'hs-show-block) 
;; hide/show hide all
(global-set-key "\C-c\M-c" 'hs-hide-all) 
;;hide/show show all
(global-set-key "\C-c\M-e" 'hs-show-all) 



; man page lookup (by default, f1 is help, but I already know how to
; bring that up using C-h)
(define-key global-map [f1]
(lambda () (interactive) (manual-entry (current-word))))

; F2 to spawn another frame
(define-key global-map [f2] (lambda () (interactive) (make-frame)))

; F3 to kill the other window
(define-key global-map [f3] (lambda () (interactive) (delete-other-windows)))

; F4 for open file C-x C-f is too long
(define-key global-map [f4] 
  (lambda (f) (interactive "FFind File:") (switch-to-buffer (ido-find-file f))))

; F5 for dired buffer of the current directory in the other window
(define-key global-map [f5]
  (lambda () (interactive) (dired-other-window default-directory)))

; F6 list buffers
(define-key global-map [f6]  
  (lambda () 
    (interactive) (list-buffers) (switch-to-buffer-other-frame "*Buffer List*")))

; F7 show bookmarks
(define-key global-map [f7] 
  (lambda () 
    (interactive) (list-bookmarks) (switch-to-buffer-other-window  "*Bookmark List*")))

;; F8 set bookmark
;;(define-key global-map [f8] (lambda () (interactive) (xcode-build)))

; F9 to kill buffer
(define-key global-map [f9] (lambda () (interactive) (kill-buffer (current-buffer))))

; F10 put back the windows (winner mode undo)
(define-key global-map [f10] (lambda () (interactive) (winner-undo)))

; F11 undo-tree-undo 
(define-key global-map [f11] (lambda () (interactive) (undo-tree-undo)))

; F12 undo-tree-redo
(define-key global-map [f12] (lambda () (interactive) (undo-tree-redo)))

