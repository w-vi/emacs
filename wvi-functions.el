;;@author wvi
(provide 'wvi-functions)

(defun untabify-buffer ()
  "Untabify current buffer"
  (interactive)
  (untabify (point-min) (point-max)))

(defun progmodes-hooks ()
  "Hooks for programming modes"
  (ggtags-mode 1)
  (yas-minor-mode-on)
  (hs-minor-mode)
  (add-hook 'before-save-hook 'progmodes-write-hooks))

(defun progmodes-write-hooks ()
  "Hooks which run on file write for programming modes"
  (prog1 nil
    (set-buffer-file-coding-system 'utf-8-unix)
    (if (not (member major-mode '(makefile-mode makefile-bsdmake-mode makefile-gmake-mode)))
        (untabify-buffer))
    (copyright-update)
    (maybe-delete-trailing-whitespace)))

(defun delete-trailing-whitespace-p ()
  "Should we delete trailing whitespace when saving this file?"
  (save-excursion
    (goto-char (point-min))
    (ignore-errors (next-line 25))
    (let ((pos (point)))
      (goto-char (point-min))
      (and (re-search-forward (concat "@author wvi") pos t) t))))

(defun maybe-delete-trailing-whitespace ()
  "Delete trailing whitespace if I am the author of this file."
  (interactive)
  (and (delete-trailing-whitespace-p) (delete-trailing-whitespace)))

(defun move-region (start end n)
  "Move the current region up or down by N lines."
  (interactive "r\np")
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (let ((start (point)))
      (insert line-text)
      (setq deactivate-mark nil)
      (set-mark start))))

(defun move-region-up (start end n)
  "Move the current line up by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) -1 (- n))))

(defun move-region-down (start end n)
  "Move the current line down by N lines."
  (interactive "r\np")
  (move-region start end (if (null n) 1 n)))

(defun move-line (n)
  "Move the current line up or down by N lines."
  (interactive "p")
  (setq col (current-column))
  (beginning-of-line) (setq start (point))
  (end-of-line) (forward-char) (setq end (point))
  (let ((line-text (delete-and-extract-region start end)))
    (forward-line n)
    (insert line-text)
    ;; restore point to original column in moved line
    (forward-line -1)
    (forward-char col)))

(defun move-line-up (n)
  "Move the current line up by N lines."
  (interactive "p")
  (move-line (if (null n) -1 (- n))))

(defun move-line-down (n)
  "Move the current line down by N lines."
  (interactive "p")
  (move-line (if (null n) 1 n)))

(defun move-line-region-up (&optional start end n)
"Move line or region up"
  (interactive "r\np")
  (if (use-region-p) (move-region-up start end n) (move-line-up n)))

(defun move-line-region-down (&optional start end n)
"Move line or region down"
  (interactive "r\np")
  (if (use-region-p) (move-region-down start end n) (move-line-down n)))


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
;;(setq split-window-preferred-function 'split-window-prefer-horizonally)


;; Never understood why Emacs doesn't have this function.
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it's visiting to NEW-NAME." (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn (rename-file name new-name 1)
               (rename-buffer new-name)
               (set-visited-file-name new-name)
               (set-buffer-modified-p nil))))))

;; Never understood why Emacs doesn't have this function, either.
(defun move-buffer-file (dir)
  "Moves both current buffer and file it's visiting to DIR." (interactive "DNew directory: ")
  (let* ((name (buffer-name))
         (filename (buffer-file-name))
         (dir
          (if (string-match dir "\\(?:/\\|\\\\)$")
              (substring dir 0 -1) dir))
         (newname (concat dir "/" name)))

    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (progn (copy-file filename newname 1)
             (delete-file filename)
             (set-visited-file-name newname)
             (set-buffer-modified-p nil)
             t))))

(defun wvi-create-file-buffer (filename)
  "Creates new file and buffer in current dir with FILENAME provided" (interactive "sNew file: ")
  (let* ((dir (substring (pwd) 10))
         (newpath (concat dir "/" filename)))
    (if (file-exists-p newpath)
        (progn (message "File '%s' already exists ... opening '%s'", newpath, filename)
               (switch-to-buffer (find-file-noselect newpath)))
      (progn (write-region "" "" newpath)
             (switch-to-buffer (find-file-noselect newpath))))
    ))

(defun copy-line-or-region ()
  "Copy current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (kill-ring-save (line-beginning-position) (line-beginning-position 2))))

(require 'dired)
(defun dired-back-to-top ()
  (interactive)
  (beginning-of-buffer)
  (dired-next-line 4))
(define-key dired-mode-map
  (vector 'remap 'beginning-of-buffer) 'dired-back-to-top)

(defun dired-jump-to-bottom ()
  (interactive)
  (end-of-buffer)
  (dired-next-line -1))
(define-key dired-mode-map
  (vector 'remap 'end-of-buffer) 'dired-jump-to-bottom)



;;;; GO Specific stuff

(defvar hook-go-pkg nil
  "History variable for `go-install-package' and `go-test-package'.")

(defun go-build ()
  "build current buffer"
  (interactive)
  (compile (concat "go build  \"" (buffer-file-name) "\"")))

(defun go-build-dir ()
  "build current directory"
  (interactive)
  (compile "go build ."))

(defun go-fix-buffer ()
  "run gofix on current buffer"
  (interactive)
  (show-all)
  (shell-command-on-region (point-min) (point-max) "go tool fix -diff"))

(defun go-install-package ()
  "install package"
  (interactive)
  (let
      ((pkg (read-from-minibuffer "Install package: " nil nil nil 'hook-go-pkg)))
    (if (not (string= pkg ""))
        (compile (concat "go install \"" pkg "\"")))))

(defun go-test-package ()
  "test package"
  (interactive)
  (let
      ((pkg (read-from-minibuffer "Test package: " nil nil nil 'hook-go-pkg)))
    (if (not (string= pkg ""))
        (compile (concat "go test \"" pkg "\"")))))
