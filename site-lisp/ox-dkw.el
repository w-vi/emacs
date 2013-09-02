;;; ox-dkw.el --- Markdown Back-End for Org Export Engine

;; Copyright (C) 2012, 2013  Free Software Foundation, Inc.

;; Author: Nicolas Goaziou <n.goaziou@gmail.com>
;; Keywords: org, wp, markdown

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This library implements a Markdown back-end (vanilla flavour) for
;; Org exporter, based on `html' back-end.
;;
;; It provides two commands for export, depending on the desired
;; output: `org-dkw-export-as-dokuwiki' (temporary buffer) and
;; `org-dkw-export-to-dokuwiki' ("txt" file).

;;; Code:

(eval-when-compile (require 'cl))
(require 'ox-html)


;;; User-Configurable Variables

(defgroup org-export-dkw nil
  "Options specific to Dokuwiki export back-end."
  :tag "Org Doku Wiki"
  :group 'org-export
  :version "24.0"
  :package-version '(Org . "8.0"))

;;; Define Back-End

(org-export-define-derived-backend 'dkw 'html
  :export-block '("DW" "DOKUWIKI")
  :filters-alist '((:filter-parse-tree . org-dkw-separate-elements))
  :menu-entry
  '(?w "Export to Dokuwiki"
       ((?W "To temporary buffer"
	    (lambda (a s v b) (org-dkw-export-as-dokuwiki a s v)))
	(?w "To file" (lambda (a s v b) (org-dkw-export-to-dokuwiki a s v)))
	(?o "To file and open"
	    (lambda (a s v b)
	      (if a (org-dkw-export-to-dokuwiki t s v)
		(org-open-file (org-dkw-export-to-dokuwiki nil s v)))))))
  :translate-alist '((bold . org-dkw-bold)
		     (code . org-dkw-verbatim)
		     (comment . (lambda (&rest args) ""))
		     (comment-block . (lambda (&rest args) ""))
		     (example-block . org-dkw-example-block)
		     (fixed-width . org-dkw-example-block)
		     (footnote-definition . ignore)
		     (footnote-reference . ignore)
		     (headline . org-dkw-headline)
		     (horizontal-rule . org-dkw-horizontal-rule)
		     (inline-src-block . org-dkw-verbatim)
		     (italic . org-dkw-italic)
		     (item . org-dkw-item)
		     (line-break . org-dkw-line-break)
		     (link . org-dkw-link)
		     (paragraph . org-dkw-paragraph)
		     (plain-list . org-dkw-plain-list)
		     (plain-text . org-dkw-plain-text)
		     (quote-block . org-dkw-quote-block)
		     (quote-section . org-dkw-example-block)
		     (section . org-dkw-section)
		     (src-block . org-dkw-example-block)
		     (template . org-dkw-template)
		     (verbatim . org-dkw-verbatim)))


;;; Filters

(defun org-dkw-separate-elements (tree backend info)
  "Make sure elements are separated by at least one blank line.

TREE is the parse tree being exported.  BACKEND is the export
back-end used.  INFO is a plist used as a communication channel.

Assume BACKEND is `dkw'."
  (org-element-map tree org-element-all-elements
    (lambda (elem)
      (unless (eq (org-element-type elem) 'org-data)
	(org-element-put-property
	 elem :post-blank
	 (let ((post-blank (org-element-property :post-blank elem)))
	   (if (not post-blank) 1 (max 1 post-blank)))))))
  ;; Return updated tree.
  tree)

;;; Transcode Functions

;;;; Bold

(defun org-dkw-bold (bold contents info)
  "Transcode BOLD object into Dokuwiki format format.
CONTENTS is the text within bold markup.  INFO is a plist used as
a communication channel."
  (format "**%s**" contents))


;;;; Code and Verbatim

(defun org-dkw-verbatim (verbatim contents info)
  "Transcode VERBATIM object into Dokuwiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (let ((value (org-element-property :value verbatim)))
    (format (cond ((not (string-match "`" value)) "\%\%%s\%\%")
		  ((or (string-match "\\``" value)
		       (string-match "`\\'" value))
		   "\%\%%s\%\%")
		  (t "\%\%%s\%\%"))
	    value)))


;;;; Example Block and Src Block

(defun org-dkw-example-block (example-block contents info)
  "Transcode EXAMPLE-BLOCK element into Dokuwiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  (replace-regexp-in-string
   "^" "    "
   (org-remove-indentation
    (org-element-property :value example-block))))


;;;; Headline

(defun org-dkw-headline (headline contents info)
  "Transcode HEADLINE element into Dokuwiki format.
CONTENTS is the headline contents.  INFO is a plist used as
a communication channel."
  (unless (org-element-property :footnote-section-p headline)
    (let* ((level (org-export-get-relative-level headline info))
	   (title (org-export-data (org-element-property :title headline) info))
	   (todo (and (plist-get info :with-todo-keywords)
		      (let ((todo (org-element-property :todo-keyword
							headline)))
			(and todo (concat (org-export-data todo info) " ")))))
	   (tags (and (plist-get info :with-tags)
		      (let ((tag-list (org-export-get-tags headline info)))
			(and tag-list
			     (format "     :%s:"
				     (mapconcat 'identity tag-list ":"))))))
	   (priority
	    (and (plist-get info :with-priority)
		 (let ((char (org-element-property :priority headline)))
		   (and char (format "[#%c] " char)))))
	   ;; Headline text without tags.
	   (heading (concat todo priority title)))
      (cond
       ;; Cannot create a headline.  Fall-back to a list.
       ((or (org-export-low-level-p headline info)
	    (and (> level 6)))
	(let ((bullet
	       (if (not (org-export-numbered-headline-p headline info)) "-"
		 (concat (number-to-string
			  (car (last (org-export-get-headline-number
				      headline info))))
			 "."))))
	  (concat bullet (make-string (- 4 (length bullet)) ? ) heading tags
		  "\n\n"
		  (and contents
		       (replace-regexp-in-string "^" "    " contents)))))))))
;;;; Horizontal Rule

(defun org-dkw-horizontal-rule (horizontal-rule contents info)
  "Transcode HORIZONTAL-RULE element into Dokuwiki format.
CONTENTS is the horizontal rule contents.  INFO is a plist used
as a communication channel."
  "----")


;;;; Italic

(defun org-dkw-italic (italic contents info)
  "Transcode ITALIC object into Dokuwiki format.
CONTENTS is the text within italic markup.  INFO is a plist used
as a communication channel."
  (format "//%s//" contents))


;;;; Item

(defun org-dkw-item (item contents info)
  "Transcode ITEM element into Dokuwiki format.
CONTENTS is the item contents.  INFO is a plist used as
a communication channel."
  (let* ((type (org-element-property :type (org-export-get-parent item)))
	 (struct (org-element-property :structure item))
	 (bullet (if (not (eq type 'ordered)) "*"
		   "-")))
    (concat bullet
	    (make-string (- 4 (length bullet)) ? )
	    (case (org-element-property :checkbox item)
	      (on "[X] ")
	      (trans "[-] ")
	      (off "[ ] "))
	    (let ((tag (org-element-property :tag item)))
	      (and tag (format "**%s:** "(org-export-data tag info))))
	    (org-trim (replace-regexp-in-string "^" "    " contents)))))


;;;; Line Break

(defun org-dkw-line-break (line-break contents info)
  "Transcode LINE-BREAK object into Dokuwiki format.
CONTENTS is nil.  INFO is a plist used as a communication
channel."
  "  \\\\\n")


;;;; Link

(defun org-dkw-link (link contents info)
  "Transcode LINK object into Dokuwiki format.
CONTENTS is the link's description.  INFO is a plist used as
a communication channel."
  (let ((--link-org-files-as-html-maybe
	 (function
	  (lambda (raw-path info)
	    ;; Treat links to `file.org' as links to `file.html', if
            ;; needed.  See `org-html-link-org-files-as-html'.
	    (cond
	     ((and org-html-link-org-files-as-html
		   (string= ".org"
			    (downcase (file-name-extension raw-path "."))))
	      (concat (file-name-sans-extension raw-path) "."
		      (plist-get info :html-extension)))
	     (t raw-path)))))
	(type (org-element-property :type link)))
    (cond ((member type '("custom-id" "id"))
	   (let ((destination (org-export-resolve-id-link link info)))
	     (if (stringp destination)	; External file.
		 (let ((path (funcall --link-org-files-as-html-maybe
				      destination info)))
		   (if (not contents) (format "<%s>" path)
		     (format "[[%s|%s]]" path contents)))
	       (concat
		(and contents (concat contents " "))
		(format "#%s"
			(format (org-export-translate "See section %s" :html info)
				(mapconcat 'number-to-string
					   (org-export-get-headline-number
					    destination info)
					   ".")))))))
	  ((org-export-inline-image-p link org-html-inline-image-rules)
	   (let ((path (let ((raw-path (org-element-property :path link)))
			 (if (not (file-name-absolute-p raw-path)) raw-path
			   (expand-file-name raw-path)))))
	     (format "!{{%s|%s}}"
		     (let ((caption (org-export-get-caption
				     (org-export-get-parent-element link))))
		       (when caption (org-export-data caption info)))
		     path)))
	  ((string= type "coderef")
	   (let ((ref (org-element-property :path link)))
	     (format (org-export-get-coderef-format ref contents)
		     (org-export-resolve-coderef ref info))))
	  ((equal type "radio")
	   (let ((destination (org-export-resolve-radio-link link info)))
	     (org-export-data (org-element-contents destination) info)))
	  ((equal type "fuzzy")
	   (let ((destination (org-export-resolve-fuzzy-link link info)))
	     (if (org-string-nw-p contents) contents
	       (when destination
		 (let ((number (org-export-get-ordinal destination info)))
		   (when number
		     (if (atom number) (number-to-string number)
		       (mapconcat 'number-to-string number "."))))))))
	  (t (let* ((raw-path (org-element-property :path link))
		    (path (cond
			   ((member type '("http" "https" "ftp"))
			    (concat type ":" raw-path))
			   ((equal type "file")
			    ;; Treat links to ".org" files as ".html",
			    ;; if needed.
			    (setq raw-path
				  (funcall --link-org-files-as-html-maybe
					   raw-path info))
			    ;; If file path is absolute, prepend it
			    ;; with protocol component - "file://".
			    (if (not (file-name-absolute-p raw-path)) raw-path
			      (concat "file://" (expand-file-name raw-path))))
			   (t raw-path))))
	       (if (not contents) (format "<%s>" path)
		 (format "[[%s |%s]]" path contents)))))))


;;;; Paragraph

(defun org-dkw-paragraph (paragraph contents info)
  "Transcode PARAGRAPH element into Dokuwiki format.
CONTENTS is the paragraph contents.  INFO is a plist used as
a communication channel."
  (let ((first-object (car (org-element-contents paragraph))))
    ;; If paragraph starts with a #, protect it.
    (if (and (stringp first-object) (string-match "\\`#" first-object))
	(replace-regexp-in-string "\\`#" "\\#" contents nil t)
      contents)))


;;;; Plain List

(defun org-dkw-plain-list (plain-list contents info)
  "Transcode PLAIN-LIST element into Dokuwiki format.
CONTENTS is the plain-list contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Plain Text

(defun org-dkw-plain-text (text info)
  "Transcode a TEXT string into Dokuwiki format.

TEXT is the string to transcode.  INFO is a plist holding
contextual information."
  (when (plist-get info :with-smart-quotes)
    (setq text (org-export-activate-smart-quotes text :html info)))
  ;; Protect ambiguous #.  This will protect # at the beginning of
  ;; a line, but not at the beginning of a paragraph.  See
  ;; `org-dkw-paragraph'.
  (setq text (replace-regexp-in-string "\n#" "\n\\\\#" text))
  ;; Protect ambiguous !
  (setq text (replace-regexp-in-string "\\(!\\)\\[" "\\\\!" text nil nil 1))
  ;; Protect `, *, _ and \
  (setq text (replace-regexp-in-string "[`*_\\]" "\\\\\\&" text))
  ;; Handle special strings, if required.
  (when (plist-get info :with-special-strings)
    (setq text (org-html-convert-special-strings text)))
  ;; Handle break preservation, if required.
  (when (plist-get info :preserve-breaks)
    (setq text (replace-regexp-in-string "[ \t]*\n" "  \n" text)))
  ;; Return value.
  text)


;;;; Quote Block

(defun org-dkw-quote-block (quote-block contents info)
  "Transcode QUOTE-BLOCK element into Dokuwiki format.
CONTENTS is the quote-block contents.  INFO is a plist used as
a communication channel."
  (replace-regexp-in-string
   "^" "> "
   (replace-regexp-in-string "\n\\'" "" contents)))


;;;; Section

(defun org-dkw-section (section contents info)
  "Transcode SECTION element into Dokuwiki format.
CONTENTS is the section contents.  INFO is a plist used as
a communication channel."
  contents)


;;;; Template

(defun org-dkw-template (contents info)
  "Return complete document string after dokuwiki conversion.
CONTENTS is the transcoded contents string.  INFO is a plist used
as a communication channel."
  contents)


;;; Interactive function

;;;###autoload
(defun org-dkw-export-as-dokuwiki (&optional async subtreep visible-only)
  "Export current buffer to a Dokuwiki buffer.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting buffer should be accessible
through the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Export is done in a buffer named \"*Org DKW Export*\", which will
be displayed when `org-export-show-temporary-export-buffer' is
non-nil."
  (interactive)
  (org-export-to-buffer 'dkw "*Org Dokuwiki Export*"
    async subtreep visible-only nil nil (lambda () (text-mode))))

;;;###autoload
(defun org-dkw-convert-region-to-dkw ()
  "Assume the current region has org-mode syntax, and convert it to Dokuwiki.
This can be used in any buffer.  For example, you can write an
itemized list in org-mode syntax in a Dokuwiki buffer and use
this command to convert it."
  (interactive)
  (org-export-replace-region-by 'dkw))


;;;###autoload
(defun org-dkw-export-to-dokuwiki (&optional async subtreep visible-only)
  "Export current buffer to a Dokuwiki file.

If narrowing is active in the current buffer, only export its
narrowed part.

If a region is active, export that region.

A non-nil optional argument ASYNC means the process should happen
asynchronously.  The resulting file should be accessible through
the `org-export-stack' interface.

When optional argument SUBTREEP is non-nil, export the sub-tree
at point, extracting information from the headline properties
first.

When optional argument VISIBLE-ONLY is non-nil, don't export
contents of hidden elements.

Return output file's name."
  (interactive)
  (let ((outfile (org-export-output-file-name ".dkw" subtreep)))
    (org-export-to-file 'dkw outfile async subtreep visible-only)))


(provide 'ox-dkw)

;; Local variables:
;; generated-autoload-file: "org-loaddefs.el"
;; End:

;;; ox-dkw.el ends here
