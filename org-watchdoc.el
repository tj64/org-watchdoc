;;; org-watchdoc.el --- Watchdog for exported Org-mode trees
;;   :PROPERTIES:
;;   :EXPORT_OPTIONS: prop:nil
;;   :wdoc_1992rwM: /home/tj/git/org-watchdoc/README.md /home/tj/git/org-watchdoc/export-templates/org-watchdoc-gh.org gfm
;;   :wdoc_1992G_r: /home/tj/gitclone/worg/org-contrib/org-watchdoc.org /home/tj/git/org-watchdoc/export-templates/org-watchdoc-worg.org org
;;   :wdoc_1992gas: /home/tj/git/org-watchdoc/targets/org-watchdoc.html /home/tj/git/org-watchdoc/export-templates/org-watchdoc-gh.org html
;;   :wdoc_1992tky: /home/tj/git/org-watchdoc/targets/org-watchdoc.txt /home/tj/git/org-watchdoc/export-templates/org-watchdoc-gh.org ascii
;;   :wdoc_1992fuB: /home/tj/git/org-watchdoc/targets/org-watchdoc.tex /home/tj/git/org-watchdoc/export-templates/org-watchdoc-gh.org latex
;;   :END:

;; Copyright (C) from 2014 Thorsten Jolitz
;; Author: Thorsten Jolitz <tjolitz at gmail dot com>
;; Keywords: org-mode, exporter, propagate changes, documentation

;;;; MetaData
;;   :PROPERTIES:
;;   :copyright: Thorsten Jolitz
;;   :copyright-years: 2014+
;;   :version:  1.0
;;   :licence:  GPL 3 or later (free software)
;;   :licence-url: http://www.gnu.org/licenses/
;;   :part-of-emacs: no
;;   :git-repo: https://github.com/tj64/org-watchdoc.git
;;   :git-clone: git://github.com/tj64/org-watchdoc.git
;;   :author: Thorsten Jolitz
;;   :author_email: tjolitz AT gmail DOT com
;;   :END:

;;;; Commentary

;; This library implements functionality for keeping exported files
;; associated with Org subtrees up-to-date.

;; Its principal use case is writing the comment-section of Emacs
;; Lisp (or other) source-code files only once (and in full Org-mode
;; using outorg.el), export it as README documentation from the
;; *outorg-edit-buffer* to html, ascii, latex/pdf,
;; markdown-github-flavor or whatever, and keep the exported doc
;; files automatically up-to-date when the original comment-section
;; of the source-buffer is edited explicitly with outorg (via M-x
;; outorg-edit-comments-and-propagate-changes).

;; org-watchdoc is just a little toolbox that can be used
;; independently from outorg too. All its functions are commands, so
;; its functionality is available for interactive use too.

;;;; Installation

;; Put this line in your init file

;; #+begin_src 'emacs-lisp
;;  (require 'org-watchdoc)
;; #+end_src

;; and make sure Emacs can find the file org-watchdoc.el.

;; To take real advantage of org-watchdoc, outshine.el and outorg.el
;; (and maybe navi-mode.el) should be installed and source-code
;; buffers should be structured with outshine headers (outcommented
;; Org-mode headers), taking care that the whole comment-section is
;; one single outline tree that is the first headline in the
;; source-code file. Use org-watchdoc.el itself as an example for
;; this kind of file structuring.

;;;; Usage

;;;;; Commands

;; Since org-watchdoc is a toolbox and not a mode, no menu or keymap
;; is specified. However, its commands can be used interactively:

;; | M-x org-watchdoc- | action                                   |
;; |-------------------+------------------------------------------|
;; | add-target        | add target-combination to watchlist      |
;; | remove-target     | remove target-combination from watchlist |
;; | propagate-changes | if md5 changed reexport all combinations |
;; | set-md5           | set org-watchdoc-md5 to current md5      |

;;;;; Interactive Use

;; In interactive use, this would be the typical order of actions:

;;  1. Export first buffer tree to desired doc files
;;     (e.g. README-GH.md or README-WORG.html)

;;  2. Add targets with point on first buffer headline.
    
;;     Targets are combinations of files the exporter writes to,
;;     export-template files to be inserted before the exporter does
;;     its work, and backends the exporter should export to, e.g.

;; #+begin_example
;;       "/home/me/proj/README-GH.md /home/me/proj/gh-tmpl.org gfm"
;;       "/home/me/proj/README-WORG.html /home/me/proj/worg-tmpl.org html"
;; #+end_example
    
;;     The three elements of such a combination are prompted from
;;     the user.

;;  3. Save and set md5 variable.

;;  4. Edit the (narrowed) first buffer tree and save

;;  5. Propagate changes.

;;     Since md5 has changed due to the edits, all registered target
;;     combinations are automatically re-exported.

;;;;; Use with Outorg

;; Assuming outshine and outorg are installed and active, do once:

;;  - Edit as Org
   
;;    In the *outorg-edit-buffer* do steps 1 and 2 described above
;;    for direct interactive use.

;; Then whenever you want to edit the source-buffer's
;; comment-section and propagate the changes to the watched doc
;; files, do:

;; #+begin_example
;;  M-x outorg-edit-comments-and-propagate-changes
;; #+end_example

;; instead of the usual 

;; #+begin_example
;;  M-x outorg-edit-comment-as-org
;; #+end_example

;; This will

;;  - Offer the first buffer tree for editing in the
;;    *outorg-edit-buffer* 

;;  - Reset `org-watchdoc-md5' immediately after edit-buffer setup 

;;  - Check if buffer md5 has changed when editing is quitted. If so,
;;    propagate the changes to the doc files registered in the subtrees
;;    watchlist.

;;;;; ChangeLog

;; | date            | author(s)       | version |
;; |-----------------+-----------------+---------|
;; | <2014-04-09 Mi> | Thorsten Jolitz |     0.9 |

;;; Requires

;;; Variables
;;;; Vars

(defvar org-watchdoc-md5 nil
  "Store current md5 of current buffer.")
(make-variable-buffer-local 'org-watchdoc-md5)

;;; Defuns

;;;; Commands

(defun org-watchdoc-propagate-changes ()
  "Propagate changes of subtree at point to watched files."
  (interactive)
  (unless (string= (md5 (current-buffer)) org-watchdoc-md5)
    (mapc
     (lambda (--prop)
       (when (string-match "^wdoc_.+$" (car --prop))
	 (let ((buf (current-buffer))
	       (wdoc-lst (split-string (cdr --prop) " " t)))
	   (with-temp-buffer
	     (insert-file-contents (nth 1 wdoc-lst))
	     (goto-char (point-max))
	     (insert-buffer-substring buf)
	     (let ((backend-as-strg (car (last wdoc-lst))))
	       (require
		(intern (concat "ox-" backend-as-strg)) nil t)
	       (org-export-to-file
		   (intern backend-as-strg) (car wdoc-lst)))))))
     (org-entry-properties))))


(defun org-watchdoc-add-target (target-file &optional export-template-file export-backend)
  "Add TARGET-FILE to watch list of current subtree.
EXPORT-BACKEND determines the backend used by `org-export-as' to
 update the doc file. Optional EXPORT-TEMPLATE-FILE is inserted
 when non-nil."
  (interactive
   (list
    (read-file-name "Target File: ")
    (read-file-name "Export Template File: ")
    (ido-completing-read "Backend (select or type): "
			 (mapcar 'symbol-name
				 org-export-backends))))
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (outline-on-heading-p 'INVISIBLE-OK)
	(ignore-errors
	  (outline-next-heading)))
      (org-entry-put-multivalued-property
       (point) (make-temp-name "wdoc_")
      (expand-file-name target-file)
      (expand-file-name export-template-file)
      export-backend)
      (unless (org-entry-get (point) "EXPORT_OPTIONS")
      (org-entry-put-multivalued-property
       (point) "EXPORT_OPTIONS" "prop:nil")))))


(defun org-watchdoc-remove-target (target-file)
  "Remove TARGET-FILE from watch list of root-tree."
  (interactive "FTarget File: ")
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (unless (outline-on-heading-p 'INVISIBLE-OK)
	(ignore-errors
	  (outline-next-heading)))
      (let ((prop
	     (delq nil
		   (mapcar
		    (lambda (--prop)
		      (when
			  (org-entry-member-in-multivalued-property
			   (point) (car-safe --prop)
			   (expand-file-name target-file))
			(car-safe --prop)))
		    (org-entry-properties)))))
	(when (consp prop)
	  (org-entry-delete (point) (car prop)))))))

(defun org-watchdoc-set-md5 ()
  "Set buffer-local variable `org-watchdoc-md5'."
  (interactive)
  (setq org-watchdoc-md5 (md5 (current-buffer))))

;;; Provide

(provide 'org-watchdoc)

;;; org-watchdoc.el ends here
