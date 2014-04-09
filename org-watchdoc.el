;;; org-watchdoc.el --- Watchdog for exported Org-mode trees

;; Copyright (C) from 2014 Thorsten Jolitz
;; Author: Thorsten Jolitz <tjolitz at gmail dot com>
;; Keywords: org-mode, exporter, propagate changes, documentation

;;;; License

;; This library is distributed under GPLv3 or later license. It is not
;; (yet) part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation, either version 3 of the License,
;; or (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <http://www.gnu.org/licenses/>

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
    
;;     "/home/me/proj/README-GH.md /home/me/proj/gh-tmpl.org gfm"
;;     "/home/me/proj/README-WORG.html /home/me/proj/worg-tmpl.org
;;     html"

;;     The three elements of such a combination are prompted from the
;;     user.

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
;; files, do

;; ,---------------------------------------
;; | M-x outorg-edit-comments-and-propagate-changes
;; `---------------------------------------

;; instead of the usual 

;; ,-------------------------------
;; | M-x outorg-edit-comment-as-org
3;; `-------------------------------

;; This will

;;  - Offer the first buffer tree for editing in the
;;  - *outorg-edit-buffer* Reset `org-watchdoc-md5' immediately
;;  - after edit-buffer setup Check if buffer md5 has changed when
;;  - editing is quitted. If so, ;; propagate the changes to the doc
;;  - files registered in the subtrees ;; watchlist.

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
       (when (string-match "^wdoc_[[:word:]]+$" (car --prop))
	 (let ((buf (current-buffer))
	       (wdoc-lst (split-string (cdr --prop) " " t)))
	       (with-temp-buffer
		   (insert-file-contents (nth 1 wdoc-lst))
		   (goto-char (point-max))
		   (insert-buffer-substring buf)
		   (org-export-to-file
		       (intern (car (last wdoc-lst)))
		       (car wdoc-lst))))))
     (org-entry-properties))))


(defun org-watchdoc-add-target (target-file &optional export-backend export-template-file)
  "Add TARGET-FILE to watch list of current subtree.
EXPORT-BACKEND determines the backend used by `org-export-as' to
 update the doc file. Optional EXPORT-TEMPLATE-FILE is inserted
 when non-nil."
  (interactive
   (list
    (read-file-name "Target File: ")
    (ido-completing-read "Backend: "
			 (mapcar 'symbol-name
				 org-export-backends))
    (read-file-name "Export Template File: ")))
  (save-excursion
    (save-restriction
      (widen)
      (outline-previous-heading)
      (org-entry-put-multivalued-property
       (point) (make-temp-name "wdoc_")
       target-file export-template-file export-backend))))
    ;; (org-watchdoc-set-md5))


(defun org-watchdoc-remove-target (target-file export-backend export-template-file)
  "Remove TARGET-FILE from watch list of current subtree.
EXPORT-BACKEND determines the backend used by `org-export-as' to
 update the doc file. Optional EXPORT-TEMPLATE-FILE is inserted
 when non-nil."
  (interactive
   (list
    (read-file-name "Target File: ")
    (ido-completing-read "Backend: "
			 (mapcar 'symbol-name
				 org-export-backends))
    (read-file-name "Export Template File: ")))
  (save-excursion
    (save-restriction
      (widen)
      (outline-previous-heading)
      (let ((prop
	     ;; (car-safe
	     (delq nil
	      (mapcar
	       (lambda (--prop)
		 (when (org-entry-member-in-multivalued-property
			(point) (car-safe --prop) target-file)
		   (car-safe --prop)))
		 (org-entry-properties)))))
	(when (consp prop)
	  (org-entry-delete (point) (car prop)))))))
  ;; (org-watchdoc-set-md5))

(defun org-watchdoc-set-md5 ()
  "Set buffer-local variable `org-watchdoc-md5'."
  (interactive)
  (setq org-watchdoc-md5 (md5 (current-buffer))))

;;; Provide

(provide 'org-watchdoc)

;;; org-watchdoc.el ends here
