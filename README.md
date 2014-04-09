- [org-watchdoc.el &#x2014; Watchdog for exported Org-mode trees](#org-watchdoc.el-&#x2014;-watchdog-for-exported-org-mode-trees)
  - [MetaData](#metadata)
  - [Commentary](#commentary)
  - [Installation](#installation)
  - [Usage](#usage)
    - [Commands](#commands)
    - [Interactive Use](#interactive-use)
    - [Use with Outorg](#use-with-outorg)
    - [ChangeLog](#changelog)


# org-watchdoc.el &#x2014; Watchdog for exported Org-mode trees

    EXPORT_OPTIONS: prop:nil
    
    wdoc_1992rwM: /home/tj/git/org-watchdoc/README.md /home/tj/git/org-watchdoc/export-templates/org-watchdoc-gh.org gfm
    
    wdoc_1992G_r: /home/tj/gitclone/worg/org-contrib/org-watchdoc.org /home/tj/git/org-watchdoc/export-templates/org-watchdoc-worg.org org

Copyright (C) from 2014 Thorsten Jolitz
Author: Thorsten Jolitz <tjolitz at gmail dot com>
Keywords: org-mode, exporter, propagate changes, documentation

## MetaData

    copyright: Thorsten Jolitz
    
    copyright-years: 2014+
    
    version: 1.0
    
    licence: GPL 3 or later (free software)
    
    licence-url: http://www.gnu.org/licenses/
    
    part-of-emacs: no
    
    git-repo: https://github.com/tj64/org-watchdoc.git
    
    git-clone: git://github.com/tj64/org-watchdoc.git
    
    author: Thorsten Jolitz
    
    author_email: tjolitz AT gmail DOT com

## Commentary

This library implements functionality for keeping exported files
associated with Org subtrees up-to-date.

Its principal use case is writing the comment-section of Emacs
Lisp (or other) source-code files only once (and in full Org-mode
using outorg.el), export it as README documentation from the
**outorg-edit-buffer** to html, ascii, latex/pdf,
markdown-github-flavor or whatever, and keep the exported doc
files automatically up-to-date when the original comment-section
of the source-buffer is edited explicitly with outorg (via M-x
outorg-edit-comments-and-propagate-changes).

org-watchdoc is just a little toolbox that can be used
independently from outorg too. All its functions are commands, so
its functionality is available for interactive use too.

## Installation

Put this line in your init file

```'emacs-lisp
(require 'org-watchdoc)
```

and make sure Emacs can find the file org-watchdoc.el.

To take real advantage of org-watchdoc, outshine.el and outorg.el
(and maybe navi-mode.el) should be installed and source-code
buffers should be structured with outshine headers (outcommented
Org-mode headers), taking care that the whole comment-section is
one single outline tree that is the first headline in the
source-code file. Use org-watchdoc.el itself as an example for
this kind of file structuring.

## Usage

### Commands

Since org-watchdoc is a toolbox and not a mode, no menu or keymap
is specified. However, its commands can be used interactively:

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">M-x org-watchdoc-</th>
<th scope="col" class="left">action</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left">add-target</td>
<td class="left">add target-combination to watchlist</td>
</tr>


<tr>
<td class="left">remove-target</td>
<td class="left">remove target-combination from watchlist</td>
</tr>


<tr>
<td class="left">propagate-changes</td>
<td class="left">if md5 changed reexport all combinations</td>
</tr>


<tr>
<td class="left">set-md5</td>
<td class="left">set org-watchdoc-md5 to current md5</td>
</tr>
</tbody>
</table>

### Interactive Use

In interactive use, this would be the typical order of actions:
1.  Export first buffer tree to desired doc files
    (e.g. README-GH.md or README-WORG.html)

2.  Add targets with point on first buffer headline.
    
    Targets are combinations of files the exporter writes to,
    export-template files to be inserted before the exporter does
    its work, and backends the exporter should export to, e.g.

    "/home/me/proj/README-GH.md /home/me/proj/gh-tmpl.org gfm"
    "/home/me/proj/README-WORG.html /home/me/proj/worg-tmpl.org html"

The three elements of such a combination are prompted from
the user.

1.  Save and set md5 variable.

2.  Edit the (narrowed) first buffer tree and save

3.  Propagate changes.
    
    Since md5 has changed due to the edits, all registered target
    combinations are automatically re-exported.

### Use with Outorg

Assuming outshine and outorg are installed and active, do once:
-   Edit as Org
    
    In the **outorg-edit-buffer** do steps 1 and 2 described above
    for direct interactive use.

Then whenever you want to edit the source-buffer's
comment-section and propagate the changes to the watched doc
files, do:

    M-x outorg-edit-comments-and-propagate-changes

instead of the usual 

    M-x outorg-edit-comment-as-org

This will

-   Offer the first buffer tree for editing in the
    **outorg-edit-buffer**

-   Reset \`org-watchdoc-md5' immediately after edit-buffer setup

-   Check if buffer md5 has changed when editing is quitted. If so,
    propagate the changes to the doc files registered in the subtrees
    watchlist.

### ChangeLog

<table border="2" cellspacing="0" cellpadding="6" rules="groups" frame="hsides">


<colgroup>
<col  class="left" />

<col  class="left" />

<col  class="right" />
</colgroup>
<thead>
<tr>
<th scope="col" class="left">date</th>
<th scope="col" class="left">author(s)</th>
<th scope="col" class="right">version</th>
</tr>
</thead>

<tbody>
<tr>
<td class="left"><span class="timestamp-wrapper"><span class="timestamp">&lt;2014-04-09 Mi&gt;</span></span></td>
<td class="left">Thorsten Jolitz</td>
<td class="right">0.9</td>
</tr>
</tbody>
</table>
