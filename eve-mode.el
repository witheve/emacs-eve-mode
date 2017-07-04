;;; eve-mode.el --- Major mode for editing Eve documents. -*- coding: utf-8; lexical-binding: t; -*-

;; Author: Joshua Cole <joshuafcole@gmail.com>
;; Maintainer: Joshua Cole <joshuafcole@gmail.com>
;; Created: 28 Jun 2017
;; Modified: 3 Jul 2017
;; Version: 0.1.0
;; Package-Requires: ((emacs "25") (polymode "1.0") (markdown-mode "2.0"))
;; Keywords: languages wp tools
;; URL: https://github.com/witheve/emacs-eve-mode

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; `eve-mode` is a major mode for editing [Eve Documents][1] built on
;; top of `polymode`.  Since Eve blocks (analagous to DB queries) are
;; conventionally embedded in markdown documents, markdown mode is
;; used as the host mode and code blocks are treated as executable eve
;; blocks.  If the fenced code block has an `info string` with a value
;; other than "eve", the code block will be ignored.  It currently has
;; support for syntax highlighting and simple indentation.  Support
;; will be added for syntax and build error reporting once the new
;; language service is written.

;; [1]: http://witheve.com/

;;; Documentation:

;; `eve-mode` adds itself to the `auto-mode-alist` for `.eve` files
;; automatically.  If you'd like to use eve-mode in a document with
;; another extension, use `M-x eve-mode`.

;;; Code:

(require 'polymode)
(require 'markdown-mode)

(defvar eve-indent-width 2 "Spaces per indentation level in Eve blocks.")
(defvar eve-font-lock-keywords nil "Font lock setup for eve-block mode.")

(let*  ((eve-sections '("search" "bind" "commit"))
       (eve-subblocks '("not" "if" "then" "else"))
       (eve-infix '("+" "-" "/" "*"))
       (eve-filter '("=" "!=" "<" "<=" ">=" ">"))
       (eve-update-operator '(":=" "+=" "-=" "<-"))

       (eve-comment-regexp "//.*$")
       (eve-sections-regexp (regexp-opt eve-sections 'words))
       (eve-subblocks-regexp (regexp-opt eve-subblocks 'words))
       (eve-infix-regexp (concat "\s" (regexp-opt eve-infix) "\s"))
       (eve-filter-regexp (concat "\s" (regexp-opt eve-filter) "\s"))
       (eve-update-operator-regexp (concat "\s" (regexp-opt eve-update-operator) "\s"))
       (eve-misc-regexp "[][,:.]")

       (eve-identifier-regexp "[^][\t\s|(){}\"',.:=#\n]+")
       (eve-tag-regexp (concat "#" eve-identifier-regexp))
       (eve-syntax-table (make-syntax-table)))

  (setq eve-font-lock-keywords
        `(
          ;; (,eve-comment-regexp . font-lock-comment-face)
          (,eve-sections-regexp . font-lock-keyword-face)
          (,eve-subblocks-regexp . font-lock-keyword-face)
          (,eve-update-operator-regexp . font-lock-type-face)
          (,eve-filter-regexp . font-lock-type-face)
          (,eve-infix-regexp . font-lock-type-face)
          (,eve-tag-regexp . font-lock-variable-name-face)
          ;; (,eve-identifier-regexp . font-lock-variable-name-face)
          (,eve-misc-regexp . font-lock-comment-delimiter-face)))

  (modify-syntax-entry ?\/ ". 12b" eve-syntax-table)
  (modify-syntax-entry ?\n "> b" eve-syntax-table)
  (modify-syntax-entry ?\r "> b" eve-syntax-table)

  (defun eve-has-font-lock-face-at-p (face pos)
    (member face (get-text-property pos 'face)))

  (defun eve-search-forward (regexp &optional bound face end)
    (let (offset)
      (while (and (not offset) (re-search-forward regexp bound t))
        (when (eve-has-font-lock-face-at-p face (- (point) 1))
          (setq offset (- (point) (line-beginning-position) (if end 0 (length (match-string 0)))))))
      offset))

  (defun eve-find-offset (regexp &optional diff face end)
    (save-excursion
      (forward-line diff)
      (let ((bound (line-end-position)))
        (eve-search-forward regexp bound face end))))

  (defun eve-levels-opened (diff)
    (save-excursion
      (forward-line diff)
      (let ((bound (line-end-position)) (levels 0))
        (while (eve-search-forward "[])([]" bound)
          (let ((prev (string (char-before))))
            (if (or (equal prev "[" ) (equal prev "(" ))
                (setq levels (+ levels 1))
              (setq levels (- levels 1)))))
        levels)))

  ;; @FIXME: This is a bit of a time bomb -- it's not string aware.
  (defun eve-rewind-until (regexp)
    (let ((lines-back 0))
      (while (not (or (bobp) (looking-at regexp)))
        (setq lines-back (+ lines-back 1))
        (forward-line -1))
      lines-back))


  ;; Based on frink-mode's well commented indent-line function.
  ;; <https://futureboy.us/frinktools/emacs/frink-mode.el>
  (defun eve-indent-line ()
    "Indent current line as eve code."
    (interactive)

    (let ((lines-back 0) cur-indent)
      (save-excursion
        ;; First, we scan back to the nearest section keyword.
        (setq lines-back (eve-rewind-until (concat "^[\s\t]*" eve-sections-regexp)))

        ;; If we couldn't find one, we're just done trying to indent.
        (when (not (bobp))
          ;; Section headers are indented to zero, section contents start indented one level.
          (if (> lines-back 0)
              (setq cur-indent eve-indent-width)
            (setq cur-indent 0))

          ;; Walk forward line by line, indenting as we go.
          (while (>= lines-back 0)
            ;; If we've *decreased* levels on *this* line, dedent ourselves by that many levels.
            (let ((levels (eve-levels-opened 0)))
              (when (< levels 0)
                (setq cur-indent (max (+ cur-indent (* levels eve-indent-width)) 0))))

            ;; If we've *increased* levels on the *previous* line, dedent ourselves by that many levels.
            (let ((levels (eve-levels-opened -1)))
              (when (> levels 0)
                (setq cur-indent (+ cur-indent (* levels eve-indent-width)))))

            ;; If the previous line had an if statement, we'll indent one level.
            (let ((if-offset (eve-find-offset "if" -1 'font-lock-keyword-face)))
              (when if-offset
                (setq cur-indent (+ cur-indent eve-indent-width))))
            ;; If the previous line had a then statement, we'll dedent one level.
            (let ((then-offset (eve-find-offset "then" -1 'font-lock-keyword-face)))
              (when then-offset
                (setq cur-indent (- cur-indent eve-indent-width))))

            (setq lines-back (- lines-back 1))
            (forward-line 1))))

      (when cur-indent
        (indent-line-to cur-indent))))


  (define-derived-mode eve-block-mode fundamental-mode "eve-block"
    "Major mode for editing Eve documents"
    (setq font-lock-multiline t)
    (set-syntax-table eve-syntax-table)
    (setq font-lock-defaults '((eve-font-lock-keywords)))
    (make-local-variable 'indent-line-function)
    (setq indent-line-function 'eve-indent-line))


  (defun eve-retrieve-block-type ()
    "Retrieve a code block's info string.  Defaults to 'eve-block'."
    (save-excursion
      (re-search-forward "^\\([`]\\{3,\\}\\|[~]\\{3,\\}\\)\s*\\(.*\\)\s*$" (line-end-position) t)
      (let ((info (match-string 2)))
        (if (or (not info) (equal info "") (equal info "eve")) "eve-block" info))))

  (defcustom eve-pm-host-eve-doc
    (pm-bchunkmode :mode 'markdown-mode
                   :init-functions '(eve-poly-markdown-remove-markdown-hooks))
    "Markdown host chunkmode."
    :group 'hostmodes
    :type 'object)

  (defcustom  eve-pm-inner-eve-block
    (pm-hbtchunkmode-auto :head-reg "^[ \t]*[`]\\{3,\\}\\|[~]\\{3,\\}.*$"
                          :tail-reg "^[ \t]*[`]\\{3,\\}\\|[~]\\{3,\\}[ \t]*$"
                          :retriever-function 'eve-retrieve-block-type
                          :font-lock-narrow t)
    "Eve block chunk."
    :group 'innermodes
    :type 'object)

  (defcustom eve-pm-poly-eve
    (pm-polymode-multi-auto :hostmode 'eve-pm-host-eve-doc
                            :auto-innermode 'eve-pm-inner-eve-block)
    "Markdown typical configuration."
    :group 'polymodes
    :type 'object)

  (define-polymode eve-mode eve-pm-poly-eve)

  (setq auto-mode-alist
        (append
         '(("\\.eve\\'" . eve-mode)))))


;;; FIXES:
;; source <https://github.com/vspinu/polymode/blob/master/modes/poly-markdown.el>
(defun eve-poly-markdown-remove-markdown-hooks ()
  "Get rid of markdown hooks that make polymode sad."
  (remove-hook 'window-configuration-change-hook 'markdown-fontify-buffer-wiki-links t)
  (remove-hook 'after-change-functions 'markdown-check-change-for-wiki-link t))


(provide 'eve-block-mode)
(provide 'eve-mode)

;;; eve-mode.el ends here
