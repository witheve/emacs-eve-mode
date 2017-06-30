;;; eve-mode.el --- Major mode for editing Eve documents. -*- coding: utf-8; lexical-binding: t; -*-

(setq eve-sections '("search" "bind" "commit"))
(setq eve-subblocks '("not" "if" "then" "else"))
(setq eve-infix '("+" "-" "/" "*"))
(setq eve-filter '("=" "!=" "<" "<=" ">=" ">"))
(setq eve-update-operator '(":=" "+=" "-=" "<-"))

(setq eve-comment-regexp "//.*$")
(setq eve-sections-regexp (regexp-opt eve-sections 'words))
(setq eve-subblocks-regexp (regexp-opt eve-subblocks 'words))
(setq eve-infix-regexp (concat "\s" (regexp-opt eve-infix) "\s"))
(setq eve-filter-regexp (concat "\s" (regexp-opt eve-filter) "\s"))
(setq eve-update-operator-regexp (concat "\s" (regexp-opt eve-update-operator) "\s"))
(setq eve-misc-regexp "[][,:.]")

(setq eve-identifier-regexp "[^][\t\s|(){}\"',.:=#\n]+")
(setq eve-tag-regexp (concat "#" eve-identifier-regexp))

(setq eve-font-lock-keywords
      `(
        ;(,eve-comment-regexp . font-lock-comment-face)
        (,eve-sections-regexp . font-lock-keyword-face)
        (,eve-subblocks-regexp . font-lock-keyword-face)
        (,eve-update-operator-regexp . font-lock-type-face)
        (,eve-filter-regexp . font-lock-type-face)
        (,eve-infix-regexp . font-lock-type-face)
        (,eve-tag-regexp . font-lock-variable-name-face)
        ;;(,eve-identifier-regexp . font-lock-variable-name-face)
        (,eve-misc-regexp . font-lock-comment-delimiter-face)

        ))

(setq eve-syntax-table (make-syntax-table))
(modify-syntax-entry ?\/ ". 12b" eve-syntax-table)
(modify-syntax-entry ?\n "> b" eve-syntax-table)
(modify-syntax-entry ?\r "> b" eve-syntax-table)


(setq auto-mode-alist
      (append
       '(("\\.eve\\'" . eve-mode))))

(setq eve-indent-width 2)

(defun levels-previously-closed (diff)
  (save-excursion
    (forward-line diff)
    (let* ((bound (line-end-position))
           (levels 0))
      (while (re-search-forward "^[\t\s]*[^[(]*?[])]" bound t)
        (setq levels (+ levels 1)))
      (message "  closing %d" levels)
      levels)))

;; @TODO Clueless about strings...
;; (defun levels-opened (diff)
;;   (save-excursion
;;     (forward-line diff)
;;     (message "  pre: %d" (point))
;;     (let ((bound (line-end-position)) (levels 0))
;;       (when (re-search-forward "[[(]\\|if\\|then" bound t)
;;         (re-search-backward "[[(]\\|if\\|then" (line-beginning-position) t)
;;         (message "  post: %d" (point))
;;         (while (re-search-forward "[])([]\\|if\\|then" bound t)
;;           (if-let ((prev (string (char-before)))
;;                    (_ (or (equal prev "[" )
;;                           (equal prev "(" )
;;                           (equal prev "f" ))))
;;               (setq levels (+ levels 1))
;;             (setq levels (- levels 1)))))
;;       (message "  opening: %d" levels)
;;       levels)))



(defun indentation-at (diff)
  (save-excursion
    (forward-line diff)
    (current-indentation)))


;; Based heavily on frink-mode's exceptionally well commented indent-line function.
;; <https://futureboy.us/frinktools/emacs/frink-mode.el>
;; (defun eve-indent-line ()
;;   "Indent current line as eve code"
;;   (interactive)
;;   (message "indent")
;;   (beginning-of-line)
;;   (if (bobp)
;;       (indent-line-to 0)

;;     (let ((indented nil) (lines-back 0) cur-indent)
;;       (progn
;;         ;; If the current line is a section, we're at 0.
;;         (if (looking-at (concat "^[\s\t]*" eve-sections-regexp))
;;             (setq cur-indent 0)
;;           ;; If we're closing pairs, we're dedenting.
;;           (message "  prev: %d" (indentation-at -1))
;;           (if (looking-at "^[\t\s]*[])]")
;;               (setq cur-indent (max (- (indentation-at -1) (* (levels-previously-closed 0) eve-indent-width)) 0))

;;             ;; If the previous line contains an excess of opening pairs, we're indenting.
;;             ;; We'll also strategically dedent if the previous line had an excess of closing pairs that didn't lead the line.
;;             (save-excursion
;;               (forward-line -1)
;;               (setq lines-back (+ lines-back 1))
;;               (when (looking-at (concat "^[\s\t]*" eve-sections-regexp))
;;                 (setq cur-indent eve-indent-width))
;;               (when-let ((levels (levels-opened 0)))
;;                 (setq cur-indent (+ (or cur-indent (current-indentation)) (* levels eve-indent-width))))

;;               (while (not cur-indent)
;;                 (message "  searching for prev")
;;                 (if (looking-at "[\s\t]*[^\s\t\n]+")
;;                     (setq cur-indent (current-indentation))
;;                   (message "    back that truck up")
;;                   (forward-line -1)
;;                   (setq lines-back (+ lines-back 1))))
;;               ))))

;;       (message "CUR %d" cur-indent)
;;       (indent-line-to (max cur-indent 0)))))

(defun levels-opened (diff)
  (save-excursion
    (forward-line diff)
    (let ((bound (line-end-position)) (levels 0))
      (while (re-search-forward "[])([]\\|if\\|then" bound t)
        (if-let ((prev (string (char-before)))
                 (_ (or (equal prev "[" )
                        (equal prev "(" )
                        (equal prev "f" ))))
            (setq levels (+ levels 1))
          (setq levels (- levels 1))))

      (message "    opening: %d" levels)
      levels)))


(defun eve-indent-line ()
  "Indent current line as eve code"
  (interactive)
  (message "Indenting: %s" (buffer-substring (line-beginning-position) (line-end-position)))

  (let ((lines-back 0) cur-indent)
    (save-excursion
      ;; First, we scan back to the nearest section keyword.
      (while (not (or (bobp) (looking-at (concat "^[\s\t]*" eve-sections-regexp))))
        (setq lines-back (+ lines-back 1))
        (forward-line -1))

      ;; If we couldn't find one, we're just done trying to indent.
      (when (not (bobp))
        ;; Section headers are indented to zero, section contents start indented one level.
        (if (> lines-back 0)
            (setq cur-indent eve-indent-width)
          (setq cur-indent 0))

        ;; Walk forward line by line, indenting as we go.
        (while (>= lines-back 0)
          (message "  phantom indenting: %s" (buffer-substring (line-beginning-position) (line-end-position)))
          ;; If we've *decreased* levels on *this* line, dedent ourselves by that many levels.
          (when-let ((levels (levels-opened 0))
                     (_ (< levels 0)))
            (message "      dedenting: %d" levels)
            (setq cur-indent (max (+ cur-indent (* levels eve-indent-width)) 0)))

          ;; If we've *increased* levels on the *previous* line, dedent ourselves by that many levels.
          (when-let ((levels (levels-opened -1))
                     (_ (> levels 0)))
            (message "      indenting: %d" levels)
            (setq cur-indent (+ cur-indent (* levels eve-indent-width))))

          (setq lines-back (- lines-back 1))
          (forward-line 1))))

    (message "  indent: %d" cur-indent)
    (indent-line-to cur-indent)))


(define-derived-mode eve-mode fundamental-mode "eve"
  "Major mode for editing Eve documents"
  (setq font-lock-multiline t)
  (set-syntax-table eve-syntax-table)
  (setq font-lock-defaults '((eve-font-lock-keywords)))
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'eve-indent-line))

(provide 'eve-mode)
