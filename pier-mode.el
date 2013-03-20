;;; pier-mode.el --- Emacs Major mode for Pier-formatted text files
;;
;; Copyright (C) 2012 Damien Cassou <damien.cassou@gmail.com>
;;
;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:
;;
;; The code below is heavily based on markdown-mode and AUCTeX.
;;
;;; Code:

(require 'magit-key-mode) ;; for popup handling

(defgroup pier nil
  "Major mode for editing text files in Pier format."
  :prefix "pier-"
  :group 'wp
  :link '(url-link "http://piercms.com"))

(defgroup pier-faces nil
  "Faces used in Pier Mode"
  :group 'pier
  :group 'faces)

(defvar pier-mode-font-lock-keywords nil
  "Syntax highlighting for Pier files.")

(setq pier-mode-font-lock-keywords nil)

(defmacro pier-defformat (name &optional face-spec regex regex-group)
  "Generate necessary vars and faces for face NAME.
NAME is the name of the specific face to create without prefix or
suffix (e.g., bold. FACE-SPEC is passed unchanged to `defface'.
REGEX is the regular expression used to match text for this
face."
  (let ((face-name (intern (format "pier-%s-face" `,name)))
        (regex-name (intern (format "pier-regex-%s" `,name))))
    `(progn
       ;; Save face specification to dedicated variable
       (defvar ,face-name ',face-name
         ,(format "Face name to use for %s text." name))
       ;; Save face specification to dedicated face
       ,(when face-spec
          `(defface ,face-name
             ,face-spec
             ,(format "Face for %s text." name)
             :group 'pier-faces))
       ;; Save regexp to dedicated variable
       ,(when regex
          `(defconst ,regex-name
             ,(pier-preprocess-regex regex)
             ,(format "Regular expression for matching %s text." name)))
       ;; Associates regex with face name for syntax highlighting:
       ,(when (and face-spec regex)
          `(add-to-list 'pier-mode-font-lock-keywords
                        (cons ,regex-name
                              ',(if regex-group
                                   (list regex-group face-name)
                                 face-name)))))))

(defun pier-preprocess-regex (regex)
  (replace-regexp-in-string
   "\\[\\[anything\\]\\]"
   "\\(.\\|\n\\)*?"
   regex
   t ;; don't interpret capital letters
   t ;; don't interpret replacement string as a regex
   ))

(defmacro pier-defformat-special-text (name face-spec markup key)
  (let ((insert-markup-fn-name (intern (format "pier-insert-%s-markup" name))))
  `(progn
     (pier-defformat
      ,name
      '((t ,(append '(:inherit pier-special-text-face) face-spec)))
      ,(concat "[^\\]\\(" (regexp-quote markup) ".*?[^\\]" (regexp-quote markup) "\\)")
      1)
     (defun ,insert-markup-fn-name ()
       (interactive)
       (pier-insert-special-text-markup ,markup))
     (add-to-list 'pier-key-mode-special-font-actions
                  '(,(format "%c" key)
                    ,(capitalize (format "%s" name))
                    ,insert-markup-fn-name)))))

(defun pier-font-lock-extend-region ()
  "Extend the search region to include an entire block of text.
This helps improve font locking for block constructs such as pre blocks."
  ;; Avoid compiler warnings about these global variables from font-lock.el.
  ;; See the documentation for variable `font-lock-extend-region-functions'.
  (eval-when-compile (defvar font-lock-beg) (defvar font-lock-end))
  (save-excursion
    (goto-char font-lock-beg)
    (let ((found (re-search-backward "\n\n" nil t)))
      (when found
        (goto-char font-lock-end)
        (when (re-search-forward "\n\n" nil t)
          (beginning-of-line)
          (setq font-lock-end (point)))
        (setq font-lock-beg found)))))

;; Syntax table
(defvar pier-syntax-table nil "Syntax table for `pier-mode'.")
(setq pier-syntax-table
      (let ((synTable (copy-syntax-table text-mode-syntax-table)))

        ;; a comment starts with a '%' and ends with a new line
        (modify-syntax-entry ?% "< b" synTable)
        (modify-syntax-entry ?\n "> b" synTable)

        synTable))

(defun pier-insert-special-text-markup (markup)
  (cond
   ((mark)
    (save-excursion
      (insert markup)
      (goto-char (mark))
      (insert markup))
      (forward-char (length markup)))
   (t
    (insert markup)
    (save-excursion
      (insert markup)))))

(defvar pier-key-mode-special-font-actions nil)

(defun pier-key-mode-groups ()
  `((special-font
     (actions ,@pier-key-mode-special-font-actions))))


;;;###autoload
(define-derived-mode pier-mode text-mode "Pier"
  "Major mode for editing Pier CMS files."
  :syntax-table pier-syntax-table
  ;; Don't fill paragraphs as Pier expects everything on one line
  (setq fill-paragraph-function (lambda (ignored) t))
  ;; Natural Pier tab width
  (setq tab-width 4)
  ;; Font lock.
  (set (make-local-variable 'font-lock-defaults)
       '(pier-mode-font-lock-keywords))
  (set (make-local-variable 'font-lock-multiline) t)
  ;; imenu
  (set (make-local-variable 'imenu-generic-expression)
       (list (list nil pier-regex-header-1 1)
             (list nil pier-regex-header-2 1)))
  ;; comments
  (set (make-local-variable 'comment-start) "%")
  ;; Multiline font lock
  (add-hook 'font-lock-extend-region-functions
            'pier-font-lock-extend-region))

(defun pier-insert-special-text-markup-popup ()
  (interactive)
  (let ((magit-key-mode-key-maps nil)
        (magit-key-mode-groups (pier-key-mode-groups)))
    (magit-key-mode 'special-font)))

(define-key pier-mode-map (kbd "C-c C-f") 'pier-insert-special-text-markup-popup)

(pier-defformat
 special-text
 '((t (:inherit font-lock-variable-name-face))))

(pier-defformat-special-text bold (:weight bold) "\"\"" ?b)
(pier-defformat-special-text italic (:slant italic) "''" ?i)
(pier-defformat-special-text strikethrough (:strike-through t) "--" ?-)
(pier-defformat-special-text subscript (:height 0.8) "@@" ?@)
(pier-defformat-special-text superscript (:height 0.8) "^^" ?^)
(pier-defformat-special-text underlined (:underline t) "__" ?_)
(pier-defformat-special-text link (:inherit link) "*" ?*)
(pier-defformat-special-text link (:inherit link) "+" ?+)
(pier-defformat-special-text monospaced (:inherit font-lock-constant-face) "==" ?=)

(pier-defformat
 note
 '((t (:inherit pier-special-text-face :weight bold)))
 "^@@note .*$")

(pier-defformat
 todo
 '((t (:inherit pier-special-text-face :weight bold)))
 "^@@todo .*$")

(pier-defformat
 header
 '((t (:inherit font-lock-function-name-face :weight bold))))

(pier-defformat
 header-1
 '((t (:inherit pier-header-face :height 1.3)))
 "^!\\([^!].*\\)$")

(pier-defformat
 header-2
 '((t (:inherit pier-header-face :height 1.25)))
 "^!!\\([^!].*\\)$")

(pier-defformat
 header-3
 '((t (:inherit pier-header-face :height 1.2)))
 "^!!!\\([^!].*\\)$")

(pier-defformat
 header-4
 '((t (:inherit pier-header-face :height 1.15)))
 "^!!!!\\([^!].*\\)$")

(pier-defformat
 script
 '((t (:inherit pier-monospaced-face)))
 "\\[\\[\\[[[anything]]\\]\\]\\]")

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.pier$" . pier-mode))

(provide 'pier-mode)
;;; pier-mode.el ends here
