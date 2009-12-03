;;; variant.el --- Convert between various English spellings.
;; 
;; $Revision: $
;; $Date: $

;; This file is not part of Emacs

;; Author: Phillip Lord
;; Maintainer: Phillip Lord

;; COPYRIGHT NOTICE
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;; 
;; This file helps with the process of converting between different
;; variants of English.  Often, you have text which you want to publish
;; in different places with different requirements for spellings and
;; dialect.  While ispell.el can help with this, it is a little
;; cumbersome; if the text has already spell checked previous in a
;; different dialect, then it is irritating to have to skip all the
;; abbreviations and specialist words again.

;; There are only three entry points `variant-translate-buffer' and
;; `variant-translate-region', which do pretty much what you expect.
;; You can change language variants with `variant-set-language'. There
;; are two real English variantions, "british-ise" and "british-ize"
;; for those people who like z's.


;; There is one non-use function, `variant-create-abbc-list' which
;; generates the file which contains the data. This uses the word
;; lists from http://wordlist.sourceforge.net. This expects an
;; existing file called variant-abbc.el.

;;; Status:
;;
;; This file has been developed and tested on Emacs 21 under
;; Windows. It's one of those occasional use files, so it's not
;; heavily tested.

;;; History:
;; 

;;; Code:

(require 'variant-abbc)
(require 'assoc)
(eval-when-compile
  (require 'cl))

(defface variant-face
  '((t
     (:weight bold :slant normal  :foreground "White" :background "Red")))
  "Face for highlighting words"
  :group 'variant)


(defvar variant-default-language
  '("british-ise" "american")
  "The default language variants to convert between.

The names should be the same as in `variant-abbc-list-order' or things
are going to break")



;; END of user modifiable variables.

;; working if it's there..
(eval-and-compile
  (condition-case nil
      (require 'working)
    (error
     (progn
       (defmacro working-status-forms (message donestr &rest forms)
         "Contain a block of code during which a working status is shown."
         (list 'let (list (list 'msg message) (list 'dstr donestr)
                          '(ref1 0))
               (cons 'progn forms)))
       
       (defun working-status (&optional percent &rest args)
         "Called within the macro `working-status-forms', show the status."
         (message "%s%s" (apply 'format msg args)
                  (if (eq percent t) (concat "... " dstr)
                    (format "... %3d%%"
                            (or percent
                                (floor (* 100.0 (/ (float (point))
                                                   (point-max)))))))))
       
       (defun working-dynamic-status (&optional number &rest args)
         "Called within the macro `working-status-forms', show the status."
         (message "%s%s" (apply 'format msg args)
                  (format "... %c" (aref [ ?- ?/ ?| ?\\ ] (% ref1 4))))
         (setq ref1 (1+ ref1)))
       
       (put 'working-status-forms 'lisp-indent-function 2)))))



;; code for reading the basic files that are necessary.  this stuff
;; will have to change if I want to get the system working with
;; voc.tab

;; None of these functions are required for use

(defvar variant-abbc-list-order
  '("american" "british-ise" "british-ize" "canadian")
  "Variant order in the abbc.tab file")
    
(defun variant-read-abbc-file()
  "Read the abbc file.

This file contains the basic spelling conversions. Returns an alist
keys by name of lists"
  (with-temp-buffer
    (insert-file-contents "abbc.tab")
    (let ((current-line 0)
          (total-line (count-lines (point-min) (point-max)))
          (abbc-list))
      (save-excursion
        ;; store the languages in an alist--we can't use a hash as it
        ;; has no write syntax.
        (amake 'abbc-list variant-abbc-list-order)
        (goto-char (point-min))
        (working-status-forms "Generating variant-abbc.el" "done"
          (while (< current-line total-line)
            (setq current-line (count-lines (point-min) (point)))
            (working-status (/ (* 100 current-line) total-line))
            (let* ((current-line-contents (buffer-substring-no-properties
                                 (line-beginning-position)
                                 (line-end-position)))
                   (split (split-string current-line-contents)))
              ;; if non nil
              (if split
                  ;; iterate through the list, attach values to the
                  ;; back end of the alist
                  (loop for i from 0 below (length variant-abbc-list-order) do
                        (let* ((variant (nth i variant-abbc-list-order))
                               (word-list (aget abbc-list variant t))
                               (word (nth i split)))
                          (push word word-list)
                          (aput 'abbc-list variant word-list)))))
            (forward-line))
          (working-status t)))
      abbc-list)))


(defun variant-create-abbc-list()
  "Create the data file required for this package.

This function assumes the a lot. It is not really an end user
function."
  (interactive)
  (save-excursion
    (find-file "variant-abbc.el")
    (goto-char (point-min))
    (search-forward ";;; Code:")
    (delete-region (point) (point-max))
    (insert "\n(setq variant-abbc \n '")
    (pp (variant-read-abbc-file) (current-buffer))
    (insert ")\n(provide 'variant-abbc)")
    (save-buffer)
    (load-file "variant-abbc.el")
    (setq variant-current-hash nil)
    (kill-buffer (current-buffer))))



;; set the language, and get the hash information that is needed.

(defun variant-generate-hash (variant-symbol-from variant-symbol-to)
  "Generate a hash for the give langauges.
Argument VARIANT-SYMBOL-FROM describes varient to convert from.
Argument VARIANT-SYMBOL-TO describes varient to convert to."
  (let ((variant-hash (make-hash-table :test 'equal))
        (variant-from
         (cdr
          (assoc variant-symbol-from variant-abbc)))
        (variant-to
         (cdr
          (assoc variant-symbol-to variant-abbc))))
    (loop for i from 0 below (length variant-from) do
          (puthash
           (nth i variant-from)
           (nth i variant-to)
           variant-hash))
    variant-hash))


(defvar variant-current-hash nil)

(defun variant-set-language ()
  "Choose the language variants to convert between with completion."
(interactive)
  (let ((from (variant-read-language "Variant From: "))
        (to (variant-read-language "Variant To: ")))
    (variant-set-language-impl from to)))

(defun variant-read-language (prompt)
  (completing-read prompt variant-abbc
                   nil t ""))

(defun variant-set-language-impl (from to)
  (setq variant-current-hash
        (variant-generate-hash from to)))

(defun variant-get-current-hash ()
  "Get the hash for the current langauge."
  (unless variant-current-hash
    (setq variant-current-hash
          (variant-set-language-impl
           (car variant-default-language)
           (cadr variant-default-language))))
  variant-current-hash)


;; main check functions....
(defun variant-translate-buffer ()
  "Translate between langauge variants in the buffer.

This function attempts to find usage of English variants and convert
between them.  Very useful if you are British and find yourself writing
for American publishers.  This way you break all the spellings last
thing."
  (interactive)
  (variant-translate-region (point-min)(point-max)))


(defun variant-translate-region (start end)
  "Translate between language variants in the between START and END."
  (interactive "r\nP")
  (save-window-excursion
    (variant-translate-region-impl start end)))


(defun variant-translate-region-impl (start end)
  (save-excursion
    (goto-char start)
    (while (< (point) (point-max))
      (let ((replacement-word
             (gethash (word-at-point)
                      (variant-get-current-hash)))
            (bounds (bounds-of-thing-at-point 'word)))
        (if replacement-word
            (variant-replace-suggest
             (car bounds)(cdr bounds)
             (word-at-point)
             replacement-word))
        (forward-word 1)))))

;; this code is depressingly close to ispell.el
(defvar variant-overlay nil)

(defun variant-replace-suggest (bound-from bound-to from to)
  (unwind-protect
      (progn
        (setq variant-overlay (make-overlay 1 1))
        (overlay-put variant-overlay 'face 'variant-face)
        (move-overlay variant-overlay bound-from bound-to (current-buffer))
        (when (y-or-n-p (format "Replace %s with %s? " from to))
          (delete-region bound-from bound-to)
          (insert to)))
    (delete-overlay variant-overlay)
    (message nil)))

    

                       


;; (pabbrev-debug-print-hash (variant-get-current-hash))
        
  


(provide 'variant)

;;; variant.el ends here
