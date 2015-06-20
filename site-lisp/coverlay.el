;;; coverlay.el --- Test coverage overlay for Emacs

;; Copyright (C) 2011-2014 Takuto Wada

;; Author: Takuto Wada <takuto.wada at gmail com>
;; Keywords: coverage, overlay
;; Homepage: https://github.com/twada/coverlay.el
;; Version: 0.4.0

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

(require 'filenotify)

;;; Commentary:
;; ------------
;;
;; Load coverlay.el in your .emacs
;;
;;     (require 'coverlay)
;;
;; Load lcov file into coverlay buffer
;;
;;     M-x coverlay-load-file /path/to/lcov-file
;;
;; Toggle overlay
;;
;;     M-x coverlay-toggle-overlays
;;
;;; Code:

(defvar coverlay-alist nil)
(defvar coverlay--watch-descriptor nil)

;;
;; Customizable variables

(defgroup coverlay nil
  "Test coverage overlay for Emacs."
  :group 'tools
  :prefix "coverlay:")

(defcustom coverlay:data-buffer-name "*coverlay-stats*"
  "temp buffer name for coverage data."
  :type 'string
  :group 'coverlay)

(defcustom coverlay:untested-line-background-color "red4"
  "background-color for untested lines."
  :type 'string
  :group 'coverlay)


;;
;; command: coverlay-load-file

;;;###autoload
(defun coverlay-load-file (filepath)
  "(re)load coverage data"
  (interactive (list (read-file-name "lcov file: ")) )
  (coverlay-create-buffer-from-filepath filepath))

;;;###autoload
(defun coverlay-watch-file (filepath)
  "watch file for coverage data"
  (interactive (list (read-file-name "lcov file: ")) )
  (coverlay-end-watch)
  (setq coverlay--watch-descriptor
        (file-notify-add-watch filepath '(change)
                               'coverlay-watch-callback)))

(defun coverlay-end-watch ()
  "removes the filewatch"
  (file-notify-rm-watch coverlay--watch-descriptor))

(defun coverlay-watch-callback (args)
  "callback for state-buffer change"
  (let ((filepath (nth 2 args)))
    (progn
      (message (format "coverlay updating from %s" filepath))
      (coverlay-create-buffer-from-filepath filepath))))

(defun coverlay-create-buffer-from-filepath (filepath)
  "get or create stats buffer from filepath"
  (let ((stats-buf (coverlay-create-stats-buffer filepath)))
    (setq coverlay-alist (coverlay-create-stats-alist-from-buffer stats-buf))))

(defun coverlay-create-stats-buffer (data-file-path)
  "get or create buffer filled with contents specified as data-file-path"
  (with-current-buffer (get-buffer-create coverlay:data-buffer-name)
    (erase-buffer)
    (insert-file-contents data-file-path)
    (goto-char (point-min))
    (current-buffer)))

(defun coverlay-create-stats-alist-from-buffer (buf)
  (coverlay-tuplize-cdr-of-alist (coverlay-reverse-cdr-of-alist (coverlay-parse-buffer buf))))

(defun coverlay-reverse-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo)) (English . (foo bar baz))) to '((Japanese . (piyo fuga hoge)) (English . (baz bar foo)))"
  (mapcar 'coverlay-reverse-cdr target-alist))

(defun coverlay-reverse-cdr (target-list)
  (cons
   (car target-list)
   (reverse (cdr target-list))))

(defun coverlay-tuplize-cdr-of-alist (target-alist)
  "convert '((Japanese . (hoge fuga piyo moge)) (English . (foo bar baz moo)))  to '((Japanese . ((hoge fuga) (piyo moge)) (English . ((foo bar) (baz moo))))"
  (mapcar 'coverlay-tuplize-cdr target-alist))

(defun coverlay-tuplize-cdr (target-list)
  (progn
    (setcdr target-list (coverlay-create-tuple-pairs (cdr target-list)))
    target-list))

(defun coverlay-create-tuple-pairs (even-list)
  "convert (foo bar baz hoge) to ((foo bar) (baz hoge))"
  (let ((result '()))
    (while even-list
      (setq result (cons (list (car even-list) (car (cdr even-list))) result))
      (setq even-list (nthcdr 2 even-list)))
    (nreverse result)))

(defun coverlay-parse-buffer (buf)
  "Parse buffer to alits. car of each element is filename, cdr is segment of lines."
  (let (alist filename)
    (with-current-buffer buf
      (while (not (eobp))
        (let ((current-line (coverlay-current-line)))
         (when (coverlay-source-file-p current-line)
           (setq filename (coverlay-extract-source-file current-line))
           (when (not (assoc filename alist))
             ;; (print (format "segments for %s does not exist" filename))
             (setq alist (cons (list filename) alist))))
         (when (coverlay-data-line-p current-line)
           (let* ((cols (coverlay-extract-data-list current-line))
                  (lineno (nth 0 cols))
                  (count (nth 1 cols)))
             (when (= count 0)
               ;; (print (format "count %d is zero" count))
               (coverlay-handle-uncovered-line alist filename lineno)))))
        (forward-line 1))
      alist)))

(defun coverlay-handle-uncovered-line (alist filename lineno)
  (let* ((file-segments (assoc filename alist))
         (segment-list-body (cdr file-segments)))
    (if (not segment-list-body)
        (setcdr file-segments (list lineno lineno))
      (if (= (car segment-list-body) (- lineno 1))
          (setcar segment-list-body lineno)
        (setcdr file-segments (append (list lineno lineno) segment-list-body))))))

(defun coverlay-current-line ()
  (buffer-substring (point)
                    (save-excursion
                      (end-of-line)
                      (point))))

(defun coverlay-source-file-p (line)
  (coverlay-string-starts-with line "SF:"))

(defun coverlay-data-line-p (line)
  (coverlay-string-starts-with line "DA:"))

(defun coverlay-end-of-record-p (line)
  (coverlay-string-starts-with line "end_of_record"))

;; http://www.emacswiki.org/emacs/ElispCookbook#toc4
(defun coverlay-string-starts-with (s begins)
  "Return non-nil if string S starts with BEGINS."
  (cond ((>= (length s) (length begins))
         (string-equal (substring s 0 (length begins)) begins))
        (t nil)))

(defun coverlay-extract-source-file (line)
  (coverlay-extract-rhs line))

(defun coverlay-extract-data-list (line)
  (mapcar 'string-to-number (split-string (coverlay-extract-rhs line) ",")))

(defun coverlay-extract-rhs (line)
  (substring line (+ (string-match "\:" line) 1)))


;;
;; command: coverlay-toggle-overlays

;;;###autoload
(defun coverlay-toggle-overlays (buffer)
  "toggle coverage overlay"
  (interactive (list (current-buffer)))
  (if (not coverlay-alist)
      (message "coverlay.el: Coverage data not found. Please use `coverlay-load-file` to load them.")
    (with-current-buffer buffer
      (if (coverlay-overlay-exists-p)
          (coverlay-clear-cov-overlays)
        (coverlay-overlay-current-buffer-with-list
         (coverlay-stats-tuples-for buffer coverlay-alist))))))

(defun coverlay-overlay-exists-p ()
  (coverlay-overlay-exists-in-list-p (overlays-in (point-min) (point-max))))

(defun coverlay-overlay-exists-in-list-p (ovl-list)
  (catch 'loop
    (dolist (ovl ovl-list)
      (if (overlay-get ovl 'coverlay) (throw 'loop t) nil)
      nil)))

(defun coverlay-clear-cov-overlays ()
  (remove-overlays (point-min) (point-max) 'coverlay t))

(defun coverlay-overlay-current-buffer-with-list (tuple-list)
  (save-excursion
    (goto-char (point-min))
    (dolist (ovl (coverlay-map-overlays tuple-list))
      (progn
        (overlay-put ovl 'face (cons 'background-color coverlay:untested-line-background-color))
        (overlay-put ovl 'coverlay t)
        ))))

(defun coverlay-map-overlays (tuple-list)
  "make-overlay for each of a TUPLE(two line-numbers) LIST."
  (mapcar 'coverlay-make-overlay tuple-list))

(defun coverlay-make-overlay (tuple)
  (make-overlay (point-at-bol (car tuple)) (point-at-eol (cadr tuple))))

(defun coverlay-stats-tuples-for (buffer stats-alist)
  (cdr (assoc (expand-file-name (buffer-file-name buffer)) stats-alist)))

;;;###autoload
(define-minor-mode coverlay-mode
  "overlays for uncovered lines"
  :lighter " lcov"
  :global t
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c l") 'coverlay-toggle-overlays)
            map))

(provide 'coverlay)
;;; coverlay.el ends here
