;;; flowtype-mode.el --- Derived mode for JSX with flow types -*- lexical-binding: t -*-
;; Package-Requires: ((dash "2.12.1"))

;;; Commentary:
;; Emacs derived mode for flowtype.

(require 'web-mode)                ; jsx minor mode
(require 'flycheck)                ; error display
(require 'eldoc)                   ; type at pos display
(require 'tabulated-list)          ; To display statistics
(require 'company)                 ; for autocomplete

(require 'json)                    ; parsing util
(require 'dash)                    ; sane list helpers

;;; Code:

(add-to-list 'magic-mode-alist '("/\\* @flow" . flowtype-mode))

(defcustom flowtype:buffer-name "*flow*"
  "buffer name."
  :type 'string
  :group 'flowtype)

(defcustom flowtype:uncovered-type-background-color "#ff9999"
  "background-color for undefined types."
  :type 'string
  :group 'flowtype)

(defcustom flowtype:base-path nil
  "base path for .flowconfig."
  :type 'string
  :group 'flowtype)


;; helper

(defmacro flowtype|measure-time (&rest body)
  "Measure the time it takes to evaluate BODY."
  `(let* ((time (current-time))
          (result ,@body))
     (message "%.06f" (float-time (time-since time)))
     result))

(defun flowtype//call-flow-into-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (flowtype|measure-time
   (let ((buf (generate-new-buffer flowtype:buffer-name)))
     (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args)
     buf)))

(defun flowtype//call-flow-on-current-buffer (&rest args)
  "Calls flow with args on the current buffer, returns the result."
  (flowtype|measure-time
   (let* ((buf (generate-new-buffer flowtype:buffer-name)))
     (unwind-protect
         (let* ((_ (message "flowtype: calling flow %s with %s" default-directory args))
                (result (apply 'call-process-region (point-min) (point-max) "flow" nil buf nil args))
               (output (with-current-buffer buf (buffer-string))))
           (when (not (= result 0))
             (message "flowtype: got status from flow %s" result))
           output)
       (kill-buffer buf)))))

(defun flowtype//call-flow-on-current-buffer-async (result-handler &rest args)
  "Calls flow with args on the current buffer asynchronously; passes the result to result-handler."
  (let* ((buf (generate-new-buffer flowtype:buffer-name))
         (process (apply #'start-process "flow" buf "flow" args)))
    (set-process-sentinel process
                          (lambda (process event)
                            (when (equal 'exit (process-status process))
                              (let ((output (with-current-buffer (process-buffer process) (buffer-string))))
                                (kill-buffer (process-buffer process))
                                (funcall result-handler output)))))
    (when (process-live-p process)
      (with-demoted-errors "flowtype: error calling flow: %s"
          (process-send-region process (point-min) (point-max))
          (process-send-eof process)))))

(defun flowtype//json-flow-call (&rest args)
  "Calls flow on the current buffer passing --json, parses the result."
  (let* ((args (append args '("--json")))
         (output (apply #'flowtype//call-flow-on-current-buffer args)))
    (when output
      (json-read-from-string output))))

(defun flowtype//json-flow-call-async (result-handler &rest args)
  "Calls flow on the current buffer passing --json asynchronously; parses the result and gives it to result-handler."
  (let ((args (append args '("--json")))
        (handler (lambda (output) (funcall result-handler (json-read-from-string output)))))
    (apply #'flowtype//call-flow-on-current-buffer-async handler args)))

(defun flowtype//column-number-at-pos (pos)
  "column number at pos"
  (save-excursion (goto-char pos) (current-column)))

(defun flowtype//pos-to-flow-location (pos)
  "Returns a list of (line col) for pos in the current buffer."
  (let ((line (line-number-at-pos pos))
        (col (1+ (flowtype//column-number-at-pos pos))))
    (list (number-to-string line) (number-to-string col))))



;; flycheck

(flycheck-define-checker javascript-flowtype-eslint
  "A Javascript syntax and style checker using eslint.

See URL `https://github.com/eslint/eslint'."
  :command ("eslint" "--parser=babel-eslint" "--format=checkstyle"
            (config-file "--config" flycheck-eslintrc)
            (option "--rulesdir" flycheck-eslint-rulesdir)
            "--stdin" "--stdin-filename" source-original)
  :standard-input t
  :error-parser flycheck-parse-checkstyle
  :error-filter (lambda (errors)
                  (seq-do (lambda (err)
                            ;; Parse error ID from the error message
                            (setf (flycheck-error-message err)
                                  (replace-regexp-in-string
                                   (rx " ("
                                       (group (one-or-more (not (any ")"))))
                                       ")" string-end)
                                   (lambda (s)
                                     (setf (flycheck-error-id err)
                                           (match-string 1 s))
                                     "")
                                   (flycheck-error-message err))))
                          (flycheck-sanitize-errors errors))
                  errors)
  :modes (flowtype-mode))
(add-to-list 'flycheck-checkers 'javascript-flowtype-eslint)

(defun flowtype//fc-convert-part (error-part checker counter)
  (let* ((desc (cdr (assoc 'descr error-part)))
         (line (cdr (assoc 'line error-part)))
         (col  (cdr (assoc 'start error-part)))
         (path  (cdr (assoc 'path error-part))))
    (flycheck-error-new-at line col 'error desc :checker checker :id counter :filename path)))

(defun flowtype//fc-convert-error (error checker counter filename)
  "Return a list of errors from ERROR."
  (let* ((msg-parts (cdr (assoc 'message error)))
         (conv (mapcar (lambda (part)
                         (flowtype//fc-convert-part part checker counter))
                       msg-parts)))
    (when (-any? (lambda (err) (equal (flycheck-error-filename err) filename)) conv)
      conv)))

(defun flowtype//fc-parse-status-errors (output checker buffer)
  "Parse flow status errors in OUTPUT."
  (let* ((json (json-read-from-string output))
         (errors (cdr (assoc 'errors json)))
         (counter 1)
         (errs (-mapcat (lambda (err)
                          (let* ((conv (flowtype//fc-convert-error err
                                                                 checker
                                                                 (number-to-string counter)
                                                                 (buffer-file-name))))
                            (when conv (setq counter (1+ counter)))
                            conv))
                        errors)))
    ;; (message "done: %s" errs)
    errs))

(defun flowtype//fc-convert-suggest (header content checker)
    ;; (message "%s: %s" header content)
    (let* ((matches (string-match "-\\([^,]\*\\)" header))
           (offset (string-to-number (match-string 1 header))))
    ;; (message "%s: %s" offset content)
    (list (flycheck-error-new-at offset 0 'warning content :checker checker))))

(defun flowtype//fc-parse-suggest (output checker buffer)
  "Parse diffs in OUTPUT."
  (let* ((regexp (rx-to-string (rx (and "@@" (submatch (zero-or-more any))))))
         ;; (_ (message "matches: %s" regexp))
         (hunks (cdr (split-string output "@@" t)))
         (suggests (-mapcat (lambda (hunk) (flowtype//fc-convert-suggest (first hunk) (second hunk) checker))
                           (-partition 2 hunks))))
    ;; (message "matches: %s" suggests)
    suggests))

(with-eval-after-load 'flycheck
  (flycheck-define-command-checker 'javascript-flowtype
    "A JavaScript syntax and style checker using Flow."
    :command '("flow" "status" "--json")
    :error-parser #'flowtype//fc-parse-status-errors
    :modes '(flowtype-mode))

  (add-to-list 'flycheck-checkers 'javascript-flowtype))

(with-eval-after-load 'flycheck
  (flycheck-define-checker javascript-flowtype-suggest
    "A JavaScript syntax and style checker using Flow."
    :command ("flow" "suggest" source-inplace)
    :error-parser flowtype//fc-parse-suggest
    :modes flowtype-mode)

  (add-to-list 'flycheck-checkers 'javascript-flowtype-suggest)
  (flycheck-add-next-checker 'javascript-flowtype 'javascript-flowtype-suggest)
  (flycheck-add-next-checker 'javascript-flowtype-suggest 'javascript-flowtype-eslint))


;; commands

(defun flowtype//get-def (pos)
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call "get-def" filename loc)))

(defun flowtype//suggest-into-buffer ()
  "Calls flow to get the definition location of the thing at pos, returns the result."
  (interactive)
  (let* ((filename (buffer-file-name))
         (diff-buffer (flowtype//call-flow-into-buffer "suggest" filename)))
    (ediff-patch-file 2 diff-buffer)))

(defun flowtype//show-flow-loc (loc)
  "Takes a flow location info and shows it."
  (let* ((filename (cdr (assq 'path loc)))
         (line (cdr (assq 'line loc)))
         (col (cdr (assq 'start loc))))
    (when (not (eq filename ""))
      (find-file filename)
      (goto-char (point-min))
      (forward-line (1- line))
      (forward-char (1- col)))))

(defun flowtype/get-def-at-point ()
  "Show the definition of the thing at point using flow."
  (interactive)
  (let* ((loc (flowtype//get-def (point)))
        (_ (message "%s" loc)))
    (flowtype//show-flow-loc loc)))

(defun flowtype//type-at-pos-async (result-handler pos)
  "Calls flow to get the type at pos asynchronously; passes the result to result-handler."
  (let* ((loc (flowtype//pos-to-flow-location pos))
         (filename (buffer-file-name)))
    (apply #'flowtype//json-flow-call-async result-handler "type-at-pos" filename loc)))

(defun flowtype//eldoc-show-type-info (data)
  "Shows the passed type info using eldoc."
  (let* ((type (cdr (assq 'type data))))
    (message "flowtype: type %s" type)
    (when (not (equal "(unknown)" type))
      (eldoc-message type))))

(defun flowtype/eldoc-show-type-at-point ()
  "Shows type at point."
  (interactive)
  (flowtype//type-at-pos-async #'flowtype//eldoc-show-type-info (point))
  nil)


;; error list

(defun flowtype//fetch-errors ()
  "fetch all errors from process"
  (let* ((json (flowtype//json-flow-call "status" flowtype:base-path))
         (errors (cdr (assoc 'errors json)))
         (_ (message "%s" errors)))
    errors))

(defun flowtype//get-path (err)
  "get path from ERR"
  (let* ((message (cdr (assoc 'message err)))
         (first-elem (first message))
         (path (assoc 'path first-elem)))
    path))

(defun flowtype//errors-tabulate ()
  "Tabulate current statistics for major mode display."
  (let* ((errors-alist (flowtype//fetch-errors))
         (errs (mapcar #'flowtype//get-path errors-alist)))
    (message "foo: %s" errs)
    (list (list "123" (vector "123" "foo")))))

(define-derived-mode flowtype-stats-mode tabulated-list-mode "flowtype-errors"
  "Mode for listing errors of flowtype-mode."
  (setq tabulated-list-format [("#" 5 :right-align t :pad-right 2)
                               ("File" 0)]
        tabulated-list-sort-key (cons "File" nil)
        tabulated-list-padding 1
        tabulated-list-entries #'flowtype//errors-tabulate)
  (tabulated-list-init-header))

;;;###autoload
(defun flowtype//display-errors ()
  "Display buffer with all errors."
  (interactive)
  (pop-to-buffer "*flowtype-errors*")
  (flowtype-stats-mode)
  (tabulated-list-print))


;; company provider

(defun flowtype//fetch-completions (&rest _)
  (interactive "P")
  (let* ((loc (flowtype//pos-to-flow-location (point)))
         (filename (buffer-file-name))
         (response (flowtype//json-flow-call "autocomplete" (car loc) (cadr loc)))
         (result (cdr (assoc 'result response)))
         (names (mapcar (lambda (res) (cdr (assoc 'name res))) result)))
    names))

(defun company-flowtype-backend (command &optional arg &rest ignored)
  (interactive (list 'interactive))

  (case command
    (interactive (company-begin-backend 'company-flowtype-backend))
    (prefix (and (eq major-mode 'flowtype-mode)
                 (company-grab-symbol)))
    (candidates
     (progn
       (message "candidates %s" arg)
       (let* ((completes (flowtype//fetch-completions))
              (_ (message "names %s" completes))
              (list (remove-if-not
                    (lambda (c) (string-prefix-p arg c))
                    completes)))
         (message "list %s" list)
         list)))))

(add-to-list 'company-backends 'company-flowtype-backend)

;; coverage overlays

(defun flowtype//clear-cov-overlays ()
  (interactive)
  "Clear all flowtype overlays in current buffer."
  (remove-overlays (point-min) (point-max) 'flowtype t))

(defun flowtype//make-overlay (tuple)
  "Make overlay for values in TUPLE."
  (let* ((linepos (point-at-bol (car tuple))))
    (make-overlay (- (+ linepos (nth 1 tuple)) 1) (+ linepos (nth 3 tuple)))))

(defun flowtype//overlay-put (ovl color)
  "Record actual overlay in OVL with COLOR."
  (overlay-put ovl 'face (cons 'background-color color))
  (overlay-put ovl 'flowtype t))

(defun flowtype//overlay-current-buffer-with-list (tuple-list)
  "Overlay current buffer acording to given TUPLE-LIST."
  (save-excursion
    (goto-char (point-min))
    (flowtype//clear-cov-overlays)
    (dolist (ovl (mapcar #'flowtype//make-overlay tuple-list))
      (flowtype//overlay-put ovl flowtype:uncovered-type-background-color))))

(defun flowtype//parse-raw-type (type)
  "Parse raw TYPE into tuple."
  (message "type: %s" type)
  (list (cdr (assoc 'line type))
        (cdr (assoc 'start type))
        (cdr (assoc 'endline type))
        (cdr (assoc 'end type))
        (cdr (assoc 'type type))))

(defun my-filter (condp lst)
  (delq nil
        (mapcar (lambda (x) (and (funcall condp x) x)) lst)))

(defun flowtype//untyped? (type)
  "True if type of TYPE is \"\" or any."
  (let* ((typename (cdr (assoc 'type type))))
    ;; (message "type: %s" typename)
    (or (string= "any" typename)
        (string= "" typename))))

(defun flowtype//parse-raw-types (types)
  "Parse raw TYPES into tuples."
  (let* ((untyped (my-filter #'flowtype//untyped? types))
         (parsed (mapcar #'flowtype//parse-raw-type untyped)))
    ;; (message "parsed: %s" parsed)
    parsed))

(defun flowtype//fetch-coverage (filename)
  "Fetch coverage data for FILENAME from flow."
  (interactive "f")
  (when (and flowtype:base-path (string-prefix-p flowtype:base-path filename))
    (let* ((data (flowtype//json-flow-call "dump-types" filename)))
      (flowtype//parse-raw-types data))))

(defun flowtype//file-load-callback ()
  "Initialize overlays in buffer after loading."
  (interactive)
  (when (eq major-mode 'flowtype-mode)
    (let* ((filename (buffer-file-name))
           (buffer-coverage-data (flowtype//fetch-coverage filename)))
      (when buffer-coverage-data
        (message (format "flowtype: coverage for file: %s" filename))
        (flowtype//overlay-current-buffer-with-list buffer-coverage-data)))))


;; mode

(defun flowtype//add-hooks ()
  "Add mode hooks."
  (add-hook 'find-file-hook #'flowtype//file-load-callback))


(define-derived-mode flowtype-mode
  web-mode "FlowJSX"
  "Major mode for JSX with flow types."
  (setq web-mode-content-type "jsx")
  (setq web-mode-markup-indent-offset 2)
  (set (make-local-variable 'eldoc-documentation-function) #'flowtype/eldoc-show-type-at-point)
  (make-local-variable 'flowtype--ast)
  (turn-on-eldoc-mode)
  (company-mode 1)
  (flycheck-mode 1)
  (flowtype//add-hooks))

(provide 'flowtype-mode)
;;; flowtype-mode ends here
