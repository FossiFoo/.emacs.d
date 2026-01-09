;;; init --- init ALL the things!

;;; Commentary:

;;; Code:

;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error nil)    ; now you should get a backtrace

;; (add-to-list (or 'customs-theme-load-path '()) "~/.emacs.d/site-lisp/base16-theme")

;; (setq package-check-signature nil)

(when (boundp 'custom-theme-load-path) (add-to-list 'custom-theme-load-path "~/.emacs.d/site-lisp/base16-theme"))

;; package.el
(require 'package)

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
	         '("gnu" . "https://elpa.gnu.org/packages/") t)
(add-to-list 'package-archives
	         '("nongnu" . "https://elpa.nongnu.org/nongnu/") t)

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/coverlay")
(add-to-list 'load-path "~/.emacs.d/site-lisp/flowtype-mode")
(add-to-list 'load-path "~/.emacs.d/site-lisp/gl-bluetooth")
(add-to-list 'load-path "~/.emacs.d/site-lisp/tapmacs")

(package-initialize)

(defvar package-list)
                                        ; list the packages you want
(setq package-list
      '(
        apheleia              ;; Reformat buffer stably
        async                 ;;Asynchronous processing in Emacs
        ;; base16-theme          ;;Collection of themes built on combinations of 16 base colors
        buffer-move           ;;easily swap buffers
        centered-cursor-mode  ;;cursor stays vertically centered
        cider                 ;;Clojure Interactive Development Environment that Rocks
        clj-refactor          ;;A collection of clojure refactoring functions
        clojure-mode          ;;Major mode for Clojure code
        color-theme           ;;install color themes
        company               ;;Modular text completion framework
        company-flow          ;;Flow backend for company-mode
        company-restclient    ;;company-mode completion back-end for restclient-mode
        company-shell         ;;Company mode backend for shell functions
        company-terraform     ;;A company backend for terraform
        cov                   ;;Show coverage stats in the fringe.
        dash                  ;;A modern list library for Emacs
        dash-functional       ;;Collection of useful combinators for Emacs Lisp
        easy-hugo             ;;Write blogs made with hugo by markdown or org-mode
        ejc-sql               ;;Emacs SQL client uses Clojure JDBC.
        epl                   ;;Emacs Package Library
        eslint-fix            ;;Fix JavaScript files using ESLint
        exec-path-from-shell  ;;Get environment variables such as $PATH from the shell
        f                     ;;Modern API for working with files and directories
        fix-muscle-memory     ;;Simple hacks to fix muscle memory problems
        flutter               ;;Tools for working with Flutter SDK
        flycheck              ;;On-the-fly syntax checking
        flycheck-clojure      ;;Flycheck: Clojure support
        ;; flycheck-golang       ;;Flycheck checker for golangci-lint
        flycheck-kotlin       ;; Support kotlin in flycheck
        flycheck-gometalinter ;;flycheck checker for gometalinter
        framesize             ;;change the size of frames in Emacs
        fuzzy                 ;;Fuzzy Matching
        gerrit-download       ;;Show gerrit reviews in a diff buffer.
        ggtags                ;;frontend to GNU Global source code tagging system
        groovy-mode           ;;Major mode for Groovy source files
	    gnu-elpa-keyring-update  ;;Update Emacs's GPG keyring for GNU ELPA
        go-eldoc              ;;eldoc for go-mode
        go-gen-test           ;;Generate tests for go code with gotests
        go-imports            ;;Insert go import statement given package name
        go-mode               ;;Major mode for the Go programming language
        go-rename             ;;Integration of the 'gorename' tool into Emacs.
        go-snippets           ;;Yasnippets for go
        gotest                ;;Launch GO unit tests
        highlight-parentheses ;;highlight surrounding parentheses
        highlight-symbol      ;;automatic and manual symbol highlighting
        hlinum                ;;Extension for linum.el to highlight current line number
        hydra                 ;;Make bindings that stick around.
        inflections           ;;convert english words between singular and plural
        jasminejs-mode        ;;A minor mode for manipulating jasmine test files
        json-mode             ;;Major mode for editing JSON files
        json-snatcher         ;;Grabs the path to JSON values in a JSON file
        know-your-http-well   ;;Look up the meaning of HTTP headers, methods, relations, status codes
        kotlin-mode           ;;Major mode for kotlin
        let-alist             ;;Easily let-bind values of an assoc-list by their names
        lsp-dart              ;;Dart support lsp-mode.
        ;; lsp-mode              ;;LSP mode
        ;; magit                 ;;A Git porcelain inside Emacs
        ;; magit-gerrit          ;;Magit plugin for Gerrit Code Review
        ;; magithub              ;;Magit interfaces for GitHub
        mic-paren             ;;advanced highlighting of matching parentheses
        multiple-cursors      ;;Multiple cursors for Emacs.
        nix-buffer            ;;Set up buffer environments with nix
        org-easy-img-insert   ;;An easier way to add images from the web in org mode
        org-preview-html      ;;automatically use eww to preview the current org file on save
        pkg-info              ;;Information about packages
        popup                 ;;Visual Popup User Interface
        queue                 ;;Queue data structure
        rainbow-mode          ;;Colorize color names in buffers
        react-snippets        ;;Yasnippets for React
        restclient            ;;An interactive HTTP client for Emacs
        s                     ;;The long lost Emacs string manipulation library.
        sass-mode             ;;Major mode for editing Sass files
        seq                   ;;Sequence manipulation functions
        smartparens           ;;Automatic insertion, wrapping and paredit-like navigation with user defined pairs.
        spinner               ;;Add spinners and progress-bars to the mode-line for ongoing operations
        terraform-mode        ;;Major mode for terraform configuration file
        tide                  ;;Typescript Interactive Development Environment
        timonier              ;;Manage Kubernetes Applications
        theme-changer         ;;Sunrise/Sunset Theme Changer for Emacs
        typescript-mode       ;;Major mode for editing typescript
  	    use-package           ;;Simplify .emacs
        ;; vue-mode              ;;Vuejs
        web-mode              ;;major mode for editing web templates
        with-editor           ;;Use the Emacsclient as $EDITOR
        yaml-mode             ;;Major mode for editing YAML files
        yasnippet             ;;Yet another snippet extension for Emacs.
        ))

                                        ; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

;;                                         ; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Local Things
(require 'gnu-elpa-keyring-update)

(setq desktop-path '("."))
(desktop-save-mode 1)

;; FIX
;; thing-cmd
;; (and
;;  (require 'thing-cmds)
;;  (global-set-key (kbd "M-?") 'cycle-thing-region)
;;  (global-set-key (kbd "M-@") 'cycle-thing-region) ; vs `mark-word'
;;  (global-set-key (kbd "C-M-?")  'mark-thing))
                                        ; vs `mark-sexp'

;;; Packaged things

(setq calendar-latitude 53.551086)
(setq calendar-longitude 9.993682)

;; FIX
;; (and (require 'theme-changer)
;;      (change-theme 'base16-atelierdune-light 'base16-atelierdune-dark))
;; (load-theme 'base16-atelierdune-light)
;; (set-variable 'coverlay:tested-line-background-color "#eeffdd")
;; (set-variable 'coverlay:untested-line-background-color "#ffeedd")
;;
;; (load-theme 'base16-atelierdune-dark)
;; (set-variable 'coverlay:tested-line-background-color "#002200")
;; (set-variable 'coverlay:untested-line-background-color "#331010")
;; (set-variable 'flowtype:uncovered-type-background-color "#550000")

;; (defun toggle-dark-light ()
;;   (interactive)
;;   (if (custom-theme-enabled-p 'base16-atelierdune-light)
;;       (and (load-theme 'base16-atelierdune-dark)
;;            (disable-theme 'base16-atelierdune-light)
;;            (set-variable 'coverlay:tested-line-background-color "#002200")
;;            (set-variable 'coverlay:untested-line-background-color "#331010")
;;            (set-variable 'flowtype:uncovered-type-background-color "#550000"))
;;     (and (load-theme 'base16-atelierdune-light)
;;          (disable-theme 'base16-atelierdune-dark)
;;          (set-variable 'coverlay:tested-line-background-color "#eeffdd")
;;          (set-variable 'coverlay:untested-line-background-color "#ffeedd"))))
;; (global-set-key [f4] 'toggle-dark-light)

;; FIX
;;presentation
(require 'framesize)
(set-frame-font "Inconsolata Nerd Font-14")
;; (set-frame-font "Inconsolata Nerd Font-20")
;; (set-default-font "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")
(menu-bar-mode -1)

;; magit
;; (require 'magit)
;; (require 'magit-gerrit)


;; centered cursor mode
(and
 (require 'centered-cursor-mode)
 (global-centered-cursor-mode +1)
 (global-set-key (kbd "<Scroll_Lock>") 'centered-cursor-mode))

;; line numbers
(global-display-line-numbers-mode t)
(require 'hlinum)
(hlinum-activate)

;; whitespace-mode
(and
 (require 'whitespace)
 (global-set-key "\C-c w" 'whitespace-mode)
 (global-set-key "\C-c t" 'whitespace-toggle-options)
 (global-set-key "\C-c=w" 'global-whitespace-mode)
 (global-set-key "\C-c=t" 'global-whitespace-toggle-options))

;; highlight-symbol
(and
 (require 'highlight-symbol)
 (define-global-minor-mode global-highlight-symbol-mode highlight-symbol-mode highlight-symbol-mode)
 (global-highlight-symbol-mode)
 (global-set-key [(control f3)] 'highlight-symbol-at-point)
 (global-set-key [f3] 'highlight-symbol-next)
 (global-set-key [(shift f3)] 'highlight-symbol-prev)
 (global-set-key [(meta f3)] 'highlight-symbol-prev)
 (global-set-key [(control meta f3)] 'highlight-symbol-query-replace))

(and
 (require 'highlight-parentheses)
 (define-global-minor-mode global-highlight-parenseses-mode highlight-parentheses-mode highlight-parentheses-mode)
 (global-highlight-parenseses-mode))

(and
 (require 'mic-paren)
 (paren-activate))

(and
 (require 'smartparens)
 (smartparens-global-mode))

;; Represent undo-history as an actual tree (visualize with C-x u)
(and (require 'undo-tree)
     (setq undo-tree-mode-lighter "")
     (global-undo-tree-mode))

;; use apheleia for indentation
(apheleia-global-mode +1)

;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; coverage in lcov format
(require 'coverlay)

;; clojure stuff
(require 'clojure-mode)
;; (require 'cider)
(require 'flycheck-clojure)
;; (add-to-list 'flycheck-checkers 'clojure-cider-eastwood)
;; (add-to-list 'flycheck-checkers 'clojure-cider-kibit)
;; (eval-after-load 'flycheck '(flycheck-clojure-setup))

;; (require 'clj-refactor)

(defun my-clojure-mode-hook ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

(defun cider-namespace-refresh ()
  (interactive)
  (cider-interactive-eval
   "(in-ns 'user)
    (reset)"))

(define-key clojure-mode-map (kbd "M-r") 'cider-namespace-refresh)

;;SQLRow

(require 'ejc-sql)

;;lsp

;; (use-package lsp-mode
;;   :ensure t
;;   :config (progn
;;             ;; use flycheck, not flymake
;;             (setq lsp-prefer-flymake nil)))
;; (use-package lsp-ui :ensure t)

;; golang

(defun gofmt-before-save ()
  (interactive)
  (when (eq major-mode 'go-mode)
    (lsp-format-buffer)
    (lsp-organize-imports)))

(use-package go-mode
  :ensure t
  :hook ((go-mode . lsp-deferred)
         (before-save . gofmt-before-save)))

;;snippets
(require 'yasnippet)
(yas-global-mode 1)
;;(yas/load-directory "~/.emacs.d/snippets/")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-go/")
(add-to-list 'yas-snippet-dirs "~/.emacs.d/snippets/yasnippet-typescript/")

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "<C-tab>") 'company-complete-common)
(add-to-list 'company-backends 'company-dabbrev)

;; (setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt yas/completing-prompt yas/x-prompt yas/no-prompt))

;; C#
(add-hook 'csharp-mode-hook #'lsp)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (web-mode-markup-indent-offset 2)
            (when (string-equal "tsx" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; enable typescript-tslint checker
(flycheck-add-mode 'typescript-tslint 'web-mode)

(require 'terraform-mode)

(require 'company-terraform)

;; flutter/dart
;; (use-package lsp-dart
;;   :config
;;   (when-let (dart-exec (executable-find "dart"))
;;     (let ((dart-sdk-path (-> dart-exec
;;                              file-chase-links
;;                              file-name-directory
;;                              directory-file-name
;;                              file-name-directory)))
;;       (setq lsp-dart-sdk-dir dart-sdk-path
;;             lsp-dart-dap-flutter-hot-reload-on-save t))))

(use-package dart-mode
  ;; Optional
  :hook (dart-mode . lsp-deferred))

(use-package flutter
  :after dart-mode
  :bind (:map dart-mode-map
              ("C-M-x" . #'flutter-run-or-hot-reload))
  :custom
  (flutter-sdk-path "/Applications/flutter/"))

;; Gupta

;; (require 'generic-x)
;; (define-generic-mode
;;     'gupta-mode
;;   '("\\!")
;;   '("If" "Else" "While" "Break" "Return"
;;     "String" "Boolean" "Long String" "Number" "Set"
;;     "not" "and" "or"
;;     "Function" "Call" "Parameters" "Returns" "Static Variables" "Local Variables"
;;     "Description"
;;     "TRUE" "FALSE")
;;   ;; `(("\\b@[a-z]+\\b" . 'font-lock-builtin-face)
;;   ;;   ("\\([a-zA-Z0-9_]+\\)\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)" (1 'font-lock-variable-name-face) (2 'font-lock-type-face))
;;   ;;   (")\\s-*:\\s-*\\([a-zA-Z0-9_]+\\)" . (1 'font-lock-type-face))
;;   ;;   ("^\\s-*\\(\\sw+\\)\\s-*=" . (1 'font-lock-type-face))
;;   ;;   ("^\\s-*\\(static\\s-*\\)?\\(\\sw+\\)\\s-*(" . (2 'font-lock-function-name-face)))
;;   nil
;;   '("\\.apt$", "\\.apt\\.indented$")
;;   nil
;;   "Mode for Gupta")

;; shell stuff from https://snarfed.org/why_i_run_shells_inside_emacs

(defvar my-local-shells
  '("*shell0*" "*shell1*" "*shell2*" "*shell3*" "*music*"))
(defvar my-remote-shells
  '() ;; '("*snarfed*" "*heaven0*" "*heaven1*" "*heaven2*" "*heaven3*")
  )
(defvar my-shells (append my-local-shells my-remote-shells))

;; kotlin format

(defun format-with-detekt-hook()
  (when (eq major-mode 'kotlin-mode)
    (let* ((root (lsp-workspace-root))
           (buffer-name (buffer-file-name))
           (file-dir (substring buffer-file-name (length root)))
           (prefix (split-string file-dir "/"))
           (root-dir (concat (nth 1 prefix) "/" (nth 2 prefix)))
           (full-dir (concat root "/" root-dir "/"))
           (default-directory full-dir))
      ;; (shell-command "../../gradlew detektFormat" nil nil)
      (start-process-shell-command "format" nil "../../gradlew detekt --auto-correct")
      )))
(add-hook 'after-save-hook 'format-with-detekt-hook)

(defun kotlin-mode-settings-hook()
  (lsp-deferred)
  (setq-local fill-indent-according-to-mode t)
  (setq-local lsp-enable-indentation nil)
  (setq-local lsp-before-save-edits nil)
  )
(add-hook 'kotlin-mode-hook 'kotlin-mode-settings-hook)

;; try to fix the kotlin lsp
(setq fill-indent-according-to-mode t)
(setq lsp-enable-indentation nil)
(setq lsp-before-save-edits nil)

(setenv "PAGER" "cat")

;; (require 'tramp)


;; own code ---------------------------

(use-package aidermacs
  :bind (("C-c a" . aidermacs-transient-menu))
  :config
  (setenv "OPENAI_API_KEY" "foo")
  (setenv "OPENAI_API_BASE" "http://localhost:1337/v1")
  :custom
  (aidermacs-use-architect-mode t)
  (aidermacs-default-model "openai/opencoder:8b"))

;; truncate buffers continuously
(add-hook 'comint-output-filter-functions 'comint-truncate-buffer)

(defun make-my-shell-output-read-only (text)
  "Add to comint-output-filter-functions to make stdout read only in my shells."
  (if (member (buffer-name) my-shells)
      (let ((inhibit-read-only t)
            (output-end (process-mark (get-buffer-process (current-buffer)))))
        (put-text-property comint-last-output-start output-end 'read-only t))))
(add-hook 'comint-output-filter-functions 'make-my-shell-output-read-only)

(defun my-dirtrack-mode ()
  "Add to shell-mode-hook to use dirtrack mode in my shell buffers."
  (when (member (buffer-name) my-shells)
    (shell-dirtrack-mode 0)
    (set-variable 'dirtrack-list '("^.*[^ ]+:\\(.*\\)>" 1 nil))
    (dirtrack-mode 1)))
(add-hook 'shell-mode-hook 'my-dirtrack-mode)

                                        ; interpret and use ansi color codes in shell output windows
(add-hook 'shell-mode-hook 'ansi-color-for-comint-mode-on)

(defun set-scroll-conservatively ()
  "Add to shell-mode-hook to prevent jump-scrolling on newlines in shell buffers."
  (set (make-local-variable 'scroll-conservatively) 10))
(add-hook 'shell-mode-hook 'set-scroll-conservatively)

;; i think this is wrong, and it buries the shell when you run emacsclient from
;; it. temporarily removing.
;; (defun unset-display-buffer-reuse-frames ()
;;   "Add to shell-mode-hook to prevent switching away from the shell buffer
;; when emacsclient opens a new buffer."
;;   (set (make-local-variable 'display-buffer-reuse-frames) t))
;; (add-hook 'shell-mode-hook 'unset-display-buffer-reuse-frames)

;; make it harder to kill my shell buffers
;; (require 'protbuf)
;; (add-hook 'shell-mode-hook 'protect-process-buffer-from-kill-mode)

(defun enter-again-if-enter ()
  "Make the return key select the current item in minibuf and shell history isearch.
An alternate approach would be after-advice on isearch-other-meta-char."
  (when (and (not isearch-mode-end-hook-quit)
             (equal (this-command-keys-vector) [13])) ; == return
    (cond ((active-minibuffer-window) (minibuffer-complete-and-exit))
          ((member (buffer-name) my-shells) (comint-send-input)))))
(add-hook 'isearch-mode-end-hook 'enter-again-if-enter)

(defadvice comint-previous-matching-input
    (around suppress-history-item-messages activate)
  "Suppress the annoying 'History item : NNN' messages from shell history isearch.
If this isn't enough, try the same thing with
comint-replace-by-expanded-history-before-point."
  (let ((old-message (symbol-function 'message)))
    (unwind-protect
        (progn (fset 'message 'ignore) ad-do-it)
      (fset 'message old-message))))

;; (defadvice comint-send-input (around go-to-end-of-multiline activate)
;;   "When I press enter, jump to the end of the *buffer*, instead of the end of
;; the line, to capture multiline input. (This only has effect if
;; `comint-eol-on-send' is non-nil."
;;   (flet ((end-of-line () (goto-char (point-max))))
;;     ad-do-it))

;; bash

(require 'bash-completion)
(bash-completion-setup)


;; serial-term
(require 'term)

(defvar fterm-queue '())
(defvar fterm-sent 0)
(defvar fterm-len 0)
(defvar fterm-half-ack "")

(defun replace-all (string to-find to-replace)
  (let ((index  (cl-search to-find string))
        (pos    0)
        (result ""))
    (while index
      (setq result (concat result
                           (substring string pos index)
                           to-replace)
            pos    (+ index (length to-find))
            index  (cl-search to-find string :start2 pos)))
    (concat result (substring string pos))))

(require 'ansi-color)

(defun rephone-serial-process-filter (process output)
  "Replace LF in output string with CR+LF."
  (term-emulate-terminal process
                         (progn
                           (let* ((found (string-match-p (regexp-quote "ok.") (concat fterm-half-ack output)))
                                  (out output ;; (if found (replace-all output "ok." "\e[0;32mok.\e[0;0m") output)
                                       ))
                             (if found
                                 (progn
                                   (setq fterm-half-ack "")
                                   (run-with-timer 0 nil (lambda () (fterm-send-queue process))))
                               (let ((l (length out)))
                                 (when (and l (> l 1))
                                   (setq fterm-half-ack (substring out -2)))))
                             (run-with-timer 0 nil 'accept-process-output)
                             (replace-all out
                                          (byte-to-string ?\n)
                                          (string ?\r ?\n))))))

(defun rephone-serial-term (port)
  "Basically duplicate SERIAL-TERM from term.el but with process
  filtering to translate LF to CR+LF."
  (interactive (list (serial-read-name)))
  (serial-supported-or-barf)
  (let* ((process (make-serial-process
                   :port port
                   :speed 115200
                   :bytesize 8
                   :parity nil
                   :stopbits 1
                   :flowcontrol nil
                   :noquery t
                   :filter 'rephone-serial-process-filter
                   :sentinel 'term-sentinel))
         (buffer (process-buffer process)))
    (with-current-buffer buffer
      (term-mode)
      (term-line-mode)
      (goto-char (point-max))
      (set-marker (process-mark process) (point)))
    (switch-to-buffer buffer)
    buffer))

(defun sterm-send-string (proc str)
  "Send to PROC the contents of STR as input.
This is equivalent to `process-send-string', except that long input strings
are broken up into chunks of size `term-input-chunk-size'.  Processes
are given a chance to output between chunks.  This can help prevent processes
from hanging when you send them long inputs on some OS's."
  (let* ((len (length str))
	     (i (min len term-input-chunk-size)))
    (process-send-string proc (substring str 0 i))
    (while (< i len)
      (let ((next-i (+ i term-input-chunk-size)))
	    (accept-process-output)
	    (process-send-string proc (substring str i (min len next-i)))
	    (setq i next-i)))))

(require 'subr-x)
(require 'benchmark)
(require 'seq)

(defun fterm-reset ()
  "FTERM-RESET reset the terminal."
  (setq fterm-half-ack "")
  (setq fterm-len 0)
  (setq fterm-sent 0))

(defun fterm-send-queue (proc)
  "FTERM-SEND-QUEUE sends the next line from queue to PROC."
  (let ((s (car-safe fterm-queue))
        (len (length fterm-queue)))
    (if (not s)
        (progn
          (message "sent %d" fterm-sent)
          (fterm-reset))
      (setq fterm-queue (cdr-safe fterm-queue))
      (term-send-string proc (concat s "\n"))
      (setq fterm-sent (+ fterm-sent (length s)))
      (if (= 0 (% len 10))
          (let ((r (- fterm-len len)))
            (message "%d %d %f%%" r fterm-len (/ (float (* 100.0 r)) fterm-len))))
      (accept-process-output))))

(defun fterm-match-include (s)
  "MATCH-INCLUDE find include in S."
  (let* ((found (string-match-p (regexp-quote "#require^[\n]*") s))
         (i (match-string 1)))
    (if found
        (progn
          (message "include: %s" i)
          i)
      (message "nope")
      nil)))

(defun fterm-prep-src (chars)
  "Fterm-Prep-Src preps the source in CHARS for send."
  (let* ((cleaned (replace-regexp-in-string "\\\\[^\n]*\n?" "\n" chars))
         (src (seq-reduce
               (lambda (a l)
                 (let ((s (string-trim l)))

                   (if (string= "" s)
                       a
                     (append a (list s)))))
               (string-split cleaned "\n")
               '())))
    src))

(defun fterm-send-chars (proc chars)
  "FTERM-SEND-CHARS sends CHARS to PROC."
  (let ((src (fterm-prep-src chars)))
    (fterm-reset)
    (setq fterm-len (length src))
    (setq fterm-queue (append fterm-queue src))
    (fterm-send-queue proc)))

(defun fterm-paste ()
  "Insert the last stretch of killed text at point."
  (interactive)
  (let ((proc (get-buffer-process (current-buffer))))
    (if proc
        (fterm-send-chars proc (current-kill 0))
      (yank))))

(defvar fterm-buffer nil)

(defun fterm-load-buffer ()
  "Load a file via TERM"
  (interactive)
  (let ((proc (get-buffer-process fterm-buffer))
        (content (save-restriction
                   (widen)
                   (buffer-substring-no-properties (point-min) (point-max)))))
    (fterm-send-chars proc content)))

(defun fterm-clear-queue ()
  "FTERM-CLEAR-QUEUE clears the send queue."
  (interactive)
  (setq fterm-queue '())
  (fterm-reset)
  (message "queue cleaned"))

;; TODO
;; - includes
;; - baud rate
;; - words/lsp
(defun fterm ()
  (interactive)
  (if fterm-buffer
      (switch-to-buffer fterm-buffer)
    (let ((buf (rephone-serial-term "/dev/ttyACM1")))
      (setq fterm-buffer buf))))

(global-set-key (kbd "C-y") 'fterm-paste)
(global-set-key (kbd "C-S-y") 'yank)
(global-set-key (kbd "C-c C-b") 'fterm-load-buffer)
(global-set-key (kbd "C-c C-t") 'fterm)
(global-set-key (kbd "C-c C-<delete>") 'fterm-clear-queue)

;; (require 'bash-completion)
;; (bash-completion-setup)

;; bash

;; (let ((dir "~"))
;;   (with-current-buffer "*shell*"
;;     (shell-cd dir)
;;     (comint-send-string nil (concat "cd " dir "\n"))))

(require 'bluetooth-battery)
(bluetooth-battery-percentages)


(defun bluetooth-test ()
  "Return a list of devices by ID, aliases and their battery levels."
  (let (result)
    (bluetooth-device-map (lambda (id dev)
                            (push (list id
                                        (bluetooth-device-property dev "Alias")
                                        (bluetooth-battery-percentage dev))
                                  result))
                          #'bluetooth-device-implements-p
                          :)
    result))

;; tap_data_characteristic = 'c3ff0005-1d8b-40fd-a56f-c7bd5d0f3370'
;; tap_service = 'c3ff0001-1d8b-40fd-a56f-c7bd5d0f3370'
;;    nus_service = '6e400001-b5a3-f393-e0a9-e50e24dcca9e'
;; raw_sensors_characteristic = '6e400003-b5a3-f393-e0a9-e50e24dcca9e'



;; (glbt-find-gatt-characteristic glbthr-hr-monitor-path
;;                                   glbthr-hr-measurement-uuid)
;; (add-hook 'glbt-gatt-characteristic-changed-hook
;; #'glbthr--handle-characteristic-changes)

;; (glbt-connect-device glbthr-hr-monitor-path)

;; movement
(windmove-default-keybindings)
(and (require 'buffer-move)
     (global-set-key (kbd "S-s-<left>") 'buf-move-left)
     (global-set-key (kbd "S-s-<right>") 'buf-move-right)
     (global-set-key (kbd "S-s-<up>") 'buf-move-up)
     (global-set-key (kbd "S-s-<down>") 'buf-move-down))

;; line switch
(defun move-text-internal (arg)
  (cond
   ((and mark-active transient-mark-mode)
    (if (> (point) (mark))
        (exchange-point-and-mark))
    (let ((column (current-column))
          (text (delete-and-extract-region (point) (mark))))
      (forward-line arg)
      (move-to-column column t)
      (insert text)
      (exchange-point-and-mark)
      (set-mark (point))
      (setq deactivate-mark nil)))
   (t
    (beginning-of-line)
    (when (or (> arg 0) (not (bobp)))
      (forward-line))
    (transpose-lines arg)
    (forward-line -1))))

(defun move-text-down (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines down."
  (interactive "*p")
  (move-text-internal arg))

(defun move-text-up (arg)
  "Move region (transient-mark-mode active) or current line
  arg lines up."
  (interactive "*p")
  (move-text-internal (- arg)))

(global-set-key (kbd "M-<up>") 'move-text-up)
(global-set-key (kbd "M-<down>") 'move-text-down)

;; stuff
(prefer-coding-system 'utf-8)
(savehist-mode 1)
(ffap-bindings)
(eldoc-mode)

(setq inhibit-startup-message t)               ; No message at startup
(setq shell-file-name "/bin/bash")             ; Set Shell for M-| command
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(setq confirm-kill-emacs 'yes-or-no-p)         ; Confirm quit
(setq-default indent-tabs-mode nil)
(setq create-lockfiles nil)
(setq backup-directory-alist '((".*" . "/tmp/")))
(setq auto-save-file-name-transforms '((".*" "/tmp/" t)))
'(show-paren-match ((((class color) (background light)) (:background "azure2"))))
(add-hook 'before-save-hook 'delete-trailing-whitespace)
(setq x-select-enable-clipboard t)             ;; Allow pasting selection outside of Emacs
(winner-mode 1)                                ;; Undo/redo window configuration with C-c <left>/<right>
(global-subword-mode 1)                        ;; Easily navigate sillyCased words

(setq gc-cons-threshold 20000000)              ;; Don't be so stingy on the memory, we have lots now. It's the distant future.

;; Save a list of recent files visited. (open recent file with C-x f)
(recentf-mode 1)
(setq recentf-max-saved-items 100) ;; just 20 is too recent

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(global-set-key "\C-x\C-b" 'buffer-menu)       ; CxCb puts point on buffer list
;; (global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)

;; custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode 0)
 '(c-basic-offset 4)
 '(cider-cljs-lein-repl
   "(do (require 'cljs.repl.node) (cemerick.piggieback/cljs-repl (cljs.repl.node/repl-env)))")
 '(column-number-mode t)
 '(comint-buffer-maximum-size 20000)
 '(comint-completion-addsuffix t)
 '(comint-get-old-input (lambda nil "") t)
 '(comint-input-ignoredups t)
 '(comint-input-ring-size 5000)
 '(comint-move-point-for-output nil)
 '(comint-prompt-read-only nil)
 '(comint-scroll-show-maximum-output t)
 '(comint-scroll-to-bottom-on-input t)
 '(company-dabbrev-downcase nil)
 '(compilation-message-face 'default)
 '(coverlay-mode t)
 '(coverlay:base-path "/home/christophmewes/projects/wawi/lidl-wawi/")
 '(coverlay:tested-line-background-color "#eeffdd")
 '(coverlay:untested-line-background-color "#ffeedd")
 '(custom-enabled-themes '(base16-atelierdune-light))
 '(custom-safe-themes
   '("c49aeb31c7a233bfc5566290427b35c8481fa2727c7c95339f4683f8cdd78ca0"
     "ee3b22b48b269b83aa385b3915d88a9bf4f18e82bb52e20211c7574381a4029a"
     "75c0b9f9f90d95ac03f8647c75a91ec68437c12ff598e2abb22418cd4b255af0"
     default))
 '(eclim-eclipse-dirs '("~/opt/eclipse"))
 '(eclim-executable "~/opt/eclipse/eclimd")
 '(flowtype:base-path
   "/home/cmewes/projects/betty/betty_ordercapture_ui/master/ui/")
 '(flowtype:uncovered-type-background-color "#ff9d9d")
 '(flycheck-eslintrc "/usr/local/.eslintrc")
 '(haskell-mode-hook '(turn-on-haskell-indentation))
 '(ispell-dictionary nil)
 '(magit-diff-use-overlays nil)
 '(mocha-command "jest")
 '(mocha-which-node
   "/home/cmewes/projects/betty/betty_ordercapture_ui/master/ui/docker-gulp.sh")
 '(package-selected-packages
   '(ac-html ac-html-bootstrap ac-html-csswatcher aggressive-indent
             aidermacs apheleia bash-completion bluetooth buffer-move
             centered-cursor-mode clj-refactor company-flow company-go
             company-restclient company-shell company-terraform cov
             easy-hugo edit-server ejc-sql emacs-eclim ement
             eslint-fix exec-path-from-shell fix-muscle-memory flutter
             flycheck-checkbashisms flycheck-clojure
             flycheck-gometalinter flycheck-jest flycheck-kotlin
             forth-mode framesize fuzzy fuzzy-match gerrit-download
             ggtags git-commit go-autocomplete go-eldoc go-gen-test
             go-guru go-imports go-mode go-rename go-snippets gotest
             groovy-mode haskell-mode highlight-parentheses
             highlight-symbol hlinum hover ht indent-tools
             jasminejs-mode jest json-mode jsonnet-mode kotlin-mode
             lsp-dart lsp-mode lsp-ui markdown-mode mic-paren
             midje-mode mocha nix-buffer nix-mode org
             org-easy-img-insert org-preview-html projectile
             protobuf-mode rainbow-mode react-snippets sass-mode
             smartparens terraform-mode theme-changer tide timonier
             transient tss typescript-mode use-package web-mode
             yaml-mode))
 '(protect-buffer-bury-p nil)
 '(show-paren-mode t)
 '(sp-base-key-bindings 'sp)
 '(sp-override-key-bindings '(("M-<backspace>") ("C-M-<backspace>")))
 '(tab-width 4)
 '(term-input-chunk-size 64)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh" nil (tramp))
 '(visible-bell t))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(fringe ((t (:background "#e8e4cf"))))
 '(linum ((t (:background "#e8e4cf" :foreground "#a6a28c"))))
 '(mode-line ((t (:box t :foreground "#999580" :background "#6e6b5e"))))
 '(mode-line-buffer-id ((t (:foreground "#b854d4" :background "#a6a28c"))))
 '(mode-line-highlight ((t :foreground "#b854d4" :background "#a6a28c" :box t :weight bold)))
 '(mode-line-inactive ((t (:background "#999580" :foreground "#6e6b5e" :box t)))))

(put 'erase-buffer 'disabled nil)
;;; init.el ends here
