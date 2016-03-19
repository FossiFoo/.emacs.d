;;; init --- init ALL the things!

;;; Commentary:

;;; Code:

;; (setq max-specpdl-size 5)  ; default is 1000, reduce the backtrace level
;; (setq debug-on-error nil)    ; now you should get a backtrace

(add-to-list 'load-path "~/.emacs.d/site-lisp")
(add-to-list 'load-path "~/.emacs.d/site-lisp/coverlay")
(add-to-list 'load-path "~/.emacs.d/site-lisp/flowtype-mode")

;; package.el
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(defvar package-list)
; list the packages you want
(setq package-list
      '(
	async                 ;;Asynchronous processing in Emacs
	auto-complete         ;;Auto Completion for GNU Emacs
        base16-theme          ;;Collection of themes built on combinations of 16 base colors
	buffer-move           ;;easily swap buffers
        centered-cursor-mode  ;;cursor stays vertically centered
	cider                 ;;Clojure Interactive Development Environment that Rocks
	clj-refactor          ;;A collection of clojure refactoring functions
	clojure-mode          ;;Major mode for Clojure code
        color-theme           ;;install color themes
	company               ;;Modular text completion framework
	company-restclient    ;;company-mode completion back-end for restclient-mode
	company-shell         ;;Company mode backend for shell functions
	company-tern          ;;Tern backend for company-mode
	dash                  ;;A modern list library for Emacs
	dash-functional       ;;Collection of useful combinators for Emacs Lisp
	edn                   ;;Support for reading and writing the edn data format from elisp
	emacs-eclim           ;;An interface to the Eclipse IDE.
	epl                   ;;Emacs Package Library
	exec-path-from-shell  ;;Get environment variables such as $PATH from the shell
	f	              ;;Modern API for working with files and directories
	flycheck              ;;On-the-fly syntax checking
	flycheck-clojure      ;;Flycheck: Clojure support
        framesize             ;;change the size of frames in Emacs
	fuzzy                 ;;Fuzzy Matching
	fuzzy-match           ;;fuzzy matching
	gerrit-download       ;;Show gerrit reviews in a diff buffer.
        ggtags                ;;frontend to GNU Global source code tagging system
	git-commit            ;;Edit Git commit messages
	groovy-mode           ;;Major mode for Groovy source files
	highlight-parentheses ;;highlight surrounding parentheses
	highlight-symbol      ;;automatic and manual symbol highlighting
	hlinum                ;;Extension for linum.el to highlight current line number
	hydra                 ;;Make bindings that stick around.
	inflections           ;;convert english words between singular and plural
        jasminejs-mode        ;;A minor mode for manipulating jasmine test files
	js2-mode              ;;Improved JavaScript editing mode
	json-mode             ;;Major mode for editing JSON files
	json-reformat         ;;Reformatting tool for JSON
	json-snatcher         ;;Grabs the path to JSON values in a JSON file
	know-your-http-well   ;;Look up the meaning of HTTP headers, methods, relations, status codes
	let-alist             ;;Easily let-bind values of an assoc-list by their names
	magit                 ;;A Git porcelain inside Emacs
	magit-gerrit          ;;Magit plugin for Gerrit Code Review
	magit-popup           ;;Define prefix-infix-suffix command combos
	mic-paren             ;;advanced highlighting of matching parentheses
	multiple-cursors      ;;Multiple cursors for Emacs.
	peg                   ;;Parsing Expression Grammars in Emacs Lisp
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
	tern                  ;;Tern-powered JavaScript integration
        theme-changer         ;;Sunrise/Sunset Theme Changer for Emacs
        undo-tree             ;;Treat undo history as a tree
	web-completion-data   ;;Shared completion data for ac-html and company-web
	web-mode              ;;major mode for editing web templates
	with-editor           ;;Use the Emacsclient as $EDITOR
	yaml-mode             ;;Major mode for editing YAML files
	yasnippet             ;;Yet another snippet extension for Emacs.
	))

; fetch the list of packages available
(unless package-archive-contents
  (package-refresh-contents))

; install the missing packages
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

;;; Local Things

;; thing-cmd
(and
 (require 'thing-cmds)
 (global-set-key (kbd "M-?") 'cycle-thing-region)
 (global-set-key (kbd "M-@") 'cycle-thing-region) ; vs `mark-word'
 (global-set-key (kbd "C-M-?")  'mark-thing))	  ; vs `mark-sexp'

;;; Packaged things

(setq calendar-latitude 53.551086)
(setq calendar-longitude 9.993682)

(and (require 'color-theme)
     (require 'theme-changer)
     (change-theme 'base16-atelierdune-light 'base16-atelierdune-dark))

;;presentation
(require 'framesize)
(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-14-*-*-*-m-0-iso10646-1")

;; eldoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)

;; magit
(require 'magit)

;; centered cursor mode
(and
 (require 'centered-cursor-mode)
 (global-centered-cursor-mode +1)
 (global-set-key (kbd "<Scroll_Lock>") 'centered-cursor-mode))

;; line numbers
(global-linum-mode)

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


;; flycheck
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;;  mode (javascript)
(add-hook 'js-mode-hook 'js2-minor-mode)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)

(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))

;; (eval-after-load 'css-mode
;;   (define-key css-mode-keymap (kbd "C-c C-x c") 'css-comb))

(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
;; (eval-after-load 'sass-mode
;;             (define-key sass-mode-keymap (kbd "C-c C-x c") 'css-comb))
;;(add-hook 'sass-mode 'ac-css-mode-setup)

;; (require 'jsx-mode)
;; (add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
;; (add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-mode))
;; (setq jsx-indent-level 4)

;; (add-hook 'jsx-mode-hook
;;                     (lambda () (auto-complete-mode 1)))

;; Tern js tooling
;;(require 'tern)
;; (eval-after-load 'tern
;;   '(progn
;;      (require 'tern-auto-complete)
;;      (tern-ac-setup)

;;      (define-key tern-mode-keymap [(control ?.)] 'tern-find-definition)
;;      (define-key tern-mode-keymap [(control ?T)] 'tern-ac-complete)))

;;(add-hook 'js-mode-hook (lambda () (tern-mode t)))
;;(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
;; (add-hook 'jsx-mode-hook (lambda () (tern-mode t)))

(defun delete-tern-process ()
  (interactive)
    (delete-process "Tern"))

(require 'jasminejs-mode)
(add-hook 'js-mode-hook (lambda () (jasminejs-mode)))
(add-hook 'js2-mode-hook (lambda () (jasminejs-mode)))
(add-hook 'jasminejs-mode-hook (lambda () (jasminejs-add-snippets-to-yas-snippet-dirs)))

(require 'mocha)

;; coverage in lcov format
(require 'coverlay)

(flycheck-define-checker jsx-eslint
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
  :modes (js-mode js2-mode flowtype-mode))
(add-to-list 'flycheck-checkers 'jsx-eslint)

(flycheck-define-checker javascript-flow
    "A JavaScript syntax and style checker using Flow.
See URL `http://flowtype.org/'."
    :command ("flow" "status" "--old-output-format" source-original)
    :error-patterns
    ((error line-start
	    (file-name)
	    ":"
	    line
	    ":"
	    (minimal-match (one-or-more not-newline))
	    ": "
	    (message (minimal-match (and (one-or-more anything) "\n")))
	    line-end))
    :modes (js-mode js2-mode js3-mode flowtype-mode))

(add-to-list 'flycheck-checkers 'javascript-flow t)

(flycheck-add-next-checker 'jsx-eslint 'javascript-flow)

;; flowtype
(require 'flowtype-mode)
(add-to-list 'magic-mode-alist '("/\\* @flow" . flowtype-mode))

;; android-mode
(require 'android-mode)
(defcustom android-mode-sdk-dir "/opt/android" "Android.")

;; closure-template
(require 'closure-template-html-mode)
(add-to-list 'auto-mode-alist '("\\.soy$" . closure-template-html-mode))

;; clojure stuff
(require 'clojure-mode)
(require 'cider)
(require 'flycheck-clojure)
(add-to-list 'flycheck-checkers 'clojure-cider-eastwood)
;; (add-to-list 'flycheck-checkers 'clojure-cider-kibit)
(eval-after-load 'flycheck '(flycheck-clojure-setup))

(require 'clj-refactor)

(defun my-clojure-mode-hook ()
    (clj-refactor-mode 1)
    (yas-minor-mode 1) ; for adding require/use/import statements
    ;; This choice of keybinding leaves cider-macroexpand-1 unbound
    (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; eclim

(require 'eclim)
(global-eclim-mode)
(require 'eclimd)

;; add the emacs-eclim source
;; (require 'ac-emacs-eclim-source)
;; (ac-emacs-eclim-config)

;;snippets
(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/snippets/")

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)

(global-set-key (kbd "<C-tab>") 'company-complete-common)
(add-to-list 'company-backends 'company-tern)
(add-to-list 'company-backends 'company-dabbrev)

;; (setq yas/prompt-functions '(yas/ido-prompt yas/dropdown-prompt yas/completing-prompt yas/x-prompt yas/no-prompt))

(require 'react-snippets)


;; shell stuff from https://snarfed.org/why_i_run_shells_inside_emacs

(defvar my-local-shells
  '("*shell0*" "*shell1*" "*shell2*" "*shell3*" "*music*"))
(defvar my-remote-shells
  '() ;; '("*snarfed*" "*heaven0*" "*heaven1*" "*heaven2*" "*heaven3*")
  )
(defvar my-shells (append my-local-shells my-remote-shells))

(require 'tramp)

(setenv "PAGER" "cat")

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

(defadvice comint-send-input (around go-to-end-of-multiline activate)
  "When I press enter, jump to the end of the *buffer*, instead of the end of
the line, to capture multiline input. (This only has effect if
`comint-eol-on-send' is non-nil."
  (flet ((end-of-line () (goto-char (point-max))))
    ad-do-it))

(require 'bash-completion)
(bash-completion-setup)

;; bash



;; movement
(windmove-default-keybindings)
(and (require 'buffer-move)
     (global-set-key (kbd "M-S-<left>") 'buf-move-left)
     (global-set-key (kbd "M-S-<right>") 'buf-move-right)
     (global-set-key (kbd "M-S-<up>") 'buf-move-up)
     (global-set-key (kbd "M-S-<down>") 'buf-move-down))

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
     (when (< arg 0)
       (forward-line -1))
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

(desktop-save-mode 1)
(savehist-mode 1)
(ffap-bindings)
(eldoc-mode)

(setq inhibit-startup-message t)               ; No message at startup
(setq shell-file-name "/bin/bash")             ; Set Shell for M-| command
(column-number-mode t)                         ; Show column number in mode-line
(blink-cursor-mode 0)                          ; No blinking cursor
(defalias 'yes-or-no-p 'y-or-n-p)              ; y/n instead of yes/no
(setq confirm-kill-emacs 'yes-or-no-p)         ; Confirm quit
(setq tab-width 4)                             ; Length of tab is 4 SPC
(setq-default indent-tabs-mode nil)
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
 '(blink-cursor-mode nil)
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
 '(compilation-message-face (quote default))
 '(coverlay:base-path "/home/cmewes/projects/betty/betty_ordercapture_ui/master/")
 '(coverlay:tested-line-background-color "#eeffdd")
 '(coverlay:untested-line-background-color "#ffeedd")
 '(custom-enabled-themes (quote (base16-atelierdune-light)))
 '(custom-safe-themes
   (quote
    ("b83c1e19c912f0d84a543b37367242f8a3ad2ed3aec80f5363d0d82ba4621e7d" "75c0b9f9f90d95ac03f8647c75a91ec68437c12ff598e2abb22418cd4b255af0" "ee3b22b48b269b83aa385b3915d88a9bf4f18e82bb52e20211c7574381a4029a" "a922c743710bb5d7c14995345549141f01211ff5089057dc718a5a33104c3fd1" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "5cc9df26a180d14a6c5fc47df24d05305636c80030a85cf65e31f420d7836688" "90b1aeef48eb5498b58f7085a54b5d2c9efef2bb98d71d85e77427ce37aec223" "1a2b131a7844bad234832963d565097efc88111b196fb75757885c159c5f8137" "b6d649c9f972b491686e7fa634535653e6222c1faca1ab71b3117854470a79ae" "64da9a8dba17dcf210420875eba3f1a5ea6272217dc403706e4e2c985aa537fa" "232f715279fc131ed4facf6a517b84d23dca145fcc0e09c5e0f90eb534e1680f" "6ae93caf30ad7eef728589a4d7b7befadecade71d78b904a64a0480608a7b61e" "7c1e99f9d46c397b3fd08c7fdd44fe47c4778ab69cc22c344f404204eb471baa" "3fb38c0c32f0b8ea93170be4d33631c607c60c709a546cb6199659e6308aedf7" "8ffaf449297bd9a08517f4b03a4df9dbf3e347652746cefceb3ee57c8e584b9f" "36012edb5bc7070a17e989984e0ecc1d1e9c94326bdd0fbd76c2a45ebfe7da54" "3a3917dbcc6571ef3942c2bf4c4240f70b5c4bc0b28192be6d3f9acd83607a24" "0b6645497e51d80eda1d337d6cabe31814d6c381e69491931a688836c16137ed" "b2028956188cf668e27a130c027e7f240c24c705c1517108b98a9645644711d9" default)))
 '(eclim-eclipse-dirs (quote ("~/opt/eclipse")))
 '(eclim-executable "~/opt/eclipse/eclimd")
 '(flowtype:base-path
   "/home/cmewes/projects/betty/betty_ordercapture_ui/master/ui/")
 '(flowtype:uncovered-type-background-color "#ff9d9d")
 '(flycheck-eslintrc "/usr/local/.eslintrc")
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(magit-diff-use-overlays nil)
 '(mocha-command "jest")
 '(mocha-which-node
   "/home/cmewes/projects/betty/betty_ordercapture_ui/master/ui/docker-gulp.sh")
 '(protect-buffer-bury-p nil)
 '(show-paren-mode t)
 '(sp-base-key-bindings (quote sp))
 '(sp-override-key-bindings (quote (("M-<backspace>") ("C-M-<backspace>"))))
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;; init.el ends here
