(setq inferior-lisp-program "/usr/bin/sbcl")

;(icy-mode 1)

(add-to-list 'load-path "~/.emacs.d/manual")

;; package.el & marmalade
(require 'package)
;; (add-to-list 'package-archives
;;              '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


(setq magit-last-seen-setup-instructions "1.4.0")
(require 'magit)

;; thing-cmd
(and
 (require 'thing-cmds)
 (global-set-key (kbd "M-?") 'cycle-thing-region)
 (global-set-key (kbd "M-@") 'cycle-thing-region) ; vs `mark-word'
 (global-set-key (kbd "C-M-?")  'mark-thing))	  ; vs `mark-sexp'

;; centered cursor mode
(and
 (require 'centered-cursor-mode)
 (global-centered-cursor-mode +1)
 (global-set-key (kbd "<Scroll_Lock>") 'centered-cursor-mode))

;; line numbers
(global-linum-mode)

;; whitespace-mode
(and
 (require 'whitespace)
 (global-set-key "\C-c_w" 'whitespace-mode)
 (global-set-key "\C-c_t" 'whitespace-toggle-options)
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

;; flycheck
(add-hook 'after-init-hook #'global-flycheck-mode)

;;  mode (javascript)
(add-hook 'js-mode-hook 'js2-minor-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)

;; Let flycheck handle parse errors
(setq-default js2-show-parse-errors nil)
(setq-default js2-strict-missing-semi-warning nil)

(add-to-list 'auto-mode-alist '("\\.gss$" . css-mode))

;; (eval-after-load 'css-mode
;;   (define-key css-mode-keymap (kbd "C-c C-x c") 'css-comb))

(add-to-list 'auto-mode-alist '("\\.scss$" . sass-mode))
;; (eval-after-load 'sass-mode
;;             (define-key sass-mode-keymap (kbd "C-c C-x c") 'css-comb))
(add-hook 'sass-mode 'ac-css-mode-setup)

(require 'jsx-mode)
(add-to-list 'auto-mode-alist '("\\.jsx\\'" . jsx-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'" . jsx-mode))
(setq jsx-indent-level 4)

(add-hook 'jsx-mode-hook
                    (lambda () (auto-complete-mode 1)))

;; Tern js tooling
(require 'tern)
(eval-after-load 'tern
  '(progn
     (require 'tern-auto-complete)
     (tern-ac-setup)

     (define-key tern-mode-keymap [(control ?.)] 'tern-find-definition)
     (define-key tern-mode-keymap [(control ?T)] 'tern-ac-complete)))

(add-hook 'js-mode-hook (lambda () (tern-mode t)))
(add-hook 'js2-mode-hook (lambda () (tern-mode t)))
(add-hook 'jsx-mode-hook (lambda () (tern-mode t)))

(defun delete-tern-process ()
  (interactive)
    (delete-process "Tern"))

;; coverage in lcov format
(require 'coverlay)

;; JSX flycheck via https://truongtx.me/2014/03/10/emacs-setup-jsx-mode-and-jsx-syntax-checking/
(require 'flycheck)
(flycheck-define-checker jsxhint-checker
  "A JSX syntax and style checker based on JSXHint."

  :command ("jsxhint" "-c" "/usr/local/.jsxhintrc" source)
  :error-patterns
  ((error line-start (1+ nonl) ": line " line ", col " column ", " (message) line-end))
  :modes (jsx-mode))

(add-to-list 'flycheck-checkers 'jsxhint-checker)


;; android-mode
(add-to-list 'load-path "/opt/android-mode")
(require 'android-mode)
(defcustom android-mode-sdk-dir "/opt/android" "")

;; closure-template
(require 'closure-template-html-mode)
(add-to-list 'auto-mode-alist '("\\.soy$" . closure-template-html-mode))

;; clojure-mode
;; (add-to-list 'load-path "/opt/clojure-mode")
(require 'clojure-mode)

;;snippets
(require 'yasnippet)
(yas-global-mode 1)
(yas/load-directory "~/.emacs.d/snippets/")

;; autocomplete
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)

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
  (flet ((end-of-line () (end-of-buffer)))
    ad-do-it))

;; not sure why, but comint needs to be reloaded from the source (*not*
;; compiled) elisp to make the above advise stick.
;; (load "comint.el.gz")

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
;;(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-fontset-auto1")
(set-default-font "-unknown-Inconsolata-normal-normal-normal-*-16-*-*-*-m-0-iso10646-1")

(desktop-save-mode 1)
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

;; Represent undo-history as an actual tree (visualize with C-x u)
(setq undo-tree-mode-lighter "")
(require 'undo-tree)
(global-undo-tree-mode)

;; Offer to create parent directories if they do not exist
;; http://iqbalansari.github.io/blog/2014/12/07/automatically-create-parent-directories-on-visiting-a-new-file-in-emacs/
(defun my-create-non-existent-directory ()
  (let ((parent-directory (file-name-directory buffer-file-name)))
    (when (and (not (file-exists-p parent-directory))
               (y-or-n-p (format "Directory `%s' does not exist! Create it?" parent-directory)))
      (make-directory parent-directory t))))

(add-to-list 'find-file-not-found-functions 'my-create-non-existent-directory)

(global-set-key "\C-x\C-b" 'buffer-menu)       ; CxCb puts point on buffer list
(global-set-key "\C-b" 'slime-complete-symbol)
(global-set-key (kbd "C-M-<backspace>") 'backward-kill-sexp)
(global-set-key (kbd "M-\\") 'slime-fuzzy-complete-symbol)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
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
 '(coverlay:untested-line-background-color "#ffeedd")
 '(haskell-mode-hook (quote (turn-on-haskell-indentation)))
 '(protect-buffer-bury-p nil)
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(tramp-default-method "ssh"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
