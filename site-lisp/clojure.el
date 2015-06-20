;; Slime additions for Clojure
;; src: http://bc.tech.coop/blog/081120.html
(defun slime-java-describe (symbol-name)
  "Get details on Java class/instance at point."
  (interactive (list (slime-read-symbol-name "Java Class/instance: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (save-excursion
    (set-buffer (slime-output-buffer))
    (unless (eq (current-buffer) (window-buffer))
      (pop-to-buffer (current-buffer) t))
    (goto-char (point-max))
    (insert (concat "(show " symbol-name ")"))
    (when symbol-name
      (slime-repl-return)
      (other-window 1))))

(defun slime-javadoc (symbol-name)
  "Get JavaDoc documentation on Java class at point."
  (interactive (list (slime-read-symbol-name "JavaDoc info for: ")))
  (when (not symbol-name)
    (error "No symbol given"))
  (set-buffer (slime-output-buffer))
  (unless (eq (current-buffer) (window-buffer))
    (pop-to-buffer (current-buffer) t))
  (goto-char (point-max))
  (insert (concat "(javadoc " symbol-name ")"))
  (when symbol-name
    (slime-repl-return)
    (other-window 1)))

(setq slime-browse-local-javadoc-root "/usr/share/doc/sun-java6-doc")

(defun slime-browse-local-javadoc (ci-name)
  "Browse local JavaDoc documentation on Java class/Interface at point."
  (interactive (list (slime-read-symbol-name "Class/Interface name: ")))
  (when (not ci-name)
    (error "No name given"))
  (let ((name (replace-regexp-in-string "\\$" "." ci-name))
	(path (concat (expand-file-name slime-browse-local-javadoc-root) "/html/api/")))
    (with-temp-buffer
      (insert-file-contents (concat path "allclasses-noframe.html"))
      (let ((l (delq nil
		     (mapcar #'(lambda (rgx)
				 (let* ((r (concat "\\.?\\(" rgx "[^./]+\\)[^.]*\\.?$"))
					(n (if (string-match r name)
					       (match-string 1 name)
					     name)))
				   (if (re-search-forward (concat "<A HREF=\"\\(.+\\)\" +.*>" n "<.*/A>") nil t)
				       (match-string 1)
				     nil)))
			     '("[^.]+\\." "")))))
	(if l
	    (browse-url (concat "file://" path (car l)))
	  (error (concat "Not found: " ci-name)))))))

(add-hook 'slime-connected-hook
          (lambda ()
            (interactive)
            (slime-redirect-inferior-output)
            (define-key slime-mode-map (kbd "C-c d") 'slime-java-describe)
            (define-key slime-repl-mode-map (kbd "C-c d") 'slime-java-describe)
            (define-key slime-mode-map (kbd "C-c D") 'slime-javadoc)
            (define-key slime-repl-mode-map (kbd "C-c D") 'slime-javadoc)
            (define-key slime-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)
            (define-key slime-repl-mode-map (kbd "C-c b") 'slime-browse-local-javadoc)
            (define-key slime-mode-map (kbd "TAB") 'slime-indent-and-complete-symbol)))
