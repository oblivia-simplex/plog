(setq *blog-directory*
      "~/www/feralmachin.es/")

(setq *blog-post-directory*
      (concat *blog-directory* "posts/"))

(setq *blog-post-template-path*
      "~/.emacs.d/mine/templates/blogpost.md")

(setq *blog-post-template*
      "---
title: \"%s\"
author: me
date: \"%s\"
tags:
- draft
%s
abstract: \"<WRITE ABSTRACT HERE>\"
---

")

(defun plog--build-header (title date tags)
  
  (format *blog-post-template* title date tags))

(defun plog--prompt-for-tags (tagstr)
  (interactive "sComma-separated tags: ")
  (split-string tagstr "," t "\\s-*"))

(defun plog--make-filename (title date)
  (let* ((quotefree
	  (replace-regexp-in-string "['\"]" "" title))
	 (clean-title
	  (replace-regexp-in-string "[^A-Za-z0-9]" "_" quotefree))
	 (cleaner-title
	  (replace-regexp-in-string "_*$" "" clean-title))
	 (cleanest-title
	  (replace-regexp-in-string "__*" "_" cleaner-title))
	 (filename (downcase
		    (format "%s--%s.md"
			    date cleanest-title))))
    filename))
  

(defun plog-prompt (title)
  (interactive "sEnter title: ")
  (let* ((date (format-time-string "%Y-%m-%d"))
	 (filename (plog--make-filename title date))
	 (tags (call-interactively 'fm--prompt-for-tags))
	 (taglist (mapconcat
		   (lambda (tag) (format "- %s" tag))
		   tags
		   "\n"))
	 (path (format "%s/%s"
		       *blog-post-directory*
		       filename))
	 (post-buffer (get-buffer-create filename)))
    (message "filename: %s" filename)
    (with-current-buffer post-buffer
      (funcall 'markdown-mode)
      (setq default-directory *blog-post-directory*))
    (switch-to-buffer-other-window post-buffer)
    ;; if the buffer is empty, or contains only whitespace
    ;; then insert the header
    (if (string-match-p "\\`[[:space:]]*\\'" (buffer-string))
	(progn
	  (goto-char (point-min))
	  (insert (plog--build-header title date taglist)))
      nil)))

(global-set-key (kbd "C-c b") 'plog-prompt)





