;; -*- lexical-binding: t -*-
;;zyt
(require 's)
(defmacro treemacs-tag-follow-mode-add-ext
	(ext-name m-mode match-function cur-tag ext-project)
  (let (
		(follow-tag-function-name (intern (s-lex-format "treemacs--follow-tag-at-point-${ext-name}")))
		)
	`(progn
	   (advice-add
		'treemacs--follow-tag-at-point
		:before-until
		(cl-defun ,follow-tag-function-name()
		  ,(s-lex-format
			"Add tag following support for an EXTENSION of type `${ext-name}'.")
		  (if (eq major-mode ',m-mode)
			  (let* ((treemacs-window (treemacs-get-local-window))
					 (buffer (current-buffer))
					 ;; (path (treemacs--follow-tag-current-path #',match-function ,cur-tag))
					 (path)
					 (project ,ext-project))
				(when (and treemacs-window project)
				  (setq path (treemacs--follow-tag-current-path #',match-function ,cur-tag))
				  (condition-case e
					  (treemacs--do-follow-tag-extension nil treemacs-window path project)
					(imenu-unavailable (ignore e))
					(error (treemacs-log-err "Encountered error while following chm tag at point: %s" e)))))
			)
		  )
		)
	   )
	)
  )
(provide 'TreemacsExt)
