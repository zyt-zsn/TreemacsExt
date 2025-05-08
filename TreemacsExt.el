;; -*- lexical-binding: t -*-
;;zyt
(require 's)
(require 'treemacs-macros)

(defun treemacs--do-follow-tag-extension (flat-index treemacs-window path project)
  "Actual tag-follow implementation, run once the necessary data is gathered.

FLAT-INDEX: Sorted list of tag paths
TREEMACS-WINDOW: Window
BUFFER-FILE: Filepath
PROJECT: Project Struct"
  (let* (;;(tag-path (treemacs--find-index-pos (point) flat-index))
		 (tag-path path)
		 (file-states '(file-node-open file-node-closed root-node-open root-node-closed))
		 (btn))
	(when tag-path
	  (treemacs-without-following
	   (with-selected-window treemacs-window
		 (setq btn (treemacs-current-button))
		 (if nil ;;btn
			 ;; first move to the nearest file when we're on a tag
			 (if (memq (treemacs-button-get btn :state) '(tag-node-open tag-node-closed tag-node))
				 (while (not (memq (treemacs-button-get btn :state) file-states))
				   (setq btn (treemacs-button-get btn :parent)))
			   ;; when that doesnt work move manually to the correct file
			   (-let [btn-path (treemacs-button-get btn :path)]
				 (unless (and (stringp btn-path) (treemacs-is-path path :same-as btn-path))
				   ;;zyt
				   ;;(treemacs-goto-file-node buffer-file project)
				   (treemacs-goto-extension-node path)
				   (setq btn (treemacs-current-button)))))
		   ;; also move manually when there is no button at point
		   ;;zyt
		   ;;(treemacs-goto-file-node buffer-file project)
		   (treemacs-goto-extension-node path)
		   (setq btn (treemacs-current-button)))
		 ;; close the button that was opened on the previous follow
		 (goto-char (treemacs-button-start btn))
		 ;; imenu already rescanned when fetching the tag path
		 ;; (let ((imenu-auto-rescan nil)
		 ;; 	   (new-file-btn))
		 ;;   ;; make a copy since this tag-path will be saved as cache, and the two modifications made here
		 ;;   ;; make it impossible to find the current position in `treemacs--find-index-pos'
		 ;;   (let* ((tag-path (copy-sequence tag-path))
		 ;; 		  (target-tag (list (car (car tag-path)))))
		 ;; 	 ;; remove position marker from target tag and move it
		 ;; 	 ;; to the end of the tag path
		 ;; 	 (setf tag-path (nconc (cdr tag-path) target-tag))
		 ;; 	 ;; the tag path also needs its file
		 ;; 	 ;;zyt
		 ;; 	 ;; (setf tag-path (cons buffer-file tag-path))
		 ;; 	 (setf tag-path (cons path tag-path))
		 ;; 	 ;; workaround: goto routines assume that at least the very first element of the followed
		 ;; 	 ;; path has a dom entry with a valid position, but this is not the case when moving to tags
		 ;; 	 ;; in a previously never-expanded file node, so we first find the file to make sure its
		 ;; 	 ;; position is known
		 ;; 	 ;;zyt
		 ;; 	 ;; (setf new-file-btn (treemacs-find-file-node buffer-file))
		 ;; 	 (setf new-file-btn (treemacs-find-node path))
		 ;; 	 (treemacs-goto-node tag-path)
		 ;; 	 (when (and treemacs--previously-followed-tag-position
		 ;; 				(not (equal (car treemacs--previously-followed-tag-position) new-file-btn)))
		 ;; 	   (-let [(prev-followed-pos . _) treemacs--previously-followed-tag-position]
		 ;; 		 (save-excursion
		 ;; 		   (when  (eq 'file-node-open (treemacs-button-get prev-followed-pos :state))
		 ;; 			 (goto-char prev-followed-pos)
		 ;; 			 (treemacs--collapse-file-node prev-followed-pos)))))
		 ;; 	 (setf treemacs--previously-followed-tag-position
		 ;; 		   (cons new-file-btn (treemacs-button-get new-file-btn :path)))))
		 (hl-line-highlight)
		 (treemacs--evade-image)
		 (when treemacs-recenter-after-tag-follow
		   (treemacs--maybe-recenter treemacs-recenter-after-tag-follow))))))
  )

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
