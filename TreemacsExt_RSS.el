(require 'dash)
(require 'treemacs)
(require 'elfeed)
(require 'elfeed-search)
(require 'treemacs-mouse-interface)
(require 'treemacs-treelib)
(require 'treemacs-rendering)
(require 'TreemacsExt)
(require 'all-the-icons-dired)
(require 's)
(require 'pcase)
(require 'url-util)

;; [[**  (bookmark--jump-via "("elfeed-search-fetch-visible" (filename . "~/.emacs.d/straight/repos/elfeed/elfeed-search.el") (front-context-string . "elfeed-search-fe") (rear-context-string . "date)))\n\n(defun ") (position . 28723) (last-modified 26584 58899 343371 0) (defaults "elfeed-search.el"))" 'switch-to-buffer-other-window)  **]]
;; (defun zyt/elfeed-update()
;;   ;; 改用原生的elfeed-search-fetch-visible
;;   (interactive)
;;   (save-excursion
;; 	(progn
;; 	  ;; ref:[[**  (bookmark--jump-via "("mark-whole-buffer" (filename  . "d:/Software/Editor/Emacs/emacs-30.1/share/emacs/30.1/lisp/simple.el") (front-context-string . "  (push-mark)\n  ") (rear-context-string . "  (interactive)\n") (position . 64374) (last-modified 26584 56588 594792 0) (defaults "simple.el"))" 'switch-to-buffer-other-window)  **]]
;; 	  (push-mark (point-max) nil t)
;; 	  (goto-char (minibuffer-prompt-end))
;; 	  ;; [[**  (bookmark--jump-via "("elfeed-update" (filename . "~/.emacs.d/straight/repos/elfeed/elfeed.el") (front-context-string . "ractive)\n  (elfe") (rear-context-string . "feeds'.\"\n  (inte") (position . 23394) (last-modified 26584 57909 665401 0) (defaults "elfeed.el"))" 'switch-to-buffer-other-window)  **]]
;; 	  (let* (
;; 			 (elfeed--inhibit-update-init-hooks t)
;; 			 (feeds (-uniq (--keep (elfeed-entry-feed it) (elfeed-search-selected))))
;; 			 )
;; 		(--map (elfeed-update-feed (elfeed-feed-url it)) feeds)
;; 		(run-hooks 'elfeed-update-init-hooks)
;; 		(elfeed-db-save)
;; 		(length feeds)
;; 		)
;; 	  )
;; 	)
;;   (deactivate-mark)
;;   nil
;;   )
(defvar-local zyt/elfeed-valid-feeds-cache nil)
(defun zyt/elfeed-list-feeds()
  (interactive)
  (or zyt/elfeed-valid-feeds-cache
	  (setq-local zyt/elfeed-valid-feeds-cache
				  (--keep
				   (let (
						 (feed-id
						  (or (and (stringp it) it)
							  (car it))
						  )
						 )
					 (and (elfeed-feed-entries feed-id)
						  (elfeed-db-get-feed feed-id))
					 )
				   elfeed-feeds
				   )
				  )
	  )
  )
;; (elfeed-db-get-feed 
(elfeed-feed-entries
 "https://planet.emacslife.com/atom.xml")

(defun treemacs-elfeed-open-entry(&optional state)
  ;; [[**  (bookmark--jump-via "("ref:elfeed-search-show-entry" (filename . "~/.emacs.d/straight/repos/elfeed/elfeed-search.el") (front-context-string . "(defun elfeed-se") (rear-context-string . "rward-line))))\n\n") (position . 33408) (last-modified 26574 24851 908485 0) (defaults "elfeed-search.el"))" 'switch-to-buffer-other-window)  **]]
  (let ((entry (treemacs-button-get (treemacs-node-at-point) :item)))
	(require 'elfeed-show)
	(when (elfeed-entry-p entry)
	  (pop-to-buffer (elfeed-search-buffer))
		(elfeed-untag entry 'unread)
		(elfeed-search-update-entry entry)
		(unless elfeed-search-remain-on-entry (forward-line))
		(elfeed-show-entry entry)
		(unless (elfeed-deref (elfeed-entry-content elfeed-show-entry))
		  (funcall browse-url-browser-function
				   (elfeed-entry-link elfeed-show-entry)
				   )
		  )
		)
	  )
	)

(defun treemacs-elfeed-view-entry(&optional state)
  ;; [[**  (bookmark--jump-via "("ref:elfeed-search-show-entry" (filename . "~/.emacs.d/straight/repos/elfeed/elfeed-search.el") (front-context-string . "(defun elfeed-se") (rear-context-string . "rward-line))))\n\n") (position . 33408) (last-modified 26574 24851 908485 0) (defaults "elfeed-search.el"))" 'switch-to-buffer-other-window)  **]]
  (let (
		(entry (treemacs-button-get (treemacs-node-at-point) :item))
		(treemacs-select-functions nil)
		)
	(require 'elfeed-show)
	(when (elfeed-entry-p entry)
	  (pop-to-buffer (elfeed-search-buffer))
		(elfeed-untag entry 'unread)
		(elfeed-search-update-entry entry)
		(unless elfeed-search-remain-on-entry (forward-line))
		(elfeed-show-entry entry)
		(unless (elfeed-deref (elfeed-entry-content elfeed-show-entry))
		  (funcall browse-url-browser-function
				   (elfeed-entry-link elfeed-show-entry)
				   )
		  )
		(treemacs-select-window)
	  )
	)
  )

(treemacs-define-entry-node-type TreemacsExt_RSS
  :label (propertize " RSS" 'face 'font-lock-keyword-face)
  :key 'TreemacsExt_RSS
  :open-icon (all-the-icons-material "rss_feed")
  :closed-icon (all-the-icons-faicon "rss")
  ;; :children (calibredb-list-ebooks)
  :children
  (zyt/elfeed-list-feeds)
  :child-type 'rss-feed
  )

(defun zyt/expand-rss-feed()
  
  )

(defun escape-question-mark (string)
  "Escapes question marks in a string with a backslash."
  (replace-regexp-in-string "?" "\\?" string t t))

(treemacs-define-expandable-node-type rss-feed
  ;; :closed-icon (treemacs-get-icon-value 'mail-plus)
  :closed-icon (all-the-icons-dired--icon "~/org-roam-files")
  ;; :closed-icon "+ "
  :open-icon (all-the-icons-dired--icon "README")
  :label (elfeed-feed-title item)
  ;; :label item
  :key item
  :children (elfeed-feed-entries item)
  :child-type 'rss-feed-entry
  :more-properties `(:feedurl ,(elfeed-feed-id item))
  ;; :ret-action (lambda(&optional state)
  ;; 				(pop-to-buffer (elfeed-search-buffer))
  ;; 				(elfeed)
  ;; 				)
  :on-expand
  (let (
		;; 类似如下youtube channel的feed url 包含`?`, 需要进行转义处理
		;; "https://www.youtube.com/feeds/videos.xml?channel_id=UCWZ3HFiJkxG1K8C4HVnyBvQ"
		(feedurl (escape-question-mark (treemacs-button-get btn :feedurl)))
		(treemacs-select-functions nil)
		)
	(pop-to-buffer (elfeed-search-buffer))
	(elfeed-search-mode)
	(elfeed-search-set-filter (format " =%s @1-year-old" feedurl))
	(treemacs-select-window)
	)
  :on-collapse
  (let (
		(treemacs-select-functions nil)
		)
	(pop-to-buffer (elfeed-search-buffer))
	(elfeed-search-clear-filter)
	(treemacs-select-window)
	)
  )

(treemacs-define-leaf-node-type rss-feed-virtual-entry
  :icon (all-the-icons-material "email")
  :label (concat
		  (format-time-string "%Y-%m-%d" (elfeed-entry-date item))
		  " "
		  (elfeed-entry-title item))
  :key item
  :ret-action #'treemacs-elfeed-open-entry
  )

;; (elfeed-entry-tags (elfeed-search-selected :ignore-region))
;; (elfeed-tagged-p 'unread (elfeed-search-selected :ignore-region))

(treemacs-define-leaf-node-type rss-feed-entry
  :icon (if
			(elfeed-tagged-p 'unread item)
		 (all-the-icons-material "email")
		 (all-the-icons-octicon "mail-read")
		 )
  :label (concat
		  (format-time-string "%Y-%m-%d" (elfeed-entry-date item))
		  " "
		  (elfeed-entry-title item))
  :key item
  ;; :children (list item)
  ;; :child-type 'rss-feed-virtual-entry
  :ret-action #'treemacs-elfeed-open-entry
  ;; :tab-action #'treemacs-elfeed-open-entry
  )
(treemacs-define-TAB-action 'treemacs-rss-feed-entry-open #'treemacs-elfeed-view-entry)
(treemacs-define-TAB-action 'treemacs-rss-feed-entry-closed #'treemacs-elfeed-view-entry)

(defun treemacs--tag-match-func-elfeed (path &optional current-tag-cache)
  (if 
	  (equal path current-tag-cache)
	  'matched
	)
  )

(defun treemacs--current-tag-path-elfeed()
  ;; [[**  (bookmark--jump-via "("enum treemacs node properties" (filename . "~/org-roam-files/20240131174059-treemacsextension.org") (front-context-string . "(treemacs-curren") (rear-context-string . "t-properties-at ") (position . 2226) (last-modified 26576 56427 670283 0) (defaults "20240131174059-treemacsextension.org"))" 'switch-to-buffer-other-window)  **]]
  ;; (treemacs-button-get (treemacs-current-button) :path)
  (interactive)
  (if (eq 'treemacs-mode major-mode)
	  (treemacs-button-get (treemacs-current-button) :path)
	(let* (
		   (entry
			(pcase major-mode
			  ('elfeed-search-mode
			   ;; (elfeed-feed-title (elfeed-search-selected :ignore-region))
			   (elfeed-search-selected :ignore-region)
			   )
			  ('elfeed-show-mode
			   ;; (elfeed-entry-feed elfeed-show-entry)
			   elfeed-show-entry
			   )
			  (_ nil)
			  )
			)
		   (feed (elfeed-entry-feed entry))
		   )
	  (append (list 'TreemacsExt_RSS) (list feed) (list entry))
	  )
	)
  )
(treemacs-tag-follow-mode-add-ext elfeed
								  elfeed-show-mode
								  treemacs--tag-match-func-elfeed
								  (treemacs--current-tag-path-elfeed)
								  t)

;; (treemacs-enable-project-extension
(treemacs-enable-top-level-extension
 :extension 'TreemacsExt_RSS
 :position 'bottom
 ;; :position 'top
 ;; :predicate (lambda (_)t)
 ;; :predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace)))))
 ;; :predicate (lambda (project) (eq project treemacs--project-of-extision-info))
 :predicate (lambda(_)(string= (treemacs-workspace->name (treemacs-current-workspace)) "Disks"))
 )

;; (treemacs-disable-project-extension
;; ;; (treemacs-disable-top-level-extension
;;  :extension 'TreemacsExt_RSS
;;  ;; :position 'bottom
;;  :position 'top
;;  )
(provide 'TreemacsExt_RSS)
