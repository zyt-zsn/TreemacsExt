(require 'info)
(require 'treemacs)
(require 'treemacs-treelib)
(require 'treemacs-mouse-interface)
(require 'treemacs-rendering)
(require 'cl-extra)
(require 'TreemacsExt)

(defun treemacs-list-info-buffer ()
  (progn
	(unless (get-buffer "*info*")
	  (let ((treemacs-buffer (current-buffer)))
		(info)
		(pop-to-buffer treemacs-buffer '(display-buffer-in-previous-window . ()) t)
		)
	  )
	(--filter
	 (eq 'Info-mode (buffer-local-value 'major-mode it))
	 (buffer-list))
	)
  )

(defvar-local ht-info-chapters nil)

(defun treemacs-list-info-files()
  (interactive)
  (when (not ht-info-chapters)
	(setq ht-info-chapters (make-hash-table)))  
  (with-current-buffer
	  (car (treemacs-list-info-buffer))
	(save-excursion
	  (let (
			(cur-node (format "(%s)%s" Info-current-file Info-current-node))
			ret
			)
		(condition-case err
			(while t
			  (Info-up)
			  ;; (print Info-current-node)
			  )
		  (t nil);;(print "ZYT: Node has no Up"))
		  )
		(let ((menu-idx 1))
		  (condition-case err
			  (while t
				(push (string-remove-suffix ")" (string-remove-prefix "(" (Info-extract-menu-counting menu-idx))) ret)
				(setq menu-idx (1+ menu-idx)))
			(t nil)))
		(unless (equal cur-node (format "(%s)%s" Info-current-file Info-current-node))
		  (Info-goto-node cur-node))
		(reverse ret)
		)
	  )
	)
  )


(treemacs-define-entry-node-type TreemacsExt_Info
  :label (propertize "Info" 'face 'font-lock-keyword-face)
  :key 'TreemacsExt_Info
  :open-icon (treemacs-get-icon-value 'info)
  :closed-icon (treemacs-get-icon-value 'info)
  :children (treemacs-list-info-files)
  :child-type 'info-file)

;; (NODE-NAME PARENT SECTION CHILDREN)
;; 参见对变量 Info-toc-nodes element 的描述
;; (Info-toc-nodes (substring-no-properties Info-current-file))

(defun info-get-nodes (depth &optional parent info-file)
  (with-current-buffer
	  (car (treemacs-list-info-buffer))
	(let ((node-list (Info-toc-nodes (substring-no-properties (or info-file Info-current-file)))))
	  (if (= 0 depth)
		  (--keep (unless (nth 1 it) ;;element: parent 为 nil
					(nth 0 it))
				  node-list)
		(--keep (when (member (nth 1 it) (or (and parent (list parent)) (info-get-nodes (1- depth) nil info-file)))
				  (nth 0 it)
				  )					;;element: parent 为 depth-1 的节点列表成员
				node-list))
	  )
	)
  )
(treemacs-define-expandable-node-type info-file
  :closed-icon "+ "
  :open-icon "- "
  :label (file-name-base item)
  :key item
  :children
  (info-get-nodes 1 nil item)
  :child-type 'info-section
  :more-properties `(:depth 0 :info-file ,item)
  :ret-action #'jump-to-info-top
  )

(treemacs-define-expandable-node-type info-section
  :label item
  :closed-icon
  (let (
		(children
		 (or
		  (gethash item ht-info-chapters)
		  (progn 
			;; (print (format "not found %s rescan" item))
			(puthash item (or (info-get-nodes (1+ (treemacs-button-get (treemacs-current-button) :depth)) item (treemacs-button-get (treemacs-current-button) :info-file))
							  'None)
					 ht-info-chapters)
			)
		  )))
	(if (and children (not (eq children 'None)))
		"+ "
	  "• "))
  :open-icon
  (let (
		(children
		 (or
		  (gethash item ht-info-chapters)
		  (progn 
			;; (print (format "not found %s rescan" item))
			(puthash item (or (info-get-nodes (1+ (treemacs-button-get (treemacs-current-button) :depth)) item (treemacs-button-get (treemacs-current-button) :info-file))
							  'None)
					 ht-info-chapters)
			)
		  )))
	(if (and children (not (eq children 'None)))
		"- "
	  "• "))
  :key item
  :children
  (let (
		(children
		 (or
		  (gethash item ht-info-chapters)
		  (progn 
			;; (print (format "not found %s rescan" item))
			(puthash item (or (info-get-nodes (1+ (treemacs-button-get (treemacs-current-button) :depth)) item (treemacs-button-get (treemacs-current-button) :info-file))
							  'None)
					 ht-info-chapters)
			)
		  )))
	(if (eq children 'None)
		nil
	  children
	  )
	)
  :child-type 'info-section
  :more-properties
  `(:depth ,(1+ (treemacs-button-get (treemacs-current-button) :depth))
		   :info-file ,(treemacs-button-get (treemacs-current-button) :info-file))
  :ret-action #'jump-to-info-section
  )

(defun jump-to-info-top (&optional _)
  (let (
		(info-buf (car (treemacs-list-info-buffer)))
		(info-file (treemacs-button-get (treemacs-current-button) :info-file))
		)
	(with-current-buffer info-buf
	  ;; (print (format "(%s)%s" info-file node))
	  (Info-find-node info-file "Top")
	  )
	(switch-to-buffer info-buf)
	)
  )

(defun jump-to-info-section (&optional _)
  (let (
		(info-buf (car (treemacs-list-info-buffer)))
		(node (treemacs-button-get (treemacs-current-button) :item))
		(info-file (treemacs-button-get (treemacs-current-button) :info-file))
		)
	(with-current-buffer info-buf
	  ;; (print (format "(%s)%s" info-file node))
	  (if t
		  (Info-goto-node (format "(%s)%s" info-file node))
		(Info-find-node info-file "Top")
		)
	  )
	(switch-to-buffer info-buf)
	)
  )

(defvar treemacs--project-of-extision-info nil)
(setq treemacs--project-of-extision-info (car (treemacs-workspace->projects (treemacs-current-workspace))))
;; (treemacs-enable-project-extension
;; (treemacs-disable-project-extension
(treemacs-enable-top-level-extension
 :extension 'TreemacsExt_Info
 :position 'bottom
 ;; :predicate (lambda (_)t)
 ;; :predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace)))))
 ;; :predicate (lambda (project) (eq project treemacs--project-of-extision-info))
 :predicate (lambda(_)(string= (treemacs-workspace->name (treemacs-current-workspace)) "Disks"))
 )

(defvar treemacs--project-of-extision-info nil)
(setq treemacs--project-of-extision-info (car (treemacs-workspace->projects (treemacs-current-workspace))))

(defun treemacs--current-tag-info ()
  Info-current-node
  )

(defun treemacs--current-tag-path-info()
  (interactive)
  (let (
		(cur-node (format "(%s)%s" Info-current-file Info-current-node))
		(ret (list Info-current-node))
		(orig-window-start (window-start))
		(old-history Info-history)
		(old-history-list Info-history-list)
		(old-history-forward Info-history-forward)
		)
	(prog1 
		(save-excursion
		  (condition-case err
			  (while t
				(Info-up 'same-file)
				;; (print Info-current-node)
				(push Info-current-node ret)
				)
			(t nil);;(print "ZYT: Node has no Up"))
			)
		  (unless (equal cur-node (format "(%s)%s" Info-current-file Info-current-node))
			(Info-goto-node cur-node)
			(set-window-start (selected-window) orig-window-start)
			)
		  ;; (if (and (equal "dir" Info-current-file)
		  ;; 		   (equal '("Top") ret))
		  ;; 	  (list "c:" 'TreemacsExt_Info)
		  ;; 	(append (list "c:" 'TreemacsExt_Info (file-name-nondirectory Info-current-file)) (cdr ret))
		  ;; 	)
		  (cond
		   (
			(and (equal "dir" Info-current-file) (equal '("Top") ret))
			;; (list "c:" 'TreemacsExt_Info)
			(list 'TreemacsExt_Info)
			)
		   ((and (string-prefix-p "*" Info-current-file) (string-suffix-p "*" Info-current-file))
			;;对于比如通过 “M-x info-apropos”动态生成的临时Info文件，不支持定位，直接返回匹配失败
			nil
			)
		   ;; (t (append (list "c:" 'TreemacsExt_Info (file-name-nondirectory Info-current-file)) (cdr ret)))
		   (t (append (list 'TreemacsExt_Info (file-name-nondirectory Info-current-file)) (cdr ret)))
		   )
		  )
	  (setq Info-history old-history)
	  (setq Info-history-list old-history-list)
	  (setq Info-history-forward old-history-forward)
	  )
	)
  )

;; (defvar-local matched-path-list nil)
(defun treemacs--tag-match-func-Info (path &optional current-tag-cache)
  (let ((current-tag-path (or current-tag-cache (treemacs--current-tag-path-info)))
		(path (if (listp path) path (list path))))
	(when
		(and
		 (listp path)
		 (eq 'TreemacsExt_Info (nth 0 path))
		 ;; (not (memq path matched-path-list))
		 )
	  ;; (push path matched-path-list)
	  ;; (print (format "%s" path))
	  (cond
	   ((equal path current-tag-path)
		;; (print (format "Matched: %s" path))
		'matched)
	   ((and (<=(length path) (length current-tag-path)) (equal path (cl-subseq current-tag-path 0 (length path))))
		;; (print "partial matched")
		;; (print (format "Parital Matched: %s to %s" path current-tag-path))
		'partial-matched
		;; 'partial-matched-ignore-following ;;暂未解决
		;;Info 的路径匹配和chm不同，部分匹配后，必定在此匹配路径的子节点中，
		;;返回'partial-matched-ignore-following，以告知treemacs--follow-tag-current-path略过后续忽略同级的其他节点
		;;直接进入此节点的展开搜索
		)
	   )
	  )
	)
  )

(treemacs-tag-follow-mode-add-ext info
								  Info-mode
								  treemacs--tag-match-func-Info
								  (treemacs--current-tag-path-info)
								  treemacs--project-of-extision-info)

(provide 'TreemacsExt_Info)
