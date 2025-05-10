(require 'dash)
(require 'treemacs)
(require 'calibredb)
(require 'treemacs-mouse-interface)
(require 'treemacs-treelib)
(require 'treemacs-rendering)
(require 'TreemacsExt)
(require 'all-the-icons-dired)
(require 's)

(defun calibredb-list-ebooks()
  (interactive)
  ;; (calibredb-search-candidates calibredb-search-filter :limit calibredb-search-page-max-rows :page page)
  (--keep (cdr it)
		  (calibredb-search-candidates calibredb-search-filter :limit 100000 :page 1)
		  )
  ;; (calibredb-search-keyword-filter calibredb-search-filter)
  ;;(list "book1" "book2" "book3")
  )

(treemacs-define-entry-node-type TreemacsExt_Calibredb
  :label (propertize " Ebooks" 'face 'font-lock-keyword-face)
  :key 'TreemacsExt_Calibredb
  :open-icon (all-the-icons-dired--icon "~/Documents")
  :closed-icon (all-the-icons-dired--icon "~/Documents")
  ;; :children (calibredb-list-ebooks)
  :children
  (--keep
   (cons
	(--keep
	 ;; 去掉 last_modified, 以便更新format后，可以根据之前保存的路径重新定位置打开的文件
	 (if (or
		  (string= ":last_modified" (symbol-name (car it)))
		  (string= ":ids" (symbol-name (car it)))
		  )
		 nil
	   it)
	 (car it)
	 )
	(cdr it)
	)
   (calibredb-list-ebooks)
   )
  :child-type 'ebook)

(defun add-format (path format-to-add)
  (let* (
		 (metainfo (cadr path))
		 )
	(cons
	 (car path)
	 (list
	  (list
	   (--keep
		;; 去掉 last_modified/file-path(calibredb add format会改写此两个部分、以及ids部分<ids已经在添加treemacs item时去掉>)
		;; 以便更新format后，可以根据之前保存的路径重新定位置打开的文件
		(cond
		 ((string= ":last_modified" (symbol-name (car it)))
		  nil)
		 (
		  (or
		   (string= ":book-format" (symbol-name (car it)))
		   (string= ":file-path" (symbol-name (car it)))
		   ;;(not (s-contains-p format-to-add (file-name-extension (cadr it)) t))
		   )
		  (let* (
				 (str-to-match (cadr it))
				 (reserve-part
				  (if (string= ":book-format" (symbol-name (car it)))
					  ""
					(concat (file-name-directory str-to-match) (file-name-base str-to-match) ".")
					)
				  )
				 (replace-part
				  (if (string= ":book-format" (symbol-name (car it)))
					  str-to-match
					(file-name-extension str-to-match)
					)
				  )
				 )
			(cons (car it)
				  (list
				   (cond
					(
					 (s-contains? (concat "." format-to-add ",") replace-part)
					 (format "%s%s,%s" 
							 reserve-part
							 (string-replace (concat "." format-to-add ",") "." replace-part)
							 format-to-add)
					 )
					(
					 (s-contains? (concat format-to-add ",") replace-part)
					 (format "%s%s,%s" 
							 reserve-part
							 (string-replace (concat format-to-add ",") "" replace-part)
							 format-to-add)
					 )
					(
					 (s-contains? (concat "," format-to-add) replace-part)
					 (concat
					  reserve-part
					  replace-part)
					 )
					(
					 (s-contains? (concat format-to-add ".") replace-part)
					 (concat 
					  reserve-part
					  replace-part)
					 )
					(t
					 (format "%s%s,%s"
							 reserve-part
							 replace-part format-to-add)
					 )
					))))
		  )
		 (t
		  it
		  )
		 )
		(car metainfo)
		)
	   )
	  )
	 )
	)
  )

(treemacs-define-leaf-node-type ebook-metainfo-value
  :label item
  ;; :icon (treemacs-get-icon-value 'node)
  :icon (all-the-icons-dired--icon "catalog")
  :key item
  )

(treemacs-define-leaf-node-type ebook-metainfo-item
  :key item
  :icon (treemacs-get-icon-value 'tag-leaf)
  ;; :icon (all-the-icons-dired--icon "metadata.json")
  :label (format "%s:%s"
				 (string-trim (symbol-name (nth 0 item)) ":")
				 (nth  1 item))
  )
(defun treemacs-calibredb-open-book(&optional state)
  (call-interactively 'calibredb-find-file
   (treemacs-button-get (treemacs-node-at-point) :item)
   )
  )

(treemacs-define-expandable-node-type ebook
  ;; :closed-icon (treemacs-get-icon-value 'mail-plus)
  :closed-icon (all-the-icons-dired--icon "~/org-roam-files")
  ;; :closed-icon "+ "
  :open-icon (all-the-icons-dired--icon "README")
  :label (calibredb-getattr item :book-title)
  :key item
  :children (--filter (not (string= "" (nth 1 it))) (car item))
  :child-type 'ebook-metainfo-item
  :ret-action #'treemacs-calibredb-open-book
  )

;; (treemacs-enable-project-extension
(treemacs-enable-top-level-extension
 :extension 'TreemacsExt_Calibredb
 :position 'bottom
 ;; :predicate (lambda (_)t)
 ;; :predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace)))))
 ;; :predicate (lambda (project) (eq project treemacs--project-of-extision-info))
 :predicate (lambda(_)(string= (treemacs-workspace->name (treemacs-current-workspace)) "Disks"))
 )

;; (treemacs-disable-project-extension
;; (treemacs-disable-top-level-extension
 ;; :extension 'TreemacsExt_Calibredb
 ;; :position 'bottom
 ;; )
(provide 'TreemacsExt_Calibredb)
