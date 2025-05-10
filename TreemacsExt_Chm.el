(require 'dash)
(require 'treemacs)
(require 'treemacs-treelib)
(require 'treemacs-rendering)
(require 'TreemacsExt)
;; (require 'w3m)
(require 'eww)
(require 'dired)

(defun treemacs-list-chm-files ()
  (->> (buffer-list)
	   (--filter (eq 'dired-mode (buffer-local-value 'major-mode it)))
	   (--map (with-current-buffer it
				(->> (dired-files-attributes (dired-current-directory))
					 (--keep (when (equal "chm" (file-name-extension (nth 1 it)))
							   (nth 1 it)))
					 )
				)
			  )
	   (flatten-tree)
	   (-distinct)
	   )
  )

(defun chm_file_path_to_decomposed_dir (chm_file_path)
  (concat "c:/tmp/" (file-name-base chm_file_path) "/")
  )

;; (setq dst_dir "c:/tmp/ICBC_Manual_EN")
;; (car (directory-files dst_dir t "\.hhc$"))
(defun parse-chm(file_path)
  (interactive)
  (let* (
		 (target file_path)
		 (dst_dir (chm_file_path_to_decomposed_dir file_path))
		 ;; (fhhc (concat dst_dir base ".hhc"))
		 ;;ZYT:Base name of decompiled hhc file is not necessorily same with that of chm file. 
		 )
	(unless (file-directory-p dst_dir)
	  ;; (start-process "Decompile" nil "hh.exe" "-decompile"  dst_dir target)
	  (call-process "hh.exe" nil  nil nil "-decompile"  dst_dir target))
	(setq fhhc (car (directory-files dst_dir t "\.hhc$")))
	(parse-hhc fhhc)
	)
  )
(defun parse-toc(ul idx &rest path)
  (nth idx (dom-non-text-children ul))
  ;; (let* (
  ;; 		 chapter
  ;; 		 (li (nth idx (dom-non-text-children ul)))
  ;; 		 )
  ;; 	(if path
  ;; 		(if (cdr path)
  ;; 			(apply 'parse-toc (nth 1 (dom-non-text-children li)) (car path) (cdr path))
  ;; 		  (parse-toc (nth 1 (dom-non-text-children li)) (car path))
  ;; 		  )
  ;; 	  (let* (
  ;; 			 (obj (dom-non-text-children li))
  ;; 			 )
  ;; 		(mapc #'(lambda(param)
  ;; 				  (when (equal "Name" (dom-attr param 'name))
  ;; 					(print (dom-attr param 'value))
  ;; 					(setq chapter (dom-attr param 'value))
  ;; 					)
  ;; 				  )
  ;; 			  (dom-non-text-children obj)
  ;; 			  )
  ;; 		chapter
  ;; 		)
  ;; 	  )
  ;; 	)
  )
;;TortoiseSVN.chm 的hhc文件格式不同于 python3810的hhc文件格式：
;;UL直接跟在LI后面，而非作为LI的childred跟在OBJECT后面
;;为了借用针对pyth3810的解析逻辑，对此类型的UL进行调整
(defun fix-ul (ul)
  (let* (
		 (fixed_ul (dom-node 'ul))
		 )
	(--keep (cond
			 (
			  (eq 'li (dom-tag it))
			  (dom-append-child fixed_ul it)
			  )
			 (
			  (and (eq 'ul (dom-tag it)) (> it-index 0))
			  (dom-append-child (last (dom-non-text-children fixed_ul)) (fix-ul it))
			  )
			 (t nil)
			 )
			(dom-non-text-children ul))
	fixed_ul
	)
  )
(defun parse-hhc(&optional hhc_path)
  (interactive)
  (with-current-buffer (or (and hhc_path (find-file-noselect hhc_path)) (current-buffer))
	(
	 progn
	 (setq dom (libxml-parse-html-region (point-min) (point-max)))
	 (setq head (nth 0 (dom-children dom)))
	 (setq body (nth 1 (dom-children dom)))
	 (let* (
			;; (ul (nth 1 (dom-non-text-children body)))
			(ul (car (--filter (eq 'ul (dom-tag it)) (dom-non-text-children body))))
			(fixed_ul (fix-ul ul))
			(cnt (length (dom-non-text-children fixed_ul)))
			(idx 0)
			(item ())
			)
	   (while (< idx cnt)
		 (setq item (append item (list (parse-toc fixed_ul idx))))
		 (setq idx (1+ idx))
		 )
	   item
	   )
	 )
	)
  )
(defvar-local ht-chm-chapters nil)
(defun treemacs-list-chapters-per-file (file_path)
  (when (not ht-chm-chapters)
	(setq ht-chm-chapters (make-hash-table)))
  (or (gethash file_path ht-chm-chapters)
	  (let ((ret (parse-chm file_path)))
		(puthash file_path ret ht-chm-chapters)
		ret))
  )
(defun treemacs-showcase--buffers-by-mode (chm_file)
  (->> (buffer-list)
	   (--filter (eq mode (buffer-local-value 'major-mode it)))
	   (--reject (string-prefix-p " " (buffer-name it)))))

(treemacs-define-entry-node-type TreemacsExt_Chm
  :label (propertize "ChmFiles" 'face 'font-lock-keyword-face)
  :key 'TreemacsExt_Chm
  :open-icon (treemacs-get-icon-value 'list)
  :closed-icon (treemacs-get-icon-value 'list)
  :children (treemacs-list-chm-files)
  :child-type 'chm_file_node)

(treemacs-define-expandable-node-type chm_file_node
  :closed-icon "+ "
  :open-icon "- "
  :label (propertize
		  ;; (file-name-nondirectory item)
		  (file-name-base item)
		  'face 'font-lock-variable-name-face
		  )
  :key item
  :children (treemacs-list-chapters-per-file item)
  :child-type 'chm_chapter_node
  ;; :more-properties `(:zytpath ,item)
  :more-properties `(:chm_file_path ,item)
  )
(defvar-local ht-children nil)
(treemacs-define-expandable-node-type chm_chapter_node
  ;;item -> dom li
  :closed-icon
  (if (> (length (dom-non-text-children item)) 1)
	  "+ "
	"• ")
  :open-icon
  (if (> (length (dom-non-text-children item)) 1)
	  "- "
	"• ")
  ;; :label (prog1 (dom-attr (nth 0 (dom-non-text-children (nth 0 (dom-non-text-children item)))) 'value)
  ;; 		   ;; (print (format "label: %s"  (dom-attr (nth 0 (dom-non-text-children (nth 0 (dom-non-text-children item)))) 'value)))
  ;; 		   )
  :label (or (gethash item ht-chm-chapters)
			 (let ((ret (dom-attr (nth 0 (dom-non-text-children (nth 0 (dom-non-text-children item)))) 'value)))
			   (puthash item ret ht-chm-chapters)
			   ;; (princ (format "Not found \"%s\" in hash table, reparse\n" ret))
			   ret))
  ;; :label (symbol-name (dom-tag item))
  :key item
  :children  ;;list of LI
  (progn
	(unless ht-children
	  (setq ht-children (make-hash-table :test 'equal)))
	(let ((ret (gethash item ht-children)))
	  (cond
	   ((equal ret 'None) nil)
	   ((null ret)
		(let* (
			   (ul_cnt (length (--keep (eq 'ul (dom-tag it)) (dom-non-text-children item))))
			   ret
			   )
		  ;; (print (format "Not found children of %s, reparse" (treemacs-showcase-LI-to-link item)))
		  (if (equal ul_cnt 1)
			  (setq ret (dom-non-text-children (nth 1 (dom-non-text-children item)))) ;;python3810的hhc文件格式： Line20 Item LI {Object{param,param}， UL{LI，LI，LI}}
			(setq ret (--map (dom-non-text-children it) (--filter (eq 'ul (dom-tag it)) (dom-non-text-children item)))) ;;script56的hhc文件格式：Line23  Item LI {Object{param,param}， UL{LI}，UL{LI}，UL{LI}}
			)
		  ;; (print (format "put children of %s: %s into hashtable" (treemacs-showcase-LI-to-link item) ret))
		  (puthash item (or ret 'None) ht-children)
		  ret
		  ))
	   (t ret)
	   )
	  )
	)
  :child-type
  'chm_chapter_node
  ;; :more-properties `(:chapter ,item :chm_file_path ,(treemacs-button-get (treemacs-button-get (treemacs-current-button) :parent) :chm_file_path))
  :more-properties `(:chapter ,item :chm_file_path ,(treemacs-button-get (treemacs-current-button) :chm_file_path))
  :dbclick-action #'treemacs-showcase-RET-buffer-action
  :ret-action #'treemacs-showcase-RET-buffer-action)  

(defun treemacs-showcase-LI-to-paragraph (li)
  (dom-attr (car (--filter
				  (equal (dom-attr it 'name) "Name")
				  (dom-non-text-children (nth 0 (dom-non-text-children li)))
				  )) 'value)
  )
(defun treemacs-showcase-LI-to-link (li)
  (let ((link 
		 (dom-attr (car (--filter
						 (equal (dom-attr it 'name) "Local")
						 (dom-non-text-children (nth 0 (dom-non-text-children li)))
						 )) 'value)
		 ))
	;; (print (format "zyt link: %s" link))
	;; (print (format "zyt: %s" (concat treemacs--tmp-dir-of-extision (car (last (split-string link "::/"))))))
	(concat treemacs--tmp-dir-of-extision (car (last (split-string link "::/")))))
  )

(defun treemacs-showcase-RET-buffer-action (&optional _)
  (let* (
		 (chapter (treemacs-button-get (treemacs-current-button) :chapter))
		 (chm_file_path (treemacs-button-get (treemacs-current-button) :chm_file_path))
		 (dst_dir (chm_file_path_to_decomposed_dir chm_file_path))
		 ;; (link (dom-attr (nth 1 (dom-non-text-children (nth 0 (dom-non-text-children chapter)))) 'value))
		 (link (dom-attr (car (--filter
							   (equal (dom-attr it 'name) "Local")
							   (dom-non-text-children (nth 0 (dom-non-text-children chapter)))
							   )) 'value))

		 ;; <param name="Local" value="mk:@MSITStore:PyWin32.chm::/_winxptheme__IsAppThemed_meth.html">
		 )
	(setq treemacs--path-of-extision-chm (treemacs-button-get (treemacs-current-button) :path))
	(setq treemacs--tmp-dir-of-extision dst_dir)
	(setq treemacs--dom-extision-chm treemacs-dom)
	;; (print chm_file_path)
	;; (print base)
	;; (print link)
	;; (print (cdr (split-string link "::")))
	;; (car (last (split-string "mk:@MSITStore:PyWin32.chm::/_winxptheme__IsAppThemed_meth.html" "::")))
	;; (print        (concat dst_dir (car (last (split-string link "::/")))))
	;; (w3m-goto-url (concat dst_dir (car (last (split-string link "::/")))))
	;;w3m对于某些连接无法解析，如：Designer.chm 中 Previous & Next导航键
	(eww-open-file (concat dst_dir (car (last (split-string link "::/")))))
	)
  ;; (let ((buffer (-some-> (treemacs-current-button)
  ;; 				  ;; (treemacs-button-get :buffer))))
  ;; 				  (treemacs-button-get :item))))
  ;; 	(when (buffer-live-p buffer)
  ;; 	  (pop-to-buffer buffer)))
  )
;; (treemacs-enable-top-level-extension
(defvar treemacs--project-of-extision-chm nil)
(defvar treemacs--path-of-extision-chm nil)
(defvar treemacs--tmp-dir-of-extision nil)
(defvar treemacs--dom-extision-chm nil)
(setq treemacs--project-of-extision-chm (car (treemacs-workspace->projects (treemacs-current-workspace))))
(treemacs-enable-project-extension
 :extension 'TreemacsExt_Chm
 :position 'top
 ;; :predicate (lambda (_)(t))
 ;; :predicate (lambda (project) (eq project (car (treemacs-workspace->projects (treemacs-current-workspace)))))
 :predicate (lambda (project) (eq project treemacs--project-of-extision-chm))
 )

(defun showchm-groups ()
  (interactive)
  (let ((bufname "*Showcase Buffers*"))
	(--when-let (get-buffer bufname) (kill-buffer it))
	(let ((buf (get-buffer-create bufname)))
	  (pop-to-buffer buf)
	  (treemacs-initialize showcase-buffers-variadic
		:with-expand-depth 'all
		:and-do (setf treemacs-space-between-root-nodes t)))))

;; follow tag mode function

(advice-add
 'treemacs--follow-tag-at-point
 :before-until
 'treemacs--follow-tag-at-point-chm
 )

(defun eww-get-dom()
  ;; (set-default-coding-systems 'cp1252)
  (let* (
		 ;; (buf (get-buffer-create "*zyt-source*"))
		 (source (plist-get eww-data :source)))
	(with-temp-buffer
	  ;; (with-current-buffer buf
	  (delete-region (point-min) (point-max))
	  (insert (or source "no source found"))
	  ;; (view-buffer buf)
	  (libxml-parse-html-region (point-min) (point-max))))
  )

(setq imenu-generic-expression (list '(nil "\\(^Generator\\)" 1)))

;; (treemacs-showcase-LI-to-link (-last-item treemacs--path-of-extision-chm))
;; (current-section)
;; (memq treemacs--path-of-extision-chm (ht-keys treemacs-dom))
;; (defun treemacs--follow-tag-current-path ()

;;   ;; (plist-get eww-data :url)
;;   ;; "file:///c:/tmp/python3810/reference/executionmodel.html#naming-and-binding"

;;   ;; (concat "file:///" (treemacs-showcase-LI-to-link (-last-item treemacs--path-of-extision-chm)))
;;   ;; "c:/tmp/python3810/reference/executionmodel.html#naming-and-binding"
;;   ;; (memq treemacs--path-of-extision-chm (ht-keys treemacs-dom))

;;   ;; (split-string (plist-get eww-data :url) "#")
;;   (let* ((cur-page-url (plist-get eww-data :url))
;; 		 (current-link (concat (car (split-string cur-page-url "#")) "#" (current-section)))
;; 		 ;; (cnt 0)
;; 		 )
;; 	(--first
;; 	 (and
;; 	  ;; (setq cnt (1+ cnt))
;; 	  (and (listp it) (listp (-last-item it)))
;; 	  (eq 'li (car (-last-item it)))
;; 	  (string-prefix-p (concat "file:///" (treemacs-showcase-LI-to-link (-last-item it))) current-link)
;; 	  (princ (format "current-link:\t%s\ntarget-link:\t%s\n" current-link (concat "file:///" (treemacs-showcase-LI-to-link (-last-item it)))))
;; 	  (equal
;; 	   ;; (concat "file:///" (treemacs-showcase-LI-to-link (-last-item treemacs--path-of-extision-chm)))
;; 	   (concat "file:///" (treemacs-showcase-LI-to-link (-last-item it)))
;; 	   current-link)
;; 	  )
;; 	 (ht-keys treemacs--dom-extision-chm))
;; 	)
;;   )
(defvar-local matched-path-list nil)
(defun treemacs--tag-match-func-Chm (path current-tag-cache)
  (let* (
		 (current-link (concat (car (split-string (car current-tag-cache) "#")) (and (cdr current-tag-cache) (concat "#" (car (cdr current-tag-cache))))))
		 (current-paragraph (cdr (cdr current-tag-cache)))
		 )
	(when
		(and
		 (listp path)
		 (eq 'TreemacsExt_Chm (nth 1 path))
		 (not (memq path matched-path-list))
		 )
	  (cond
	   ((and (listp (-last-item path)) (eq 'li (car (-last-item path)))) ;;chm chapter node
		(let* (
			   (str-to-match (concat "file:///" (car (split-string (treemacs-showcase-LI-to-link (-last-item path)) "/index.html"))))
			   (paragraph-to-match (treemacs-showcase-LI-to-paragraph (-last-item path)))
			   )
		  ;; (princ (format "str-to-matc:%s\n" str-to-match))
		  ;; (princ (format "current-link:\t%s\ntarget-link:\t%s\n" current-link str-to-match))
		  (when (string-prefix-p str-to-match current-link)
			(setq ret path)
			(setq matched-str str-to-match)
			(push path matched-path-list)
			(if (or
				 (equal str-to-match current-link)
				 ;;针对有id(#)情形
				 ;; <LI> <OBJECT type="text/sitemap">
				 ;; <param name="Name" value="Basic Logging Tutorial">
				 ;; <param name="Local" value="howto/logging.html#basic-logging-tutorial">
				 ;; </OBJECT>
				 (equal current-paragraph paragraph-to-match)
				 ;;针对无id(#)情形
				 ;; <LI> <OBJECT type="text/sitemap">
				 ;; <param name="Name" value="Logging HOWTO">
				 ;; <param name="Local" value="howto/logging.html">
				 ;; </OBJECT>
				 (when (<= (length (dom-non-text-children (-last-item path))) 1)
				   ;; (print "Hello")
				   t
				   )
				 ;; 针对 eww 在html中能解析出子 paragraph 但 hhc 中无响应子连接的情况
				 )
				'matched
			  'partial-matched
			  )
			)
		  ))
	   ((and
		 (= 3 (length path))
		 (stringp (nth 2 path))
		 (string-prefix-p (concat "file:///" (chm_file_path_to_decomposed_dir (nth 2 path)))
						  current-link
						  ;; link file:///c:/tmp/python3810/contents.html
						  ;; path ("c:" TreemacsExt_Chm "c:/tmp/tmp/python3810.chm" (li..))
						  ))  ;;chm file node
		(setq ret path)
		(setq matched-str (concat "file:///" (chm_file_path_to_decomposed_dir (nth 2 path))))
		(push path matched-path-list)
		'partial-matched
		)
	   ;; ((equal path '("c:" TreemacsExt_Chm))
	   ;;  (setq ret path)
	   ;;  (push path matched-path-list)
	   ;;  'partial-matched
	   ;;  )
	   )
	  )
	)
  )

;; (defun treemacs--follow-tag-at-point-chm ()
;;   "Follow the tag at point in the treemacs view."
;;   (interactive)
;;   (when (eq major-mode 'eww-mode)
;; 	  (let* ((treemacs-window (treemacs-get-local-window))
;; 			 (path (treemacs--follow-tag-current-path 'treemacs--tag-match-func-Chm (cons (plist-get eww-data :url) (current-section))))
;; 			 (project treemacs--project-of-extision-chm))
;; 		(when (and treemacs-window project)
;; 		  (condition-case e
;; 			  (treemacs--do-follow-tag-extension nil treemacs-window path project)
;; 			(imenu-unavailable (ignore e))
;; 			(error (treemacs-log-err "Encountered error while following chm tag at point: %s" e)))))))


(defun treemacs--get-imenu-index-chm ()
  "Fetch imenu index of FILE."
  (let (
		;; (buff)
		(buff (current-buffer))
		(result)
		(mode)
		;; (existing-buffer (get-file-buffer file))
		(org-imenu-depth (max 10 (or 0 (and (boundp 'org-imenu-depth) org-imenu-depth)))))
	(ignore org-imenu-depth)
	;; (if existing-buffer
	;;     (setq buff existing-buffer)
	;;   (cl-letf (((symbol-function 'run-mode-hooks) (symbol-function 'ignore)))
	;;     (setq buff (find-file-noselect file))))
	(condition-case e
		(when (buffer-live-p buff)
		  (with-current-buffer buff
			(let ((imenu-generic-expression
				   (if (eq major-mode 'emacs-lisp-mode)
					   (or treemacs-elisp-imenu-expression
						   imenu-generic-expression)
					 imenu-generic-expression))
				  (imenu-create-index-function
				   (if (eq major-mode 'org-mode)
					   #'org-imenu-get-tree
					 imenu-create-index-function)))
			  (setf result (and (or imenu-generic-expression imenu-create-index-function)
								(imenu--make-index-alist t))
					mode major-mode)))
		  ;; (unless existing-buffer (kill-buffer buff))
		  (when result
			(when (string= "*Rescan*" (caar result))
			  (setf result (cdr result)))
			(unless (equal result '(nil))
			  (treemacs--post-process-index result mode))))
	  (imenu-unavailable (ignore e))
	  (error (prog1 nil (treemacs-log-err "Encountered error while following tag at point: %s" e))))))


(defun imenu--make-index-alist-chm (&optional noerror)
  "Create an index alist for the definitions in the current buffer.
This works by using the hook function `imenu-create-index-function'.
Report an error if the list is empty unless NOERROR is supplied and
non-nil.

See `imenu--index-alist' for the format of the index alist."
  (or (and imenu--index-alist
		   (or (not imenu-auto-rescan)
			   (and imenu-auto-rescan
					(> (buffer-size)  imenu-auto-rescan-maxout))))
	  ;; Get the index; truncate if necessary.
	  (progn
		(setq imenu--index-alist
			  (save-excursion
				(save-restriction
				  (widen)
				  (funcall imenu-create-index-function))))
		(imenu--truncate-items imenu--index-alist)))
  (or imenu--index-alist noerror
	  (imenu-unavailable-error
	   "No items suitable for an index found in this buffer"))
  (or imenu--index-alist
	  (setq imenu--index-alist (list nil)))
  (if imenu-auto-rescan
	  imenu--index-alist
	;; Add a rescan option to the index.
	(cons imenu--rescan-item imenu--index-alist)))

(defun treemacs--flatten&sort-imenu-index-chm ()
  "Flatten current file's imenu index and sort it by tag position.
The tags are sorted into the order in which they appear, regardless of section
or nesting depth."
  (if (eq major-mode 'pdf-view-mode)
	  'unsupported
	(let* ((imenu-auto-rescan t)
		   (org? (eq major-mode 'org-mode))
		   ;; (index (-> (buffer-file-name) (treemacs--get-imenu-index)))
		   (index (treemacs--get-imenu-index-chm))
		   (flat-index (if org?
						   (treemacs--flatten-org-mode-imenu-index index)
						 (treemacs--flatten-imenu-index index)))
		   (first (caar flat-index))
		   ;; in org mode buffers the first item may not be a cons since its position
		   ;; is still stored as a text property
		   (semantic? (and (consp first) (overlayp (cdr first))))
		   (compare-func (if (memq major-mode '(markdown-mode adoc-mode))
							 #'treemacs--compare-markdown-tag-paths
						   #'treemacs--compare-tag-paths)))
	  (cond
	   (semantic?
		;; go ahead and just transform semantic overlays into markers so we dont
		;; have trouble with comparisons when searching a position
		(dolist (tag-path flat-index)
		  (let ((leaf (car tag-path))
				(marker (make-marker)))
			(setcdr leaf (move-marker marker (overlay-start (cdr leaf)))))))
	   ;; same goes for an org index, since headlines with children store their
	   ;; positions as text properties
	   (org?
		(dolist (tag-path flat-index)
		  (let ((leaf (car tag-path)))
			(when (stringp leaf)
			  (setcar tag-path (cons leaf (get-text-property 0 'org-imenu-marker leaf))))))))
	  (sort flat-index compare-func))))

;; file:///c:/tmp/python3810/howto/functional.html#combinatoric-functions
;; <div class="section" id="combinatoric-functions">

;; (defun zyt()
;;   (interactive)
;;   (dom-search
;;    (plist-get eww-data :dom)
;;    (lambda(node)
;; 	 (let* ((tag (dom-tag node)))
;; 	   (when (and
;; 			  (eq tag 'div)
;; 			  (equal (dom-attr node 'class) "section"))
;; 		 (princ (dom-attr node 'id))
;; 		 (princ "\n")
;; 		 )
;; 	   )
;; 	 nil
;; 	 )
;;    )
;;   )

(defun dom-sections()
  (--map (car it) (zyt))
  )
(defvar-local ht-dom-heads nil)
(defun dom-heads()
  (unless ht-dom-heads
	(setq ht-dom-heads (make-hash-table)))
  (or (gethash (plist-get eww-data :url) ht-dom-heads)
	  (puthash (plist-get eww-data :url) (--map (cdr it) (zyt)) ht-dom-heads))
  )

(defun current-head()
  (while (and (not (bobp)) (not (member (car (split-string (thing-at-point 'line t) "[\n\r]+")) (dom-heads))))
	(forward-line -1))
  )

(defun current-section()
  (let (
		(heads (dom-heads))
		line
		section
		found?
		)
	(save-excursion 
	  (while (not (or (bobp) found?))
		(setq line (car (split-string (thing-at-point 'line t) "[\n\r]+")))
		(setq found? (--any (when (equal line (cdr it))
							  ;; (setq section (car it))
							  (setq section it)
							  )
							(zyt)))
		(unless found?
		  (forward-line -1)
		  )
		))
	section
	)
  )
(defvar-local ht-zyt-list nil)
(defun zyt()
  ;; (interactive)
  (unless ht-zyt-list
	(setq ht-zyt-list (make-hash-table)))
  (or (gethash (plist-get eww-data :url) ht-zyt-list)
	  (puthash (plist-get eww-data :url) 
			   (let (
					 (dom (plist-get eww-data :dom))
					 (section-head-alist ())
					 )
				 (dom-search
				  dom
				  (lambda(node)
					(let* (
						   (tag (dom-tag node))
						   )
					  (when (memq tag (list 'h1 'h2 'h3 'h4))
						(setq parent (dom-parent dom node))
						(if (and
							 (eq (dom-tag parent) 'div)
							 (equal (dom-attr parent 'class) "section"))
							;; (princ (format "ID %s" (dom-attr parent 'id)))
							(progn
							  ;; (cond
							  ;;  ((eq tag 'h1) (princ "H1:"))
							  ;;  ((eq tag 'h2) (princ "H2:\t"))
							  ;;  ((eq tag 'h3) (princ "H3:\t\t"))
							  ;;  ((eq tag 'h4) (princ "H4:\t\t\t")))
							  ;; (princ (dom-text node))
							  ;; (princ "\n")
							  (setq section-head-alist (append section-head-alist (list (cons (dom-attr parent 'id) (dom-text node)))))
							  t
							  )
						  )
						)
					  )
					)
				  )
				 section-head-alist
				 )
			   ht-zyt-list)
	  )
  )

(treemacs-tag-follow-mode-add-ext chm
								  eww-mode
								  treemacs--tag-match-func-Chm
								  (cons (plist-get eww-data :url) (current-section))
								  treemacs--project-of-extision-chm)



;; Functions & Files from official packages modified by zyt

;; (defun treemacs--reopen-node (btn &optional git-info)
;;   "Reopen file BTN.
;; GIT-INFO is passed through from the previous branch build."
;;   (pcase (treemacs-button-get btn :state)
;;     ('dir-node-closed  (treemacs--expand-dir-node btn :git-future git-info))
;;     ('file-node-closed (treemacs--expand-file-node btn))
;;     ('tag-node-closed  (treemacs--expand-tag-node btn))
;;     ('root-node-closed (treemacs--expand-root-node btn))
;;     ;; (other             (funcall (alist-get other treemacs-TAB-actions-config) btn))))
;; 	;; Modified by zyt
;;     (other             (funcall (alist-get other treemacs-TAB-actions-config) nil))))

;; treemacs-treelib.el
(provide 'TreemacsExt_Chm)

;; Local Variables:
;; coding: utf-8-dos
;; End:
