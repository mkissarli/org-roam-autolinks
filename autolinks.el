(defun strip-text-properties(txt)
  "Strips text properties from strings."
  (set-text-properties 0 (length txt) nil txt)
  txt)

(defun trim-link (link)
  (s-replace "[[" ""
             (s-replace "]]" "" link)))

(defun get-links-in-buffer (buffer)
  (loop for x in (s-match-strings-all
                  "\\[\\[.*?\\]\\]"
                  buffer)
        collect (strip-text-properties (first x))))

(defun find-new-links (buffer)
  "Finds all the new links in a given buffer as denoted in org-mode. Doesn't take
into account docstrings/code ect. i.e. [[link]]"
  (delete-dups
   (mapcar
    'trim-link
    (remove-if
     (lambda (x) (s-contains? "][" x))
     (flatten-list    
      (mapcar (lambda (x) (s-split
                           "\\]\\]\\[\\["
                           x))
              (get-links-in-buffer buffer)))))))

(defun create-links-for-org-roam (links)
  (loop for x in links
        do (progn
             (org-roam-find-file-immediate x nil nil t)
             (kill-buffer))))

(defun create-links-for-org-roam-buffer ()
  (interactive)
  (create-links-for-org-roam (find-new-links (buffer-string))))

(defun create-links-for-org-roam-file (file-path)
  (with-temp-buffer
    (insert-file-contents file-path)
    (create-links-for-org-roam-buffer)))

(defun create-links-for-org-roam-folder (dirname)
  (mapc #'create-links-for-org-roam-file (directory-files-recursively dirname "\\.org$" nil)))

;(create-links-for-org-roam-folder "~/org")
