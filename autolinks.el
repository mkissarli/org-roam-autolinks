[[test]][[propaganda]][[chomsky]]

(defun strip-text-properties(txt)
  "Strips text properties from strings."
  (set-text-properties 0 (length txt) nil txt)
  txt)
strip-text-properties

(defun trim-link (link)
  (s-replace "[[" ""
             (s-replace "]]" "" link)))

(defun get-links-in-buffer (buffer)
  (loop for x in (s-match-strings-all
                  "\\[\\[.*]]"
                  buffer)
        collect (strip-text-properties (first x))))

(defun find-new-links (buffer)
  "Finds all the new links in a given buffer as denoted in org-mode. Doesn't take
into account docstrings/code ect. i.e. [[link]]"
  (delete-dups
   (mapcar
    'trim-link
    (flatten-list
     (mapcar (lambda (x) (s-split
                          "\\]\\]\\[\\["
                          x))
             (get-links-in-buffer buffer))))))

(find-new-links (buffer-string))
("test" "propaganda" "chomsky" "link")


(defun create-links-for-org-roam (links)
  (loop for x in links
        do (progn
             (org-roam-find-file-immediate x nil nil t)
             (kill-buffer))))

(defun create-links-for-org-roam-buffer ()
  (create-links-for-org-roam (find-new-links (buffer-string))))

(create-links-for-org-roam-buffer)
