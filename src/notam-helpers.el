(load-file "./notam-definitions.el")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions to work with NOTAM Files
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Item B and C replacement
(defun now-as-aftn (no-of-days)
  "Return today plus no-of-days as aftn DTG"
  (interactive)
  (concat
   (format-time-string
    "%y%m%d%H%M"
    (time-add (current-time) (seconds-to-time (* 60 60 24 no-of-days))))))

(defun date-as-aftn (no-of-days)
  "Return today plus no-of-days as aftn DTG"
  (interactive)
  (concat
   (format-time-string
    "%y%m%d"
    (time-add (current-time) (seconds-to-time (* 60 60 24 no-of-days))))))


(defun replace-all-item-B (new-item-B)
  (perform-replace "B)[[:digit:]]\\{10\\}" (concat "B)" new-item-B "0800") nil t nil))

(defun replace-all-item-C (new-item-C)
  (perform-replace "C)[[:digit:]]\\{10\\}" (concat "C)" new-item-C "1800") nil t nil))

(defun notam-set-all-item-B-to-now-plus-days (days)
  (replace-all-item-B (date-as-aftn days)))

(defun notam-set-all-item-C-to-now-plus-days (days)
  (replace-all-item-C (date-as-aftn days)))

(defun notam-update-all-item-B-and-C (daysB daysC)
  (beginning-of-buffer)
  (notam-set-all-item-B-to-now-plus-days daysB)
  (beginning-of-buffer)
  (notam-set-all-item-C-to-now-plus-days daysC))

(defun notam-update-all-item-B-and-C-to-default ()
  (notam-update-all-item-B-and-C 0 7))


;; Empty all comments
(defun notam-reset-comments (offset step from-start)
  (if
      from-start
      (beginning-of-buffer))
  (let ((inc-variable offset))
    (while (re-search-forward "\\(--<\\)\\(.*\\)\\(>--\\)" nil t)
      (replace-match (format "\\1 No. %d \\3" inc-variable))
      (setq inc-variable (+ inc-variable step)))))


;; Re-Number Notams
(defun notam-reset-numbers (offset step from-start)
  (if
      from-start
      (beginning-of-buffer))
  (let ((inc-variable offset))
    (while (re-search-forward "\\(([A-Z]\\)\\([[:digit:]]\\{4\\}\\)\\(/[[:digit:]]\\{2\\}\\)" nil t)
      (replace-match (format "\\1%04d\\3" inc-variable))
      (setq inc-variable (+ inc-variable step)))))

(defun notam-reset-all-numbers ()
  (beginning-of-buffer)
  (notam-reset-numbers 1 1 t))

;; Q-Line setzen


;; Templates
(defun notam-insert-template (notam-nr qfir coord loci)
  (interactive)
  (insert "---<   >---\n")
  (insert "VAA0026 171526\n")
  (insert "GG LSZZNAFC\n")
  (insert "171526 EUECYNYX\n")
  (insert (concat "(" notam-nr " NOTAMN\n"))
  (insert (concat "Q)" qfir "/QKKCC/IV/NBO/A///" coord "\n"))
  (insert (concat "A)" loci "\n"))
  (insert (concat "B)" (now-as-aftn 0) "\n"))
  (insert (concat "C)" (now-as-aftn 7) "\n"))
  (insert "D)\n")
  (insert "E)\n")
  )

(defun notam-insert-template-lszh ()
  (insert-notam-template (read-string "NOTAM-No.: ") "LSAS" "TBD" "LSZH"))


(defun notam-insert()
  (notam-insert-template
   (read-string "NOTAM-No: ")
   (read-string "QFIR: ")
   (read-string "Coordinates: ")
   (read-string "LOCI: ")))

;; Notam overview
(defun copy-lines-matching-re (re)
  "find all lines matching the regexp RE in the current buffer
putting the matching lines in a buffer named *matching*"
  (interactive "sRegexp to match: ")
  (let ((result-buffer (get-buffer-create "*matching*")))
    (with-current-buffer result-buffer
      (erase-buffer))
    (save-match-data
      (save-excursion
        (goto-char (point-min))
        (while (re-search-forward re nil t)
          (princ (buffer-substring-no-properties (line-beginning-position)
                                                 (line-beginning-position 2))
                 result-buffer))))
    (pop-to-buffer result-buffer)))

(defun notam-overview ()
  (copy-lines-matching-re "\\(([A-Z]\\)\\([[:digit:]]\\{4\\}\\)\\(/[[:digit:]]\\{2\\}\\)")
  (beginning-of-buffer)
  (perform-replace "\^B(" "" nil t nil)
  (beginning-of-buffer)
  (notam-overview-mode))


;; Playground
