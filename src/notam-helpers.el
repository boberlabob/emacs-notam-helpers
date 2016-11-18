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
    (time-add (current-time) (seconds-to-time (* 60 60 24 no-of-days))
              ))))

(defun replace-all-item-B (new-item-B)
  (perform-replace "B)[[:digit:]]\\{10\\}" (concat "B)" new-item-B) nil t nil))

(defun replace-all-item-C (new-item-C)
  (perform-replace "C)[[:digit:]]\\{10\\}" (concat "C)" new-item-C) nil t nil))

(defun set-all-item-B-to-now-plus-days (days)
  (replace-all-item-B (now-as-aftn days)))

(defun set-all-item-C-to-now-plus-days (days)
  (replace-all-item-C (now-as-aftn days)))

(defun update-all-item-B-and-C-to-default ()
  (beginning-of-buffer)
  (set-all-item-B-to-now-plus-days 0)
  (beginning-of-buffer)
  (set-all-item-C-to-now-plus-days 7))

(defun update-all-item-B-and-C (daysB daysC)
  (beginning-of-buffer)
  (set-all-item-B-to-now-plus-days daysB)
  (beginning-of-buffer)
  (set-all-item-C-to-now-plus-days daysC))

;; Re-Number Notams
(defun reset-notam-numbers (offset step from-start)
  (if
      from-start
      (beginning-of-buffer))
  (let ((inc-variable offset))
    (while (re-search-forward "\\(([A-Z]\\)\\([[:digit:]]\\{4\\}\\)\\(/[[:digit:]]\\{2\\}\\)" nil t)
      (replace-match (format "\\1%04d\\3" inc-variable))
      (setq inc-variable (+ inc-variable step)))))

(defun reset-notam-numbers-from-start ()
  (beginning-of-buffer)
  (reset-notam-numbers-from-here 1 1 t))

(defun insert-notam-template (notam-nr qfir coord loci)
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

(defun insert-notam-lszh (notam-nr)
  (insert-notam-template notam-nr "LSAS" "TBD" "LSZH"))


(defun insert-notam()
  (insert-notam-template
   (read-string "NOTAM-No:")
   (read-string "QFIR:")
   (read-string "Coordinates:")
   (read-string "LOCI")))

;; Playground
