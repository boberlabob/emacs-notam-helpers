;; Major Mode to highlight AFTN Files with NOTAMS
(setq my-highlights
      '(("[A-Z][0-9]\\{4\\}/[0-9]\\{2\\}" . font-lock-function-name-face) ;; Notam Nr
        ("[A-Z][0-9]\\{4\\}/[[:space:]]" . font-lock-warning-face) ;; Notam Nr with missing year
        ("NOTAM\\(N\\|C\\|R\\)" . font-lock-keyword-face) ;; Notam type
        ("C\)\s?\\(PERM\\|UFN\\)" . font-lock-constant-face) ;; ItemC extension
        ("[0-9]\\{10\\}\s?\\(EST\\)" 1  font-lock-constant-face) ;; ItemC extension
        ("---<.*>---" . font-lock-comment-face) ;; Comments
        ("\\(B\\|C\\)\)\s?\\([0-9]\\{6\\}\\)\\([0-9]\\{4\\}\\)" 2 font-lock-builtin-face) ;;Date in item B and C
        ("\\(B\\|C\\)\)\s?\\([0-9]\\{6\\}\\)\\([0-9]\\{4\\}\\)" 3 font-lock-string-face) ;;Time in item B and C
        ("[0-9]\\{4\\}\\(N\\|S\\)" . font-lock-builtin-face) ;; Long
        ("[0-9]\\{5\\}\\(E\\|W\\)" . font-lock-string-face) ;; Lat
        ("[0-9]\\{6\\})" . font-lock-warning-face) ;;
        ("^A\)\\|^B\)\\|C\)\\|^D\)\\|^E\)\\|^Q\)" . font-lock-constant-face)))

(define-derived-mode notam-mode fundamental-mode
  (setq font-lock-defaults '(my-highlights))
  (setq mode-name "NOTAM File"))

(add-to-list 'auto-mode-alist '("\\.aftn\\'" . notam-mode))
