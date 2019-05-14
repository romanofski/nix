(use-package company-auctex)

(use-package latex
    :mode
    ("\\.tex\\'" . latex-mode)
    :bind
    (:map LaTeX-mode-map
          ("M-<delete>" . TeX-remove-macro)
          ("C-c C-r" . reftex-query-replace-document)
          ("C-c C-g" . reftex-grep-document))
    :config
    (setq TeX-auto-save t
          TeX-save-query nil       ; don't prompt for saving the .tex file
          TeX-parse-self t
          TeX-show-compilation nil         ; if `t`, automatically shows compilation log
          LaTeX-babel-hyphen nil ; Disable language-specific hyphen insertion.
          ;; `"` expands into csquotes macros (for this to work, babel pkg must be loaded after csquotes pkg).
          LaTeX-csquotes-close-quote "}"
          LaTeX-csquotes-open-quote "\\enquote{"
          TeX-file-extensions '("Rnw" "rnw" "Snw" "snw" "tex" "sty" "cls" "ltx" "texi" "texinfo" "dtx"))

    ;; Add standard Sweave file extensions to the list of files recognized  by AuCTeX.
    (add-hook 'TeX-mode-hook (lambda () (reftex-isearch-minor-mode)))
    )
