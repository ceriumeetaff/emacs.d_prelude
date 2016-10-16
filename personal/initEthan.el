;; fu's personal config
(scroll-bar-mode -1)
;; key-binding
(global-set-key (kbd "S-SPC") 'set-mark-command)

;; add the tex path in emacs
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
(setq exec-path (append exec-path '("/Library/TeX/texbin")))
