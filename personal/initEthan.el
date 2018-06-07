;; fu's personal config
(scroll-bar-mode -1)
;; key-binding
(global-set-key (kbd "S-SPC") 'set-mark-command)
;; startup with fullcreen
(add-hook 'emacs-startup-hook 'toggle-frame-fullscreen)

;; add the tex path in emacs
(setenv "PATH" "/usr/local/bin:/Library/TeX/texbin/:$PATH" t)
(setq exec-path (append exec-path '("/Library/TeX/texbin")))

;; artist mode key map

(add-hook 'artist-mode-hook
          (lambda()
            (local-set-key (kbd "s-1") 'org-mode)
            (local-set-key (kbd "s-2") 'artist-select-op-pen-line) ;;f2=pen mode
            (local-set-key (kbd "s-3") 'artist-select-op-line) ;;f3=line
            (local-set-key (kbd "s-4") 'artist-select-op-square) ;;f4=rectangle
            (local-set-key (kbd "s-5") 'artist-select-op-ellipse)  ;;f5=ellipse
           ))
