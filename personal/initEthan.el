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
            (local-set-key (kbd "s-2") 'artist-select-op-pen-line) ;;command-2=pen mode
            (local-set-key (kbd "s-3") 'artist-select-op-line) ;;command-3=line
            (local-set-key (kbd "s-4") 'artist-select-op-square) ;;command-4=rectangle
            (local-set-key (kbd "s-5") 'artist-select-op-ellipse)  ;;command-5=ellipse
            (local-set-key (kbd "s-6") 'artist-select-op-text-see-thru) ;;command-6=text
           ))


;;yasnippets
(add-to-list 'load-path
             "~/.emacs.d/elpa/yasnippet-0.13.0")

(require 'yasnippet)
(yas-global-mode 1)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

;;htmlize
(add-to-list 'load-path
             "~/.emacs.d/modules/htmlize.el")
(require 'htmlize)
