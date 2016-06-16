;;; prelude-org.el --- Emacs Prelude: org-mode configuration.
;;
;; Copyright © 2011-2016 Bozhidar Batsov
;;
;; Author: Bozhidar Batsov <bozhidar@batsov.com>
;; URL: https://github.com/bbatsov/prelude
;; Version: 1.0.0
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; Commentary:

;; Some basic configuration for org-mode.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 3
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Code:

(add-to-list 'auto-mode-alist '("\\.org\\’" . org-mode))
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cb" 'org-iswitchb)
(global-set-key "\C-cc" 'org-capture)
(setq org-log-done t)

(defun prelude-org-mode-defaults ()
  (let ((oldmap (cdr (assoc 'prelude-mode minor-mode-map-alist)))
        (newmap (make-sparse-keymap)))
    (set-keymap-parent newmap oldmap)
    (define-key newmap (kbd "C-c +") nil)
    (define-key newmap (kbd "C-c -") nil)
    (make-local-variable 'minor-mode-overriding-map-alist)
    (push `(prelude-mode . ,newmap) minor-mode-overriding-map-alist))
)

(setq prelude-org-mode-hook 'prelude-org-mode-defaults)

(add-hook 'org-mode-hook (lambda () (run-hooks 'prelude-org-mode-hook)))

(provide 'prelude-org)

;;; prelude-org.el ends here

;; -----------------------------------------------------------------------------
;; ethanfu's config
;; record the closed note
(setq org-log-done 'note)
;; -----------------------------------------------------------------------------
;;set up capture
(setq org-directory "~/gitfu/fuprivate/notes/")
(setq org-default-notes-file (concat org-directory "fnote.org"))
(setq org-capture-templates
      '(("t" "Todo" entry (file+headline "~/Dropbox/gtd/gtdfu.org" "Tasks")
	 "* TODO %?\n  %i\n %a" :clock-in t :clock-resume t :prepend t)
	("i" "Inbox" entry (file+headline (concat org-directory "inbox.org") "Inbox")
	 "* %?\n %i\n %T\n %a")
	("n" "Note" entry (file+headline (concat org-directory "fnote.org") "Note")
	 "* %?\n %i\n %T\n %a")
	("v" "Vocabulary" entry (file+headline (concat org-directory "vocabulary.org") "Vocabulary")
         "* %^{The word} \n:PROPERTIES:\n:Part-of-speech: %^{Part of speech|verb|noun|adj|adv}\n \n %t\n %^{Extended word (may be empty)} \n** Answer \n%^{The definition}")
        ("j" "Journal" entry (file+datetree (concat org-directory "journal.org"))
	 "* %?\nEntered on %U\n  %i\n  %a")))
;; ----------------------------------------------------------------------------
;; load markdown expordown exporter automatically with org-mode
(eval-after-load "org"
  '(require 'ox-md nil t))
;; -----------------------------------------------------------------------------
;; orgmode export to pdf,using xelatex
;; add to org header, add the ctex package, and set the page size
;; #+LATEX_HEADER: \usepackage{ctex}
(setq org-latex-packages-alist
      '("
\\usepackage{ctex}
\\usepackage{graphicx}
\\usepackage[top=2.54cm, bottom=2.54cm, left=3.17cm, right=3.17cm]{geometry}
"))
(setq org-latex-compiler "XeLaTex")
(setq org-latex-pdf-process '("xelatex -interaction nonstopmode %f"
                              "xelatex -interaction nonstopmode %f"))
;; -----------------------------------------------------------------------------
;; babel
(org-babel-do-load-languages
 (quote org-babel-load-languages)
 (quote ((emacs-lisp . t)
         (java . t)
         (dot . t)
         (ditaa . t)
         (R . t)
         (python . t)
         (ruby . t)
         (gnuplot . t)
         (clojure . t)
         (sh . t)
         (ledger . t)
         (org . t)
         (plantuml . t)
         (latex . t)
	 (C .t)
	 )))
;; bebel evaluate don't ask questions
(setq org-confirm-babel-evaluate nil)
;; -----------------------------------------------------------------------------
;; display babel pictures
;; C-c C-c then C-c C-x C-v
(add-hook 'org-babel-after-execute-hook 'bh/display-inline-images 'append)
;; Make babel results blocks lowercase
(setq org-babel-results-keyword "results")

(defun bh/display-inline-images ()
  (condition-case nil
      (org-display-inline-images)
    (error nil)))
;; -----------------------------------------------------------------------------
;; set plantuml path
(setq org-plantuml-jar-path
      (expand-file-name "/usr/local/Cellar/emacs/24.5/share/emacs/24.5/lisp/contrib/scripts/plantuml.jar"))

;; -----------------------------------------------------------------------------
;; plantumlDemo
;; #+begin_src plantuml :file tryout.png :cmdline -charset UTF-8
;;  Alice -> Bob: synchronous call
;;  Alice ->> Bob: asynchronous call
;; #+end_src
;; dita
;; #+BEGIN_SRC ditaa :file ${1:export-file-name} :cmdline -r -s 0.8
;; ${0}
;; #+END_SRC
;; dot
;; #+BEGIN_SRC dot :file ${1:export-file-name}.png :cmdline -Kdot -Tpng
;; title ${0}
;; #+END_SRC
;; uml
;; #+BEGIN_SRC plantuml :file ${1:export-file-name} :cmdline -charset UTF-8
;; title ${0}
;; #+END_SRC
;; -----------------------------------------------------------------------------
;; org默认使用"_下标"来定义一个下标，使用"^上标"定义一个上标，但这种方式在中文环境中与下划线冲突。
;; 这里强制使用"_{下标}"来定义一个下标。"^{上标}"来定义一个上标。
;; (setq org-export-with-sub-superscripts '{})
;; (setq org-use-sub-superscripts '{})
