(require 'package)
(package-initialize)


(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("elpy" . "https://jorgenschaefer.github.io/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(global-set-key (kbd "C-c <up>") 'delete-indentation)
(setq sort-fold-case t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)

(fset 'copy-buffer-contents-return-point
   (lambda (&optional arg)
     "Keyboard macro."
     (interactive "p")
     (kmacro-exec-ring-item
      (quote ([24 104 134217847 21 67108896 21 67108896] 0 "%d")) arg)))


(setq inhibit-startup-screen t
      ring-bell-function 'ignore
      shell-command-switch "-ic"
      desktop-files-not-to-save "^$"
      executable-prefix-env t
      show-paren-delay 0)
(tool-bar-mode -1)
(show-paren-mode t)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-c š") 'hippie-expand)
;; (fset 'yes-or-no-p 'y-or-n-p)

(set-file-name-coding-system 'utf-8)
(xterm-mouse-mode)

(windmove-default-keybindings)
;; remove more csi escapes from the output to make ipython console
;; legible and to make it possible to use unix terminal with colors
(require 'ansi-color)
(setq
 ansi-color-drop-regexp
 "\\|\\(\\[\\(\\(\\([0-9]+\\)?[ABCDsuK]\\)\\|[012]?[JK]\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\|\\?[0-9]+[hl]\\|6n\\)\\)")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

;; utf-8 aliases
(define-coding-system-alias 'utf8 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'UTF8 'utf-8)

(unless (package-installed-p 'use-package)
  (package-install 'use-package))

(use-package company
  :ensure t
  :config
  (setq company-frontends
   '(company-preview-if-just-one-frontend
     company-echo-metadata-frontend
     company-echo-frontend)))

(use-package paradox
  :ensure t
  :config
  (paradox-enable))

(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))


(use-package multi-term
  :ensure t
  :bind (("C-c s" . switch-to-terminal))
  :config
  (defun buffer-term-mode-p (b)
    (equal 'term-mode (buffer-local-value 'major-mode b)))

  (defun switch-to-terminal (new)
    (interactive "P")
    (require 'seq)
    (let ((terminals (mapcar #'buffer-name
			     (seq-filter #'buffer-term-mode-p
					 (buffer-list)))))
      (if (or (not terminals) new)
	  (multi-term)
	(switch-to-buffer
	   (completing-read
	    (format "Terminal name (default %s): " (car terminals))
	    terminals nil t nil nil (car terminals))))))
  (setq
   multi-term-program "/usr/bin/zsh"
   multi-term-dedicated-select-after-open-p t))

(use-package yasnippet
  :ensure t
  :config
  (add-to-list
   'yas-snippet-dirs
   (concat (file-name-directory
	    (file-truename
	     (or load-file-name buffer-file-name)))
	   "snippets")))

;; gettext
;; (use-package po-mode
;;   :ensure t
;;   :mode "\\.po\\'\\|\\.po\\.")

;; direx
(use-package direx
  :ensure t
  :config
  (global-set-key (kbd "C-c d d") 'direx:jump-to-directory-other-window)
  (global-set-key (kbd "C-c d p") 'direx-project:jump-to-project-root))

;; projectile
(use-package projectile
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-mode +1))

;; dired
(use-package dired
  :defer t
  :bind (("C-c ." . toggle-hide-hidden-files))
  :config
  (setq dired-listing-switches "-alh"
	dired-recursive-copies 'always
	dired-recursive-deletes 'always
	wdired-allow-to-change-permissions t
	dired-dwim-target t)

  (defun toggle-hide-hidden-files ()
    (interactive)
    (let ((alternatives (split-string dired-omit-files "\\\\|"))
	  (hidden-file-regex "^\\..+$")
	  (or-rgx "\\|"))
      (setq dired-omit-files
	    (if (member hidden-file-regex alternatives)
		(mapconcat #'identity (remove hidden-file-regex alternatives) or-rgx)
	      (mapconcat #'identity (cons hidden-file-regex alternatives) or-rgx)))
      (dired-omit-mode -1)
      (dired-omit-mode 1)))

  (add-hook 'dired-mode-hook #'dired-omit-mode)
  (require 'dired-x))

;; magit
(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-c g") 'magit-status))

(defun path-from-home (path)
  (concat (getenv "HOME") path))

;; spell checking
(use-package flyspell
  :config
  (setenv
   "DICPATH"
   (string-join
    `(,(path-from-home
  	"/.config/libreoffice/4/user/uno_packages/cache/uno_packages/lu3045rtignw.tmp_/dict-en.oxt")
      ,(path-from-home
  	"/.config/libreoffice/4/user/uno_packages/cache/uno_packages/lu3045rtignt.tmp_/pack-sl.oxt")
      "/usr/share/hunspell")
    ":"))
  (setq ispell-program-name "/usr/bin/hunspell"
	flyspell-use-meta-tab nil))

;; markdown
;; copied from https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :defer t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;;; yaml
(use-package yaml-mode
  :ensure t
  :defer t)

;; web mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-engine "selmer"))

;; javascript
;; xref-js2 requires the silver searcher (ag) to be installed
(use-package js2-mode
  :ensure t
  :defer t
  :mode
  (("\\.js$" . js2-mode)
   ("\\.jsx$" . js2-jsx-mode))
  :bind (:map js2-mode-map
	      ("C-c C-l" . indium-eval-buffer))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2)
  (setq-default js2-strict-trailing-comma-warning nil)
  (use-package tern
    :ensure t
    :if (executable-find "tern")
    :config
    (defun my-js-mode-hook ()
      "Hook for `js-mode'."
      (set (make-local-variable 'company-backends)
           '((company-tern company-files company-yasnippet))))
    (add-hook 'js2-mode-hook 'my-js-mode-hook)
    (setq js2-include-node-externs t)
    (add-hook 'js2-mode-hook 'tern-mode)

    (use-package company-tern
      :ensure t
      ;:if (executable-find "tern")
      :config
      ;; Disable completion keybindings, as we use xref-js2 instead
      (define-key tern-mode-keymap (kbd "M-.") nil)
      (define-key tern-mode-keymap (kbd "M-,") nil)
      (add-hook 'js2-mode-hook 'company-mode)))

  (use-package js2-refactor
    :ensure t
    ;:diminish js2-refactor-mode "𝐉𝐫"
    :bind
    (:map js2-mode-map
          ("C-k" . js2r-kill)
          ("C-c h r" . js2-refactor-hydra/body))
    :config
    (js2r-add-keybindings-with-prefix "C-c C-r"))
  (add-hook 'js2-mode-hook 'js2-refactor-mode)

  (use-package xref-js2
    :ensure t
    :config

    ;; js-mode (which js2 is based on) binds "M-." which conflicts with xref, so
    ;; unbind it.
    (define-key js-mode-map (kbd "M-.") nil)

    (add-hook 'js2-mode-hook
	      (lambda ()
		(add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t))))

  (use-package indium
    :ensure t
    :config (add-hook 'js2-mode-hook 'indium-interaction-mode)))

;; coffee script
(use-package coffee-mode
  :ensure t
  :defer t
  :config
  (setq coffee-tab-width 2)
  (setq coffee-indent-tabs-mode nil)
  (setq coffee-indent-like-python-mode t))

;; latex
(use-package tex-mode
  :mode "\\.tex\\'"
  :ensure auctex
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq-default TeX-master t)
  (setq LaTeX-math-abbrev-prefix "đ")
  (add-hook 'LaTeX-mode-hook 'auto-fill-mode)
  (add-hook 'LaTeX-mode-hook 'flyspell-mode)
  (add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
  (setq TeX-electric-sub-and-superscript t)
  (add-hook 'LaTeX-mode-hook (lambda () (TeX-fold-mode 1)))
  (add-hook 'LaTeX-mode-hook 'outline-minor-mode)
  (add-hook 'LaTeX-mode-hook 'TeX-source-correlate-mode)
  (setq TeX-source-correlate-start-server t)
  (setq TeX-PDF-mode t)
  (setq TeX-view-program-selection
	'(((output-dvi has-no-display-manager)
	   "dvi2tty")
	  ((output-dvi style-pstricks)
	   "dvips and gv")
	  (output-dvi "xdvi")
	  (output-pdf "Zathura")
	  (output-html "xdg-open")))

  (use-package reftex
    :config
    (add-hook 'LaTeX-mode-hook #'turn-on-reftex)
    (setq reftex-plug-into-AUCTeX t)))
;;(add-to-list 'auto-mode-alist '("\\.tex\\'" . latex-mode))

;; smex and ido
(ido-mode t)
(ido-everywhere t)
(use-package smex
  :ensure t
  :config
  (smex-initialize)
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands))


;; python
(use-package elpy
  :defer t
  :ensure t
  :init
  (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; (add-hook 'python-mode-hook #'elpy-use-ipython t)

  (defun pipenv-enable ()
    "Activate the virtual environment returned by `pipenv --venv'."
    (interactive)
    (let* ((cmd-output (split-string
			(shell-command-to-string "pipenv --venv")))
	   (message (if (< 1 (length cmd-output))
			(string-join (butlast cmd-output) ?\n)
		      nil))
	   (venv (car (last cmd-output))))
      (if message
	  (princ message)
	(pyvenv-activate venv))))

  (add-to-list
   'elpy-project-root-finder-functions
   (lambda () (locate-dominating-file default-directory "Pipfile")))

  (setq elpy-rpc-backend "jedi"
	elpy-rpc-virtualenv-path 'current))

;;; inflection
(use-package string-inflection
  :ensure t
  :bind ("C-c i" . 'string-inflection-all-cycle))
