(require 'package)
(package-initialize)


(setq package-archives
      '(("melpa" . "https://stable.melpa.org/packages/")
	("elpy" . "https://jorgenschaefer.github.io/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")))
(add-to-list 'package-archives '("org" . "https://orgmode.org/elpa/") t)

(electric-indent-mode -1)
(global-set-key (kbd "<M-dead-circumflex>") 'delete-indentation)
(setq sort-fold-case t)
(add-hook 'package-menu-mode-hook 'hl-line-mode)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)


(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq shell-command-switch "-ic")
(setq executable-prefix-env t)
(tool-bar-mode -1)
(show-paren-mode t)
(setq show-paren-delay 0)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-š") 'hippie-expand)
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c q") 'save-buffers-kill-emacs)

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

(use-package company
  :ensure t
  :config
  (setq company-frontends
   '(company-preview-if-just-one-frontend
     company-echo-metadata-frontend
     company-echo-frontend)))


(use-package multi-term
  :ensure t
  :bind (("C-c s" . switch-to-terminal))
  :config
  (defun buffer-term-mode-p (b)
    (equal 'term-mode (buffer-local-value 'major-mode b)))

  (defun switch-to-terminal ()
    (interactive)
    (require 'seq)
    (let ((terminals (mapcar #'buffer-name
			     (seq-filter #'buffer-term-mode-p
					 (buffer-list)))))
      (if terminals
	  (switch-to-buffer
	   (completing-read
	    (format "Terminal name (default %s): " (car terminals))
	    terminals nil t nil nil (car terminals)))
	(multi-term))))
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
(use-package po-mode
  :ensure t
  :mode "\\.po\\'\\|\\.po\\.")

;; direx
(use-package direx
  :ensure t
  :config
  (global-set-key (kbd "C-c d d") 'direx:jump-to-directory-other-window)
  (global-set-key (kbd "C-c d p") 'direx-project:jump-to-project-root))

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
		(string-join (remove hidden-file-regex alternatives) or-rgx)
	      (string-join (cons hidden-file-regex alternatives) or-rgx)))
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
  :defer t
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
  (setq ispell-program-name "/usr/bin/hunspell")
  (setq flyspell-use-meta-tab nil))

;; markdown
;; copied from https://jblevins.org/projects/markdown-mode/
(use-package markdown-mode
  :ensure t
  :commands (markdown-mode gfm-mode)
  :mode (("README\\.md\\'" . gfm-mode)
         ("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; org-mode
(use-package org
  :ensure t
  :defer t
  :config
  (add-hook 'org-mode-hook #'auto-fill-mode)
  (add-hook 'org-mode-hook #'flyspell-mode))

  ;; (global-set-key "\C-cl" 'org-store-link)
  ;; (global-set-key "\C-ca" 'org-agenda)
  ;; (global-set-key "\C-cc" 'org-capture)
  ;; (global-set-key "\C-cb" 'org-iswitchb)
  
  ;; (defun add-slovenian-translations ()
  ;;   (let ((sl-tr '(("Author" (:default "Avtor"))
  ;; 		 ("Date" (:default "Datum"))
  ;; 		 ("Equation" (:ascii "Enacba" :default "Enačba"))
  ;; 		 ("Figure" (:default "Slika"))
  ;; 		 ("Figure %d:" (:default "Slika %d"))
  ;; 		 ("Table" (:default "Tabela"))
  ;; 		 ("Tabel %d" (:default "Tabela %d"))
  ;; 		 ("Table of Contents" (:default "Kazalo vsebine")))))
  ;;     (setq org-export-dictionary
  ;; 	  (mapcar (lambda (l)
  ;; 		    (if (assoc (car l) sl-tr)
  ;; 			(apply #'list
  ;; 			       (car l)
  ;; 			       (cons (cons "sl" (cadr (assoc (car l) sl-tr)))
  ;; 				     (cdr l)))
  ;; 		      l))
  ;; 		  org-export-dictionary))))
  
  ;;(add-hook 'org-mode-hook #'add-slovenian-translations)
  

;; web mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-engine "selmer"))

;; rust
(use-package rust-mode
  :ensure t
  :defer t
  :config
  (use-package racer
    :ensure t
    :config
    (setq racer-cmd "~/.cargo/bin/racer")
    (setq racer-rust-src-path "~/rust/src")
    (add-hook 'rust-mode-hook #'racer-mode))
  (add-hook 'racer-mode-hook #'eldoc-mode)
  (add-hook 'racer-mode-hook #'company-mode))

;; javascript
;; xref-js2 requires the silver searcher (ag) to be installed
(use-package js2-mode
  :mode
  (("\\.js$" . js2-mode)
   ("\\.jsx$" . js2-jsx-mode))
  :bind (:map js2-mode-map
	      ("C-c C-l" . indium-eval-buffer))
  :config
  ;; have 2 space indentation by default
  (setq js-indent-level 2)

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

;; paredit modifications
(use-package paredit
  :ensure t
  :defer t
  :config
  (add-hook 'paredit-mode-hook
	    (lambda ()
	      (define-key paredit-mode-map (kbd "M-s") nil)
	      (define-key paredit-mode-map (kbd "M-š") 'paredit-splice-sexp))))

;; clojure
(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)

  (use-package clj-refactor
    :ensure t
    :config
    (add-hook 'clojure-mode-hook 'clj-refactor-mode))

  (use-package cider
    :ensure t
    :config
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (setq cider-default-repl-command "boot")
    (setq cider-doc-xref-regexp "\\[\\[\\(.*?\\)\\]\\]")

    (defun cider-test-luminus-test-infer-ns (x)
      (let ((parts (split-string x "\\.")))
	(string-join `(,(car parts) "test" ,@(cdr parts)) ".")))

    (setq cider-test-infer-test-ns #'cider-test-luminus-test-infer-ns)))

;;; haskell
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (use-package ghc
    :ensure t)
  (use-package ghc-imported-from
    :ensure t)
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (setq haskell-process-args-ghci
          '("-ferror-spans" "-fshow-loaded-modules"))
  (setq haskell-process-args-cabal-repl
	'("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  (setq haskell-process-args-stack-ghci
	'("--ghci-options=-ferror-spans -fshow-loaded-modules"
	  "--no-build" "--no-load"))
  (setq haskell-process-args-cabal-new-repl
	'("--ghc-options=-ferror-spans -fshow-loaded-modules")))

(use-package ess
  :ensure t
  :defer t)

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
  (add-hook 'LaTeX-mode-hook 'server-start)
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
(use-package python
  :defer t
  :config
  (use-package elpy
    :ensure t
    :config
    (elpy-enable)
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

    (setq elpy-rpc-backend "jedi")))


;; ruby
(use-package ruby-mode
  :defer t
  :config
  (use-package rvm
    :if (executable-find "rvm")
    :ensure t
    :config
    (add-hook 'ruby-mode-hook
	      (lambda () (rvm-activate-corresponding-ruby))))

  (use-package inf-ruby
    :ensure t
    :config
    (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode)))

(use-package haml-mode
  :ensure t
  :mode "\\.haml\\'")

;; slime
(use-package slime
  :ensure t
  :defer t
  :config
  (setq slime-lisp-implementations
	'((sbcl ("sbcl" "--noinform") :coding-system utf-8-unix)))
  (setq slime-default-lisp 'sbcl)
  (require 'slime-autoloads)
  (setq slime-contribs '(slime-fancy))
  (add-hook 'lisp-mode-hook 'paredit-mode))



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-for-comint-mode t)
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(compilation-message-face (quote default))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes (quote (sanityinc-solarized-dark)))
 '(custom-safe-themes
   (quote
    ("4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4" "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328" "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" default)))
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-flymake elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(elpy-shell-capture-last-multiline-output t)
 '(elpy-shell-echo-input nil)
 '(exec-path
   (quote
    ("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin" "/usr/lib/jvm/default/bin" "/usr/bin/site_perl" "/usr/bin/vendor_perl" "/usr/bin/core_perl" "/home/gregor/.rvm/bin" "/usr/lib/emacs/26.1/x86_64-pc-linux-gnu" "/home/gregor/bin")))
 '(fci-rule-color "#073642")
 '(grep-command "grep --color -nH -E ")
 '(grep-find-command
   (quote
    ("find . -type f -exec grep --color -nH -E  {} +" . 42)))
 '(grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -E <R> {} +")
 '(grep-highlight-matches (quote auto))
 '(grep-template "grep <X> <C> -nH -E <R> <F>")
 '(grep-use-null-device nil)
 '(highlight-changes-colors (quote ("#d33682" "#6c71c4")))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#002b36" 0.25)
    (quote
     ("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2"))))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   (quote
    (("#073642" . 0)
     ("#546E00" . 20)
     ("#00736F" . 30)
     ("#00629D" . 50)
     ("#7B6000" . 60)
     ("#8B2C02" . 70)
     ("#93115C" . 85)
     ("#073642" . 100))))
 '(hl-bg-colors
   (quote
    ("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D" "#00736F" "#546E00")))
 '(hl-fg-colors
   (quote
    ("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36")))
 '(magit-diff-use-overlays nil)
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files (quote ("~/Documents/asistenti/code/todos.org")))
 '(org-babel-load-languages
   (quote
    ((emacs-lisp . t)
     (shell . t)
     (awk . t)
     (haskell . t)
     (clojure . t)
     (dot . t)
     (python . t)
     (ruby . t)
     (gnuplot . t)
     (latex . t)
     (calc . t)
     (R . t))))
 '(org-export-backends (quote (ascii html icalendar latex md odt)))
 '(package-selected-packages
   (quote
    (indium color-theme-sanityinc-solarized org po-mode gettext multi-term company-tern use-package js2-refactor xref-js2 ess clj-refactor page-break-lines paredit hippie-expand-slime slime inf-ruby rvm company-go haml-mode docker docker-tramp dockerfile-mode cargo racer rust-mode rust-playground web-mode web-mode-edit-element markdown-mode cider ghc ghc-imported-from haskell-mode merlin iedit auto-complete utop yaml-mode coffee-mode magit direx cdlatex auctex elpy smex)))
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(reftex-use-external-file-finders t)
 '(safe-local-variable-values
   (quote
    ((js2-additional-externs "Meteor" "Tracker" "FlowRouter" "RocketChat" "$" "Session" "Random" "Template")
     (js2-additional-externs "Meteor" "Tracker" "FlowRouter" "RocketChat" "$")
     (cider-cljs-lein-repl . "(do (require 'figwheel-sidecar.repl-api)
         (figwheel-sidecar.repl-api/start-figwheel!)
         (figwheel-sidecar.repl-api/cljs-repl))")
     (cider-cljs-lein-repl . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (web-mode-engine . selmer)
     (web-mode-engine . django)
     (web-mode-engine . "django")
     (engine . "selmer")
     (web-mode-engine . "selmer")
     (eval
      (lambda nil
	(when
	    (string=
	     (file-name-extension buffer-file-name)
	     "html")
	  (web-mode)))))))
 '(send-mail-function (quote smtpmail-send-it))
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#dc322f")
     (40 . "#cb4b16")
     (60 . "#b58900")
     (80 . "#859900")
     (100 . "#2aa198")
     (120 . "#268bd2")
     (140 . "#d33682")
     (160 . "#6c71c4")
     (180 . "#dc322f")
     (200 . "#cb4b16")
     (220 . "#b58900")
     (240 . "#859900")
     (260 . "#2aa198")
     (280 . "#268bd2")
     (300 . "#d33682")
     (320 . "#6c71c4")
     (340 . "#dc322f")
     (360 . "#cb4b16"))))
 '(vc-annotate-very-old-color nil)
 '(web-mode-auto-close-style 2)
 '(web-mode-enable-block-face t)
 '(web-mode-enable-engine-detection t)
 '(weechat-color-list
   (quote
    (unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00" "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2" "#93115C" "#d33682" "#00736F" "#2aa198" "#839496" "#657b83"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
;; (require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line

(put 'scroll-left 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
