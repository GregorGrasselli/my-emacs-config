(require 'package)
(package-initialize)


(setq package-archives
      '(("melpa-stable" . "https://stable.melpa.org/packages/")
	("melpa" . "https://melpa.org/packages/")
	("elpy" . "https://jorgenschaefer.github.io/packages/")
	("gnu" . "http://elpa.gnu.org/packages/")
        ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

(electric-indent-mode -1)
(global-set-key (kbd "<M-dead-circumflex>") 'delete-indentation)
(setq sort-fold-case t)
(put 'narrow-to-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
(global-unset-key (kbd "C-z"))

(defun copy-buffer-file-name ()
  (interactive)
  (kill-new (file-name-nondirectory (buffer-file-name))))

(setq inhibit-startup-screen t)
(setq ring-bell-function 'ignore)
(setq shell-command-switch "-ic")
(setq desktop-files-not-to-save "^$")
(setq executable-prefix-env t)
(tool-bar-mode -1)
(scroll-bar-mode -1)
(show-paren-mode t)
(setq show-paren-delay 0)
(global-set-key (kbd "C-c h") 'hl-line-mode)
(global-set-key (kbd "C-š") 'hippie-expand)
(global-set-key (kbd "C-c o") 'other-frame)
(global-set-key (kbd "C-c q") 'save-buffers-kill-emacs)
;; (fset 'yes-or-no-p 'y-or-n-p)

(set-file-name-coding-system 'utf-8)


(windmove-default-keybindings)
;; remove more csi escapes from the output to make ipython console
;; legible and to make it possible to use unix terminal with colors
(require 'ansi-color)
(setq
 ansi-color-drop-regexp
 "\\|\\(\\[\\(\\(\\([0-9]+\\)?[ABCDsuK]\\)\\|[012]?[JK]\\|=[0-9]+[hI]\\|[0-9;]*[Hf]\\|\\?[0-9]+[hl]\\|6n\\)\\)")

(add-to-list 'load-path "/usr/share/emacs/site-lisp/")

(setq-default indent-tabs-mode nil)

;; utf-8 aliases
(define-coding-system-alias 'utf8 'utf-8)
(define-coding-system-alias 'UTF-8 'utf-8)
(define-coding-system-alias 'UTF8 'utf-8)

;; completions
(setq completions-max-height 20)
(setq completion-auto-select 'second-tab)

;;; calendar
(setq calendar-week-start-day 1
      holiday-general-holidays nil
      holiday-local-holidays
      '((holiday-fixed 1 1 "New Year's Day")
	(holiday-fixed 1 2 "New Year holiday")
	(holiday-fixed 2 8 "Prešeren Day")
	(holiday-fixed 4 27 "Day of Uprising Against Occupation")
	(holiday-fixed 5 1 "Labor Day / May Day")
	(holiday-fixed 5 2 "Labour Day holiday")
	(holiday-fixed 6 25 "Statehood Day")
	(holiday-fixed 8 15 "Assumption of Mary")
	(holiday-fixed 10 31 "Reformation Day")
	(holiday-fixed 11 1 "Remembrance Day")
	(holiday-fixed 12 25 "Christmas Day")
	(holiday-fixed 12 26 "Independence and Unity Day"))
      holiday-bahai-holidays nil
      holiday-hebrew-holidays nil
      holiday-islamic-holidays nil
      holiday-oriental-holidays nil
      calendar-view-holidays-initially-flag nil)

(use-package elfeed
  :ensure t
  :defer t
  :bind ("C-x w" . elfeed))

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

(use-package move-text
  :ensure t
  :config
  (move-text-default-bindings))

(use-package eat :ensure t)

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

(use-package dired
  :config
  (setq dired-listing-switches
        "-l --almost-all --human-readable --no-group"
	dired-recursive-copies 'always
	dired-recursive-deletes 'always
	wdired-allow-to-change-permissions t)
  ;; this command is useful when you want to close the window of `dirvish-side'
  ;; automatically when opening a file
  (put 'dired-find-alternate-file 'disabled nil))

(use-package dirvish
  :ensure t
  :init
  (dirvish-override-dired-mode)
  :custom
  (dirvish-quick-access-entries ; It's a custom option, `setq' won't work
   '(("h" "~/"                          "Home")
     ("d" "~/Downloads/"                "Downloads")
     ("m" "/mnt/"                       "Drives")
     ("e" "/sudo:root@localhost:/etc")  "Modify program settings"))
  :config
  ;; (dirvish-peek-mode)             ; Preview files in minibuffer
  ;; (dirvish-side-follow-mode)      ; similar to `treemacs-follow-mode'
  (setq dirvish-mode-line-format
        '(:left (sort symlink) :right (omit yank index)))
  (setq dirvish-attributes           ; The order *MATTERS* for some attributes
        '(vc-state subtree-state nerd-icons collapse git-msg file-time file-size)
        dirvish-side-attributes
        '(vc-state nerd-icons collapse file-size))
  ;; open large directory (over 20000 files) asynchronously with `fd' command
  (setq dirvish-large-directory-threshold 20000)
  :bind ; Bind `dirvish-fd|dirvish-side|dirvish-dwim' as you see fit
  (("C-c f" . dirvish)
   :map dirvish-mode-map               ; Dirvish inherits `dired-mode-map'
   (";"   . dired-up-directory)        ; So you can adjust `dired' bindings here
   ("?"   . dirvish-dispatch)          ; [?] a helpful cheatsheet
   ("a"   . dirvish-setup-menu)        ; [a]ttributes settings:`t' toggles mtime, `f' toggles fullframe, etc.
   ("f"   . dirvish-file-info-menu)    ; [f]ile info
   ("o"   . dirvish-quick-access)      ; [o]pen `dirvish-quick-access-entries'
   ("s"   . dirvish-quicksort)         ; [s]ort flie list
   ("r"   . dirvish-history-jump)      ; [r]ecent visited
   ("l"   . dirvish-ls-switches-menu)  ; [l]s command flags
   ("v"   . dirvish-vc-menu)           ; [v]ersion control commands
   ("*"   . dirvish-mark-menu)
   ("y"   . dirvish-yank-menu)
   ("N"   . dirvish-narrow)
   ("^"   . dirvish-history-last)
   ("TAB" . dirvish-subtree-toggle)
   ("M-f" . dirvish-history-go-forward)
   ("M-b" . dirvish-history-go-backward)
   ("M-e" . dirvish-emerge-menu)))

;; projectile
(use-package projectile
  :after eat
  :ensure t
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (projectile-register-project-type 'python-poetry '("pyproject.toml")
                                    :project-file "pyproject.toml")
  (defun copy-buffer-file-project-name ()
    (interactive)
    (kill-new (file-relative-name (buffer-file-name) (projectile-project-root))))

  (defun insert-file-name-and-line-number ()
    "Insert the project file path and line number at point as `path/to/file:<line-num>'."
    (interactive)
    (let ((file-path (file-relative-name (buffer-file-name) (projectile-project-root)))
          (line-number (number-to-string (line-number-at-pos))))
      (insert (concat file-path ":" line-number))))

  (defun my-projectile-project-find-function (dir)
    (let ((root (projectile-project-root dir)))
      (and root (cons 'transient root))))
  (projectile-mode t)
  (with-eval-after-load 'project
    (add-to-list 'project-find-functions 'my-projectile-project-find-function))

  (defun run-project-terminals ()
    "Open project terminals defined in `project-terminals` dir-local, cd and run cmds."
    (interactive)
    (let ((terms (or (and (boundp 'project-terminals) project-terminals)
                     (user-error "No project-terminals variable defined"))))
      (dolist (term terms)
        (let* ((name (car term))
               (props (cdr term))
               (dir (expand-file-name (plist-get props :dir) (projectile-project-root)))
               (cmd (plist-get props :cmd))
               (buf-name (format "*%s*" name)))
          (let ((buf (eat-term buf-name)))
            (with-current-buffer buf
              (eat-send-string buf (format "cd %s\n" dir))
              (when cmd
                (eat-send-string buf (concat cmd "\n")))))))))

  (defun projectile-run-project-or-terminals ()
    "Run project terminals if declared, otherwise fallback to `projectile-run-project`."
    (interactive)
    (if (and (boundp 'project-terminals) project-terminals)
        (run-project-terminals)
      (projectile-run-project))))

;; dired
(use-package dired
  :defer t
  :bind (("C-c ." . toggle-hide-hidden-files))
  :config
  

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

(use-package flycheck
  :ensure t
  :defer t
  :config
  (defun flycheck-parse-pylama (output checker buffer)
    (mapcar (lambda (err)
              (let-alist err
                ;; Pylint can return -1 as a line or a column, hence the call to
                ;; `max'.  See `https://github.com/flycheck/flycheck/issues/1383'.
                (flycheck-error-new-at
                 (and .lnum (max .lnum 1))
                 (and .col (max (1+ .col) 1))
                 (pcase .etype
                   ;; See "pylint/utils.py"
                   ((or "E" "F") 'error)
                   ((or "I" "C") 'info)
                   ((or "W" "R" _) 'warning))
                 ;; Drop lines showing the error in context
                 (and (string-match (rx (*? nonl) eol) .message)
                      (match-string 0 .message))
                 :checker checker
                 :id .number
                 :buffer buffer
                 :group .source
                 :filename .filename)))
            (car (flycheck-parse-json output))))
  (flycheck-define-checker python-pylama
    "Pylama python syntax checker."
    :command ("pylama" "--format" "json" source-inplace)
    :error-parser flycheck-parse-pylama
    :working-directory flycheck-python-find-project-root
    :modes python-mode)
  (add-to-list 'flycheck-checkers 'python-pylama))

;;; copilot
;; (require 'quelpa-use-package)
;; (use-package copilot
;;   :quelpa (copilot :fetcher github
;;                    :repo "zerolfx/copilot.el"
;;                    :branch "main"
;;                    :files ("dist" "*.el"))
;;   :bind (:map copilot-mode-map
;;               ("<f6>" . 'copilot-accept-completion)))
;; Docker
(use-package dockerfile-mode
  :ensure t
  :defer t)

(use-package nix-mode
  :ensure t)

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

;;; jenkinsfile
(use-package groovy-mode
  :ensure t
  :defer t)

;; org-mode
(use-package org
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
  ;; 		 ("Table %d" (:default "Tabela %d"))
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


(use-package gnuplot
  :defer t
  :ensure t)

(use-package ob-mermaid
  :defer t
  :ensure t
  :config
  (setq ob-mermaid-cli-path "/usr/bin/mmdc")
  )

;; web mode
(use-package web-mode
  :ensure t
  :mode "\\.html?\\'"
  :config
  (setq web-mode-engine "selmer"
	indent-tabs-mode nil))

;; elm
(use-package elm-mode
  :ensure t
  :hook ((elm-mode . elm-format-on-save-mode)
         (elm-mode . elm-indent-mode)))


(use-package boogie-friends
  :ensure t
  :config
  (setq flycheck-dafny-executable "dafny"))

;; Returns the parent directory containing a .project.el file, if any,
;; to override the standard project.el detection logic when needed.

;;; elixir
(use-package inf-elixir
  :ensure t
  :defer t)

(use-package elixir-ts-mode
  :ensure t
  :defer t
  :requires project
  :hook
  (elixir-ts-mode . eglot-ensure)
  (elixir-ts-mode . yas-minor-mode))

(defun asdf-enable ()
  "Setup asdf for environment."
  (interactive)
  (let ((shims-path (substitute-env-vars "$HOME/.asdf/shims"))
        (bin-path (directory-file-name (file-name-directory (substitute-env-vars "$HOME/.asdf/bin/asdf")))))
    (setenv "PATH" (concat shims-path ":" bin-path ":" (getenv "PATH")))
    (setq exec-path (nconc (list shims-path bin-path) exec-path))))

(asdf-enable)

;; golang

(use-package go-mode
  :ensure t
  :defer t
  :config
  (add-hook 'go-mode-hook 'eglot-ensure)
  (add-hook 'go-mode-hook (lambda () (setq tab-width 4))))

(use-package go-playground :ensure t)


;; rust
(use-package rustic
  :ensure t
  :defer t
  :hook
  (rustic-mode . (lambda () (yas-minor-mode 1)))
  :config
  (setq rustic-lsp-client 'eglot)
  (setq rustic-format-on-save t))

;; json
(use-package json-mode
  :ensure t
  :defer t
  :config
  (setq js-indent-level 2))

;; typescript
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(use-package tide
  :ensure t
  :hook ((typescript-ts-mode . tide-setup)
         (typescript-ts-mode . company-mode)
         (typescript-ts-mode . flycheck-mode))
  ;; :config
  ;; (setq tide-tsserver-process-environment '("TSS_LOG=-level verbose -file /home/gregor/tss.log"))
  )

;; (add-to-list 'eglot-server-programs '((js-mode typescript-mode) . (eglot-deno "deno" "lsp")))

;; (defclass eglot-deno (eglot-lsp-server) ()
;;   :documentation "A custom class for deno lsp.")

;; (cl-defmethod eglot-initialization-options ((server eglot-deno))
;;   "Passes through required deno initialization options"
;;   (list :enable t
;;         :lint t))

(use-package rescript-mode
  :ensure t
  :defer t
  :hook ((rescript-mode . (lambda () (electric-indent-local-mode -1))))
  :config
  (add-to-list 'eglot-server-programs
         '(rescript-mode . ("/home/gregor/.local/npm-packages/bin/rescript-language-server" "--stdio"))))

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

(use-package cider
  :ensure t
  :after (clojure-mode)
  :config
  (add-hook 'cider-repl-mode-hook 'paredit-mode)

  (defun cider-test-luminus-test-infer-ns (x)
    (let ((parts (split-string x "\\.")))
      (string-join `(,(car parts) "test" ,@(cdr parts)) ".")))

  (setq cider-doc-xref-regexp "\\[\\[\\(.*?\\)\\]\\]"
	cider-test-infer-test-ns (lambda (x) (concat x "-test"))
	cider-repl-prompt-function 'cider-repl-prompt-abbreviated))

(use-package clj-refactor
    :ensure t
    :after (clojure-mode)
    :config
    (add-hook 'clojure-mode-hook 'clj-refactor-mode))

(use-package clojure-mode
  :ensure t
  :defer t
  :config
  (add-hook 'clojure-mode-hook 'paredit-mode)
  (add-hook 'clojure-mode-hook 'eldoc-mode))

(use-package eglot
  :ensure t
  :config
  (add-to-list 'eglot-server-programs '(elixir-ts-mode "/home/gregor/programs/elixir-ls/elixir-ls-v0.29.2/language_server.sh"))
  (add-to-list 'eglot-server-programs '(haskell-mode . ("/home/gregor/.ghcup/bin/haskell-language-server-wrapper" "--lsp")))
  (setq eglot-events-buffer-size 0))

;;; haskell
(use-package haskell-mode
  :ensure t
  :defer t
  :config
  (add-hook 'haskell-mode-hook 'haskell-indentation-mode)
  ;; (add-hook 'haskell-mode-hook 'haskell-doc-mode)
  (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
  (add-hook 'haskell-mode-hook 'eglot-ensure)
  ;; (setq haskell-process-args-ghci
  ;;         '("-ferror-spans" "-fshow-loaded-modules"))
  ;; (setq haskell-process-args-cabal-repl
  ;; 	'("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  ;; (setq haskell-process-args-stack-ghci
  ;; 	'("--ghci-options=-ferror-spans -fshow-loaded-modules"
  ;; 	  "--no-build" "--no-load"))
  ;; (setq haskell-process-args-cabal-new-repl
  ;; 	'("--ghc-options=-ferror-spans -fshow-loaded-modules"))
  )
;; (use-package nix-haskell-mode
;;   :ensure t
;;   :hook (haskell-mode . nix-haskell-mode))
;; (use-package lsp-mode
;;   :ensure t
;;   :hook (haskell-mode . lsp)
;;   :commands lsp
;;   :config
;;   (setq lsp-enable-symbol-highlighting nil
;;         lsp-lens-enable nil
;;         lsp-headerline-breadcrumb-enable nil
;;         lsp-keymap-prefix "C-ž")
;;   (define-key lsp-mode-map (kbd "C-ž") lsp-command-map))
;; (use-package lsp-ui
;;   :ensure t
;;   :commands lsp-ui-mode
;;   :config
;;   (setq lsp-ui-doc-enable nil
;;         lsp-ui-sideline-enable nil))
;; (use-package lsp-haskell
;;   :ensure t
;;   :config
;;  (setq lsp-haskell-server-path "haskell-language-server-wrapper")
;;  (setq lsp-haskell-server-args ())
;;  ;; Comment/uncomment this line to see interactions between lsp client/server.
;;  (setq lsp-log-io t))

;; (use-package dante
;;   :ensure t
;;   :after haskell-mode
;;   :commands 'dante-mode
;;   :init
;;   (add-hook 'haskell-mode-hook 'flycheck-mode)
;;   (add-hook 'haskell-mode-hook 'dante-mode)
;;   (add-hook 'haskell-mode-hook 'interactive-haskell-mode)
;;   :config
;;   (setq flymake-no-changes-timeout nil
;; 	flymake-start-syntax-check-on-newline nil
;; 	flycheck-check-syntax-automatically '(save mode-enabled))
;;   )

(use-package ess
  :ensure t
  :defer t)

(use-package eglot-jl
  :ensure t
  :defer t)

(use-package julia-mode
  :ensure t
  :defer t
  :after (eglot-jl)
  :init
  (eglot-jl-init))

;; latex
(use-package tex-mode
  :mode "\\.\\(la\\)?tex\\'"
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
  (setq bibtex-dialect 'biblatex)

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
;; (add-hook 'python-mode-hook 'eglot-ensure)
(use-package elpy
  :defer t
  :ensure t
  ;; :init
  ;; (advice-add 'python-mode :before 'elpy-enable)
  :config
  ;; (add-hook 'python-mode-hook 'copilot-mode)
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
  
  (defun poetry-enable ()
    "Activate the virtual environment poetry uses for the project."
    (interactive)
    (require 'seq)
    (pyvenv-deactivate)
    (let* ((virtual-env-python (shell-command-to-string "poetry env list | grep Activated"))
	   (virtual-env (substring virtual-env-python 0 (- (length virtual-env-python) (length " (Activated)")))))
      (pyvenv-activate (concat "/home/gregor/.cache/pypoetry/virtualenvs/" virtual-env "/")))
    (elpy-rpc-restart))

  (add-to-list
   'elpy-project-root-finder-functions
   (lambda () (locate-dominating-file default-directory "Pipfile")))
  (add-to-list
   'elpy-project-root-finder-functions
   (lambda () (locate-dominating-file default-directory "pyproject.toml")))
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
    (add-hook 'elpy-mode-hook 'flycheck-mode))

  (setq elpy-rpc-backend "jedi"
	elpy-rpc-virtualenv-path 'current))

;;; inflection
(use-package string-inflection
  :ensure t
  :bind ("C-c i" . 'string-inflection-all-cycle))


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


;;; clean up modeline
;;; source https://www.masteringemacs.org/article/hiding-replacing-modeline-strings
(defvar mode-line-cleaner-alist
  `((auto-complete-mode . " α")
    (outline-mode . " ο")
    (projectile-mode . "")
    (auto-revert-mode . "")
    (yas/minor-mode . " υ")
    (paredit-mode . " π")
    (eldoc-mode . "")
    (abbrev-mode . "")
    (interactive-haskell-mode . "")
    (highlight-indentation-mode . "")
    ;; Major modes
    (lisp-interaction-mode . "λ")
    (hi-lock-mode . "")
    (python-mode . "Py")
    (elpy-mode . "")
    (haskell-mode . "Hs")
    (company-mode . "")
    (lsp-mode . "")
    (emacs-lisp-mode . "EL")
    (web-mode . "web")))


(defun clean-mode-line ()
  (interactive)
  (cl-loop for cleaner in mode-line-cleaner-alist
        do (let* ((mode (car cleaner))
                 (mode-str (cdr cleaner))
                 (old-mode-str (cdr (assq mode minor-mode-alist))))
             (when old-mode-str
                 (setcar old-mode-str mode-str))
               ;; major mode
             (when (eq mode major-mode)
               (setq mode-name mode-str)))))


(add-hook 'after-change-major-mode-hook 'clean-mode-line)

;;; alias the new `flymake-report-status-slim' to
;;; `flymake-report-status'
;; (defalias 'flymake-report-status 'flymake-report-status-slim)
;; (defun flymake-report-status-slim (e-w &optional status)
;;   "Show \"slim\" flymake status in mode line."
;;   (when e-w
;;     (setq flymake-mode-line-e-w e-w))
;;   (when status
;;     (setq flymake-mode-line-status status))
;;   (let* ((mode-line " Φ"))
;;     (when (> (length flymake-mode-line-e-w) 0)
;;       (setq mode-line (concat mode-line ":" flymake-mode-line-e-w)))
;;     (setq mode-line (concat mode-line flymake-mode-line-status))
;;     (setq flymake-mode-line mode-line)
;;     (force-mode-line-update)))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-for-comint-mode t)
 '(compilation-message-face 'default)
 '(csv-separators '("," "\11" ";"))
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#839496")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(zenburn))
 '(custom-safe-themes
   '("09b833239444ac3230f591e35e3c28a4d78f1556b107bafe0eb32b5977204d93"
     "9fb561389e5ac5b9ead13a24fb4c2a3544910f67f12cfcfe77b75f36248017d0"
     "f079ef5189f9738cf5a2b4507bcaf83138ad22d9c9e32a537d61c9aae25502ef"
     "18cf5d20a45ea1dff2e2ffd6fbcd15082f9aa9705011a3929e77129a971d1cb3"
     "4cf3221feff536e2b3385209e9b9dc4c2e0818a69a1cdb4b522756bcdf4e00a4"
     "4aee8551b53a43a883cb0b7f3255d6859d766b6c5e14bcb01bed572fcbef4328"
     "a8245b7cc985a0610d71f9852e9f2767ad1b852c2bdea6f4aadc12cce9c4d6d0"
     "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4"
     default))
 '(diff-switches '("-u" "--color='never'"))
 '(dired-guess-shell-alist-user
   '(("\\.epub\\'" "zathura &") ("\\.pdf\\'" "zathura &")
     ("\\.docx?\\'" "lowriter &")))
 '(eglot-confirm-server-edits nil)
 '(elfeed-feeds '("https://clojure.org/feed.xml"))
 '(elpy-modules
   '(elpy-module-company elpy-module-eldoc elpy-module-pyvenv
                         elpy-module-highlight-indentation
                         elpy-module-yasnippet
                         elpy-module-sane-defaults))
 '(elpy-rpc-virtualenv-path 'current)
 '(elpy-shell-capture-last-multiline-output t)
 '(elpy-shell-echo-input nil)
 '(exec-path
   '("/usr/local/bin" "/usr/bin" "/bin" "/usr/local/sbin"
     "/usr/lib/jvm/default/bin" "/usr/bin/site_perl"
     "/usr/bin/vendor_perl" "/usr/bin/core_perl"
     "/home/gregor/.rvm/bin" "/usr/lib/emacs/26.1/x86_64-pc-linux-gnu"
     "/home/gregor/bin"))
 '(grep-command "grep --color -nH -E ")
 '(grep-find-command '("find . -type f -exec grep --color -nH -E  {} +" . 42))
 '(grep-find-ignored-directories
   '("SCCS" "RCS" "CVS" "MCVS" ".src" ".svn" ".git" ".hg" ".bzr" "_MTN"
     "_darcs" "{arch}" ".pytest_cache" ".mypy_cache"
     "bitsuisse.egg-info"))
 '(grep-find-template "find <D> <X> -type f <F> -exec grep <C> -nH -E <R> {} +")
 '(grep-highlight-matches 'auto)
 '(grep-template "grep <X> <C> -nH -E <R> <F>")
 '(grep-use-null-device nil)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map (solarized-color-blend it "#002b36" 0.25)
          '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900"
            "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#93a1a1")
 '(highlight-tail-colors
   '(("#073642" . 0) ("#546E00" . 20) ("#00736F" . 30) ("#00629D" . 50)
     ("#7B6000" . 60) ("#8B2C02" . 70) ("#93115C" . 85)
     ("#073642" . 100)))
 '(hl-bg-colors
   '("#7B6000" "#8B2C02" "#990A1B" "#93115C" "#3F4D91" "#00629D"
     "#00736F" "#546E00"))
 '(hl-fg-colors
   '("#002b36" "#002b36" "#002b36" "#002b36" "#002b36" "#002b36"
     "#002b36" "#002b36"))
 '(magit-diff-use-overlays nil)
 '(magit-repository-directories
   '(("/home/gregor/Documents/sluzba/blueoceangaming/" . 1)
     ("/home/gregor/Documents/projekti/" . 1)
     ("/home/gregor/Documents/Mucek/" . 1)))
 '(mode-line-format
   '("%e" mode-line-front-space mode-line-mule-info mode-line-client
     mode-line-modified mode-line-remote
     mode-line-frame-identification mode-line-buffer-identification
     "   " mode-line-position "  " mode-line-modes mode-line-misc-info
     mode-line-end-spaces))
 '(nxml-slash-auto-complete-flag t)
 '(org-agenda-files
   '("/home/gregor/Documents/sluzba/blueoceangaming/temp/todos.org"
     "/home/gregor/Documents/personal/calendar.org"))
 '(org-babel-load-languages
   '((emacs-lisp . t) (shell . t) (awk . t) (haskell . t) (elixir . t)
     (clojure . t) (dot . t) (python . t) (ruby . t) (gnuplot . t)
     (calc . t) (R . t) (mermaid . t)))
 '(org-export-backends '(ascii html latex md odt))
 '(package-selected-packages
   '(async auto-complete boogie-friends browse-kill-ring cargo cdlatex
           cider clj-refactor clojure-snippets
           color-theme-sanityinc-solarized company-go company-tern
           copilot csv-mode dante dirvish docker dockerfile-mode eat
           editorconfig eglot-jl elfeed elixir-ts-mode
           elixir-yasnippets elm-mode elm-yasnippets elpy ess exercism
           gettext gnuplot go-mode go-playground graphviz-dot-mode
           groovy-mode haml-mode haskell-mode haskell-snippets
           hippie-expand-slime iedit indium inf-elixir inf-ruby intero
           js2-mode js2-refactor json-mode julia-mode lsp-mode magit
           markdown-mode merlin mermaid-mode move-text nerd-icons
           nix-haskell-mode nix-mode nix-update nixpkgs-fmt
           nushell-mode ob-elixir ob-mermaid page-break-lines paradox
           paredit po-mode projectile py-snippets quelpa
           quelpa-use-package rescript-mode rust-mode rust-playground
           rustic rvm slime smartparens-mode smex string-inflection
           tern tide use-package utop web-mode web-mode-edit-element
           websocket xref-js2 yaml-mode yasnippet-classic-snippets
           yasnippet-snippets zenburn-theme))
 '(paradox-github-token t)
 '(pos-tip-background-color "#073642")
 '(pos-tip-foreground-color "#93a1a1")
 '(project-vc-extra-root-markers '("mix.exs"))
 '(reftex-use-external-file-finders t)
 '(rust-indent-offset 4)
 '(safe-local-variable-values
   '((ispell-local-dictionary . sl_SI) (web-mode-engine . "elixir")
     (org-todo-keywords (sequence "TODO" "IN_PROGRESS" "|" "DONE"))
     (org-todo-keywords (sequence "TODO" "IN PROGRESS" "|" "DONE"))
     (org-todo-keywords quote
                        ((sequence "TODO" "IN PROGRESS" "|" "DONE")))
     (flycheck-checker . python-pylama)
     (flycheck-select-checker . python-pylama)
     (ispell-local-dictionary . slovenian)
     (org-todo-keywords sequence "TODO" "REFINEMENT REVIEW" "|"
                        "READY" "DONE")
     (org-todo-keywords quote
                        ((sequence "TODO" "REFINEMENT REVIEW" "|"
                                   "READY" "DONE")))
     (case-fold-search) (ispell-dictionary . slovenian)
     (cider-clojure-cli-global-options . -O:default-jvm-opts)
     (js2-additional-externs "Meteor" "Tracker" "FlowRouter"
                             "RocketChat" "$" "Session" "Random"
                             "Template")
     (js2-additional-externs "Meteor" "Tracker" "FlowRouter"
                             "RocketChat" "$")
     (cider-cljs-lein-repl
      . "(do (require 'figwheel-sidecar.repl-api)\12         (figwheel-sidecar.repl-api/start-figwheel!)\12         (figwheel-sidecar.repl-api/cljs-repl))")
     (cider-cljs-lein-repl
      . "(do (use 'figwheel-sidecar.repl-api) (start-figwheel!) (cljs-repl))")
     (web-mode-engine . selmer) (web-mode-engine . django)
     (web-mode-engine . "django") (engine . "selmer")
     (web-mode-engine . "selmer")
     (eval
      (lambda nil
        (when (string= (file-name-extension buffer-file-name) "html")
          (web-mode))))))
 '(send-mail-function 'smtpmail-send-it)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#073642" 0.2))
 '(split-height-threshold 100)
 '(split-width-threshold 200)
 '(term-default-bg-color "#002b36")
 '(term-default-fg-color "#839496")
 '(typescript-indent-level 2)
 '(web-mode-auto-close-style 2)
 '(web-mode-enable-block-face t)
 '(web-mode-enable-engine-detection t)
 '(weechat-color-list
   '(unspecified "#002b36" "#073642" "#990A1B" "#dc322f" "#546E00"
                 "#859900" "#7B6000" "#b58900" "#00629D" "#268bd2"
                 "#93115C" "#d33682" "#00736F" "#2aa198" "#839496"
                 "#657b83"))
 '(windmove-swap-states-default-keybindings '([ignore] control shift))
 '(windmove-wrap-around t))
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
(put 'dired-find-alternate-file 'disabled nil)
