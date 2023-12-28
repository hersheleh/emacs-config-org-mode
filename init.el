(defun gsh/display-startup-time ()
  (message "Emacs loaded in %s with %d garbage collections."
           (format "%.2f seconds"
                   (float-time
                    (time-subtract after-init-time before-init-time)))
           gcs-done))
(add-hook 'emacs-startup-hook #'gsh/display-startup-time)

(require 'package)
(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("org" . "https://orgmode.org/elpa/")
        ("elpa" . "https://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

;; (unless (package-installed-p 'use-package)
;;   (package-install 'use-package))
;; (require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
(setq use-package-verbose nil)

(defvar my-macbook-p (equal system-type 'darwin))
(defvar my-sc-thinkpad-p (equal (system-name) "opamp"))
(global-auto-revert-mode)        ; revert buffers when changed on disk

(use-package frame
  :ensure nil
  :config
  (setq initial-frame-alist '((fullscreen . maximized))))

;; Set Dired default directory
;; (setq default-directory "C:/Users/GrishaKhachaturyan/hub/")
;; Disable warning spam
(setq warning-minimum-level :emergency)
;; set re-builder sytax to string
(setq reb-re-syntax 'string)
;; keep folders clean
(setq backup-directory-alist
      `(("." . ,(expand-file-name "tmp/backups" user-emacs-directory))))
;; create path for auto save mode
(make-directory (expand-file-name "tmp/auto-saves/" user-emacs-directory) t)

(setq auto-save-list-file-prefix (expand-file-name
                                  "tmp/auto-saves/sessions/"
                                  user-emacs-directory)
      auto-save-file-name-transforms
      `((".*",(expand-file-name "tmp/auto-saves/" user-emacs-directory) t)))

(use-package no-littering
  :demand t)

;; Disable lock files. They are interfering with terraform-ls on linux
(setq create-lockfiles nil)

;; Disable emacs creating backup file *~
(setq make-backup-files nil)

(setq display-time-default-load-average nil)
(display-time)

(electric-indent-mode)
(add-hook 'prog-mode-hook 'electric-pair-local-mode)
;; (electric-quote-mode)

(defun move-line-up ()
  (interactive)
  (transpose-lines 1)
  (forward-line -2))

(defun move-line-down ()
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1))

(global-set-key (kbd "M-<up>") 'move-line-up)
(global-set-key (kbd "M-<down>") 'move-line-down)

;; remap save-buffers-kill-terminal from C-x C-c to C-x q
(if (not (daemonp))
    (global-unset-key (kbd "C-x  C-c"))) ; i always accidentilly press this

(global-set-key (kbd "C-x q") 'save-buffers-kill-emacs)

(global-unset-key (kbd "C-z"))          ; unmap suspend-frame from C-z
(global-unset-key (kbd "C-x C-z"))

;; switch buffers directionally using ijkl keys similar to right hand wasd
;; (global-set-key (kbd "C-c i") 'windmove-up)
;; (global-set-key (kbd "C-c k") 'windmove-down)
;; (global-set-key (kbd "C-c j") 'windmove-left)
;; (global-set-key (kbd "C-c l") 'windmove-right)

;; switch buffers directionally using arrow keys
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; add C-c o binding to other-window
;; (global-set-key (kbd "C-c o") 'other-window) ; (o)ther

;; split buffer with v and h keys
;; (global-set-key (kbd "C-c b h") 'split-window-right) ;(h)orizontal
;; (global-set-key (kbd "C-c b v") 'split-window-below) ;(v)ertical

;; delete other windows
;; (global-set-key (kbd "C-c b o") 'delete-other-windows) ; (o)ne window
;; (global-set-key (kbd "C-c b c") 'delete-window)        ; (c)lose this window

(global-unset-key (kbd "M-j"))       ; was default-indent-new-line
(global-unset-key (kbd "M-k"))       ; was kill-sentence
(global-set-key (kbd "M-j") 'scroll-up-line) ; scroll up
(global-set-key (kbd "M-k") 'scroll-down-line) ; scroll down

;; (add-hook 'before-save-hook 'whitespace-cleanup)

(defun scroll-up-window-half ()
  "Scroll the buffer window up by half the length of the window."
  (interactive)
  (scroll-up (/ (window-total-height) 2)))
(defun scroll-down-window-half ()
  "Scroll the buffer window down by half the length of the window."
  (interactive)
  (scroll-down (/ (window-total-height) 2)))

(global-unset-key (kbd "C-v"))          ; unset default page down key
(global-unset-key (kbd "M-v"))          ; unset default page up key
(global-set-key (kbd "C-v") 'scroll-up-window-half)
(global-set-key (kbd "M-v") 'scroll-down-window-half)

(global-set-key (kbd "M-z") 'zap-up-to-char)

(global-unset-key (kbd "M-SPC"))
(defun insert-underscore ()
  "Inserting an underscore '_' character"
  (interactive)
  (insert #x5F))
(global-set-key (kbd "M-SPC") 'insert-underscore)

(scroll-bar-mode -1)          ; remove scroll bar
(column-number-mode)          ; show column number in modline
(tool-bar-mode -1)
(menu-bar-mode -1)
(setq ring-bell-function 'ignore)
;;(global-display-line-numbers-mode 1) ; enable line numbers in margin globably
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq inhibit-startup-message t)     ; No splash screen
;; (global-visual-line-mode t)
;; Remove title bar in emacs-plus version on Mac
;; (add-to-list 'default-frame-alist '(undecorated . t))

(defun gsh/set-font ()
  (message "Setting font")
  (set-frame-font "Ubuntu Mono-13:bold" nil t))

(when my-macbook-p (set-frame-font "Menlo 14" nil t))

;; Set font for windows when you have it
;; (set-frame-font "Consolas-11:bold" nil t)

(when my-sc-thinkpad-p
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda(frame)
                  (with-selected-frame frame
                    (gsh/set-font))))
    (gsh/set-font)))

(use-package exec-path-from-shell
  :demand t
  :config
  (when (memq window-system '(mac ns x))
    (exec-path-from-shell-initialize)
    ))

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook
  (prog-mode . ws-butler-mode)
  (org-mode . ws-butler-mode))

(use-package doom-themes
  :demand t
  ;; :custom
  ;; (doom-monokai-classic-brighter-comments t)
  ;; (doom-acario-dark-brighter-comments t)
  :config
  (setq doom-themes-enable-bold t     ; if nil, bold is universally disabled
        doom-themes-enable-italic t)  ; if nil, italcs is universally disabled
  ;; (custom-set-variables
  ;; '(doom-molokai-brighter-comments t))
  ;; (load-theme 'doom-monokai-classic t)
  ;; (load-theme 'doom-acario-dark t)
  ;; (load-theme 'doom-moonlight t)
  (load-theme 'doom-badger t)

  ;; customize the doom monkai theme
  (custom-set-faces
   '(counsel--mark-ring-highlight ((t (:inherit highlight))))
   ;; '(ivy-current-match ((t (:background "#fd971f" :foreground "black"))))
   '(show-paren-match ((t (:background "#FD971F" :foreground "black"
                                       :weight ultra-bold))))))

(use-package doom-modeline
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :demand t
  :if (display-graphic-p))

(use-package all-the-icons-dired
  ;; :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

;; (recentf-mode 1)

(use-package ivy
  :bind (("C-s" . counsel-grep-or-swiper)
         ("C-M-s" . swiper-isearch)
         ("C-r" . swiper-backward)
         ;; ("C-c C-r" . ivy-resume)
         ;; ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-find-file)
         ("C-c r" . counsel-recentf)    ; open recent file
         ("C-c f" . counsel-recentf)    ; open recent file
         ("C-c C-f" .  counsel-recentf)
         ("C-h a" . counsel-apropos)
         ("C-h d" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h o" . counsel-describe-symbol)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-h b" . counsel-descbinds)
         ("C-x b" . counsel-switch-buffer)
         ("C-c T" . counsel-load-theme)
         :map minibuffer-local-map
         ("C-r" . counsel-minibuffer-history)
         ;; ("C-c g" . counsel-git)
         ;; ("C-c j" . counsel-git-grep)
         ;; ("C-c k" . counsel-ag)
         ;; ("C-x l" . counsel-locate)
         ;; ("C-S-o" . counsel-rhythmbox)
         )
  :config
  (ivy-mode 1))

(use-package ivy-hydra)

(use-package ivy-rich
  ;; :after counsel
  :init
  (ivy-rich-mode 1)
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

(use-package all-the-icons-ivy-rich
  :after ivy
  :init (all-the-icons-ivy-rich-mode 1))

(use-package counsel
  :config
  (setq ivy-initial-inputs-alist nil))  ; Don't start searches with ^

(use-package dired
  :ensure nil
  :commands (dired dired-jump)
  :custom ((dired-listing-switches "-ghoa --group-directories-first"))
  :init
  (put 'dired-find-alternate-file 'disabled nil)
  (defun dired-up-alternate-directory ()
    (interactive) (find-alternate-file ".."))
  :bind (:map
         dired-mode-map
         ("h" . dired-up-alternate-directory)
         ("l" . dired-find-alternate-file)
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("J" . dired-goto-file)
         ("K" . kill-buffer-and-window))
  :config
  (when my-macbook-p
    (setq insert-directory-program "gls" dired-use-ls-dired t))
  (setq dired-listing-switches "-ghoa --group-directories-first")
  ;; (setq insert-directory-program "C:\\Program Files\\Git\\usr\\bin\\ls")
  ;; (setq ls-lisp-use-insert-directory-program t)
  )

(use-package magit
  :commands magit-status)

(use-package which-key
  :defer 0
  :bind
  (("C-c w w" . which-key-show-major-mode)
   ("C-c w i" . which-key-show-minor-mode-keymap))
  :config
  (setq which-key-idle-delay 0.8)
  (which-key-mode))

(use-package ivy-prescient
  :after counsel
  :init
  (ivy-prescient-mode 1)
  :config
  (setq ivy-prescient-retain-classic-highlighting t)
  (prescient-persist-mode))

(use-package treemacs
  :defer t
  :custom
  (treemacs-python-executable "python")
  :config
  (setq treemacs-git-mode nil)
  (treemacs-project-follow-mode)
  :hook
  (treemacs-select . windmove-right)

  )
;; (use-package treemacs-icons-dired
;;   :after dired
;;   :config (treemacs-icons-dired-mode))

;; (use-package rotate)

(use-package hydra
  :init
  (global-unset-key (kbd "C-c b l"))
  :bind (("C-c b" . hydra-windows/body)
         ("C-c o" . hydra-other-window/body))
  )
;; hydra to condense other window commands
(defhydra hydra-other-window ()
  "other window commands"
  ("f" find-file-other-window "find file")
  ("b" counsel-switch-buffer-other-window "switch buffer"))
;; Hydra for managing buffers
(defhydra hydra-windows (global-map "C-c" :hint nil)
  "
^Move^       ^Split^           ^Delete^             ^Shift^      ^Misc^
^^^^^^^^----------------------------------------------------------------------------------
_i_: up      _v_: vertical     _o_: other windows   _I_: up      ^ ^
_k_: down    _h_: horizontal   _d_: this window     _K_: down    _b_: switch buffer
_j_: left    ^ ^               ^ ^                  _J_: left    _F_: find file
_l_: right   ^ ^               ^ ^                  _L_: right   _p_: switch project
"
  ("l" windmove-right)
  ("j" windmove-left)
  ("i" windmove-up)
  ("k" windmove-down)
  ("v" split-window-below)
  ("h" split-window-right)
  ("d" delete-window)
  ("o" delete-other-windows)
  ("I" buf-move-up)
  ("K" buf-move-down)
  ("J" buf-move-left)
  ("L" buf-move-right)
  ;; ("r" rotate-layout)
  ("b" counsel-switch-buffer)
  ("F" counsel-find-file)
  ("p" project-switch-project)
  ;; ("g" revert-buffer-quick)
  ("q" nil "quit"))

(use-package lsp-mode
  ;; :after flycheck
  :commands (lsp lsp-deferred)
  :init
  (setq lsp-keymap-prefix "C-x l")
  (setq lsp-use-plists t)
  ;; (lsp-treemacs-sync-mode 1)
  :hook
  ;; (js-mode . lsp-deferred)
  (terraform-mode . lsp-deferred)
  ;; (lsp-mode . lsp-treemacs-symbols)
  ;; (lsp-mode . treemacs-select-window)
  ;; :custom
  ;; ;; (lsp-terraform-server "C:/Users/GrishaKhachaturyan/stand_alone_prgrms/bin/terraform-lsp")
  ;; (lsp-terraform-ls-server
  ;;  "C:/Users/GrishaKhachaturyan/.vscode/extensions/hashicorp.terraform-2.25.1-win32-x64/bin/terraform-ls"
  ;;  )
  :config
  ;; (setq lsp-disabled-clients '(tfls))
  (lsp-enable-which-key-integration t)
  (lsp-modeline-code-actions-mode -1)
  ;; (setq lsp-modeline-diagnostics-enable nil)
  (with-eval-after-load 'lsp-mode
    (add-to-list 'lsp-file-watch-ignored-directories
                 "[/\\\\]\\\.env.*\\'"))
  )

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; (setq lsp-eldoc-enable-hover nil)
  (setq lsp-ui-enable-hover nil)
  ;; (setq lsp-ui-sideline-code-actions nil)

  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil))

;; (use-package sideline
;;   :after lsp-mode

;;   :init
;;   (setq sideline-backends-right '(sideline-lsp)))

(use-package lsp-ivy
  :init
  (defun ivy-update-candidates-dynamic-collection-workaround-a (old-fun &rest args)
    (cl-letf (((symbol-function #'completion-metadata) #'ignore))
      (apply old-fun args)))
  (advice-add #'ivy-update-candidates :around #'ivy-update-candidates-dynamic-collection-workaround-a)
  )

(use-package dap-mode
  ;; :ensure t
  :commands dap-debug
  :config
  (require 'dap-ui)
  ;; (dap-auto-configure-mode 1)

  (dap-mode 1)
  (dap-ui-mode 1)
  (dap-tooltip-mode 1)
  (dap-ui-controls-mode 1)

  ;; lldb config
  ;; (setq dap-lldb-debug-program '("/usr/local/opt/llvm/bin/lldb-vscode"))
  ;; (setq dap-lldb-debug-program '("/usr/local/bin/lldb-vscode"))
  (setq dap-print-io t))

(use-package realgud)                   ; RealGUD debugger

(use-package company
  :custom
  (company-minimum-prefix-length 1)
  (company-idle-delay 0.0)
  (company-tooltip-idle-delay 0.0)
  (company-candidates-cache t)
  :hook
  (prog-mode . company-mode)            ; add completion to programming language modes
  (inferior-python-mode-hook . company-mode)
  ;; (org-mode . company-mode)            ; add completion to org-mode
  )
;; :config
;; (add-hook 'after-init-hook 'global-company-mode)

;; (use-package company-box          ; Show icons in company complettions
;;   :hook (company-mode . company-box-mode))

(use-package flycheck
  ;; :custom
  ;; (flycheck-python-pycompile-executable "python")
  ;; (flycheck-python-pylint-executable "pylint")
  ;; (flycheck-python-pyright-executable "python")
  ;; (flycheck-python-mypy-executable "python")
  ;; (flycheck-python-flake8-executable "python")

  )

(use-package iedit)

(use-package yasnippet
  :config (yas-global-mode 1))

(use-package treesit
  :ensure nil
  :config
  (setq treesit-extra-load-path '("/usr/local/lib")))

(use-package cc
  :ensure nil
  :mode ("\\.keymap\\'" . c-mode)
  :hook
  (c++-mode . lsp-deferred)
  :config
  (require 'dap-cpptools)
  (require 'dap-lldb)                  ; not stopping at breakpoints. look at upgrading
  (dap-cpptools-setup)

  (dap-register-debug-template
   "cpptools::Run Configuration reverse_string"
   (list :type "cppdbg"
         :request "launch"
         :name "cpptools::Run Configuration"
         :MIMode "gdb"
         :program "${workspaceFolder}/cpp/reverse_string"
         :cwd "${workspaceFolder}/cpp"))
  ;; Debug Configuration for reverse_string.cpp
  (dap-register-debug-template
   "LLDB::Run reverse_string"
   (list :type "lldb-vscode"
         :request "launch"
         :cwd "${workspaceFolder}cpp/"
         :program "${workspaceFolder}cpp/reverse_string"
         :name "LLDB::Run reverse_string")))

(use-package dockerfile-mode)
(use-package docker)

(use-package sclang
  :ensure nil
  :load-path
  (lambda ()
    (cond
     (my-macbook-p "/Users/Grisha/Library/Application Support/SuperCollider/downloaded-quarks/scel/el")
     (my-sc-thinkpad-p "~/.local/share/SuperCollider/downloaded-quarks/scel/el")))
  :mode ("\\.scd\\'" . sclang-mode)
  :bind(:map sclang-mode-map
             ("C-c C-l"    . sclang-eval-line)
             ("C-<return>" . sclang-eval-defunsclang)
             ("C-." . sclang-main-stop)
             ("C-c C-h" . sclang-find-help-in-gui))
  :custom
  (sclang-eval-line-forward nil)
  :config
  (unbind-key "C-c h" sclang-mode-map)
  (when my-macbook-p
    (setq exec-path
          (append
           exec-path
           '("/Applications/SuperCollider.app/Contents/MacOS/")))))

(use-package python
  :ensure nil
  :custom
  ;; python config
  (dap-python-executable "python3")
  (dap-python-debugger 'debugpy)

  ;; :bind ( :map python-mode-map
  ;;         ("C-c r" . nil))
  :hook
  (python-mode . lsp-deferred)
  ;; (lsp-diagnostics-mode . (lambda ()
  ;;                           ;; (flycheck-add-next-checker
  ;;                           ;;  'lsp 'python-pylint)
  ;;                           ;; (flycheck-disable-checker 'lsp)
  ;;                           ;; (flycheck-select-checker 'python-pylint)
  ;;                           ))




  :config
  (require 'dap-python)                ; also not stopping at breakpoints. look at upgrading
  ;; (setq py-python-command "python3")
  ;; (setq py-shell-name "python")
  (setq lsp-pylsp-server-command "~/.local/bin/pylsp")
  (setq python-shell-interpreter "python3")
  ;; Debug Configuration for python unittest
  (dap-register-debug-template
   "Python :: Run unittest (buffer)"
   (list :type "python"
         :args ""
         :cwd nil
         :program nil
         :module "unittest"
         :request "launch"
         :name "Python :: Run unittest (buffer)"))
  ;; Debug Configuration for python file which reads from stdin
  (dap-register-debug-template
   "Python :: Run file User Input (buffer)"
   (list :type "python"
         :args ""
         :cwd nil
         :module nil
         :program nil
         :console "integratedTerminal"  ; launches vterm
         :request "launch"
         :name "Python :: Run file User Input (buffer)"))
  (dap-register-debug-template
   "Python :: Debug PDF args"
   (list :type "python"
         :cwd nil
         :module nil
         :program nil
         :console "integratedTerminal"  ; launches vterm
         :request "launch"
         :name "Python :: Run file User Input (buffer)"
         :args "~/hub/ripl/pw-backend/src/pw_backend_app/parser/MW-562withoutfein_filled_out2.pdf"))

  )

;; fix run-python codec errors on windows
(setenv "LANG" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")

(use-package pyvenv)

(use-package pug-mode)

(use-package jtsx-ts-mode
  :ensure nil
  :custom
  (typescript-ts-mode-indent-offset 4)
  :mode ("\\.tsx\\'" "\\.jsx\\'")
  :hook
  (tsx-ts-mode . lsp-deferred)
  :config
  ;; (setq standard-indent 2)
  )
  ;;                           (if (eq major-mode 'tsx-ts-mode)
  ;;                               (flycheck-select-checker
  ;;                                'javascript-eslint))

(use-package typescript-ts-mode
  :ensure nil
  :mode ("\\.ts\\'" "\\.js\\'")
  :hook
  (typescript-ts-mode . lsp-deferred))

(use-package json-ts-mode
  :ensure nil
  :mode ("\\.json\\'")
  :hook
  (json-ts-mode . lsp-deferred)
  :config
  (setq js-indent-level 2))

(use-package csharp-mode
  :ensure nil
  :hook
  (c-sharp-mode . lsp-deferred))

(use-package shader-mode)

(use-package terraform-mode
  :defer t)

(use-package markdown-preview-mode)

(use-package vterm
  :commands vterm
  ;; :hook
  ;; turn off line numbers in vterm
  ;; (vterm-mode . (lambda () (display-line-numbers-mode 0)))
  ;; execute bash_profile for this terminal session
  ;; :hook
  ;; (vterm-mode . (lambda () (vterm-send-string "source ~/.bash_profile\n")))
  )

(use-package rainbow-delimiters
  :hook (prog-mode . rainbow-delimiters-mode))

(defun g/org-babel-tangle-config()
  (when (string-equal (buffer-file-name)
                      (expand-file-name "~/.emacs.d/config.org"))
    (let ((org-confirm-babel-evaluate nil))
      (org-babel-tangle))))

(add-hook 'org-mode-hook (lambda () (add-hook 'after-save-hook
                                              #'g/org-babel-tangle-config)))

(use-package org
  :defer t
  ;; :after (org-timeline)
  :bind (:map org-mode-map
              ("C-c C-f" . hydra-org/org-forward-heading-same-level)
              ("C-c C-b" . hydra-org/org-backward-heading-same-level)
              ("C-c C-p" . hydra-org/org-previous-visible-heading)
              ("C-c C-n" . hydra-org/org-next-visible-heading)
              ("M-n" . org-metadown)
              ("M-p" . org-metaup)
              ("C-c C-j" . counsel-org-goto)
              ("C-c t" . org-todo))
  :hook
  (org-mode . visual-line-mode)
  (org-mode . visual-fill-column-mode)
  (org-mode . electric-pair-local-mode)
  :custom
  (org-priority-highest 65)
  (org-priority-lowest 69)
  (org-priority-default 67)
  (org-agenda-start-with-clockreport-mode t)
  (org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
  :config
  ;; Org Capture Configuration
  (setq org-default-notes-file (concat org-directory "/notes.org"))
  ;; Org Agenda
  (setq org-agenda-span 'day)
  (setq org-agenda-include-diary nil)
  ;; Add graphical timeline to org agenda
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
  (setq org-agenda-files
        '("~/hub/orgs/my_todos.org"
          "~/hub/orgs/music_todos.org"
          "~/hub/orgs/house_todos.org"
          "~/hub/orgs/mental_todos.org"
          ;; "~/hub/new_projects/orgi/orgi_plan.org"
          ;; "~/hub/recording_bullet_journal/super_collider_projects/sc_bujo.org"
          ;; "~/.emacs.d/config.org"
          ))
  (setq org-todo-keywords
        ;; '((sequence "BACKLOG" "TODO(t)" "TEST(s)" "RECUR(r)" "NEXT(n)"  "|" "DONE(d!)"))
        '((sequence "RECUR(r)" "TODO(t)" "CHOOSE(c)" "|" "DONE(d!)"))
        ;; '((sequence "RECUR" "TODO" "CHOOSE" "|" "DONE"))
        )
  (setq org-agenda-start-with-log-mode t)
  (setq org-agenda-log-mode-items '(clock state))
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-startup-indented t)
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.20))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.17))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.15))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.12))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.10))))
   )

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (defhydra hydra-org ()
    "org hydra"
    ("n" org-next-visible-heading "next visible")
    ("p" org-previous-visible-heading "prev visible")
    ("f" org-forward-heading-same-level "forward level")
    ("b" org-backward-heading-same-level "backward level")
    ("C-n" org-next-visible-heading)
    ("C-p" org-previous-visible-heading)
    ("C-f" org-forward-heading-same-level)
    ("C-b" org-backward-heading-same-level)
    ("TAB" org-cycle "cycle")
    ;; ("M-j" org-metadown "move down")
    ;; ("M-k" org-metaup "move up")
    ("q" nil "quit"))
  )
()
;; org-agenda timeline view
(use-package org-timeline)

(use-package org-superstar
  :after org
  :hook (org-mode . org-superstar-mode))

(use-package org-roam
  :custom
  (org-roam-directory "~/hub/org-roam")
  :bind (("C-c n l" . org-roam-buffer-toggle)
         ("C-c n f" . org-roam-node-find)
         ("C-c n i" . org-roam-node-insert))
  :config
  (org-roam-db-autosync-mode)
  (org-roam-setup))

(use-package visual-fill-column
  :hook
  (visual-fill-column-mode . (lambda () (setq visual-fill-column-width 80))))

(use-package org-pomodoro
  :after org
  :custom
  ;; (org-pomodoro-audio-player (executable-find "play"))
  (org-pomodoro-overtime-sound
   "/home/grish/.emacs.d/my-statics/pom_sounds/bell_quiet.wav")
  (org-pomodoro-start-sound
   "/home/grish/.emacs.d/my-statics/pom_sounds/bell_quiet.wav")
  (org-pomodoro-finished-sound
   "/home/grish/.emacs.d/my-statics/pom_sounds/bell_quiet.wav")
  (org-pomodoro-short-break-sound
   "/home/grish/.emacs.d/my-statics/pom_sounds/bell_quiet.wav")
  (org-pomodoro-long-break-sound
   "/home/grish/.emacs.d/my-statics/pom_sounds/bell_quiet.wav")
  (org-pomodoro-short-break-length 7)
  (org-pomodoro-ticking-sound-p nil)
  (org-pomodoro-manual-break t))

;; The following fixes sounds not working on windows
;; (use-package sound-wav)
;; (use-package powershell)

(use-package helpful
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable)
  :bind
  (("C-h ." . helpful-at-point)    ; show help docs for current symbol
   ("C-h j" . helpful-at-point)
   ([remap describe-function] . counsel-describe-function)
   ([remap describe-command] . helpful-command)
   ([remap describe-variable] . counsel-describe-variable)
   ([remap describe-key] . helpful-key)
   :map helpful-mode-map
   ("k" . kill-buffer-and-window)))

(use-package dashboard
  :demand t
  :after (page-break-lines all-the-icons)
  :init
  (setq dashboard-display-icons-p t)
  (setq dashboard-icon-type 'nerd-icons)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  :config
  (setq line-move-visual nil)
  (setq dashboard-set-navigator nil)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-center-content nil)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((agenda . 6)
                          (projects . 5)
                          (recents . 5)
                          ))
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-agenda-sort-strategy
        '(todo-state-down
          priority-up
          time-down))
  (setq dashboard-agenda-time-string-format "%b %d %Y %a ")
  ;; (setq dashboard-agenda-prefix-format " %i %-12:c %s ")
  (setq dashboard-agenda-prefix-format " %i %s ")
  (setq dashboard-agenda-release-buffers t)
  ;; (setq initial-buffer-choice
  ;;       (lambda () (get-buffer-create "*dashboard*")))
  (dashboard-setup-startup-hook)
  ;; dashboard icons don't quite load.
  ;; buffer needs to be reverted
  ;; (add-hook 'server-after-make-frame-hook
  ;;           'revert-buffer)
  )

(use-package page-break-lines
  :demand t
  :config (page-break-lines-mode))

(use-package dashboard-ls
  :demand t)

(use-package savehist
  :after counsel
  :init
  (savehist-mode 1)
  (setq history-length 25))

;; Set Garbage collection threshold back down after startup completes
(add-hook 'emacs-startup-hook
          (lambda ()
            ;; (setq gc-cons-threshold (* 100 1000 1000))
            (setq file-name-handler-alist default-file-name-handler-alist)
            ))
;; (setq gc-cons-threshold (* 2 1000 1000))
