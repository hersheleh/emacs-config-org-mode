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

(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-always-ensure t)
(setq use-package-always-defer t)
;; (setq use-package-verbose t)

;; Set Dired default directory
(setq default-directory "C:/Users/GrishaKhachaturyan/hub/")
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

;; remap save-buffers-kill-terminal from C-x C-c to C-x q
(global-unset-key (kbd "C-x  C-c")) ; i always accidentilly press this key
(global-set-key (kbd "C-x q") 'save-buffers-kill-emacs)

(global-unset-key (kbd "C-z"))          ; unmap suspend-frame from C-z
(global-unset-key (kbd "C-x C-z"))

;; switch buffers directionally using ijkl keys similar to right hand wasd
(global-set-key (kbd "C-c i") 'windmove-up)
(global-set-key (kbd "C-c k") 'windmove-down)
(global-set-key (kbd "C-c j") 'windmove-left)
(global-set-key (kbd "C-c l") 'windmove-right)

;; switch buffers directionally using arrow keys
(global-set-key (kbd "C-c <up>") 'windmove-up)
(global-set-key (kbd "C-c <down>") 'windmove-down)
(global-set-key (kbd "C-c <left>") 'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)

;; add C-c o binding to other-window
;; (global-set-key (kbd "C-c o") 'other-window) ; (o)ther

;; split buffer with v and h keys
(global-set-key (kbd "C-c b h") 'split-window-right) ;(h)orizontal
(global-set-key (kbd "C-c b v") 'split-window-below) ;(v)ertical

;; delete other windows
(global-set-key (kbd "C-c b o") 'delete-other-windows) ; (o)ne window
(global-set-key (kbd "C-c b c") 'delete-window)        ; (c)lose this window

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

(global-unset-key (kbd "M-SPC"))
(defun insert-underscore ()
  "Inserting an underscore '_' character"
  (interactive)
  (insert #x5F))
(global-set-key (kbd "M-SPC") 'insert-underscore)

(scroll-bar-mode -1)          ; remove scroll bar
(column-number-mode)          ; show column number in modline
;;(global-display-line-numbers-mode 1) ; enable line numbers in margin globably
(add-hook 'prog-mode-hook 'display-line-numbers-mode)
(setq inhibit-startup-message t)     ; No splash screen
(global-visual-line-mode t)

(set-frame-font "Consolas-10:bold" nil t)

(setq-default indent-tabs-mode nil)

(use-package ws-butler
  :hook (prog-mode . ws-butler-mode))

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
  (load-theme 'doom-acario-dark t)

  ;; customize the doom monkai theme
  (custom-set-faces
   '(counsel--mark-ring-highlight ((t (:inherit highlight))))
   ;; '(ivy-current-match ((t (:background "#fd971f" :foreground "black"))))
   '(show-paren-match ((t (:background "#FD971F" :foreground "black"
                                       :weight ultra-bold))))))

(use-package doom-modeline
  ;; :demand t
  :init (doom-modeline-mode 1))

(use-package all-the-icons
  :if (display-graphic-p))

(use-package all-the-icons-dired
  ;; :after all-the-icons
  :hook (dired-mode . all-the-icons-dired-mode)
  :config
  (setq all-the-icons-dired-monochrome nil))

;; (recentf-mode 1)

(use-package ivy
  ;; :diminish
  :bind (("C-s" . swiper)
         ;; ("C-c C-r" . ivy-resume)
         ;; ("<f6>" . ivy-resume)
         ("M-x" . counsel-M-x)
         ("C-x C-f" . counsel-find-file)
         ("C-x f" . counsel-find-file)
         ("C-c r" . counsel-recentf)    ; open recent file
         ("C-c f" . counsel-recentf)    ; open recent file
         ("C-c C-f" .  counsel-recentf)
         ("C-h d" . counsel-describe-function)
         ("C-h v" . counsel-describe-variable)
         ("C-h o" . counsel-describe-symbol)
         ("C-h l" . counsel-find-library)
         ("C-h i" . counsel-info-lookup-symbol)
         ("C-h u" . counsel-unicode-char)
         ("C-h b" . counsel-descbinds)
         ("C-x b" . counsel-switch-buffer)
         ("C-c t" . counsel-load-theme)
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
  :after counsel
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
  :bind (:map
         dired-mode-map
         ("h" . dired-up-directory)
         ("l" . dired-find-file)
         ("j" . dired-next-line)
         ("k" . dired-previous-line)
         ("J" . dired-goto-file)
         ("K" . kill-current-buffer))
  :config
  ;; on windows use git bash ls
  (setq insert-directory-program "C:\\Program Files\\Git\\usr\\bin\\ls")
  (setq ls-lisp-use-insert-directory-program t))

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
  :hook (ivy-mode . ivy-prescient-mode))

(use-package treemacs
  :defer t
  :custom (treemacs-python-executable "python")
  :config (treemacs-project-follow-mode))
;; (use-package treemacs-icons-dired
;;   :after dired
;;   :config (treemacs-icons-dired-mode))

(use-package rotate)

(use-package hydra
  :bind (("C-x w" . hydra-windows/body)
         ("C-c o" . hydra-other-window/body))
  )
;; hydra to condense other window commands
(defhydra hydra-other-window ()
  "other window commands"
  ("f" find-file-other-window "find file")
  ("b" counsel-switch-buffer-other-window "switch buffer"))
;; Hydra for managing buffers
(defhydra hydra-windows (:hint nil)
  "
^Move^       ^Split^           ^Delete^             ^Shift^      ^Misc^
^^^^^^^^----------------------------------------------------------------------------------
_i_: up      _v_: vertical     _o_: other windows   _I_: up      _r_: rotate layout  _g_: refresh
_k_: down    _h_: horizontal   _d_: this window     _K_: down    _b_: switch buffer
_j_: left    ^ ^               ^ ^                  _J_: left    _f_: find file
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
  ("r" rotate-layout)
  ("b" counsel-switch-buffer)
  ("f" counsel-find-file)
  ("p" project-switch-project)
  ("g" revert-buffer-quick)
  ("q" nil "quit"))

(use-package lsp-mode
  ;; :init
  ;; (setq lsp-keymap-prefix "C-x l")
  :commands (lsp lsp-deferred)
  ;; :init
  ;; (setq lsp-keymap-prefix "C-c l")
  :hook
  (js-mode . lsp-deferred)
  (terraform-mode . lsp-deferred)

  :custom
  ;; (lsp-terraform-server "C:/Users/GrishaKhachaturyan/stand_alone_prgrms/bin/terraform-lsp")
  (lsp-terraform-ls-server
   "C:/Users/GrishaKhachaturyan/.vscode/extensions/hashicorp.terraform-2.25.1-win32-x64/bin/terraform-ls"
   )
  :init
  (setq lsp-keymap-prefix "C-x l")
  :config
  ;; (setq lsp-disabled-clients '(tfls))
  (lsp-enable-which-key-integration t)
  (setq lsp-diagnostics-provider :none)
  (setq lsp-modeline-diagnostics-enable nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :config
  ;; (setq lsp-eldoc-enable-hover nil)
  (setq lsp-ui-enable-hover nil)
  (setq lsp-ui-doc-show-with-cursor t)
  (setq lsp-ui-doc-position 'bottom)
  (setq lsp-signature-auto-activate nil)
  (setq lsp-signature-render-documentation nil))

;; (use-package sideline
;;   :after lsp-mode

;;   :init
;;   (setq sideline-backends-right '(sideline-lsp)))

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
  :hook
  (prog-mode . company-mode)            ; add completion to programming language modes
  ;; (org-mode . company-mode)            ; add completion to org-mode
  )
;; :config
;; (add-hook 'after-init-hook 'global-company-mode)

(use-package company-box          ; Show icons in company complettions
  :hook (company-mode . company-box-mode))

(use-package flycheck
  :custom
  (flycheck-python-pycompile-executable "python")
  (flycheck-python-pylint-executable "python")
  (flycheck-python-pyright-executable "python")
  (flycheck-python-mypy-executable "python")
  (flycheck-python-flake8-executable "python")
  :config
  ;; (global-flycheck-mode)
  )

(use-package iedit)

;; (use-package yasnippet
;;   :config (yas-global-mode 1))

(use-package tree-sitter
  :config
  (require 'tree-sitter))

(use-package tree-sitter-langs
  :config
  (require 'tree-sitter)
  :hook ('python-mode . tree-sitter-hl-mode))

(use-package cc
  :ensure nil
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

;; (use-package sclang-extensions)

(use-package python
  :ensure nil
  :custom
  ;; python config
  (dap-python-executable "python")
  (dap-python-debugger 'debugpy)

  ;; :bind ( :map python-mode-map
  ;;         ("C-c r" . nil))
  :hook
  (python-mode . lsp-deferred)
  :config
  (require 'dap-python)                ; also not stopping at breakpoints. look at upgrading
  ;; (setq py-python-command "python3")
  ;; (setq py-shell-name "python")
  (setq python-shell-interpreter "python")
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
         :console "integratedterminal"  ; launches vterm
         :request "launch"
         :name "Python :: Run file User Input (buffer)")))

;; fix run-python codec errors on windows
(setenv "LANG" "en_US.UTF-8")
(setenv "PYTHONIOENCODING" "utf-8")

(use-package pyvenv)

(use-package terraform-mode
  :defer t)

(use-package markdown-preview-mode)

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
              ;; ("C-c C-p" . hydra-org/body)
              ;; ("C-c C-n" . hydra-org/body)
              ("M-n" . org-metadown)
              ("M-p" . org-metaup))
  :custom
  (org-priority-highest 65)
  (org-priority-lowest 68)
  (org-priority-default 67)
  :config
  ;; Org Agenda
  (setq org-agenda-span 'day)
  (setq org-agenda-include-diary t)
  ;; Add graphical timeline to org agenda
  (add-hook 'org-agenda-finalize-hook 'org-timeline-insert-timeline :append)
  (setq org-agenda-files
        '("~/hub/ripl/orgs/tickets.org"
          "~/hub/ripl/orgs/my_tasks.org"
          ;; "~/hub/new_projects/orgi/orgi_plan.org"
          ;; "~/hub/recording_bullet_journal/super_collider_projects/sc_bujo.org"
          ;; "~/.emacs.d/config.org"
          ))
  (setq org-todo-keywords
        '((sequence "BACKLOG" "TODO(t)" "NEXT(n)" "RECUR(r)" "TEST(s)" "|" "DONE(d!)")))
  (setq org-agenda-start-with-log-mode t)
  (setq org-log-done 'time)
  (setq org-log-into-drawer t)

  (setq org-startup-indented t)
  (custom-set-faces
   '(org-level-1 ((t (:inherit outline-1 :height 1.20))))
   '(org-level-2 ((t (:inherit outline-2 :height 1.10))))
   '(org-level-3 ((t (:inherit outline-3 :height 1.07))))
   '(org-level-4 ((t (:inherit outline-4 :height 1.05))))
   '(org-level-5 ((t (:inherit outline-5 :height 1.00))))
   )

  (add-to-list 'org-structure-template-alist '("el" . "src emacs-lisp"))

  (defhydra hydra-org (org-mode-map "C-c")
    "org hydra"
    ("C-n" org-next-visible-heading "next heading")
    ("C-p" org-previous-visible-heading "prev heading")
    ("M-j" org-metadown "move down")
    ("M-k" org-metaup "move up")
    ("q" nil "quit"))
  )
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
  (org-roam-setup))

(use-package visual-fill-column
  :hook
  (visual-fill-column-mode . (lambda () (setq visual-fill-column-width 80))))

(use-package org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

(use-package org-pomodoro
    :after org
    :config
    (setq org-pomodoro-short-break-length 7)
    (setq org-pomodoro-ticking-sound-p nil)
    (setq org-pomodoro-manual-break t))

;; The following fixes sounds not working on windows
(use-package sound-wav)
(use-package powershell)

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
   ("k" . kill-current-buffer)))

(use-package buffer-move
  :bind (("C-c b l" . buf-move-right)
         ("C-c b j" . buf-move-left)
         ("C-c b i" . buf-move-up)
         ("C-c b k" . buf-move-down)))

(use-package dashboard
  :demand t
  ;; :after page-break-lines
  :config
  (setq line-move-visual nil)
  (setq dashboard-startup-banner 'logo)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-center-content nil)
  (setq dashboard-projects-backend 'project-el)
  (setq dashboard-items '((agenda . 6)
                          (projects . 7)
                          (recents . 7)
                          ;; (bookmarks . 3)
                          ))
  (setq dashboard-page-separator "\n\f\n")
  (setq dashboard-agenda-sort-strategy '(time-up))
  (setq dashboard-agenda-time-string-format "%b %d %Y %a ")
  (dashboard-setup-startup-hook))

(use-package page-break-lines
  :demand t
  :config (page-break-lines-mode))

(use-package savehist
  :after counsel
  :init
  (savehist-mode 1)
  (setq history-length 25))

;; Set Garbage collection threshold back down after startup completes
(add-hook 'emacs-startup-hook
          (lambda ()
            (setq gc-cons-threshold (* 2 1000 1000))
            (setq file-name-handler-alist default-file-name-handler-alist)))
;; (setq gc-cons-threshold (* 2 1000 1000))
