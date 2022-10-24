(org-babel-load-file
 (expand-file-name
  "config.org"
  user-emacs-directory))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes
   '("a138ec18a6b926ea9d66e61aac28f5ce99739cf38566876dc31e29ec8757f6e2" default))
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")
     ("el" . "src emacs-lisp")))
 '(package-selected-packages
   '(yasnippet iedit buffer-move w3m org-roam-ui projectile beacon org-roam helpful ivy-rich rainbow-delimiters vterm page-break-lines all-the-icons org-superstar company company-mode doom-modeline use-package))
 '(warning-suppress-types '(((python python-shell-completion-native-turn-on-maybe)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(counsel--mark-ring-highlight ((t (:inherit highlight))))
 '(ivy-current-match ((t (:background "#fd971f" :foreground "black")))))
(put 'erase-buffer 'disabled nil)
