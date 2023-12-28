;; Set Garbage collection threshold very high at startup
(setq gc-cons-threshold (* 100 1000 1000))
;; For lsp-mode:
;;  Increase the amount of data which Emacs reads from the process
(setq read-process-output-max (* 1024 1024))

(defvar default-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)
