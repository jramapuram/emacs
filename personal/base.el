;; get rid of weird undo-tree and fix tab width
(global-undo-tree-mode 0)
(setq mac-option-modifier 'super)
(setq mac-command-modifier 'meta)

;; remove those shitty ^M 's
(defun remove-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
(add-hook 'text-mode-hook 'remove-dos-eol)
(add-hook 'c++-mode-hook 'remove-dos-eol)
(add-hook 'c-mode-hook 'remove-dos-eol)

;; parse cuda properly
(add-to-list 'auto-mode-alist '("\\.cu\\'" . c++-mode))

;; python remaps
(add-hook 'python-mode-hook
          '(lambda ()
             (setq python-shell-virtualenv-path "~/.venv")
             (define-key anaconda-mode-map (kbd "M-,") 'anaconda-mode-go-back)
             (define-key anaconda-mode-map (kbd "M-*") 'anaconda-mode-find-assignments)))
