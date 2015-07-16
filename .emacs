(require 'package)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)

(defun ensure-package-installed (&rest packages)
  "Assure every package is installed, ask for installation if it’s not.

Return a list of installed packages or nil for every skipped package."
  (mapcar
   (lambda (package)
     ;; (package-installed-p 'evil)
     (if (package-installed-p package)
         nil
       (if (y-or-n-p (format "Package %s is missing. Install it? " package))
           (package-install package)
         package)))
   packages))

;; make sure to have downloaded archive description.
;; Or use package-archive-contents as suggested by Nicolas Dudebout
(or (file-exists-p package-user-dir)
    (package-refresh-contents))

(ensure-package-installed 'auto-complete 'auto-complete-auctex 'yasnippet 'auto-complete-c-headers 'cmake-mode 'rust-mode 'auctex 'company 'multiple-cursors 'neotree 'jedi) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-auctex)
(require 'company)
(require 'multiple-cursors)
(require 'neotree)

(ac-config-default)
(global-auto-complete-mode t)
;; (add-to-list 'ac-modes 'c++-mode)

(require 'yasnippet)
(yas-global-mode 1)

;; OSX Paste issue
(defun pt-pbpaste ()
	"Paste data from pasteboard."
	(interactive)
	(shell-command-on-region
	 (point)
	 (if mark-active (mark) (point))
	 "pbpaste" nil t))

(defun pt-pbcopy ()
	"Copy region to pasteboard."
	(interactive)
	(print (mark))
	(when mark-active
		(shell-command-on-region
		 (point) (mark) "pbcopy")
		(kill-buffer "*Shell Command Output*")))

;; C/C++ Related
(defun my:ac-c-header-init ()
  (require 'auto-complete-c-headers)
  (add-to-list 'ac-sources 'ac-source-c-headers)
  (add-to-list 'achead:include-directories '"/usr/include /usr/local/include /Applications/Xcode.app/Contents/Developer/Toolchains/XcodeDefault.xctoolchain/usr/include/c++/v1/"))
(add-hook 'c++-mode-hook 'my:ac-c-header-init)
(add-hook 'c-mode-hook 'my:ac-c-header-init)
(setq c-default-style "linux" c-basic-offset 2)

;; slime / quicklisp related
;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
;; (setq inferior-lisp-program "/usr/local/bin/sbcl")

;; Generate buffer
(defun generate-buffer ()
	(interactive)
	(switch-to-buffer (make-temp-name "scratch")))

;; Rust mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq racer-rust-src-path "/usr/local/include/rust/src")
(setq racer-cmd "~/Dropbox/Apps/racer/target/release/racer")
(add-to-list 'load-path "~/Dropbox/Apps/racer/editors/emacs")
(eval-after-load "rust-mode" '(require 'racer))
(setq rust-indent-method-chain "rust-align-to-method-chain" rust-indent-offset 2)

;; Latex related
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)

;; Python related
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(jedi:install-server)

;; Eshell related
(add-hook 'eshell-mode-hook
	  '(lambda nil
	     (eshell/export "EDITOR=emacsclient -n")
	     (eshell/export "VISUAL=emacsclient -n")
	     (add-to-list 'eshell-visual-commands "htop" "python")))

;; Multiple cursor related
;(global-set-key (kbd "C-c C-c") 'mc/edit-lines)
;TODO: This is required, but wtf! (setq transient-mark-mode nil)
(global-set-key (kbd "C-c C-c") 'mc/mark-pop)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this)

;; Generic
(show-paren-mode 1)
(electric-pair-mode 1)
(setq shell-file-name "bash")
(global-set-key [f1] 'eshell)
(global-set-key [f8] 'neotree-toggle)
(global-set-key [f2] 'generate-buffer)
(global-set-key [?\C-x ?\C-y] 'pt-pbpaste)
(global-set-key [?\C-x ?\M-w] 'pt-pbcopy)
(setq tramp-default-method "ssh")
(setq-default tab-width 2)
(put 'erase-buffer 'disabled nil)
(setq make-backup-files nil)
