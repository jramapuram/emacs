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

(setq jr-packages  '(auto-complete
		   auto-complete-auctex
		   yasnippet
		   auto-complete-c-headers
		   cmake-mode
		   rust-mode
		   auctex
		   company
		   multiple-cursors
		   neotree
		   racer
		   helm
		   jedi))
(dolist (pkg jr-packages)
  (ensure-package-installed pkg)) ;  --> (nil nil) if iedit and magit are already installed

;; activate installed packages
(package-initialize)

(require 'auto-complete)
(require 'auto-complete-config)
(require 'auto-complete-auctex)
(require 'company)
(require 'multiple-cursors)
(require 'neotree)
(require 'helm)
(require 'helm-config)

(ac-config-default)
(global-auto-complete-mode t)
;; (add-to-list 'ac-modes 'c++-mode)

;; Yasnipped related
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

;; Auto update packages
(when (not package-archive-contents)
  (package-refresh-contents))
(dolist (pkg jr-packages)
  (when (and (not (package-installed-p pkg))
           (assoc pkg package-archive-contents))
    (package-install pkg)))

;; Generate buffer
(defun generate-buffer ()
	(interactive)
	(switch-to-buffer (make-temp-name "scratch")))

;; Rust mode
(autoload 'rust-mode "rust-mode" nil t)
(add-to-list 'auto-mode-alist '("\\.rs\\'" . rust-mode))
(setq racer-rust-src-path "/usr/local/include/rust/src")
(setq racer-cmd "~/Dropbox/Apps/racer/target/release/racer")
;(add-to-list 'load-path "~/Dropbox/Apps/racer/editors/emacs")
(eval-after-load "rust-mode" '(require 'racer))
(setq rust-indent-method-chain "rust-align-to-method-chain" rust-indent-offset 2)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
(add-hook 'rust-mode-hook 
  '(lambda () 
     (racer-activate)
     (company-mode 1)
     (local-set-key (kbd "C-c .") #'racer-find-definition)
     (setq company-tooltip-align-annotations t)
     (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))

;; Latex related
(setq TeX-auto-save t)
(setq TeX-parse-self t)
(setq-default TeX-master nil)
(add-hook 'LaTeX-mode-hook 'visual-line-mode)
(add-hook 'LaTeX-mode-hook 'flyspell-mode)
(add-hook 'LaTeX-mode-hook 'LaTeX-math-mode)
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)
(setq reftex-plug-into-AUCTeX t)
(setenv "PATH" (concat "/Library/TeX/texbin:/usr/local/bin:" (getenv "PATH")))

;; Python related
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)
(jedi:install-server)

;; Helm related
;;(define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t)

(helm-mode 1)

;; ORG Mode related
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;; Eshell related
(add-hook 'eshell-mode-hook
	  '(lambda nil
	     (eshell/export "EDITOR=emacsclient -n")
	     (eshell/export "VISUAL=emacsclient -n")
	     (add-to-list 'eshell-visual-commands "python")))

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
(global-set-key [tab] 'tab-indent-or-complete)
(global-set-key (kbd "M-s M-s") 'rgrep)
(setq save-interprogram-paste-before-kill t)
(setq tramp-default-method "ssh")
(setq-default tab-width 2)
(put 'erase-buffer 'disabled nil)
(setq make-backup-files nil)
