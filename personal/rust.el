(prelude-require-package 'racer)
(prelude-require-package 'flycheck-rust)

(if (eq system-type 'gnu/linux) (setq racer-cmd "~/Dropbox/Apps/racer_linux/target/release/racer"))
(if (eq system-type 'darwin) (setq racer-cmd "~/Dropbox/Apps/racer/target/release/racer"))
(setq racer-rust-src-path "/usr/local/include/rust/src")
(eval-after-load "rust-mode" '(require 'racer))
(setq rust-indent-method-chain "rust-align-to-method-chain" rust-indent-offset 2)

(add-hook 'rust-mode-hook 
  '(lambda () 
     (racer-activate)
     (racer-turn-on-eldoc)
     (add-hook 'flycheck-mode-hook #'flycheck-rust-setup)
     (local-set-key (kbd "C-c .") #'racer-find-definition)
     (setq company-tooltip-align-annotations t)
     (local-set-key (kbd "TAB") #'company-indent-or-complete-common)))
