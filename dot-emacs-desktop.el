(defconst location 'desktop)
(defconst base-load-path "~/elisp")
(add-to-list 'load-path base-load-path)
(add-to-list 'load-path (concat base-load-path "/mystuff"))
(load "emacs-setup")

;;__________________________________________________________________________
;;;; customization
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-fill-mode t)
 '(auto-save-default nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-under-as-char t)
 '(current-language-environment "English")
 '(fill-column 80)
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
 '(icicle-download-dir "c:/elisp/icicles")
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(org-hide-leading-stars t)
 '(org-level-color-stars-only nil)
 '(pulse-delay 0.01)
 '(pulse-iterations 3)
 '(semantic-idle-work-parse-neighboring-files-flag nil)
 '(tab-always-indent nil)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(transient-mark-mode t))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:stipple nil :background "SystemWindow" :foreground "SystemWindowText" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :family "outline-ProggyCleanTT"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Brown"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "blue"))))
 '(bold-italic ((t (:slant italic :weight bold :family "courier"))))
 '(italic ((((supports :underline t)) (:slant italic :family "courier")))))
