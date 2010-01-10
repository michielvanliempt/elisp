;;;; general
;; for compatibility with older Aquamacs versions
(defvar aquamacs-140-custom-file-upgraded t)
(unless (fboundp 'auto-detect-longlines) (defun auto-detect-longlines () t))

(defconst location 'home)
(defconst base-load-path "~/elisp")
(add-to-list 'load-path base-load-path)
(add-to-list 'load-path (concat base-load-path "/mystuff"))
(load "emacs-setup")

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(auto-save-default nil)
 '(case-fold-search t)
 '(column-number-mode t)
 '(cperl-continued-statement-offset 4)
 '(cperl-indent-level 4)
 '(cperl-under-as-char t)
 '(fill-column 80)
 '(global-auto-revert-mode t nil (autorevert))
 '(global-font-lock-mode t nil (font-lock))
 '(icicle-download-dir nil)
 '(indent-tabs-mode nil)
 '(make-backup-files nil)
 '(org-hide-leading-stars t)
 '(org-level-color-stars-only nil)
 '(pulse-delay 0.01)
 '(pulse-iterations 3)
 '(tab-always-indent nil)
 '(tab-stop-list (quote (4 8 12 16 20 24 28 32 36 40 44 48 52 56 60)))
 '(tab-width 4)
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 110 :width normal :foundry "apple" :family "ProggyClean"))))
 '(bold-italic ((t (:slant italic :weight bold :family "courier"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Brown"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "blue"))))
 '(italic ((((supports :underline t)) (:slant italic :family "courier")))))


;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/elisp/elpa/package.el"))
  (package-initialize))
