;; actually this depends on where and how emacs is called...
(let ((f (selected-frame)))
  (set-frame-size f 210 80)
  (set-frame-position f 40 60))

;;__________________________________________________________________________
;;;; modules
(add-to-list 'load-path "~/.emacs.d")
(add-to-list 'load-path "~/.emacs.d/icicles")
(load "icicles")
(icy-mode)

(require 'compile)
(require 'eshell)
(require 'ssh)
(add-to-list 'load-path "~/.emacs.d/egg")
(require 'egg)

;;org-mode
(add-to-list 'load-path "~/.emacs.d/org-6.24b/lisp")
(add-to-list 'load-path "~/.emacs.d/org-6.24b/contrib/lisp")
(require 'org-install)

;;cedet
(load-file "~/.emacs.d/cedet-1.0pre6/common/cedet.el")
(require 'ede)
(global-ede-mode 1)
(ede-cpp-root-project "Samples" :file "~/impala/build/gmake/Samples/Makefile")
(semantic-load-enable-gaudy-code-helpers)
(semantic-add-system-include "~/impala/" 'c++-mode)

(require 'impala)

;;__________________________________________________________________________
;; paredit
(require 'paredit)
(defun enable-paredit-hook () (paredit-mode 1))
(mapc (lambda (mode)
        (let ((hook (intern (concat (symbol-name mode) "-mode-hook"))))
          (add-hook hook 'enable-paredit-hook)))
      '(emacs-lisp lisp inferior-lisp clojure))

;;__________________________________________________________________________
;;;; eshell
(defun eshell/emacs (&rest args)
  "open file(s) in other windows"
  (if (null args)
      (bury-buffer)
    (mapc #'find-file-other-window
          (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

;; for some reason eshell must be called before this defun, otherwise eshell
;; still doesn't know what to do
(defun eshell/make (&rest args)
  (compile (eshell-flatten-and-stringify (cons "make" args))))

(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-z") 'bury-buffer)
            (add-to-list 'eshell-visual-commands "ssh")))

(defun eshell/impala (&rest args)
  (impala-run (eshell-flatten-and-stringify args)))
;;__________________________________________________________________________
;;; c and cc mode
(setq c-mode-hook nil)
(add-hook 'c-mode-common-hook
          (lambda ()
            (turn-on-font-lock)
            (setq c-basic-offset 4)
            (c-set-offset 'substatement-open 0)
            (imenu-add-to-menubar "Functions")))
(add-hook 'c++-mode-hook 
          (lambda ()
            (setq c-basic-offset 4)))
(setq c-recognize-knr-p nil) ; speedup: dont look for old-style functions

;;__________________________________________________________________________
;;; general settings
(setq indent-tabs-mode nil) ; never use tabs for indentation (instead use spaces)
(delete-selection-mode 1) ; Typing will delete the selection
(setq truncate-partial-width-windows nil) ; No truncation of lines in all windows less than full frame wide
(setq truncate-lines nil)
(fset 'yes-or-no-p 'y-or-n-p)

;;__________________________________________________________________________
;;;; modes
(setq auto-mode-alist (append '(("\\.[Hh][Tt][Mm][Ll]$" . sgml-mode))
                              '(("\\.menu$" . fundamental-mode))
                              '(("\\.clj" . clojure-mode))
                              '(("\\.org$" . org-mode))
                              '(("\\.m_.*" . fundamental-mode))
                              '(("\\.clp$" . emacs-lisp-mode))
                              '(("\\.xsl" . sgml-mode))
                              '(("\\.svg" . sgml-mode))
                              '(("\\.c" . c++-mode))
                              '(("\\.h" . c++-mode))
                              '(("\\.m" . octave-mode))
                              '(("[Mm]ake*.*" . makefile-mode))
                              auto-mode-alist))


;;__________________________________________________________________________
;;;; utilities
(defun zap-up-to-char (arg char)
  "Kill up to, but not including ARGth occurence of CHAR"
  (interactive "p\ncZap up to char: ")
  (let ((direction (if (>= arg 0) 1 -1)))
    (kill-region (point)
                 (progn
                   (forward-char direction)
                   (unwind-protect
                       (search-forward (char-to-string char) nil nil arg)
                     (backward-char direction))
                   (point)))))

(defun zap-up-to-char-back (arg char)
  "Kill backwards to, but not including ARGth occurence of CHAR"
  (interactive "p\ncZap back to char: ")
  (zap-up-to-char (- arg) char))

(defun workspace-run ()
  (interactive)
  (progn
    (delete-other-windows)
    (eshell)
    (split-window-horizontally (let ((w (window-width)))
                                 (if (> w 150)
                                     (- w 50)
                                   (* 2 (/ w 3)))))
    (split-window-vertically -10)
    (other-window 1)
    (switch-to-buffer "*ssh*")
    (unless (string= mode-name "Term")
      (kill-buffer)
      (eshell-command "ssh node445"))
    (other-window 1)
    (find-file "~/VideoSearch/voc2007devel/surf-forest")
    (other-window 1)))

(defun workspace-git ()
  (interactive)
  (progn
    (cd "~/impala")
    (egg-status)
    (select-window
     (or (get-window-with-predicate
          (lambda (window)
            (save-current-buffer
              (set-buffer (window-buffer window))
              (string= mode-name "Egg-Status"))))
         (progn
           (error "egg window didn't open?")
           select-window))
     )
    (delete-other-windows)))


(defun recompile-other-window ()
  "find last compilation and recompile, error if there was no
previous compilation"
  (interactive)
  (progn
    (select-window (or (get-buffer-window "*compilation*")
                       (get-buffer-window (switch-to-buffer-other-window "*compilation*"))))
    (if (string= mode-name "Compilation")
        (recompile)
      (progn
        (message "couldn't find previous compilation")
        (kill-buffer)
        (eshell)))))

(defvar recent-code-buffer-list-depth 0)
(defvar recent-special-buffer-list-depth 0)

(defun find-recent-special-buffer ()
  "cycle through list of buffer and return first one with code
that is not displayed. If last n commands were also
find-recent-code-buffer then take the n+1th buffer"
  (interactive)
  (if (equal last-command 'find-recent-special-buffer)
      (incf recent-special-buffer-list-depth)
    (setq recent-special-buffer-list-depth 1))
  (let ((old-case case-fold-search))
    (setq case-fold-search nil)
    (let ((special-buffer-list (remove-if-not
                                (lambda (buf) (string-match "^\\*[a-z].*\\*$"
                                                            (buffer-name buf)))
                                (buffer-list))))
      (if (>= recent-special-buffer-list-depth (length special-buffer-list))
          (setq recent-special-buffer-list-depth 1))
      (switch-to-buffer (nth recent-special-buffer-list-depth special-buffer-list)))))

(defun find-recent-code-buffer ()
  "cycle through list of buffer and return first one with code
that is not displayed. If last n commands were also
find-recent-code-buffer then take the n+1th buffer"
  (interactive)
  (if (equal last-command 'find-recent-code-buffer)
      (incf recent-code-buffer-list-depth)
    (setq recent-code-buffer-list-depth 1))
  (let ((code-buffer-list (remove-if-not
                           (lambda (buf) (string-match ".emacs\\|.*\\(h\\|c\\|cpp\\|hh\\|el\\)$"
                                                       (buffer-name buf)))
                           (buffer-list))))
    (if (>= recent-code-buffer-list-depth (length code-buffer-list))
        (setq recent-code-buffer-list-depth 1))
    (switch-to-buffer (nth recent-code-buffer-list-depth code-buffer-list))))

(global-set-key (kbd "<f6> c") 'find-recent-code-buffer)
(global-set-key (kbd "<f6> s") 'find-recent-special-buffer)


(defun last-hidden-file-buffer ()
  (let ((list (buffer-list))
        buf
        file-buf)
    (while list
      (setq buf (car list))
      (if (and (buffer-file-name buf)
               (null (get-buffer-window buf t)))
          (setq file-buf buf))
      (setq list (if file-buf () (cdr list))))
    file-buf))

(defun switch-to-last-hidden-file ()
  (interactive)
  (let ((buf (last-hidden-file-buffer)))
    (switch-to-buffer (or buf *scratch*))))

(defun workspace-coding ()
  (interactive)
  (progn
    (cd "~/impala")
    (switch-to-buffer "impala.org")
    (unless (string= mode-name "Org")
      (kill-buffer)
      (find-file "impala.org"))
    (delete-other-windows)
    (split-window-horizontally)
    (split-window-vertically)
    (other-window 1)
    (switch-to-last-hidden-file)
    (other-window 1)
    (split-window-vertically)
    (switch-to-last-hidden-file)
    (other-window 1)
    (switch-to-last-hidden-file)))
;;__________________________________________________________________________
;;;; key bindings

(global-set-key (kbd "\e \e 1") 'workspace-coding)
(global-set-key (kbd "\e \e 2") 'workspace-git)
(global-set-key (kbd "\e \e 3") 'workspace-run)
(global-set-key "\C-z" 'eshell)
(global-set-key "\M-z" 'zap-up-to-char)
(global-set-key [(meta shift z)] 'zap-up-to-char-back)
(global-set-key "\r" 'newline-and-indent)
(global-set-key "\C-cc" 'comment-region)
(global-set-key "\C-cv" 'semantic-decoration-include-visit)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key [(control x) (control shift b)]  'switch-to-last-hidden-file)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key [(control meta shift u)] 'up-list)
(global-set-key [(f5) (f5)] 'recompile-other-window)

(define-key senator-mode-map [(control shift n)] 'senator-next-tag)
(define-key senator-mode-map [(control shift p)] 'senator-previous-tag)
(define-key senator-mode-map (kbd "C-<")  'senator-fold-tag)
(define-key senator-mode-map (kbd "M-C-<")  'semantic-tag-folding-fold-all)
(define-key senator-mode-map (kbd "C->")  'senator-unfold-tag)

;;__________________________________________________________________________
;;;; customization
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
 '(global-semantic-tag-folding-mode t nil (semantic-util-modes))
 '(icicle-download-dir nil)
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
 '(tool-bar-mode nil))

(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 120 :width normal :foundry "windows" :family "proggyclean"))))
 '(bold-italic ((t (:slant italic :weight bold :family "courier"))))
 '(font-lock-builtin-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-comment-face ((((class color) (background light)) (:foreground "DarkGreen"))))
 '(font-lock-constant-face ((((class color) (background light)) (:foreground "blue"))))
 '(font-lock-keyword-face ((((class color) (background light)) (:foreground "Blue"))))
 '(font-lock-string-face ((((class color) (background light)) (:foreground "Brown"))))
 '(font-lock-type-face ((((class color) (background light)) (:foreground "blue"))))
 '(italic ((((supports :underline t)) (:slant italic :family "courier")))))