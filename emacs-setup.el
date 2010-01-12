;; actually this depends on where and how emacs is called...
(let ((f (selected-frame)))
  (set-frame-size f 210 80)
  (set-frame-position f 40 20))

(defun my-add-load-path (subdir)
  "adds a local path"
  (add-to-list 'load-path (concat base-load-path "/" subdir)))

(mapc 'my-add-load-path '("icicles" "egg" "org-mode/lisp" "org-mode/contrib/lisp"
			  "cedet-1.0pre6/common"))

;;; This was installed by package-install.el.
;;; This provides support for the package system and
;;; interfacing with ELPA, the package archive.
;;; Move this code earlier if you want to reference
;;; packages in your .emacs.
(when
    (load
     (expand-file-name "~/elisp/elpa/package.el"))
  (package-initialize))


;;__________________________________________________________________________
;;;; modules

(load "icicles")
(icy-mode)

(require 'compile)
(require 'ssh)
(require 'egg)

;;org-mode
(require 'org-install)

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
(require 'eshell)
(add-hook 'eshell-mode-hook
          (lambda ()
            (local-set-key (kbd "C-z") 'bury-buffer)
            (add-to-list 'eshell-visual-commands "ssh")))

(eshell) ; for some reason some of the custom functions were not recognised
         ; if ehsell wasn't started yet

(defun eshell/o (&rest args)
  "open file(s) in other windows"
  (if (null args)
      (bury-buffer)
    (mapc #'find-file-other-window
          (mapcar #'expand-file-name (eshell-flatten-list (reverse args))))))

(defun eshell/v (&rest args)
  "view file(s)"
  (mapc #'view-file
        (mapcar #'expand-file-name (eshell-flatten-list (reverse args)))))

(defun eshell/make (&rest args)
  (compile (eshell-flatten-and-stringify (cons "make" args))))

(defun eshell/impala (&rest args)
  (impala-run (eshell-flatten-and-stringify args)))
;;__________________________________________________________________________
;;; c and cc mode
;;cedet
(unless (equal location 'home)
  (load-file "~/elisp/cedet-1.0pre6/common/cedet.el")
  (require 'ede)
  (global-ede-mode 1)
  (ede-cpp-root-project "Samples" :file "~/impala/build/gmake/Samples/Makefile")
  (semantic-load-enable-gaudy-code-helpers)
  (semantic-add-system-include "~/impala/" 'c++-mode)
  (define-key senator-mode-map "\C-cv" 'semantic-decoration-include-visit)
  (define-key senator-mode-map [(control shift n)] 'senator-next-tag)
  (define-key senator-mode-map [(control shift p)] 'senator-previous-tag)
  (define-key senator-mode-map (kbd "C-<")  'senator-fold-tag)
  (define-key senator-mode-map (kbd "M-C-<")  'semantic-tag-folding-fold-all)
  (define-key senator-mode-map (kbd "C->")  'senator-unfold-tag))

;;general c stuff
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
;; clojure
(defun find-jars-in (folder-list)
  (mapcan (lambda (folder)
            (when (file-directory-p folder)
              (directory-files folder t ".jar$")))
          folder-list))

;;; eval after load?
(require 'swank-clojure)
(setq swank-clojure-extra-classpaths (append swank-clojure-extra-classpaths
                                             '("~/coding/clj/penumbra"
                                               "~/coding/clj/penumbra/src")
                                             (find-jars-in '("~/coding/clj/penumbra/lib/")))
      swank-clojure-library-paths (append swank-clojure-library-paths
                                          '("~/coding/clj/penumbra/native/macosx/x86")))

;;;;;;;;;; in order to have different commands for clojure-master and clojure-new
;; we could make different slime commands: then we have to set the program
;; commands ourselves... (see value of var)
;; (setq slime-lisp-implementations
;;       '(
;;         (clojure ("/Users/pinochle/bin/clojure") :init swank-clojure-init)))

;;__________________________________________________________________________
;;; general settings
(setq indent-tabs-mode nil) ; never use tabs for indentation (instead use spaces)
(delete-selection-mode 1) ; Typing will delete the selection
(setq truncate-partial-width-windows nil) ; No truncation of lines in all
                                          ; windows less than full frame wide
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

(defun word-count ()
  (interactive)
  (message "Word count: %s" (how-many "\\w+" (point-min) (point-max))))

(defun kill-ring-save-line (arg)
  (interactive "p")
  (let (start end)
    (if (< arg 0)
        (setq start (+ arg 2) end 2)
      (setq start 1 end (+ arg 1)))
    (kill-ring-save (line-beginning-position start) (line-beginning-position end))
    end))

(defun duplicate-line (arg)
  (interactive "p")
  (beginning-of-line (kill-ring-save-line arg))
  (yank '(1)))

(defun kill-whole-line (arg)
  (interactive "p")
  (kill-line (if (< arg 0) (+ arg 1) 0))
  (kill-line (if (< arg 1) 1 arg)))

;;______________________________________
;;; window and buffer functions


(defun switch-to-buffer-or-window (buffer-name &optional other-window)
  "If the buffer is active in a window, select that window.
Otherwise switch to the buffer.

If other-window is t the buffer will be opened in another window."
  (interactive "Bswitch to buffer:\nP")
  (let ((already-open (get-buffer-window buffer-name))) 
    (if already-open
        (select-window already-open)
      (if other-window
          (select-window (get-buffer-window (switch-to-buffer-other-window buffer-name)))
        (switch-to-buffer buffer-name)))))

(defun recompile-other-window ()
  "find last compilation and recompile, error if there was no
previous compilation"
  (interactive)
  (progn
    (switch-to-buffer-or-window "*compilation*" t)
    (if (string= mode-name "Compilation")
        (recompile)
      (progn
        (message "couldn't find previous compilation")
        (kill-buffer)
        (eshell)))))

(defvar recent-code-buffer-list-depth 0)
(defvar recent-special-buffer-list-depth 0)

(defun switch-to-impala-buffer ()
  "switch to buffer named *impala* if it exists"
  (interactive)
  (switch-to-buffer-or-window "*impala*" nil)
  (unless (string= mode-name "impala")
    (kill-buffer)))

(defun switch-to-orgmode-buffer ()
  "switch to buffer named impala.org if it exists"
  (interactive)
  (switch-to-buffer-or-window "impala.org" nil)
  (unless (string= mode-name "Org")
    (kill-buffer)))

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

;;______________________________________
;;; 'workspace' functions

(defun workspace-git ()
  (interactive)
  (progn
    (cd "~/impala")
    (egg-status)
    (switch-to-buffer-or-window "*impala-status@/home4/mliempt/impala/.git*")
    (delete-other-windows)
    (switch-to-buffer-or-window "*impala-log@/home4/mliempt/impala/.git*" t)))

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

(global-set-key "\r" 'newline-and-indent)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c l") 'org-store-link)
(global-set-key (kbd "C-c a") 'org-agenda)
(global-set-key (kbd "C-z") 'eshell)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key [(meta shift z)] 'zap-up-to-char-back)
(global-set-key [(meta shift d)] 'kill-whole-line)
(global-set-key [(control x) (control shift b)]  'switch-to-last-hidden-file)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key [(control meta shift u)] 'up-list)
(global-set-key [(control meta shift k)] 'duplicate-line)
(global-set-key [(meta shift k)] 'kill-ring-save-line)
(global-set-key (kbd "<f5> <f5>") 'recompile-other-window)
(global-set-key (kbd "<f5> 1") 'workspace-coding)
(global-set-key (kbd "<f5> 2") 'workspace-git)
(global-set-key (kbd "<f5> 3") 'workspace-run)
(global-set-key (kbd "<f5> i") 'switch-to-impala-buffer)
(global-set-key (kbd "<f5> o") 'switch-to-orgmode-buffer)
(global-set-key (kbd "<f5> c") 'find-recent-code-buffer)
(global-set-key (kbd "<f5> s") 'find-recent-special-buffer)
(global-set-key (kbd "<f5> e") 'egg-status)
