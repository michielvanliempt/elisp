';;; impala.el capture and parse output of impala pipeline

;; code copied and adapted from compile

(require 'comint)
(define-key comint-mode-map "\C-ck" 'comint-kill-subjob)


;;;;============================================================================
;;;; faces, very repetitive; can macros output a defface, deffav and setq at the
;;;; same time?
(defgroup impala-faces nil
  "Faces for impala mode.")

(defmacro impala-make-face (name-part face-def)
  (let* ((s (symbol-name name-part))
         (face-name (intern (concat "impala-" s)))
         (var-name (intern (concat (symbol-name face-name) "-face"))))
    `(progn
       (defface ,face-name
         ',face-def
         "Face for impalamode."
         :group 'impala-faces)
       (defvar ,var-name)
       (setq ,var-name ',face-name))))

;; (macroexpand '(impala-make-face timestamp
;;                                 ((((class color) (background light))
;;                                   :foreground "grey60" :height 0.8)
;;                                  (((class color) (background dark))
;;                                   :foreground "grey40" :height 0.8))))

(impala-make-face debug ((((class color) (background light))
                          (:weight bold :foreground "dodger blue"))
                         (((class color) (background dark))
                          (:weight bold :foreground "blue"))))

(impala-make-face timestamp ((((class color) (background light))
                              :foreground "grey60" :height 0.8)
                             (((class color) (background dark))
                              :foreground "grey40" :height 0.8)))

(impala-make-face bullshit ((((class color) (background light))
                             :foreground "grey90" :height 0.8)
                            (((class color) (background dark))
                             :foreground "grey30" :height 0.8)))

(impala-make-face command-base ((((class color) (background light))
                              :foreground "navy" :inherit variable-pitch)
                             (((class color) (background dark))
                              :foreground "SteelBlue" :inherit variable-pitch)
                             (t)))

(impala-make-face command-large ((t :inherit impala-command-base :height 1.2))) 
(impala-make-face command-small ((t :inherit impala-command-base :height 0.8)))

;;;;============================================================================
;;;; regex's

(defun make-ilog-regexp (label)
  (concat "\\(" label "\\) +\\(\\[.*?\\]\\)\\(.*\\)"))
(defconst impala-debug-exp (make-ilog-regexp "DEBUG"))
(defconst impala-info-exp (make-ilog-regexp "INFO"))
(defconst impala-warning-exp (make-ilog-regexp "WARN"))
(defconst impala-error-exp (make-ilog-regexp "ERROR"))
(defconst impala-error-server-exp "ERROR \\[Impala.Util.Channel \\] SendRequest.*?:.*?:.*?-\\(.*\\)")
(defconst impala-timestamp-exp "\\(Mon\\|Tue\\|Wed\\|Thu\\|Fri\\|Sat\\|Sun\\).*?[0-9]+:[0-9]+:[0-9]+,[0-9]+")
(defconst impala-prun-cmd-exp "\\(prun .*?00:00\\) \\(\\w*\\) \\([0-9]+\\) \\(.+?\\)\\(--.*\\)")
(defconst impala-cmd-names-exp
  (regexp-opt (directory-files "~/impala/lib/x86_64-linux-gcc" nil "^[a-z]" nil)))
(defconst impala-local-cmd-exp (concat "\\(" impala-cmd-names-exp ".*?\\) \\(--.*\\)"))
(defconst impala-command-exp (concat impala-prun-cmd-exp "\\|" impala-local-cmd-exp))
(defconst impala-reservation-exp "Reservation number [0-9]+: Reserved [0-9]+ hosts for [0-9]+ seconds")
(defconst impala-run-on-exp "Run on [0-9]+ hosts for [0-9]+ seconds from .*")
(defconst impala-nodes-exp ":\\( node[0-9]+/[0-9]+\\)+")
(defconst impala-prun-error-exp "\\(Prun fatal error:\\)\\(.*\\)")
(defconst impala-separator-exp "^=*$")
(defconst impala-script-section-exp "^\\.\\.\\..*\\.\\.\\.$")

;; (string-match impala-debug-exp
;;               "Tue 11:06:38,431 DEBUG [Sandbox.Koen.InterestPointFeature ] start FindInterestPoints")
;; (let ((s "constructCodebook voc2007devel.txt none-surf_sc_3-2-4_clusterinput_2 Codebooks/none-surf.tab 4096 --dataServer node445.das3.science.uva.nl:12000"))
;;   (string-match impala-local-cmd-exp s)
;;   (print (match-string 1 s))
;;   (print (match-string 3 s)))
(let ((s "...compiling tests..."))
  (string-match impala-script-section-exp s))

(defvar exes (directory-files "~/impala/lib/x86_64-linux-gcc" nil "^[a-z]" nil))


;;;;============================================================================
;;;; syntax highlighting

(defvar impala-font-lock-keywords nil "font lock keywords")
(setq impala-font-lock-keywords
      `((,impala-warning-exp (1 compilation-warning-face) (2 impala-bullshit-face)
                             (3 compilation-warning-face))
        (,impala-error-server-exp 1 compilation-error-face)
        (,impala-error-exp (1 compilation-error-face) (2 impala-bullshit-face)
                           (3 compilation-error-face))
        (,impala-info-exp (1 compilation-info-face) (2 impala-bullshit-face)
                          (3 compilation-info-face))
        (,impala-debug-exp (1 impala-debug-face) (2 impala-bullshit-face)
                           (3 impala-debug-face))
        (,impala-timestamp-exp . impala-timestamp-face)
        (,impala-prun-cmd-exp (1 impala-bullshit-face) (2 impala-command-large-face)
                              (3 impala-timestamp-face) (4 impala-command-large-face)
                              (5 impala-command-small-face))
        (,impala-local-cmd-exp (1 impala-command-large-face) (3 impala-command-small-face))
        (,impala-reservation-exp . impala-bullshit-face) 
        (,impala-run-on-exp . impala-bullshit-face)
        (,impala-nodes-exp . impala-bullshit-face)
        (,impala-prun-error-exp (1 impala-command-small-face) (2 compilation-error-face))
        (,impala-separator-exp . compilation-error-face)
        (,impala-script-section-exp . impala-command-large-face)))

;;;;============================================================================
;;;; interactive commands

(defvar impala-run-in-progress nil)

(defmacro define-impala-navigation-fun (item)
  "macro that defines navigation function with name
  \"impala-next-[item]\", which uses variable
  \"impala-[item]-emp\""
  (let* ((fun-name (intern (concat "impala-next-" item)))
         (fun-name-back (intern (concat "impala-previous-" item)))
         (var-name (intern (concat "impala-" item "-exp")))
         (docstring (concat "search for argth occurence of " item))
         (docstring-back (concat docstring " backwards")))
    `(progn
       (defun ,fun-name (arg)
         ,docstring
         (interactive "p")
         (if (> arg 0)
             (end-of-line)
           (beginning-of-line))
         (search-forward-regexp ,var-name nil nil arg)
         (beginning-of-line))
       (defun ,fun-name-back (arg)
         ,docstring-back
         (interactive "p")
         (,fun-name (- arg))))))

;;(macroexpand '(define-impala-navigation-fun "command"))

(define-impala-navigation-fun "info")
(define-impala-navigation-fun "debug")
(define-impala-navigation-fun "warning")
(define-impala-navigation-fun "error")
(define-impala-navigation-fun "command")
;(define-impala-navigation-fun "separator")
;; kan dit niet?
;;(mapc 'define-impala-navigation-fun '("info" "debug" "command" "warning" "error" "separator"))

;;;;============================================================================
;;; keymap

(defvar impala-key-map nil "key bindings for impala mode")
(let ((map (copy-keymap comint-mode-map)))
  (define-key map "c" 'impala-next-command)
  (define-key map "C" 'impala-previous-command)
  (define-key map "d" 'impala-next-debug)
  (define-key map "D" 'impala-previous-debug)
  (define-key map "i" 'impala-next-info)
  (define-key map "I" 'impala-previous-info)
  (define-key map "w" 'impala-next-warning)
  (define-key map "W" 'impala-previous-warning)
  (define-key map "e" 'impala-next-error)
  (define-key map "E" 'impala-previous-error)
  (setq impala-key-map map))


(define-derived-mode impala-mode comint-mode "impala"
  "mode for browsing / navigating output of a impala run"
  (font-lock-add-keywords nil impala-font-lock-keywords)
  (use-local-map impala-key-map))

(defun impala-run (command &optional dir)
  "Run command in dir and parse output. Normally command is a
  shell script such as Dennis' do_* scripts or Michiels surf-*
  scripts. No care is taken to see whether command actualy is a
  impala command."
  (let* ((thisdir default-directory)
         outwin outbuf)
    (with-current-buffer (setq outbuf (get-buffer-create "*impala*"))
      (let ((comp-proc (get-buffer-process (current-buffer))))
        (if comp-proc
            (if (or (not (eq (process-status comp-proc) 'run))
                    (yes-or-no-p "An impala process is running; kill it? "))
                (condition-case ()
                    (progn
                      (interrupt-process comp-proc)
                      (sit-for 1)
                      (delete-process comp-proc))
                  (error nil))
              (error "Cannot have two processes in `%s' at once"
                     (buffer-name)))))
      ;; first transfer directory from where M-x impala was called
      (setq default-directory thisdir)
      ;; Make buffer read-only.  The filter can still write it.
      (let ((inhibit-read-only t)
            (default-directory thisdir))
        (erase-buffer)
        ;; Select the desired mode.
        (setq buffer-read-only nil)
        (with-no-warnings (impala-mode))
        (font-lock-add-keywords nil impala-font-lock-keywords)
        (turn-on-font-lock)
        ;; (if highlight-regexp
        ;;     (set (make-local-variable 'compilation-highlight-regexp)
        ;;          highlight-regexp))
        ;; (if (or compilation-auto-jump-to-first-error
        ;;         (eq compilation-scroll-output 'first-error))
        ;;     (set (make-local-variable 'compilation-auto-jump-to-next) t))
        ;; Output a mode setter, for saving and later reloading this buffer.
        (insert "-*- mode: impala; default-directory: " (prin1-to-string default-directory)
                " -*-\n"
                (format "%s started at %s\n\n"
                        mode-name
                        (substring (current-time-string) 0 19))
                command "\n")
        (setq thisdir default-directory))
      (set-buffer-modified-p nil))
    ;; Pop up the impala buffer.
    ;; ??? http://lists.gnu.org/archive/html/emacs-devel/2007-11/msg01638.html
    (switch-to-buffer outbuf)
    (setq outwin (display-buffer outbuf))
    (select-window outwin)
    (with-current-buffer outbuf
      (progn
        (set-window-start outwin (point-min))

        ;; Position point as the user will see it.
        (let ((desired-visible-point (point-max)))
          (if (eq outwin (selected-window))
              (goto-char desired-visible-point)
            (set-window-point outwin desired-visible-point)))

        ;; Start the process
        (if (fboundp 'start-process)
            (let ((proc (get-buffer-process
                         (with-no-warnings
                           (comint-exec
                            outbuf (downcase mode-name)
                            (if (file-remote-p default-directory)
                                "/bin/sh"
                              shell-file-name)
                            nil `("-c" ,command))))))
              ;; Use (point-max) here so that output comes in
              ;; after the initial text,
              ;; regardless of where the user sees point.
              (set-marker (process-mark proc) (point-max) outbuf)
              (setq impala-run-in-progress proc ))
          ;; No asynchronous processes available.
          (message "Executing `%s'..." command)
          ;; Fake modeline display as if `start-process' were run.
          (sit-for 0)                   ; Force redisplay
          (save-excursion
            ;; Insert the output at the end, after the initial text,
            ;; regardless of where the user sees point.
            (goto-char (point-max))
            (let* ((buffer-read-only nil) ; call-process needs to modify outbuf
                   (status (call-process shell-file-name nil outbuf nil "-c"
                                         command)))
              (cond ((numberp status)
                     (compilation-handle-exit
                      'exit status
                      (if (zerop status)
                          "finished\n"
                        (format "exited abnormally with code %d\n" status))))
                    ((stringp status)
                     (compilation-handle-exit 'signal status
                                              (concat status "\n")))
                    (t
                     (compilation-handle-exit 'bizarre status status)))))
          ;; Without async subprocesses, the buffer is not yet
          ;; fontified, so fontify it now.
          (let ((font-lock-verbose nil)) ; shut up font-lock messages
            (font-lock-fontify-buffer))
          (set-buffer-modified-p nil)
          (message "Executing `%s'...done" command))))))

(provide 'impala)