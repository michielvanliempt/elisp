;;__________________________________________________________________________
;;;;   Programming - Clojure

;; (defconst clj-root (concat (expand-file-name "~") "/coding/clj/"))
;; (setq load-path (append (list (concat clj-root "slime")
;;                               (concat clj-root "slime/contrib")
;;                               (concat clj-root "clojure-mode"))
;;                         load-path))

;; (require 'clojure-mode)
;; (setq swank-clojure-jar-path (concat clj-root "/swank-clojure/swank-clojure-1.0.jar")
;;       swank-clojure-jar-home "~/.swank-clojure/"
;;       swank-clojure-classpath
;;       (list
;;        "~/.swank-clojure/*"
;;        "~/coding/lisp/clojure/libs/*"))  ;; All my Java jars

;; (require 'slime)
;; (eval-after-load "slime"
;;   '(progn
;;      (slime-setup '(slime-repl))
;;      (setq slime-lisp-implementations
;; 	   `((clojure ("clj-cmd") :init swank-clojure-init)
;; 	     ,@slime-lisp-implementations))))

;; (defun lisp-enable-paredit-hook () (paredit-mode 1))
;; (add-hook 'clojure-mode-hook 'lisp-enable-paredit-hook)
;; (require 'swank-clojure-autoload)
;; (custom-set-variables
;;  '(swank-clojure-extra-classpaths '("/Users/michielvanliempt/coding/clj/clojure-contrib:/Users/michielvanliempt/coding/clj/swank-clojure"))
;;  '(swank-clojure-extra-vm-args '("-Xrunjdwp:transport=dt_socket,server=y,suspend=n,address=8888"))
;;  '(swank-clojure-java-path "/System/Library/Frameworks/JavaVM.framework/Versions/1.6.0/Home/bin/java"))

;; (add-hook 'slime-connected-hook (lambda ()
;; 				  (require 'clojure-mode)
;; 				  (slime-redirect-inferior-output)
;; 				  (def-slime-selector-method ?j
;; 				    "most recently visited clojure-mode buffer."
;; 				    (slime-recently-visited-buffer 'clojure-mode))
;; 				  (define-key slime-mode-map (kbd "C-c b")
;; 				    'slime-browse-local-javadoc)
;; 				  (define-key slime-repl-mode-map (kbd "C-c b")
;; 				    'slime-browse-local-javadoc)
;; 				  (define-key slime-mode-map (kbd "C-c d")
;; 				    'slime-java-describe)
;; 				  (define-key slime-repl-mode-map (kbd "C-c d")
;; 				    'slime-java-describe)
;; 				  (define-key slime-mode-map (kbd "C-c D")
;; 				    'slime-javadoc)
;; 				  (define-key slime-repl-mode-map (kbd "C-c D")
;; 				    'slime-javadoc)))

;; ;; slime
;; (add-to-list 'load-path "~/coding/clj/slime")
;; (require 'slime)
;; (slime-setup)
