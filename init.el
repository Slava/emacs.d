;;; init the package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)
(when (not package-archive-contents)
    (package-refresh-contents))


;;;;;;;;;;;;;;;;;;;; evil ;;;;;;;;;;;;;;;;;;;;
;;; turn on evil
(require 'evil)
(evil-mode t)
(setq evil-leader/in-all-states 1)
(global-evil-leader-mode)
(evil-leader/set-leader ",")
(require 'evil-search-highlight-persist)
(global-evil-search-highlight-persist t)
(evil-leader/set-key "<leader>/" 'evil-search-highlight-persist-remove-all)

;;; use "jk" as escape
(define-key evil-insert-state-map "j" #'cofi/maybe-exit)
(evil-define-command cofi/maybe-exit ()
  :repeat change
  (interactive)
  (let ((modified (buffer-modified-p)))
    (insert "j")
    (let ((evt (read-event (format "Insert %c to exit insert state" ?k)
               nil 0.5)))
      (cond
       ((null evt) (message ""))
       ((and (integerp evt) (char-equal evt ?k))
    (delete-char -1)
    (set-buffer-modified-p modified)
    (push 'escape unread-command-events))
       (t (setq unread-command-events (append unread-command-events
                          (list evt))))))))

;;; surround
(require 'evil-surround)
(global-evil-surround-mode 1)

;;; matchit beyond brackets
(require 'evil-matchit)
(global-evil-matchit-mode 1)

;;; powerline
(require 'powerline)
(powerline-evil-vim-color-theme)
(display-time-mode t)


;;;;;;;;;;;;;;;;;;;; helm ;;;;;;;;;;;;;;;;;;;;
;;; helm
(require 'helm-config)
(helm-mode 1)
(setq helm-M-x-fuzzy-match t)
(setq helm-quick-update t)

;;; projectile
(require 'projectile)
(setq projectile-globally-ignored-directories
      (append projectile-globally-ignored-directories '(".git"
                                                        ".svn"
                                                        ".hg"
                                                        "build"
                                                        ".build"
                                                        ".build.*"
                                                        ".cache"
                                                        ".meteor"
                                                        )))
(projectile-global-mode)
(setq projectile-enable-caching t)
(setq projectile-require-project-root nil)

;;; ido-flx
(require 'flx-ido)
(ido-mode 1)
(ido-everywhere 1)
(flx-ido-mode 1)
;; disable ido faces to see flx highlights.
(setq ido-enable-flex-matching t)
(setq ido-use-faces nil)


;;; helm/projectile integration
(require 'helm-projectile)
(setq projectile-completion-system 'helm)
(helm-projectile-on)


;;;;;;;;;;;;;;;;;;;; misc ;;;;;;;;;;;;;;;;;;;;

;;; no back up files in the same folder
(setq
 backup-by-copying t      ; don't clobber symlinks
 backup-directory-alist
 '(("." . "~/.emacs.d/backup"))    ; don't litter my fs tree
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)       ; use versioned backups

;;; remember state between reloads
(setq save-place-file "~/.emacs.d/saveplace")
(setq-default save-place t)
(require 'saveplace)

;;; no scroll-bars
(scroll-bar-mode -1)

;;; remove the toolbar
(tool-bar-mode -1)

;;; maximized
(custom-set-variables
 '(initial-frame-alist (quote ((fullscreen . maximized)))))

;;; theme
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("bb08c73af94ee74453c90422485b29e5643b73b05e8de029a6909af6a3fb3f58" default))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(load-theme 'sanityinc-tomorrow-day)

;;; whitespace
(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;; load the .bash_profile for shell
(cond
  ((eq window-system 'ns) ; macosx
   ;; Invoke login shells, so that .profile or .bash_profile is read
   (setq shell-command-switch "-lc")))

;;; same for interactive subshell
(setq explicit-bash-args '("--login" "-i"))

;;;;;;;;;;;;;;;;;;;; bindings ;;;;;;;;;;;;;;;;;;;;
(define-key evil-normal-state-map (kbd "C-n") 'helm-projectile-find-file)

;;; autocomplete for ocaml
(setq merlin-use-auto-complete-mode 'easy)

;;;;;;;;;;;;;;;;;;;; not me ;;;;;;;;;;;;;;;;;;;;
;; Basic .emacs with a good set of defaults, to be used as template for usage
;; with OCaml, OPAM, and tuareg
;;
;; Requires tuareg or ocaml mode installed on the system
;;
;; Author: Louis Gesbert <louis.gesbert@ocamlpro.com>
;; Released under CC(0)

;; Generic, recommended configuration options

(custom-set-variables
 '(indent-tabs-mode nil)
 '(compilation-context-lines 2)
 '(compilation-error-screen-columns nil)
 '(compilation-scroll-output t)
 '(compilation-search-path (quote (nil "src")))
 '(electric-indent-mode nil)
 '(next-line-add-newlines nil)
 '(require-final-newline t)
 '(sentence-end-double-space nil)
 '(show-trailing-whitespace t)
 '(visible-bell t)
 '(show-paren-mode t)
 '(next-error-highlight t)
 '(next-error-highlight-no-select t)
 '(backup-directory-alist '(("." . "~/.local/share/emacs/backups")))
 '(ac-use-fuzzy nil)
 )

;; ANSI color in compilation buffer
(require 'ansi-color)
(defun colorize-compilation-buffer ()
  (toggle-read-only)
  (ansi-color-apply-on-region (point-min) (point-max))
  (toggle-read-only))
(add-hook 'compilation-filter-hook 'colorize-compilation-buffer)

;; Some key bindings

(global-set-key [f3] 'next-match)
(defun prev-match () (interactive nil) (next-match -1))
(global-set-key [(shift f3)] 'prev-match)
(global-set-key [backtab] 'auto-complete)

;; OCaml configuration
;;  - better error and backtrace matching
(defun set-ocaml-error-regexp ()
  (set
   'compilation-error-regexp-alist
   '("[Ff]ile \\(\"\\(.*?\\)\", line \\(-?[0-9]+\\)\\(, characters \\(-?[0-9]+\\)-\\([0-9]+\\)\\)?\\)\\(:\n\\(\\(Warning .*?\\)\\|\\(Error\\)\\):\\)?"
    2 3 (5 . 6) (9 . 11) 1 (8 compilation-message-face))))

(add-hook 'tuareg-mode-hook 'set-ocaml-error-regexp)
(add-hook 'ocaml-mode-hook 'set-ocaml-error-regexp)
;; ## added by OPAM user-setup for emacs / base ## 04e6df2fb0196279508d9d1895d30b61 ## you can edit, but keep this line
;; Base configuration for OPAM

(defun opam-shell-command-to-string (command)
  "Similar to shell-command-to-string, but returns nil unless the process
  returned 0 (shell-command-to-string ignores return value)"
  (let* ((return-value 0)
         (return-string
          (with-output-to-string
            (setq return-value
                  (with-current-buffer standard-output
                    (process-file shell-file-name nil t nil
                                  shell-command-switch command))))))
    (if (= return-value 0) return-string nil)))

(defun opam-update-env ()
  "Update the environment to follow current OPAM switch configuration"
  (interactive)
  (let ((env (opam-shell-command-to-string "opam config env --sexp")))
    (when env
      (dolist (var (car (read-from-string env)))
        (setenv (car var) (cadr var))
        (when (string= (car var) "PATH")
          (setq exec-path (split-string (cadr var) path-separator)))))))

(opam-update-env)

(setq opam-share
  (let ((reply (opam-shell-command-to-string "opam config var share")))
    (when reply (substring reply 0 -1))))

(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
;; OPAM-installed tools automated detection and initialisation

(defun opam-setup-tuareg ()
  (add-to-list 'load-path (concat opam-share "/tuareg"))
  (load "tuareg-site-file"))

(defun opam-setup-ocp-indent ()
  (require 'ocp-indent))

(defun opam-setup-ocp-index ()
  (require 'ocp-index))

(defun opam-setup-merlin ()
  (require 'merlin)
  (add-hook 'tuareg-mode-hook 'merlin-mode t)
  (add-hook 'caml-mode-hook 'merlin-mode t)
  (set-default 'ocp-index-use-auto-complete nil)
  (set-default 'merlin-use-auto-complete-mode 'easy)
  ;; So you can do it on a mac, where `C-<up>` and `C-<down>` are used
  ;; by spaces.
  (define-key merlin-mode-map
    (kbd "C-c <up>") 'merlin-type-enclosing-go-up)
  (define-key merlin-mode-map
    (kbd "C-c <down>") 'merlin-type-enclosing-go-down)
  (set-face-background 'merlin-type-face "skyblue"))

(defun opam-setup-utop ()
  (autoload 'utop "utop" "Toplevel for OCaml" t)
  (autoload 'utop-setup-ocaml-buffer "utop" "Toplevel for OCaml" t)
  (add-hook 'tuareg-mode-hook 'utop-setup-ocaml-buffer))

(setq opam-tools
  '(("tuareg" . opam-setup-tuareg)
    ("ocp-indent" . opam-setup-ocp-indent)
    ("ocp-index" . opam-setup-ocp-index)
    ("merlin" . opam-setup-merlin)
    ("utop" . opam-setup-utop)))

(defun opam-detect-installed-tools ()
  (let*
      ((command "opam list --installed --short --safe --color=never")
       (names (mapcar 'car opam-tools))
       (command-string (mapconcat 'identity (cons command names) " "))
       (reply (opam-shell-command-to-string command-string)))
    (when reply (split-string reply))))

(setq opam-tools-installed (opam-detect-installed-tools))

(defun opam-auto-tools-setup ()
  (interactive)
  (dolist
      (f (mapcar (lambda (x) (cdr (assoc x opam-tools))) opam-tools-installed))
    (funcall (symbol-function f))))

(opam-auto-tools-setup)
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
;; ## added by OPAM user-setup for emacs / tuareg ## 4bd841ebbde819dd00bd2cd248c073a3 ## you can edit, but keep this line
;; Load tuareg from its original switch when not found in current switch
(when (not (assoc "tuareg" opam-tools-installed))
  (add-to-list 'load-path "/Users/slava/.opam/4.02.1/share/tuareg")
  (load "tuareg-site-file"))
;; ## end of OPAM user-setup addition for emacs / tuareg ## keep this line
;; ## added by OPAM user-setup for emacs / ocp-indent ## c6b1b7fc7ac8f410604b25a588b11669 ## you can edit, but keep this line
;; Load ocp-indent from its original switch when not found in current switch
(when (not (assoc "ocp-indent" opam-tools-installed))
  (load-file "/Users/slava/.opam/4.02.1/share/emacs/site-lisp/ocp-indent.el")
  (setq ocp-indent-path "/Users/slava/.opam/4.02.1/bin/ocp-indent"))
;; ## end of OPAM user-setup addition for emacs / ocp-indent ## keep this line
