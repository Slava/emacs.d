;;; init the package manager
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)


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


;;;;;;;;;;;;;;;;;;;; ocaml ;;;;;;;;;;;;;;;;;;;;

;;; load merlin, requires merlin to be installed with opam
(setq opam-share (substring (shell-command-to-string "opam config var share 2> /dev/null") 0 -1))
(add-to-list 'load-path (concat opam-share "/emacs/site-lisp"))
(require 'merlin)

;;; load on tuareg mode
(add-hook 'tuareg-mode-hook 'merlin-mode)


;;;;;;;;;;;;;;;;;;;; not me ;;;;;;;;;;;;;;;;;;;;
