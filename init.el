;-*-Emacs-Lisp-*-

;;; Commentary:
;;
;; I have nothing substantial to say here.
;;
;;; Code:


;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'custom-theme-load-path (expand-file-name "themes" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

;; Just while I'm working on it.
(add-to-list 'load-path (expand-file-name "octopress-mode" user-emacs-directory))
(require 'octopress-mode)

;; Essential settings.
(setq inhibit-splash-screen t
      inhibit-startup-message t
      inhibit-startup-echo-area-message t)
(menu-bar-mode -1)
(tool-bar-mode -1)
(when (boundp 'scroll-bar-mode)
  (scroll-bar-mode -1))
(show-paren-mode 1)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(global-visual-line-mode nil)
(setq-default left-fringe-width nil)
(setq-default indent-tabs-mode nil)
(eval-after-load "vc" '(setq vc-handled-backends nil))
(setq vc-follow-symlinks t)
(setq large-file-warning-threshold nil)
(setq split-width-threshold nil)
(setq visible-bell nil)

;;; File type overrides.
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html$\\'" . web-mode))

(add-to-list 'auto-mode-alist '("\\.jsx$" . web-mode))
(defadvice web-mode-highlight-part (around tweak-jsx activate)
  (if (equal web-mode-content-type "jsx")
      (let ((web-mode-enable-part-face nil))
        ad-do-it)
    ad-do-it))

;;; My own configurations, which are bundled in my dotfiles.
(require 'project-root)
(require 'init-utils)
(require 'init-mac)
(require 'init-global-functions)
(require 'init-elpa)
(require 'init-fonts)
(require 'init-gtags)
(require 'init-evil)
(require 'init-maps)
(require 'init-w3m)
(require 'init-php)
(require 'init-powerline)

(maybe-require-package 'wgrep)
(maybe-require-package 'wgrep-ag)
(autoload 'wgrep-ag-setup "wgrep-ag")
(add-hook 'ag-mode-hook 'wgrep-ag-setup)

(when (maybe-require-package 'exec-path-from-shell)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(when (string= system-type "gnu/linux")
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "google-chrome"))

(when (maybe-require-package 'avy)
  (setq avy-background t))

(maybe-require-package 'ag)
(maybe-require-package 'auto-complete)
(maybe-require-package 'dictionary)
(maybe-require-package 'emmet-mode)
(maybe-require-package 'flycheck)
(maybe-require-package 'guide-key)
(maybe-require-package 'helm)
(maybe-require-package 'helm-projectile)
(maybe-require-package 'highlight-symbol)
(maybe-require-package 'magit)
(maybe-require-package 'markdown-mode)
(maybe-require-package 'php-extras)
(maybe-require-package 'projectile)
(maybe-require-package 'twittering-mode)
(maybe-require-package 'web-mode)
(maybe-require-package 'color-theme-sanityinc-tomorrow)
(maybe-require-package 'mmm-mode)
(maybe-require-package 'yaml-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'key-chord)
(maybe-require-package 'evil-matchit)
(maybe-require-package 'evil-rebellion)
(maybe-require-package 'spacegray-theme)
(maybe-require-package 'smooth-scrolling)

(require 'mmm-mode)
(setq mmm-global-mode 'maybe)

(mmm-add-classes
 '((markdown-cl
    :submode emacs-lisp-mode
    :face mmm-declaration-submode-face
    :front "^```cl[\n\r]+"
    :back "^```$")))

(mmm-add-mode-ext-class 'markdown-mode nil 'markdown-cl)

;;; Don't display this nag about reverting buffers.
(setq magit-last-seen-setup-instructions "1.4.0")

;;; Helpers for GNUPG, which I use for encrypting/decrypting secrets.
(require 'epa-file)
(epa-file-enable)
(setq-default epa-file-cache-passphrase-for-symmetric-encryption t)

;;; Always use guide-key mode, it is awesome.
(guide-key-mode 1)
(setq-default guide-key/guide-key-sequence t
              guide-key/idle-delay 0.5)

(defvar show-paren-delay 0
  "Delay (in seconds) before matching paren is highlighted.")

(defvar backup-dir "~/.emacs.d/backups/")
(setq backup-directory-alist (list (cons "." backup-dir)))
(setq make-backup-files nil)
(setq-default highlight-symbol-idle-delay 1.5)

;;(global-auto-complete-mode t)
(require 'auto-complete-config)
(ac-config-default)
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")
(setq-default ac-dwim nil)
(setq-default ac-use-menu-map t)
(define-key ac-menu-map (kbd "<backtab>") 'ac-previous)

;; projectile
(defvar projectile-enable-caching nil
  "Tell Projectile to not cache project file lists.")
(projectile-global-mode)

;; matchit
(global-evil-matchit-mode 1)

;; This appears to be necessary only in Linux?
(require 'helm-projectile)

;;; Use Helm all the time.
(setq helm-buffers-fuzzy-matching t)
(helm-mode 1)

;;; Use evil surround mode in all buffers.
(global-evil-surround-mode 1)

;;; Flycheck mode:
(add-hook 'flycheck-mode-hook
          (lambda ()
            (evil-define-key 'normal flycheck-mode-map (kbd "]e") 'flycheck-next-error)
            (evil-define-key 'normal flycheck-mode-map (kbd "[e") 'flycheck-previous-error)))

;;; Helm mode:
(define-key helm-find-files-map (kbd "C-k") 'helm-find-files-up-one-level)

;;; Org mode:
(evil-leader/set-key-for-mode 'org-mode
  "t"  'org-set-tags
  "p"  '(lambda ()
         (interactive)
         (org-insert-property-drawer))
  "d"  'org-deadline
  "a"  'org-agenda
  "ns" 'org-narrow-to-subtree
  "$"  'org-archive-subtree)

(add-hook 'org-mode-hook
          (lambda ()
            (evil-define-key 'normal org-mode-map (kbd "TAB") 'org-cycle)
            (evil-define-key 'normal org-mode-map (kbd "C-\\") 'org-insert-heading)
            (evil-define-key 'insert org-mode-map (kbd "C-\\") 'org-insert-heading)
            (auto-fill-mode)
            (flyspell-mode)))

;;; Lisp interaction mode & Emacs Lisp mode:
(add-hook 'lisp-interaction-mode-hook
          (lambda ()
            (define-key lisp-interaction-mode-map (kbd "<C-return>") 'eval-last-sexp)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (define-key emacs-lisp-mode-map (kbd "<C-return>") 'eval-last-sexp)))

;;; Magit mode (which does not open in evil-mode):
(add-hook 'magit-mode-hook
          (lambda ()
            (define-key magit-mode-map (kbd ",o") 'delete-other-windows)))

;;; Emmet mode:
(add-hook 'emmet-mode-hook
          (lambda ()
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-l") 'emmet-next-edit-point)
            (evil-define-key 'insert emmet-mode-keymap (kbd "C-S-h") 'emmet-prev-edit-point)))

;;; js2 mode:
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.es6$" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx$" . js2-mode))
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(js2-allow-keywords-as-property-names t)
 '(js2-auto-indent-p t)
 '(js2-basic-offset 2)
 '(js2-bounce-indent-flag nil)
 '(js2-bounce-indent-p nil)
 '(js2-cleanup-whitespace t)
 '(js2-enter-indents-newline t)
 '(js2-global-externs (quote ("window" "$" "module" "require" "exports" "getResource")))
 '(js2-highlight-level 3)
 '(js2-idle-timer-delay 0.2)
 '(js2-indent-on-enter-key nil)
 '(js2-mirror-mode nil)
 '(js2-mode-indent-ignore-first-tab nil)
 '(js2-strict-inconsistent-return-warning t)
 '(js2-use-font-lock-faces nil))

;;; Web mode:
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-style-padding 2)
            (emmet-mode)
            (flycheck-add-mode 'html-tidy 'web-mode)
            (flycheck-mode)))

;;; Emacs Lisp mode:
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (turn-on-eldoc-mode)
            (highlight-symbol-mode)))

;;; SH mode:
(add-hook 'sh-mode-hook (lambda ()
                          (setq sh-basic-offset 2)
                          (setq sh-indentation 2)))

;;; Let me move the selection like a normal human in the Grizzl results buffer.
(add-hook 'grizzl-mode-hook (lambda ()
                              (define-key *grizzl-keymap* (kbd "C-j") 'grizzl-set-selection-1)
                              (define-key *grizzl-keymap* (kbd "C-k") 'grizzl-set-selection+1)))

;;; Javascript mode:
(add-hook 'javascript-mode-hook (lambda ()
                                  (set-fill-column 80)
                                  (turn-on-auto-fill)
                                  (setq js-indent-level 2)))

;;; Markdown mode:
(add-hook 'markdown-mode-hook (lambda ()
                                (set-fill-column 80)
                                (turn-on-auto-fill)
                                (flyspell-mode)))

;;; HTML mode:
(add-hook 'html-mode-hook (lambda ()
                            (setq sgml-basic-offset 2)
                            (setq indent-tabs-mode nil)))

(defun helm-project-files ()
  (interactive)
  (helm-other-buffer '(helm-c-source-projectile-files-list) "*Project Files*"))

;;; key bindings for evil
;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;; jk for escape
(key-chord-mode 1)
(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

;; Helm shortcuts
(define-key evil-normal-state-map "\C-n" 'helm-projectile-find-file)
(define-key evil-normal-state-map "\C-p" 'helm-buffers-list)

;; Scrolling

(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

;; Maximize
(toggle-frame-maximized)

;; Font

(if (member "Menlo" (font-family-list))
    (set-face-attribute
     'default nil :font "Menlo 14"))

(put 'narrow-to-region 'disabled nil)
(load-theme 'spacegray t)

(when (maybe-require-package 'diminish)
  (require 'diminish)
  (eval-after-load "highlight-symbol"
    '(diminish 'highlight-symbol-mode))
  (diminish 'helm-mode)
  (diminish 'guide-key-mode)
  (diminish 'mmm-mode)
  (diminish 'undo-tree-mode))

;;; sRGB doesn't blend with Powerline's pixmap colors, but is only
;;; used in OS X. Disable sRGB before setting up Powerline.
(when (memq window-system '(mac ns))
  (setq ns-use-srgb-colorspace nil))

(provide 'emacs)
(helm-projectile-switch-project)
;;; emacs ends here
