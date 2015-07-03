;-*-Emacs-Lisp-*-

;; Added by Package.el.  This must come before configurations of
;; installed packages.  Don't delete this line.  If you don't want it,
;; just comment it out by adding a semicolon to the start of the line.
;; You may delete these explanatory comments.
(package-initialize)

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(add-to-list 'exec-path "/usr/local/bin")

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
(setq c-basic-offset 2)

(add-to-list 'desktop-locals-to-save 'buffer-undo-list)
(desktop-save-mode t)

(require 'project-root)
(require 'init-elpa)
(require 'init-mac)
(require 'init-evil)
(require 'init-powerline)

(when (maybe-require-package 'exec-path-from-shell)
  (when (memq window-system '(mac ns))
    (exec-path-from-shell-initialize)))

(when (string= system-type "gnu/linux")
      (setq browse-url-browser-function 'browse-url-generic
            browse-url-generic-program "google-chrome"))

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
(maybe-require-package 'projectile)
(maybe-require-package 'web-mode)
(maybe-require-package 'mmm-mode)
(maybe-require-package 'yaml-mode)
(maybe-require-package 'js2-mode)
(maybe-require-package 'key-chord)
(maybe-require-package 'evil-matchit)
(maybe-require-package 'evil-rebellion)
(maybe-require-package 'powerline)
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
(setq projectile-require-project-root nil)

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
 '(js2-global-externs
   (quote
    ("window" "$" "module" "require" "exports" "getResource")))
 '(js2-highlight-level 3)
 '(js2-idle-timer-delay 0.2)
 '(js2-indent-on-enter-key nil)
 '(js2-mirror-mode nil)
 '(js2-mode-indent-ignore-first-tab nil)
 '(js2-strict-inconsistent-return-warning t)
 '(js2-use-font-lock-faces nil)
 '(safe-local-variable-values (quote ((no-byte-compile t)))))

;;; Web mode:
(require 'web-mode)
(add-hook 'web-mode-hook
          (lambda ()
            (setq web-mode-attr-indent-offset 2)
            (setq web-mode-style-padding 2)
            (setq web-mode-markup-indent-offset 2)
            (setq web-mode-code-indent-offset 2)
            (setq web-mode-css-indent-offset 2)
            (setq web-mode-enable-css-colorization t)
            (emmet-mode)
            (flycheck-mode)))

(setq web-mode-ac-sources-alist
  '(("css" . (ac-source-css-property))
    ("html" . (ac-source-words-in-buffer ac-source-abbrev))))

(defun rm-maybe-jsx-mode ()
  (when (string-equal "jsx" web-mode-content-type)
    (js2-minor-mode 1)))
(add-hook 'web-mode-hook 'rm-maybe-jsx-mode)
(add-to-list 'web-mode-content-types '("jsx" . "\\.jsx?\\'"))
(add-to-list 'auto-mode-alist '("\\.css?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))

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

;;; key bindings for evil
;; esc quits
(define-key evil-normal-state-map [escape] 'keyboard-quit)
(define-key evil-visual-state-map [escape] 'keyboard-quit)
(define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
(define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

;;; eshell
(defun fish-path (path max-len)
  "Return a potentially trimmed-down version of the directory PATH, replacing
parent directories with their initial characters to try to get the character
length of PATH (sans directory slashes) down to MAX-LEN."
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str
                        (cond ((= 0 (length (car components))) "/")
                              ((= 1 (length (car components)))
                               (concat (car components) "/"))
                              (t
                               (if (string= "."
                                            (string (elt (car components) 0)))
                                   (concat (substring (car components) 0 2)
                                           "/")
                                 (string (elt (car components) 0) ?/)))))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (reduce (lambda (a b) (concat a "/" b)) components))))

(defun rjs-eshell-prompt-function ()
  (concat (fish-path (eshell/pwd) 40)
          (if (= (user-uid) 0) " # " " $ ")
          "\n"))
(setq eshell-prompt-function 'rjs-eshell-prompt-function)

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (name   (car (last (split-string parent "/" t)))))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " name "*"))

    (insert (concat "ls"))
    (eshell-send-input)))

(global-set-key (kbd "C-!") 'eshell-here)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(add-hook 'eshell-preoutput-filter-functions
          'ansi-color-apply)

;; Scrolling

(require 'smooth-scrolling)
(setq smooth-scroll-margin 3)

;; Maximize
(toggle-frame-maximized)

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

(my-powerline-default-theme)

(provide 'emacs)
;;; emacs ends here
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
