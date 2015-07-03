(when (maybe-require-package 'evil-leader)
  ;; Evil leader must be loaded before evil (as documented).
  (global-evil-leader-mode)

  (evil-leader/set-leader ",")
  (setq evil-leader/in-all-states 1)
  (evil-leader/set-key
    ":"  'eval-expression
    "b"  'helm-mini             ;; Switch to another buffer
    "B"  'magit-blame-mode
    "d"  (lambda () (interactive) (evil-ex-call-command nil "bdelete" nil))
    "D"  'open-current-line-in-codebase-search
    "f"  'helm-imenu            ;; Jump to function in buffer
    "g"  'magit-status
    "h"  'fontify-and-browse    ;; HTML-ize the buffer and browse the result
    "l"  'whitespace-mode       ;; Show invisible characters
    "o"  'delete-other-windows  ;; C-w o
    "s"  'ag-project            ;; Ag search from project's root
    "r"  (lambda () (interactive) (font-lock-fontify-buffer) (redraw-display))
    "S"  'delete-trailing-whitespace
    "w"  'save-buffer
    "x"  'helm-M-x
    "y"  'yank-to-x-clipboard))

(when (maybe-require-package 'evil-jumper)
  (global-evil-jumper-mode))
(maybe-require-package 'evil-surround)
(maybe-require-package 'evil-indent-textobject)

(when (maybe-require-package 'evil)
  ;; Always use evil mode.
  (evil-mode 1)

  ;; My personal evil settings.
  (setq evil-want-C-u-scroll t)
  (setq-default evil-want-C-i-jump nil)
  (setq-default evil-symbol-word-search t)
  (setq-default evil-shift-width 2)

  (add-to-list 'evil-buffer-regexps '("\\*magit:"))
  (add-to-list 'evil-buffer-regexps '("\\*Flycheck"))
  (add-to-list 'evil-emacs-state-modes 'flycheck-error-list-mode)

  (evil-add-hjkl-bindings ag-mode-map 'normal
    "n"   'evil-search-next
    "N"   'evil-search-previous
    "RET" 'compile-goto-error)

  (evil-add-hjkl-bindings occur-mode-map 'emacs
    (kbd "/")       'evil-search-forward
    (kbd "n")       'evil-search-next
    (kbd "N")       'evil-search-previous
    (kbd "C-d")     'evil-scroll-down
    (kbd "C-u")     'evil-scroll-up
    (kbd "C-w C-w") 'other-window)

  (evil-add-hjkl-bindings org-agenda-mode-map 'emacs
    "RET" 'org-agenda-switch-to)

  (defun next-conflict-marker ()
    (interactive)
    (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t)
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;; Global bindings.
  (evil-define-key 'normal org-mode-map (kbd "]n") 'org-forward-heading-same-level)
  (evil-define-key 'normal org-mode-map (kbd "[n") 'org-backward-heading-same-level)

  ;; jk for escape
  (key-chord-mode 1)
  (setq key-chord-two-keys-delay 0.5)
  (key-chord-define evil-insert-state-map "jk" 'evil-normal-state)

  ;; Helm shortcuts
  (define-key evil-normal-state-map "\C-n" 'helm-projectile-find-file)
  (define-key evil-normal-state-map (kbd "C-S-n") 'helm-find-files)
  (define-key evil-normal-state-map "\C-p" 'helm-projectile-switch-to-buffer)
  (define-key evil-normal-state-map (kbd "C-S-p") 'helm-buffers-list)
  (define-key evil-normal-state-map (kbd "C-SPC") 'helm-projectile-switch-project)
  (define-key evil-normal-state-map (kbd "M-r") 'helm-eshell-history)


  (defun minibuffer-keyboard-quit ()
    "Abort recursive edit.
In Delete Selection mode, if the mark is active, just deactivate it;
then it takes a second \\[keyboard-quit] to abort the minibuffer."
    (interactive)
    (if (and delete-selection-mode transient-mark-mode mark-active)
        (setq deactivate-mark  t)
      (when (get-buffer "*Completions*") (delete-windows-on "*Completions*"))
      (abort-recursive-edit)))

  ;; Make escape quit everything, whenever possible.
  (define-key evil-normal-state-map [escape] 'keyboard-quit)
  (define-key evil-visual-state-map [escape] 'keyboard-quit)
  (define-key minibuffer-local-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-ns-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-completion-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-must-match-map [escape] 'minibuffer-keyboard-quit)
  (define-key minibuffer-local-isearch-map [escape] 'minibuffer-keyboard-quit)

  ;; My own Ex commands.
  (evil-ex-define-cmd "om" 'om-status))

(provide 'init-evil)
