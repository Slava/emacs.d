(when (maybe-require-package 'evil-leader)
  ;; Evil leader must be loaded before evil (as documented).
  (global-evil-leader-mode)

  (evil-leader/set-leader "SPC")
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

  ;; Bindings for Magit modes: start in normal mode. Go to insert mode to enter magit commands
  (evil-set-initial-state 'magit-mode 'normal)
  (evil-set-initial-state 'magit-status-mode 'normal)
  (evil-set-initial-state 'magit-diff-mode 'normal)
  (evil-set-initial-state 'magit-log-mode 'normal)
  (evil-set-initial-state 'magit-commit-mode 'normal)
  (evil-define-key 'normal magit-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
  (evil-define-key 'normal magit-log-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
  (evil-define-key 'normal magit-diff-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)
  (evil-define-key 'normal magit-commit-mode-map
    "j" 'magit-goto-next-section
    "k" 'magit-goto-previous-section)

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

  (defun next-conflict-marker ()
    (interactive)
    (search-forward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-max) t)
    (move-beginning-of-line nil))

  (defun previous-conflict-marker ()
    (interactive)
    (search-backward-regexp "\\(>>>>\\|====\\|<<<<\\)" (point-min) t)
    (move-beginning-of-line nil))

  ;;; Org mode:
  (defun bb/org-eol-call (fun)
    "Go to end of line and call provided function"
    (end-of-line)
    (funcall fun)
    (evil-append nil))

  (defun bb/org-insert-item ()
    (interactive)
    (if (not (org-in-item-p))
        (bb/org-eol-call (lambda () (insert "\n- ")))
      (bb/org-eol-call 'org-insert-item)))

  (use-package
   org
   :config
   (progn
     (evil-define-key 'normal org-mode-map

       ;; Movement
       "gJ" 'outline-next-visible-heading
       "gK" 'outline-previous-visible-heading
       "gj" 'org-forward-heading-same-level
       "gk" 'org-backward-heading-same-level
       "gh" 'outline-up-heading
       (kbd "M-n") 'next-error
       (kbd "M-p") 'previous-error
       "$" 'org-end-of-line
       "^" 'org-beginning-of-line

       ;; Insertion of headings and items
       "gO" (lambda () (interactive)
              (bb/org-eol-call 'org-insert-heading-after-current)
              (org-metaright))
       "go" (lambda () (interactive)
              (bb/org-eol-call 'org-insert-heading-after-current))
       "gT" (lambda () (interactive)
              (bb/org-eol-call 'org-insert-heading-after-current)
              (org-metaright)
              (org-todo))
       "gt" (lambda () (interactive)
              (bb/org-eol-call 'org-insert-heading-after-current)
              (org-todo))
       "gI" (lambda () (interactive)
              (bb/org-insert-item)
              (org-metaright))
       "gi" 'bb/org-insert-item

       ;; Common keys
       "-" 'org-ctrl-c-minus
       "gc" 'org-ctrl-c-ctrl-c
       "g*" 'org-ctrl-c-star
       (kbd "g RET") 'org-ctrl-c-ret

       ;; Insertions and setters
       "g-" 'org-table-insert-hline
       "gp" 'org-set-property
       "g." 'org-time-stamp
       "g!" 'org-time-stamp-inactive
       "gf" 'org-footnote-action
       "gl" 'org-insert-link
       "t" 'org-todo

       ;; Clocking
       "gxi" 'org-clock-in
       "gxo" 'org-clock-out
       "gxx" 'org-clock-in-last
       "gxd" 'org-clock-display
       "gxr" 'org-clock-report

       ;; Other
       "gs" 'org-sort
       "g/" 'org-sparse-tree
       (kbd "g SPC") 'org-remove-occur-highlights
       (kbd "TAB") 'org-cycle)

     ;; ";t" 'org-show-todo-tree
     ;; ";a" 'org-agenda

     (mapc (lambda (state)
             (evil-define-key state org-mode-map
               (kbd "M-l") 'org-metaright
               (kbd "M-h") 'org-metaleft
               (kbd "M-k") 'org-metaup
               (kbd "M-j") 'org-metadown
               (kbd "M-L") 'org-shiftmetaright
               (kbd "M-H") 'org-shiftmetaleft
               (kbd "M-K") 'org-shiftmetaup
               (kbd "M-J") 'org-shiftmetadown
               (kbd "M-o") '(lambda () (interactive)
                              (bb/org-eol-call
                               '(lambda()
                                  (org-insert-heading)
                                  (org-metaright))))
               (kbd "M-t") '(lambda () (interactive)
                              (bb/org-eol-call
                               '(lambda()
                                  (org-insert-todo-heading nil)
                                  (org-metaright))))))
           '(normal insert))

     (evil-leader/set-key-for-mode 'org-mode
       "oh" 'helm-org-headlines)

     (setq org-log-done 'time)
     (setq org-clock-into-drawer t)

     (evil-leader/set-key
       "oa" 'org-agenda
       "oc" 'org-capture)

     (add-hook
      'org-mode-hook
      (lambda () (interactive)
        (org-indent-mode)
        (visual-line-mode)))

     (use-package
      org-agenda
      :config
      (progn
        (setq org-agenda-window-setup 'current-window)
        (define-key org-agenda-mode-map "j" 'org-agenda-next-line)
        (define-key org-agenda-mode-map "k" 'org-agenda-previous-line)
        (define-key org-agenda-mode-map "n" 'org-agenda-goto-date)
        (define-key org-agenda-mode-map "p" 'org-agenda-capture)
        (define-key org-agenda-mode-map ":" 'evil-ex)))))

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
