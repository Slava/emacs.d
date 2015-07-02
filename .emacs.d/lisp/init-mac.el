;;; Provide settings that only work on Macs.
(require 'init-fonts)

(when (memq window-system '(mac ns))
  (add-to-list 'default-frame-alist '(font . "Source Code Pro-13"))
  (set-face-attribute 'default t :font "Source Code Pro-1")
  (sanityinc/set-frame-font-size 13)
  (define-key global-map (kbd "<s-return>") 'toggle-frame-fullscreen))

(provide 'init-mac)
