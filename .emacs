;; Red Hat Linux default .emacs initialization file
;; Are we running XEmacs or Emacs?
;;defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(setq windmove-window-distance-delta 2)
 
;;(add-to-list 'default-frame-alist '(left . 20))
;;(add-to-list 'default-frame-alist '(top . 0))
;;(add-to-list 'default-frame-alist '(height . 80))
;;(add-to-list 'default-frame-alist '(width . 100))

(when (display-graphic-p)
  ;; disables scrollbar
;;  (scroll-bar-mode -1)
;;  (menu-bar-mode -1)
  ;; disable the top toolbar
  (tool-bar-mode -1))

(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/auto-save-list")
(add-to-list 'load-path "~/.emacs.d/ac-dict")

(require 'package) 
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize) 

(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")

(setq ac-modes
      '(emacs-lisp-mode lisp-interaction-mode lisp-mode scheme-mode
                        c-mode cc-mode c++-mode java-mode objc-mode
                        perl-mode cperl-mode python-mode ruby-mode
                        ecmascript-mode javascript-mode php-mode css-mode
                        makefile-mode sh-mode fortran-mode f90-mode ada-mode
                        xml-mode sgml-mode
                        haskell-mode literate-haskell-mode
                        emms-tag-editor-mode
                        asm-mode
                        org-mode))

(ac-config-default)
(global-auto-complete-mode t)

(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
(require 'yasnippet)

(yas/initialize)

(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")
(yas/load-directory "~/.emacs.d/packages/yasnippet/mysnippets")
;(yas/load-directory "~/.emacs.d/packages/yasnippet/objc-snippets")

;(defun yas-load-objc ()
;  (interactive)
;  (yas--minor-mode t)
;  (yas--load-directory "~/.emacs.d/packages/yasnippet/objc-snippets")
;)


(define-key global-map [button4] 'scroll-down)
(define-key global-map [button5] 'scroll-up)
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.
(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key [f5] 'compile)
(global-set-key "\C-x\C-v" 'speedbar-get-focus)
(global-set-key "\C-x\C-c" `save-buffers-kill-emacs)
;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)

(require 'setnu)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(display-time)
(defun insert-time-string ()
  (interactive)
  (insert (current-time-string) ))

;; custom key bindings

(global-set-key "\C-cd" 'insert-time-string)
(global-set-key "\C-g" 'goto-line)
 
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(c-default-style
   (quote
    ((c-mode . "bsd")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (other . "gnu"))))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(display-battery-mode t)
 '(display-time-mode t)
 '(font-lock-mode t t (font-lock))
 '(gutter-buffers-tab-visible-p nil)
 '(inhibit-startup-screen t)
 '(mail-user-agent (quote message-user-agent))
 '(notmuch-saved-searches
   (quote
    (("new qemu" . "tag:qemu AND tag:unread")
     (:name "new xen" :query "tag:xen and tag:unread")
     (:name "tome" :query "tag:unread and to:ncmike@ncultra.org"))))
 '(notmuch-search-oldest-first nil)
 '(paren-mode (quote sexp) nil (paren))
 '(query-user-mail-address nil)
 '(safe-local-variable-values (quote ((c-file-style . bsd))))
 '(send-mail-function (quote sendmail-send-it))
 '(sendmail-program "/usr/bin/msmtp")
 '(spice-output-local "Gnucap")
 '(spice-simulator "Gnucap")
 '(spice-waveform-viewer "Gwave")
 '(tool-bar-mode nil)
 '(toolbar-visible-p nil)
 '(tramp-debug-buffer t)
 '(user-mail-address "ncmike@ncultra.org"))

;;(require 'notmuch)

;; (setq notmuch-saved-searches '(("new qemu" . "tag:qemu AND tag:unread")))



;;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;;'(default ((t (:inherit nil :stipple nil :background "white" :foreground "black" :inverse-video nil :box nil :strike-through nil :overline nil :underline nil :slant normal :weight normal :height 128 :width normal :foundry "urw" :family "Nimbus Mono L")))))
;(require 'efs)
(require 'tramp)
(require 'calendar)
(require 'cscope)
(require 'xcscope)
(require 'cl)
(add-hook 'fundamental-mode 'mail-mode 'auto-fill-mode) 

(add-hook 'text-mode-hook 'turn-on-setnu-mode)

(defun insert-c-token ()
  (interactive)
  (insert "-*- linux-c -*-"))
(global-set-key [f6] 'insert-c-token)

(defconst xen-style
  '( "bsd"
     (c-basic-offset . 4)
     (indent-tabs-mode . nil)
     (tab-width . 4)
     (label . 1)
    )
  "Xen C Programming Style")
(c-add-style "xen" xen-style )

(defconst linux-c-style
	'( "K&R"
	(c-basic-offset . 8)
	(indent-tabs-mode . t)
	(tab-width . 8)
	)
"Linux CodingStyle")
(c-add-style "linux" linux-c-style )
(setq c-default-style "linux")

(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)))
(add-hook 'c-mode-common-hook 'turn-on-setnu-mode)

(add-hook 'objc-mode-hook
 	  (lambda () (auto-complete-mode t)))
(add-hook 'c-mode-common-hook
	  (lambda () 
	    (yas/minor-mode t)))
(add-hook 'objc-mode-hook
 	  (lambda () (yas-load-objc)))

(setq auto-mode-alist
	      (append '(("\\.hpp" . c++-mode))  auto-mode-alist ))

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))

(setq c-style-variables-are-local-p t)
(setq auto-c-mode-alist
  '(
	("/linux"   . "linux")
	("/xen"     . "xen")
        ("/Classes" . "bsd")
        (""         . "bsd")))

(defun my-c-mode-hooks ()
  "Look at auto-c-mode-alist to decide on the c style mode"
  (save-excursion
    (let ((name  (file-name-sans-versions buffer-file-name))
          (alist auto-c-mode-alist)
          (mode nil))
      (while (and (not mode) alist)
        (if (string-match (car (car alist)) name)
            (if (and (consp (cdr (car alist)))
                     (nth 2 (car alist)))
                (progn
                  (setq mode (car (cdr (car alist)))
                        name (substring name 0 (match-beginning 0))
                        keep-going t))
              (setq mode (cdr (car alist))
                    keep-going nil)))
        (setq alist (cdr alist)))
      (c-set-style mode)
      (setnu-mode())

)))
	
(add-hook 'c-mode-hook 'my-c-mode-hooks)
(add-hook 'c++-mode-hook 'my-c-mode-hooks)
(add-hook 'objc-mode-hook 'my-c-mode-hooks)
(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)))

(defun trim-trailing-whitespace()
  (interactive)
  (save-excursion
	(beginning-of-buffer)
	(replace-regexp "[  \t]+$" "")))

(defun checkpatch()
  (interactive)
  (compile (concat "~/bin/checkpatch.pl --emacs --no-tree " (buffer-file-name))))

(defun checksrc()
  (interactive)
  (compile (concat "~/bin/checkpatch.pl --emacs --no-tree --file --no-signoff " (buffer-file-name))))

(defun checksource()
  (interactive)
  (compile (concat "~/src/checkpatch/checkpatch.emacs.pl --emacs --no-tree --no-patch  " (buffer-file-name))))

(defun gcc()
  (interactive)
  (compile (concat "gcc -ggdb " (buffer-file-name))))


(defun whack-whitespace (arg)
      "Delete all white space from point to the next word.  With prefix ARG
    delete across newlines as well.  The only danger in this is that you
    don't have to actually be at the end of a word to make it work.  It
    skips over to the next whitespace and then whacks it all to the next
    word."
      (interactive "P")
      (let ((regexp (if arg "[ \t\n]+" "[ \t]+")))
        (re-search-forward regexp nil t)
        (replace-match "" nil nil)))

(defun delete-horizontal-space-forward () ; adapted from `delete-horizontal-space'
      "*Delete all spaces and tabs after point."
      (interactive "*")
      (delete-region (point) (progn (skip-chars-forward " \t") (point))))

(defun backward-delete-char-hungry (arg &optional killp)
      "*Delete characters backward in \"hungry\" mode.
    See the documentation of `backward-delete-char-untabify' and
    `backward-delete-char-untabify-method' for details."
      (interactive "*p\nP")
      (let ((backward-delete-char-untabify-method 'hungry))
        (backward-delete-char-untabify arg killp)))
;;(setq load-home-init-file t) ; don't load init file from ~/.xemacs/init.el

;;---------------------------------------------------------------

;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
;'(default ((t (:family "Courier 10 Pitch" :foundry "bitstream" :slant normal :weight normal :height 111 :width normal)))))

;;-------------------more stuff-----------------------

;; a copy key, leaves the text as-is
(global-set-key "\C-Q" 'copy-region-as-kill)

(global-set-key "\C-B" 'compile)

(defun ew()
  (interactive)
;;  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-vertically)
  (balance-windows))

;(global-set-key [f9] 'windmove-right)
(global-set-key (kbd "C-<right>") 'windmove-right)
(global-set-key (kbd "C-<left>") 'windmove-left)
(global-set-key (kbd "C-<up>") 'windmove-up)
(global-set-key (kbd "C-<down>") 'windmove-down)


(defun toggle-fullscreen ()
  "Toggle full screen on X11"
  (interactive)
  (when (eq window-system 'x)
    (set-frame-parameter
     nil 'fullscreen
     (when (not (frame-parameter nil 'fullscreen)) 'fullboth))))

(global-set-key [f11] 'toggle-fullscreen)
