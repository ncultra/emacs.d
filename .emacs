;; Red Hat Linux default .emacs initialization file
;; Are we running XEmacs or Emacs?
;;defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

(setq windmove-window-distance-delta 2)
(desktop-save-mode 1)

(when (getenv "EMACSSAVEMODEDIR")
  (setq desktop-path (list . (getenv "EMACSSAVEMODEDIR"))) )

; automatically load a locked desktop file
(setq desktop-load-locked-desktop t)

; from http://www.emacswiki.org/emacs/Desktop#toc3
; "add something like this to your init file to auto-save your desktop when Emacs is idle: – Doom"
(require 'desktop)
  (defun my-desktop-save ()
    (interactive)
    ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
    (if (eq (desktop-owner) (emacs-pid))
        (desktop-save desktop-dirname)))
  (add-hook 'auto-save-hook 'my-desktop-save)

(when (display-graphic-p)
  (tool-bar-mode -1))

;;(add-to-list 'load-path "~/.emacs.d/")
(add-to-list 'load-path "~/.emacs.d/packages")
(add-to-list 'load-path "~/.emacs.d/auto-save-list")
(add-to-list 'load-path "~/.emacs.d/ac-dict")

(require 'package)
(add-to-list 'package-archives
             '("melpa-stable" . "https://stable.melpa.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

(package-initialize)  ;load and activate packages, including auto-complete
(ac-config-default)
(global-auto-complete-mode t)

(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

(autoload 'gfm-mode "markdown-mode"
   "Major mode for editing GitHub Flavored Markdown files" t)
(add-to-list 'auto-mode-alist '("README\\.md\\'" . gfm-mode))


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
(global-auto-revert-mode t)
(global-auto-complete-mode t)

;;(add-to-list 'load-path "~/.emacs.d/packages/yasnippet")
;;(require 'yasnippet)

;;(yas/initialize)

;;(yas/load-directory "~/.emacs.d/packages/yasnippet/snippets")
;;(yas/load-directory "~/.emacs.d/packages/yasnippet/mysnippets")
;;(yas/load-directory "~/.emacs.d/packages/yasnippet/objc-snippets")

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
(global-set-key "\C-x\C-b" `list-buffers)
;; Turn on font-lock mode for Emacs
(global-font-lock-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

(display-time)
(defun insert-time-string ()
  (interactive)
  (insert (current-time-string) ))

;; custom key bindings

(global-set-key "\C-c" 'desktop-change-dir)
(global-set-key "\C-g" 'goto-line)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(ansi-color-names-vector
   ["black" "red3" "ForestGreen" "yellow3" "blue" "magenta3" "DeepSkyBlue" "gray50"])
 '(c-default-style
   (quote
    ((c-mode . "bsd")
     (c++-mode . "stroustrup")
     (java-mode . "java")
     (other . "gnu"))))
 '(column-number-mode t)
 '(custom-enabled-themes (quote (wombat)))
 '(delete-old-versions t)
 '(display-battery-mode t)
 '(display-time-mode t)
 '(font-lock-mode t t (font-lock))
 '(global-display-line-numbers-mode t)
 '(gutter-buffers-tab-visible-p nil)
 '(inhibit-startup-screen t)
 '(kept-new-versions 10)
 '(kept-old-versions 1)
 '(mail-user-agent (quote message-user-agent))
 '(package-selected-packages (quote (yasnippet auto-complete)))
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
 '(user-mail-address "ncmike@ncultra.org")
 '(vc-make-backup-files t)
 '(version-control t))

;(require 'efs)
(require 'tramp)

(setq tramp-default-method "ssh")

;(require 'calendar)
(require 'cscope)
(require 'xcscope)
(require 'cl)
(add-hook 'fundamental-mode 'mail-mode 'auto-fill-mode)

(defun insert-c-token ()
  (interactive)
  (insert "-*- linux-c -*-"))

(defun insert-lm-src ()
  (interactive)
  (insert-file-contents "~/.emacs.d/lm_src_template"))
(global-set-key [f7] 'insert-lm-src)

(defun insert-lm-src-hdr ()
  (interactive)
  (insert-file-contents "~/.emacs.d/lm_src_header"))
(global-set-key [f8] 'insert-lm-src-hdr)

(defun insert-printk-debug()
  (interactive)
  (insert "pr_debug(\"(%s): \\n\", __func__);"))
(global-set-key [f6] 'insert-printk-debug)

(defun insert-log-event-debug()
  (interactive)
   (insert "pr_debug(\"(%s): line %d\\n\", __func__, __LINE__);"))
(global-set-key [f9] 'insert-log-event-debug)

(defun insert-asm-bp()
  (interactive)
  (insert "wmb(); asm volatile(\"int $3\\n\\t\"::: \"memory\");"))
;(global-set-key [f10] 'insert-asm-bp)


(defconst lm
  '( "bsd"
     (c-basic-offset . 2)
     (tab-width . 2)
     (indent-tabs-mode . nil)
     (c-hanging-braces-alist     . ((substatement-open before)
				    (brace-list-open)))
     (c-cleanup-list             . (one-liner-defun
				    empty-defun-braces
				    defun-close-semi
				    list-close-comma
				    c-lineup-C-comments))
     )
  "lm C style")
(c-add-style "lm" lm )

(defconst xen-style
  '( "bsd"
     (c-basic-offset . 4)
     (indent-tabs-mode . nil)
     (tab-width . 4)
     (label . 1)
    )
  "Xen C Programming Style")
(c-add-style "xen" xen-style )

(defun c-lineup-arglist-tabs-only (ignored)
  "Line up argument lists by tabs, not spaces"
  (let* ((anchor (c-langelem-pos c-syntactic-element))
	 (column (c-langelem-2nd-pos c-syntactic-element))
	 (offset (- (1+ column) anchor))
	 (steps (floor offset c-basic-offset)))
    (* (max steps 1)
       c-basic-offset)))

(defconst linux-c-style
	'( "K&R"
	   (c-basic-offset . 8)
	   (c-label-minimum-indentation . 0)
	   (c-offsets-alist . (
			       (arglist-close         . c-lineup-arglist-tabs-only)
			       (arglist-cont-nonempty .
						      (c-lineup-gcc-asm-reg c-lineup-arglist-tabs-only))
			       (arglist-intro         . +)
			       (brace-list-intro      . +)
			       (c                     . c-lineup-C-comments)
			       (case-label            . 0)
			       (comment-intro         . c-lineup-comment)
			       (cpp-define-intro      . +)
			       (cpp-macro             . -1000)
			       (cpp-macro-cont        . +)
			       (defun-block-intro     . +)
			       (else-clause           . 0)
			       (func-decl-cont        . +)
			       (inclass               . +)
			       (inher-cont            . c-lineup-multi-inher)
			       (knr-argdecl-intro     . 0)
			       (label                 . -1000)
			       (statement             . 0)
			       (statement-block-intro . +)
			       (statement-case-intro  . +)
			       (statement-cont        . +)
			       (substatement          . +)
			       ))
	   (indent-tabs-mode . t)
	   (show-trailing-whitespace . t)
	   )
	)
(c-add-style "linux" linux-c-style )
(setq c-default-style "linux")

(add-hook 'c-mode-common-hook
          (lambda () (c-toggle-auto-hungry-state 1)))

(add-hook 'objc-mode-hook
 	  (lambda () (auto-complete-mode t)))
;;(add-hook 'c-mode-common-hook
;;	  (lambda ()
;;	    (yas/minor-mode t)))
;;(add-hook 'objc-mode-hook
;; 	  (lambda () (yas-load-objc)))

(setq auto-mode-alist
	      (append '(("\\.hpp" . c++-mode))  auto-mode-alist ))

(setq auto-mode-alist
      (cons '("\\.m$" . objc-mode) auto-mode-alist))
(setq auto-mode-alist
      (cons '("\\.mm$" . objc-mode) auto-mode-alist))

(setq c-style-variables-are-local-p t)
(setq auto-c-mode-alist
  '(
    ("/linux"          . "linux")
    ("/rmpopt"          . "linux")
    ("/Classes"        . "bsd")
    ("/lm_hypervisor"  . "lm")
    ("/flag-test"      . "lm")
    (""                . "lm")))

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

(defun trim-and-save()
  (interactive)
  (trim-trailing-whitespace)
  (save-buffer 64))

(global-set-key "\C-x\C-s" `trim-and-save)

;; Stolen from (http://endlessparentheses.com/ansi-colors-in-the-compilation-buffer-output.html)
(require 'ansi-color)
(defun endless/colorize-compilation ()
  "Colorize from `compilation-filter-start' to `point'."
  (let ((inhibit-read-only t))
    (ansi-color-apply-on-region
     compilation-filter-start (point))))

(add-hook 'compilation-filter-hook
          #'endless/colorize-compilation)


;; Stolen from (https://oleksandrmanzyuk.wordpress.com/2011/11/05/better-emacs-shell-part-i/)
(defun regexp-alternatives (regexps)
  "Return the alternation of a list of regexps."
  (mapconcat (lambda (regexp)
               (concat "\\(?:" regexp "\\)"))
             regexps "\\|"))

(defvar non-sgr-control-sequence-regexp nil
  "Regexp that matches non-SGR control sequences.")

(setq non-sgr-control-sequence-regexp
      (regexp-alternatives
       '(;; icon name escape sequences
         "\033\\][0-2];.*?\007"
         ;; non-SGR CSI escape sequences
         "\033\\[\\??[0-9;]*[^0-9;m]"
         ;; noop
         "\012\033\\[2K\033\\[1F"
         )))

(defun filter-non-sgr-control-sequences-in-region (begin end)
  (save-excursion
    (goto-char begin)
    (while (re-search-forward
            non-sgr-control-sequence-regexp end t)
      (replace-match ""))))

(defun filter-non-sgr-control-sequences-in-output (ignored)
  (let ((start-marker
         (or comint-last-output-start
             (point-min-marker)))
        (end-marker
         (process-mark
          (get-buffer-process (current-buffer)))))
    (filter-non-sgr-control-sequences-in-region
     start-marker
     end-marker)))

(add-hook 'comint-output-filter-functions
          'filter-non-sgr-control-sequences-in-output)

(defun checkpatch()
  (interactive)
  (compile (concat "~/src/linux-mdday/scripts/checkpatch.pl --emacs --root=/home/mdday/src/linux-mdday/ --strict "  (buffer-file-name))))

(defun checksrc()
  (interactive)
  (compile (concat "~/src/linux-mdday/scripts/checkpatch.pl --emacs --root=/home/mdday/src/linux-mdday/ --file --no-signoff " (buffer-file-name))))

(defun gcc()
  (interactive)
  (compile (concat "gcc -ggdb " (buffer-file-name))))

(defun lmcb()
  (interactive)
  (compile "~/bin/lmcb.sh"))
(global-set-key "\C-B" 'lmcb)

(defun lmb()
  (interactive)
  (compile "~/bin/lmb.sh"))
(global-set-key "\C-x\C-x" 'lmb)

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

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Courier 10 Pitch" :foundry "bitstream" :slant normal :weight normal :height 111 :width normal)))))

;;-------------------more stuff-----------------------

;; a copy key, leaves the text as-is
(global-set-key "\C-Q" 'copy-region-as-kill)

;(global-set-key "\C-B" 'compile)

(defun ew()
  (interactive)
;;  (split-window-horizontally)
  (split-window-horizontally)
  (split-window-vertically)
  (balance-windows))

(defun ew3()
  (interactive)
  (split-window-horizontally)
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
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
