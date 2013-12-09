;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))

;; No splash screen.
(setq inhibit-startup-message t)

;; Extra package repositories.
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; For stuff that is not in a package repository
(let ((default-directory "~/.emacs.d/lisp"))
  (normal-top-level-add-subdirs-to-load-path))

;; Theme
(load-theme 'wombat t)

;; Use /bin/sh for shell.
;;
;; I like to use fish shell as my login shell, but there are
;; incompatabilities with its scripting language.
(setq shell-file-name "/bin/sh")

;; ido
(setq ido-everywhere t)
(setq ido-enable-flex-matching t)
(ido-mode t)
(ido-ubiquitous-mode t)

;; Handy line-related things
(global-linum-mode t)
(column-number-mode t)

;; Remember open buffers for next session.
(desktop-save-mode t)

;; Display battery level in modeline if a battery is present.
(when (and (boundp 'battery-status-function)
           (not (string-match-p
                 "N/A"
                 (battery-format
                  "%B"
                  (funcall battery-status-function)))))
  (display-battery-mode t))

;; Kill buffers that haven't been modified in awhile.
(require 'midnight)

;; Easy window movement.
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Snippets.
(auto-insert-mode t)

;; Tabs and alignment
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq c-basic-indent 2)
(setq c-default-style "k&r")
(setq c-basic-offset 4)
(setq tab-width 2)
(which-function-mode t)
(electric-indent-mode t)

;;;
;;; Javascript
;;;

(setq js-indent-level 2)
(setq js2-basic-offset 2)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

;;;
;;; Lisp
;;;

;; Highlight matching parens, automatically insert pairs, use rainbow
;; delimiters and use paredit for Lisp buffers.
(require 'rainbow-delimiters)
(require 'paredit)
(global-rainbow-delimiters-mode t)
(show-paren-mode t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode t)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode t)))

;;;
;;; Scheme
;;;

(put 'syntax-parameterize 'scheme-indent-function 1)
(put 'colambda 'scheme-indent-function 1)
(put 'codefine 'scheme-indent-function 1)
(put 'with-agenda 'scheme-indent-function 1)
(put 'gl-begin 'scheme-indent-function 1)
(put 'with-gl-push-matrix 'scheme-indent-function 0)
(put 'with-gl-bind-texture 'scheme-indent-function 2)
(put 'with-sprite-batch 'scheme-indent-function 1)
(put 'with-test-prefix 'scheme-indent-function 0)

;;;
;;; Ruby
;;;

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))

;;;
;;; SQL
;;;

;; Don't wrap lines so that table listings with a lot of columns remain readable.
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

;;;
;;; StumpWM
;;;

(require 'stumpwm-mode)
(add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . common-lisp-mode))

;;;
;;; ibuffer
;;;

(setq ibuffer-saved-filter-groups
      '(("default"
         ("guile-2d" (filename . "Code/guile-2d/"))
         ("dired" (mode . dired-mode))
         ("org" (mode . org-mode))
         ("erc" (mode . erc-mode)))))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;;;
;;; Elfeed
;;;

(setq elfeed-feeds
      '("http://planet.gnu.org/rss20.xml"
        "http://cs.worcester.edu/blog/feed/"))

;; Load machine specific emacs configuration
(defvar local-config-filename "~/.emacs.d/local.el")
(if (file-exists-p local-config-filename)
    (load local-config-filename))

;; Handy functions courtesy of whattheemacs.d
(defun open-line-below ()
  (interactive)
  (if (eolp)
      (newline)
    (end-of-line)
    (newline))
  (indent-for-tab-command))

(defun open-line-above ()
  (interactive)
  (beginning-of-line)
  (newline)
  (forward-line -1)
  (indent-for-tab-command))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

(defun change-theme (theme)
  "Disable all active themes and load THEME."
  (interactive
   (lexical-let ((themes (mapcar 'symbol-name (custom-available-themes))))
     (list (intern (completing-read "Load custom theme: " themes)))))
  (mapc 'disable-theme custom-enabled-themes)
  (load-theme theme t))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; erc configuration
(load "~/.emacs.d/erc.el")

;; Keybinds
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "RET") 'newline-and-indent)
(global-set-key (kbd "<C-return>") 'open-line-below)
(global-set-key (kbd "<C-M-return>") 'open-line-above)
(global-set-key (kbd "C-c c") 'comment-or-uncomment-region)
(global-set-key (kbd "C-c g") 'magit-status)
(global-set-key (kbd "C-c i") 'start-irc)
(global-set-key (kbd "C-x g") 'goto-line)
(global-set-key (kbd "C-x p") 'package-list-packages)
(global-set-key (kbd "C-c C-f") 'ff-find-other-file)
(global-set-key (kbd "C-c s") 'geiser-connect)
(global-set-key (kbd "C-c b") 'bundle-install)
(global-set-key (kbd "C-c r") 'rinari-rake)
(global-set-key (kbd "C-c f") 'elfeed)
(global-set-key (kbd "M-%") 'query-replace-regexp)
;; No more minimizing Emacs by accident.
(global-unset-key (kbd "C-z"))

;; Enable some disabled-by-default functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
