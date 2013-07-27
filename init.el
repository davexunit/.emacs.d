;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-fringe-mode 0)

;; No splash screen.
(setq inhibit-startup-message t)

;; Extra package repositories.
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Theme
(load-theme 'soothe t)

;; Handy line-related things
(global-linum-mode t)
(column-number-mode t)
(global-hl-line-mode t)

;; Remember open buffers for next session.
(desktop-save-mode t)

;; Kill buffers that haven't been modified in awhile.
(require 'midnight)

;; Interactively do shit!
(require 'ido)
(ido-mode t)
(ido-ubiquitous-mode t)

;; Easy window movement.
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Highlight matching parens, automatically insert pairs, use rainbow
;; delimiters and use paredit for Lisp buffers.
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(show-paren-mode t)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode t)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode t)))

;; Auto-completion is sick!
(require 'auto-complete)
(global-auto-complete-mode t)

;; Snippets.
(auto-insert-mode t)

;; Tabs and alignment
(setq-default indent-tabs-mode nil)
(setq indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-basic-indent 2)
(setq c-default-style "k&r" c-basic-offset 4)
(setq tab-width 4)
(setq js-indent-level 2)
(setq js2-basic-offset 2)
(which-function-mode t)
(electric-indent-mode t)

;; Scheme
(put 'syntax-parameterize 'scheme-indent-function 1)
(put 'coroutine 'scheme-indent-function 1)
(put 'define-coroutine 'scheme-indent-function 1)
(put 'gl-begin 'scheme-indent-function 1)
(put 'with-gl-bind-texture 'scheme-indent-function 2)

;; Ruby
(require 'rvm)
(require 'ruby-test-mode)
(require 'rinari)

;; Rinari is an awesome for Rails development.
(global-rinari-mode t)

;; Rake files are ruby, too, as are gemspecs, rackup files, etc.
(add-to-list 'auto-mode-alist '("\\.rake$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.thor$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.gemspec$" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\.ru$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Rakefile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Thorfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Gemfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Capfile$" . ruby-mode))
(add-to-list 'auto-mode-alist '("Vagrantfile$" . ruby-mode))

;; SQL
;; Don't wrap lines so that table listings with a lot of columns remain readable.
(add-hook 'sql-interactive-mode-hook (lambda () (setq truncate-lines t)))

;;;
;;; StumpWM
;;;

(add-to-list 'auto-mode-alist '("\\.stumpwmrc$" . stumpwm-mode))

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

;; Turn a C function declaration into a function prototype.
(defun c-declaration-to-prototype ()
  (interactive)
  (move-end-of-line nil)
  (when (char-equal (char-before) (string-to-char ";"))
    (delete-backward-char 1))
  (insert " {")
  (newline-and-indent)
  (newline)
  (insert "}")
  (previous-line)
  (move-end-of-line nil))

(defun cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a before-save-hook, and that
might be bad."
  (interactive)
  (delete-trailing-whitespace)
  (set-buffer-file-coding-system 'utf-8))

;; Various superfluous white-space. Just say no.
(add-hook 'before-save-hook 'cleanup-buffer-safe)

;; erc configuration
(load "~/.emacs.d/erc.el")

;; Keybinds
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

;; Enable some disabled-by-default functions
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; Messy stuff lives beyond here
;; TODO: Move to its own file

;; Patch for linum-mode
(defun linum-update-window (win)
  "Update line numbers for the portion visible in window WIN."
  (goto-char (window-start win))
  (let ((line (line-number-at-pos))
        (limit (window-end win t))
        (fmt (cond ((stringp linum-format) linum-format)
                   ((eq linum-format 'dynamic)
                    (let ((w (length (number-to-string
                                      (count-lines (point-min) (point-max))))))
                      (concat " %" (number-to-string w) "d ")))))
        (width 0))
    (run-hooks 'linum-before-numbering-hook)
    ;; Create an overlay (or reuse an existing one) for each
    ;; line visible in this window, if necessary.
    (while (and (not (eobp)) (<= (point) limit))
      (let* ((str (if fmt
                      (propertize (format fmt line) 'face 'linum)
                    (funcall linum-format line)))
             (visited (catch 'visited
                        (dolist (o (overlays-in (point) (point)))
                          (when (equal-including-properties
                                 (overlay-get o 'linum-str) str)
                            (unless (memq o linum-overlays)
                              (push o linum-overlays))
                            (setq linum-available (delq o linum-available))
                            (throw 'visited t))))))
        (setq width (max width (length str)))
        (unless visited
          (let ((ov (if (null linum-available)
                        (make-overlay (point) (point))
                      (move-overlay (pop linum-available) (point) (point)))))
            (push ov linum-overlays)
            (overlay-put ov 'before-string
                         (propertize " " 'display `((margin left-margin) ,str)))
            (overlay-put ov 'linum-str str))))
      ;; Text may contain those nasty intangible properties, but that
      ;; shouldn't prevent us from counting those lines.
      (let ((inhibit-point-motion-hooks t))
        (forward-line))
      (setq line (1+ line)))
    (set-window-margins win width (cdr (window-margins win)))))
