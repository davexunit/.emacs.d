;; Turn off mouse interface early in startup to avoid momentary display
(if (fboundp 'menu-bar-mode) (menu-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(set-fringe-mode 0)

;; No splash screen.
(setq inhibit-startup-message t)

;; Line numbers are good!
(global-linum-mode t)
(column-number-mode t)

;; Extra package repositories.
(require 'package)
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/") t)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

;; Theme
(load-theme 'zenburn t)

;; Remeber open buffers for next session.
(desktop-save-mode t)

;; Interactively do shit!
(require 'ido)
(ido-mode t)
(ido-ubiquitous-mode t)

;; Easy window movement.
(require 'windmove)
(windmove-default-keybindings 'meta)

;; Better buffer naming.
(require 'uniquify)

;; Save point position between sessions
(require 'saveplace)
(setq-default save-place t)
(setq save-place-file (expand-file-name ".places" user-emacs-directory))

;; Highlight matching parens, automatically insert pairs, use rainbow
;; delimiters and use paredit for Lisp buffers.
(require 'autopair)
(require 'rainbow-delimiters)
(global-rainbow-delimiters-mode t)
(show-paren-mode t)
(autopair-global-mode 1)
(add-hook 'emacs-lisp-mode-hook       (lambda () (paredit-mode t)))
(add-hook 'lisp-mode-hook             (lambda () (paredit-mode t)))
(add-hook 'lisp-interaction-mode-hook (lambda () (paredit-mode t)))
(add-hook 'scheme-mode-hook           (lambda () (paredit-mode t)))

;; Auto-completion is sick!
(require 'auto-complete)
(global-auto-complete-mode t)

;; Syntax checker.
(require 'flymake)
(flymake-mode t)

;; Snippets.
(auto-insert-mode t)

;; Coding style.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "k&r" c-basic-offset 4)
(which-function-mode t)

;; Scheme
(put 'syntax-parameterize 'scheme-indent-function 1)
(put 'describe 'scheme-indent-function 1)
(put 'it 'scheme-indent-function 1)
(put 'before-each 'scheme-indent-function 1)
(put 'after-each 'scheme-indent-function 1)
(put 'stub 'scheme-indent-function 1)
(put 'coroutine 'scheme-indent-function 1)

;; Handy functions courtesy of whattheemacs.d.
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
(global-set-key (kbd "C-c p") 'c-declaration-to-prototype)

;; Messy stuff lives beyond here

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
(put 'upcase-region 'disabled nil)
