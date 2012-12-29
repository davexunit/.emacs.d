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

;; Theme
(load-theme 'solarized-dark t)
(custom-set-faces
 '(default ((t (:family "Monospace" :foundry "xos4" :slant normal :weight normal :height 80 :width normal)))))

;; Extra package repositories.
(require 'package)
(add-to-list 'package-archives '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

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

;; Highlight matching parens and automatically insert pairs.
(show-paren-mode t)
(require 'autopair)
(autopair-global-mode t)

;; Auto-completion is sick!
(require 'auto-complete)
(global-auto-complete-mode t)

;; Syntax checker.
(require 'flymake)
(flymake-mode t)

;; Snippets.
(auto-insert-mode t)
(yas-global-mode t)

;; Coding style.
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq c-basic-offset 4)
(setq c-default-style "k&r" -basic-offset)
(which-function-mode t)

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

;; Messy stuff lives beyond here

;; IRC
(require 'erc)
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '(("rizon.net" "#/g/sicp" "#/g/amedev")
        ("freenode.net" "#mediagoblin" "#libre.fm" "#allegro" "#guile" "#emacs" "#vidyadev")
        ("gimp.org" "#evolution")))
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE" "324" "329" "332" "333" "353" "477"))

;; Nickserv
(load "~/.emacs.d/.ercpasswords")
(add-hook 'erc-after-connect
    	  '(lambda (SERVER NICK)
    	     (cond
    	      ((string-match "freenode\\.net" SERVER)
    	       (erc-message "PRIVMSG" (concat "NickServ identify " freenode-password)))
              
    	      ((string-match "rizon\\.net" SERVER)
    	       (erc-message "PRIVMSG" (concat "NickServ identify " rizon-password))))))

;; Join IRC channels
(defun start-irc ()
  (interactive)
  (erc :server "irc.freenode.net" :port 6667 :nick "davexunit")
  (erc :server "irc.rizon.net"    :port 6667 :nick "davexunit"))

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
