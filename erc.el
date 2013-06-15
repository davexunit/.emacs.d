(require 'erc)
(require 'erc-log)
(require 'erc-notify)
(require 'erc-spelling)
(require 'erc-autoaway)
(require 'erc-services)
(require 'notifications)

;; Notify the user of private messages
(defun erc-private-message-notify (proc parsed)
  (let ((nick (car (erc-parse-user (erc-response.sender parsed))))
        (target (car (erc-response.command-args parsed)))
        (msg (erc-response.contents parsed)))
    (notifications-notify :title nick :body msg)))

;; (add-hook 'erc-server-PRIVMSG-functions 'erc-private-message-notify)

;; Notify the user when their nick is mentioned
(defun erc-nick-mentioned-notify (match-type nick message)
  "Shows a notification when user's nick is mentioned."
  (when (and (not (posix-string-match "^\\** *Users on #" message))
             (eq math-type 'current-nick))
    (notifications-notify
     :title nick
     :body message
     :app-icon "/usr/share/notify-osd/icons/gnome/scalable/status/notification-message-im.svg"
     :urgency 'low)))

;; (add-hook 'erc-text-matched-hook 'erc-nick-mentioned-notify)

;; Nickname
(setq erc-nick "davexunit")

;; Interpret mIRC-style color commands in IRC chats
(setq erc-interpret-mirc-color t)

;; The following are commented out by default, but users of other
;; non-Emacs IRC clients might find them useful.
;; Kill buffers for channels after /part
(setq erc-kill-buffer-on-part t)
;; Kill buffers for private queries after quitting the server
(setq erc-kill-queries-on-quit t)
;; Kill buffers for server messages after quitting the server
(setq erc-kill-server-buffer-on-quit t)

;; open query buffers in the current window
(setq erc-query-display 'buffer)

;; exclude boring stuff from tracking
(erc-track-mode t)
(setq erc-track-exclude-types '("JOIN" "NICK" "PART" "QUIT" "MODE"
                                "324" "329" "332" "333" "353" "477"))

;; logging
(setq erc-log-channels-directory "~/.erc/logs/")

(if (not (file-exists-p erc-log-channels-directory))
    (mkdir erc-log-channels-directory t))

(setq erc-save-buffer-on-part t)

;; truncate long irc buffers
(erc-truncate-mode +1)

;; enable spell checking
(erc-spelling-mode 0)

;; utf-8 always and forever
(setq erc-server-coding-system '(utf-8 . utf-8))

;; Auto-join channels
(erc-autojoin-mode t)
(setq erc-autojoin-channels-alist
      '(("rizon.us" "#/g/sicp" "#/g/amedev")
        ("freenode.net" "#mediagoblin" "#libre.fm" "#allegro" "#guile" "#emacs"
         "#vidyadev" "#libreplanet-gaming" "diaspora-dev")))

;; Secret password file
(load "~/.emacs.d/.ercpasswords")

;; Stuff to make authenticating with Rizon work
(erc-services-mode t)
(setq erc-prompt-for-nickserv-password nil)
(setq erc-nickserv-identify-mode 'autodetect)
(setq erc-nickserv-passwords
      `((Rizon (("davexunit" . ,rizon-password)))))

;; Start and stop erc
(defun start-irc ()
  "Connect to IRC."
  (interactive)
  (erc :server "irc.rizon.us"     :port 6667 :nick erc-nick :password rizon-password)
  (erc :server "irc.freenode.net" :port 6667 :nick erc-nick :password freenode-password))

(defun filter-erc-server-buffers ()
  (delq nil
        (mapcar
         (lambda (x) (and (erc-server-buffer-p x) x))
         (buffer-list))))

(defun stop-irc ()
  "Disconnects from all irc servers"
  (interactive)
  (dolist (buffer (filter-erc-server-buffers))
    (message "Server buffer: %s" (buffer-name buffer))
    (with-current-buffer buffer
      (erc-quit-server "Later"))))
