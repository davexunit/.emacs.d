;; Exec path tweak for stuff installed via brew
(push "/usr/local/bin" exec-path)

;; The latest ruby-test-mode doesn't work with rspec 1.3.2
;;(setq package-load-list '((ruby-test-mode "1.1") all))

;; Font
(set-default-font "Source_Code_Pro-14")

;; Helpful keyboard tweak to make things easier.
(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'super)

;; Seems to fix some of the graphical glitches with linum
(set-fringe-mode '(8 . 0))
;; No tabs damn it!
(setq-default indent-tabs-mode nil)
(setq tab-width 4)
(setq indent-tabs-mode nil)
(setq js-indent-level 2)
(setq c-basic-indent 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-markup-indent-offset 2)
(setq web-mode-indent-style 2)
(setq js2-basic-offset 2)

;; Ruby
(require 'rvm)
(require 'ruby-test-mode)
(require 'feature-mode)
(require 'ruby-tools)
(require 'inf-ruby)
(require 'rinari)
(require 'yaml-mode)

;; RVM
(rvm-use "ree-1.8.7" "rails2311")

;; Rinari is an awesome Rails development minor mode
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

;; YAML mode
(add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; Rinari (Minor Mode for Ruby On Rails)
;; (setq rinari-major-modes
;;       (list 'mumamo-after-change-major-mode-hook 'dired-mode-hook 'ruby-mode-hook
;;             'css-mode-hook 'yaml-mode-hook 'javascript-mode-hook))

;; Fix newline-and-indent in ruby-mode
;; (add-hook 'ruby-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "RET") 'reindent-then-newline-and-indent)))

;; Use web-mode for editing code embedded in HTML.
(add-to-list 'load-path "~/.emacs.d/elisp/web-mode")
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))

;; MySQL
(setq sql-mysql-program "/usr/local/bin/mysql")
(setq sql-user "root")
(setq sql-password "")
(setq sql-server "localhost")

;; Some customizations done from an Emacs menu
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(custom-safe-themes (quote ("501caa208affa1145ccbb4b74b6cd66c3091e41c5bb66c677feda9def5eab19c" "d2622a2a2966905a5237b54f35996ca6fda2f79a9253d44793cfe31079e3c92b" default)))
 '(desktop-save-mode t)
 '(ido-enable-flex-matching t)
 '(ido-mode (quote both) nil (ido))
 '(ido-ubiquitous-mode t)
 '(save-place t nil (saveplace))
 '(show-paren-mode t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
