;;; init.el --- my emacs config file
;;; Commentary:
;;; Code:
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(require 'evil)
(evil-mode 1)

(require 'helm-config)
(global-set-key (kbd "M-x") #'helm-M-x)
(global-set-key (kbd "M-y") #'helm-show-kill-ring)
(global-set-key (kbd "C-x C-b") #'helm-mini)
(global-set-key (kbd "C-x r b") #'helm-filtered-bookmarks)
(global-set-key (kbd "C-x C-f") #'helm-find-files)
(helm-mode 1)
(helm-autoresize-mode t)
(setq-default helm-autoresize-max-height 20
              helm-autoresize-min-height 20)

(require 'all-the-icons)
(require 'neotree)
(global-set-key [f4] 'neotree-toggle)
(setq neo-theme 'icons)
(evil-define-key 'normal neotree-mode-map (kbd "TAB") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "SPC") 'neotree-quick-look)
(evil-define-key 'normal neotree-mode-map (kbd "q") 'neotree-hide)
(evil-define-key 'normal neotree-mode-map (kbd "RET") 'neotree-enter)
(evil-define-key 'normal neotree-mode-map (kbd "g") 'neotree-refresh)
(evil-define-key 'normal neotree-mode-map (kbd "n") 'neotree-next-line)
(evil-define-key 'normal neotree-mode-map (kbd "p") 'neotree-previous-line)
(evil-define-key 'normal neotree-mode-map (kbd "A") 'neotree-stretch-toggle)
(evil-define-key 'normal neotree-mode-map (kbd "H") 'neotree-hidden-file-toggle)

(require 'highlight-symbol)
(highlight-symbol-nav-mode)
(add-hook 'prog-mode-hook 'highlight-symbol-mode)
(setq highlight-symbol-on-navigation-p t)
(global-set-key [(control f3)] 'highlight-symbol)
(global-set-key [f3] 'highlight-symbol-next)
(global-set-key [(shift f3)] 'highlight-symbol-prev)
(global-set-key [(meta f3)] 'highlight-symbol-query-replace)

(require 'highlight-indent-guides)
(add-hook 'prog-mode-hook 'highlight-indent-guides-mode)
(setq highlight-indent-guides-method 'column)

(require 'dashboard)
(dashboard-setup-startup-hook)
(setq dashboard-items '((projects . 5)
                        (recents . 15)))
(evil-define-key 'normal dashboard-mode-map (kbd "TAB") 'dashboard-next-section)

(require 'ace-popup-menu)
(ace-popup-menu-mode 1)

;; ======== PROJECTILE =============
(projectile-mode +1)
;(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

(require 'helm-projectile)
(helm-projectile-on)
(setq projectile-switch-project-action 'neotree-projectile-action)
;; ============================

(require 'magit)
(require 'evil-magit)
(global-set-key (kbd "C-c g") 'magit-status)

(require 'company)
(add-hook 'after-init-hook 'global-company-mode)
(setq company-backends (delete 'company-semantic company-backends))
(setq company-backends (delete 'company-capf company-backends))
;(add-to-list 'company-backends 'company-c-headers)

(add-to-list 'default-frame-alist '(font . "M+ 1mn-9" ))
(set-face-attribute 'default t :font "M+ 1mn-9" )

;; Looks configurations
(tool-bar-mode 0)
(scroll-bar-mode 0)
(menu-bar-mode 0)
(blink-cursor-mode 0)
(setq make-backup-files nil)
(setq auto-save-default nil)
(setq inhibit-splash-screen t)
(global-linum-mode t)
(load-theme 'atom-one-dark t)

(show-paren-mode t)

;; Indentation and code formatting
(setq standard-indent 4)
(setq tab-width 4)
(setq indent-tabs-mode nil)

(global-set-key (kbd "RET") 'newline-and-indent)

(setq-default c-basic-offset 4
              tab-width 4
              indent-tabs-mode nil)

;; activate whitespace-mode to view all whitespace characters
(global-set-key (kbd "C-c w") 'whitespace-mode)

(require 'smartparens-config)
(show-smartparens-global-mode +1)
(smartparens-global-mode 1)

(require 'vimish-fold)
(require 'evil-vimish-fold)
(vimish-fold-mode 1)
(evil-vimish-fold-mode 1)

(require 'dimmer)
(dimmer-mode)

(require 'git-gutter-fringe+)
(global-git-gutter+-mode)
(git-gutter-fr+-minimal)

;; when you press RET, the curly braces automatically
;; add another newline
(sp-with-modes '(c-mode c++-mode)
  (sp-local-pair "{" nil :post-handlers '(("||\n[i]" "RET")))
  (sp-local-pair "/*" "*/" :post-handlers '((" | " "SPC")
                                            ("* ||\n[i]" "RET"))))

;; show unncessary whitespace that can mess up your diff
(add-hook 'prog-mode-hook (lambda () (interactive) (setq show-trailing-whitespace 1)))
(add-hook 'after-init-hook #'global-flycheck-mode)

(require 'yasnippet)
(yas-global-mode 1)

;; C++ config
(require 'irony)
(add-hook 'c++-mode-hook 'irony-mode)
(add-hook 'c-mode-hook 'irony-mode)
(add-hook 'c++-mode-hook 'modern-c++-font-lock-mode)
(add-hook 'c++-mode-hook
          (lambda () (setq-default flycheck-clang-language-standard "c++17")))

(defun my-irony-mode-hook ()
  (define-key irony-mode-map [remap completion-at-point]
    'irony-completion-at-point-async)
  (define-key irony-mode-map [remap complete-symbol]
    'irony-completion-at-point-async))
(add-hook 'irony-mode-hook 'my-irony-mode-hook)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)
(add-hook 'irony-mode-hook 'company-irony-setup-begin-commands)
(setq irony-additional-clang-options '("-std=c++17"))
(require 'company-irony-c-headers)
(eval-after-load 'company
  '(add-to-list
    'company-backends '(company-irony-c-headers company-irony)))

(require 'rtags)
(define-key c-mode-base-map [f2] 'rtags-find-symbol-at-point)
(define-key c-mode-base-map (kbd "C-c r") 'rtags-find-references-at-point)
(define-key c-mode-base-map (kbd "C-c v") 'rtags-find-virtuals-at-point)
(define-key c-mode-base-map (kbd "M-<left>") 'rtags-location-stack-back)
(define-key c-mode-base-map (kbd "M-<right>") 'rtags-location-stack-forward)
(setq-default rtags-completions-enabled t)
(eval-after-load 'company
  '(add-to-list
    'company-backends 'company-rtags))
(setq-default rtags-use-helm t)
(setq-default rtags-display-result-backend 'helm)
(add-hook 'c-mode-hook 'rtags-start-process-unless-running)
(add-hook 'c++-mode-hook 'rtags-start-process-unless-running)
(add-hook 'kill-emacs-hook 'rtags-quit-rdm)

;(require 'cmake-ide)
;(cmake-ide-setup)
;(setq cmake-ide-flags-c++ (append '("-std=c++17")))

;(add-hook 'c++-mode-hook 'flycheck-mode-hook)
;(add-hook 'c-mode-hook 'flycheck-mode-hook)
;(eval-after-load 'flycheck '(add-hook 'flycheck-mode-hook #'flycheck-irony-setup))
;(add-hook 'c++-mode-hook (lambda () (setq flycheck-clang-language-standard "c++17")))

(require 'srefactor)
(semantic-mode 1)
(define-key c-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)
(define-key c++-mode-map (kbd "M-RET") 'srefactor-refactor-at-point)

(define-key c++-mode-map [f9] 'gud-break)
(define-key c++-mode-map [f10] 'gud-next)
(define-key c++-mode-map [f11] 'gud-step)

(setq company-idle-delay 0)
(define-key c-mode-map (kbd "C-SPC") 'company-complete)
(define-key c++-mode-map (kbd "C-SPC") 'company-complete)
(setq company-idle-delay 0.2)

(require 'function-args)
(fa-config-default)

(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
(setq-default c-default-style "stroustrup")

(add-to-list 'c-offsets-alist '(arglist-close . c-lineup-close-paren))
;(add-to-list 'c-offsets-alist '(arglist-cont . 0))
;(defadvice c-lineup-arglist (around my activate)
;  "Improve indentation of continued C++11 lambda function opened as argument."
;  (setq ad-return-value
;        (if (and (equal major-mode 'c++-mode)
;                 (ignore-errors
;                   (save-excursion
;                     (goto-char (c-langelem-pos langelem))
;                     ;; Detect "[...](" or "[...]{". preceded by "," or "(",
;                     ;;   and with unclosed brace.
;                     (looking-at ".*[(,][ \t]*\\[[^]]*\\][ \t]*[({][^}]*$"))))
;            0                           ; no additional indent
;          ad-do-it)))                   ; default behavior

(defun insert-include-guards ()
  "Insert C include guards."
  (let* ((filename (file-name-nondirectory (file-name-sans-extension buffer-file-name)))
         (define (concat (upcase filename) "_H_INCLUDED")))
    (progn
      (insert (concat "#ifndef " define "\n"))
      (insert (concat "#define" define "\n"))
      (insert "\n\n\n")
      (insert (concat "#endif // " define)))))

(defun insert-class-definition ()
  "Insert C++ class definition."
  (let ((class-name (read-from-minibuffer "Class name: ")))
    (progn
      (insert (concat "class " class-name " {\n"))
      (insert "public:\n")
      (insert "private:\n")
      (insert "};\n"))))

(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

; Haskell config
;(add-hook 'haskell-mode-hook #'hindent-mode)
;(eval-after-load 'haskell-mode
;  '(define-key haskell-mode-map [f8] 'haskell-navigate-imports))

;(let ((my-cabal-path (expand-file-name "~/.cabal/bin")))
;  (setenv "PATH" (concat my-cabal-path path-separator (getenv "PATH")))
;  (add-to-list 'exec-path my-cabal-path))
;(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 ;'(company-ghc-show-info t)
 ;'(haskell-process-auto-import-loaded-modules t)
 ;'(haskell-process-log t)
 ;'(haskell-process-suggest-remove-import-lines t)
 ;'(haskell-process-type (quote stack-ghci))
 ;'(haskell-tags-on-save t)
 ;'(package-selected-packages
 ;  (quote
 ;   (git-gutter-fringe+ evil-vimish-fold vimish-fold yatemplate yasnippet-snippets yasnippet ace-popup-menu dashboard markdown-mode all-the-icons d-mode evil-quickscope highlight highlight-blocks highlight-symbol rtags cmake-font-lock cmake-mode diff-hl highlight-unique-symbol modern-cpp-font-lock highlight-indent-guides srefactor neotree magit helm flycheck-irony flycheck-hdevtools flycheck-haskell flycheck-ghcmod flycheck-clangcheck evil-smartparens company-irony-c-headers company-irony company-ghci company-ghc company-c-headers cmake-ide auto-complete atom-one-dark-theme))))

;(eval-after-load 'haskell-mode '(progn
;  (define-key haskell-mode-map (kbd "C-c C-l") 'haskell-process-load-or-reload)
;  (define-key haskell-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;  (define-key haskell-mode-map (kbd "C-c C-n C-t") 'haskell-process-do-type)
;  (define-key haskell-mode-map (kbd "C-c C-n C-i") 'haskell-process-do-info)
;  (define-key haskell-mode-map (kbd "C-c C-n C-c") 'haskell-process-cabal-build)
;  (define-key haskell-mode-map (kbd "C-c C-n c") 'haskell-process-cabal)))
;(eval-after-load 'haskell-cabal '(progn
;  (define-key haskell-cabal-mode-map (kbd "C-c C-z") 'haskell-interactive-switch)
;  (define-key haskell-cabal-mode-map (kbd "C-c C-k") 'haskell-interactive-mode-clear)
;  (define-key haskell-cabal-mode-map (kbd "C-c C-c") 'haskell-process-cabal-build)
;  (define-key haskell-cabal-mode-map (kbd "C-c c") 'haskell-process-cabal)))
;(eval-after-load 'haskell-mode
;  '(define-key haskell-mode-map (kbd "C-c C-o") 'haskell-compile))
;(eval-after-load 'haskell-cabal
;  '(define-key haskell-cabal-mode-map (kbd "C-c C-o") 'haskell-compile))

;(autoload 'ghc-init "ghc" nil t)
;(autoload 'ghc-debug "ghc" nil t)
;(add-hook 'haskell-mode-hook (lambda () (ghc-init)))

;(add-hook 'haskell-mode-hook 'structured-haskell-mode)

;(add-to-list 'company-backends 'company-ghc)

;(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
; )
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (makefile-executor dimmer function-args evil-magit helm-projectile projectile yatemplate yasnippet-snippets srefactor rtags neotree modern-cpp-font-lock markdown-mode magit highlight-unique-symbol highlight-symbol highlight-indent-guides highlight-blocks highlight helm git-gutter-fringe+ flycheck-irony flycheck-hdevtools flycheck-haskell flycheck-ghcmod flycheck-clangcheck evil-vimish-fold evil-smartparens evil-quickscope diff-hl dashboard d-mode company-irony-c-headers company-irony company-ghci company-ghc company-c-headers cmake-ide cmake-font-lock auto-complete atom-one-dark-theme all-the-icons ace-popup-menu))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
