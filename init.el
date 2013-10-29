
;;OSX meta-key fixes

;;(setq mac-option-key-is-meta nil)
;;(setq mac-command-key-is-meta t)
;;(setq mac-command-modifier 'meta)
;;(setq mac-option-modifier nil)

(prefer-coding-system 'utf-8)

;; Disable startup message 
(setq inhibit-startup-message t)

(global-set-key (kbd "C-x C-m") 'shell)

;; Jump to a definition in the current file. 
(global-set-key (kbd "C-x C-i") 'imenu)

(add-to-list 'load-path "~/.emacs.d/vendor/better-defaults")
(require 'better-defaults)

(add-to-list 'load-path "~/.emacs.d/vendor/dash")
(require 'dash)

(add-to-list 'load-path "~/.emacs.d/vendor/s")
(require 's)

(add-to-list 'load-path "~/.emacs.d/vendor/pkg-info")
(require 'pkg-info)

(add-to-list 'load-path "~/.emacs.d/vendor/paredit")
(require 'paredit)

;;(setq make-backup-files nil auto-save-default nil)

(when window-system
  (setq frame-title-format '(buffer-file-name "%f" ("%b")))
  (tooltip-mode -1)
  (mouse-wheel-mode t)
  (blink-cursor-mode -1))

(add-to-list 'safe-local-variable-values '(lexical-binding .t))
(add-to-list 'safe-local-variable-values '(whitespace-line-column . 80))

(add-to-list 'load-path "~/.emacs.d/vendor/ido-ubiquitous")
(require 'ido-ubiquitous)


(ido-ubiquitous-mode)
(setq ido-enable-prefix nil
      ido-enable-flex-matching t
      ido-auto-merge-work-directories-length nil
      ido-create-new-buffer 'always
      ido-use-filename-at-point 'guess
      ido-use-virtual-buffers t
      ido-handle-duplicate-virtual-buffers 2
      ido-max-prospects 10)


(set-default 'indent-tabs-mode nil)
(set-default 'indicate-empty-lines t)
(set-default 'imenu-auto-rescan t)

(add-hook 'text-mode-hook 'turn-on-auto-fill)

(eval-after-load "ispell"
  '(when (executable-find ispell-program-name)
     (add-hook 'text-mode-hook 'turn-on-flyspell)))


;;enable paredit mode
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)


(add-to-list 'load-path "~/.emacs.d/vendor/clojure-mode")
(add-to-list 'load-path "~/.emacs.d/vendor/nrepl")

(require 'clojure-mode)

(defun esk-paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;json to js-mode
(add-to-list 'auto-mode-alist '("\\.json$" . js-mode))

;;;###autoload
(eval-after-load 'js
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (add-hook 'js-mode-hook 'esk-paredit-nonlisp)
          (setq js-indent-level 2)
          ;; fixes problem with pretty function font-lock
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1) "\u0192")
                                 nil)))))))

(require 'nrepl)

;;
;; change font size
;;

(defun increase-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (ceiling (* 1.10
                                  (face-attribute 'default :height)))))

(defun decrease-font-size ()
  (interactive)
  (set-face-attribute 'default nil :height
                      (floor (* 0.9
                                (face-attribute 'default :height)))))

;;
;; key bindings
;;

(global-set-key (kbd "C-=") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)




