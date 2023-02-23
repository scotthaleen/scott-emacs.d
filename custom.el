

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(gnutls-algorithm-priority "normal:-vers-tls1.3")
 '(package-selected-packages
   '(racket-mode go-imports hcl-mode org-tree-slide dockerfile-mode python-mode go-mode janet-mode csv-mode jq-mode zmq yasnippet yaml-mode which-key web-mode undo-tree smartrep rainbow-delimiters projectile operate-on-number neotree monokai-theme minimap markdown-mode magit json-mode highlight-symbol helm flycheck find-file-in-project fill-column-indicator feature-mode el-get dired-details cyberpunk-theme company all-the-icons ag ac-slime)))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(load "~/.emacs.d/restclient-jq.el")

;; rss ticker
(require 'newsticker)
;;(require 'w3m)
;;(setq newsticker-html-renderer 'w3m-region)

(setq newsticker-retrieval-interval 600)
(setq newsticker-url-list-defaults nil)
(setq newsticker-url-list
      '(("Hacker News Top" "https://hnrss.org/newest?points=200&count=50")
        ("Hacker News Front Page" "https://hnrss.org/frontpage?count=50")))

(global-set-key (kbd "C-c t s") 'newsticker-start)
(global-set-key (kbd "C-c t n") 'newsticker-treeview)

(setq python-shell-interpreter "python"
      ;;python-shell-interpreter-args "console --simple-prompt"
      ;;python-shell-prompt-detect-failure-warning nil
      )

;;(add-to-list 'python-shell-completion-native-disabled-interpreters             "python")

(setq python-shell-completion-native-disabled-interpreters '("python"))

;;(defun rollin () (interactive (insert "(._.) ( |:) (.-.) (;| ) (._.) ( |:) (.-.)")))

(defun prom-docker () (interactive) (insert-file-contents "~/.emacs.d/custom-inserts/docker.tmpl" nil))

(global-set-key (kbd "C-c 8 d s") 'scott-test)

;; Term

(use-package term
  :config
  (setq explicit-shell-file-name "/usr/local/bin/bash")
  ;;(setq explicit-zsh-args '())         ;; Use 'explicit-<shell>-args for shell-specific args

  ;; Match the default Bash shell prompt.  Update this if you have a custom prompt
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))



;;;
;; install eterm-color eterm-256color
;; tic -o ~/.terminfo ~/Source/emacs/etc/e/eterm-color.ti
;; tic -o ~/.terminfo ~/.emacs.d/elpa/eterm-256color-20210224.2241/eterm-256color.ti
;;

(use-package eterm-256color
  :hook (term-mode . eterm-256color-mode))

(add-hook 'term-mode-hook '(lambda () (linum-mode 0)))


(defun bash-term ()
  (interactive)
  (term "/bin/local/bin/bash"))


;; Org Mode Configuration ------------------------------------------------------
;;; https://github.com/daviwil/emacs-from-scratch/blob/1a13fcf0dd6afb41fce71bf93c5571931999fed8/init.el

(defun custom/org-mode-setup ()
  (org-indent-mode)
  (variable-pitch-mode 1)
  (visual-line-mode 1))

(defun custom/org-font-setup ()
  ;; Replace list hyphen with dot
  (font-lock-add-keywords 'org-mode
                          '(("^ *\\([-]\\) "
                             (0 (prog1 () (compose-region (match-beginning 1) (match-end 1) "•"))))))

  ;; Set faces for heading levels
  (dolist (face '((org-level-1 . 1.2)
                  (org-level-2 . 1.1)
                  (org-level-3 . 1.05)
                  (org-level-4 . 1.0)
                  (org-level-5 . 1.1)
                  (org-level-6 . 1.1)
                  (org-level-7 . 1.1)
                  (org-level-8 . 1.1)))
    (set-face-attribute (car face) nil :font "Cantarell" :weight 'regular :height (cdr face)))

  ;; Ensure that anything that should be fixed-pitch in Org files appears that way
  (set-face-attribute 'org-block nil :foreground nil :inherit 'fixed-pitch)
  (set-face-attribute 'org-code nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-table nil   :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-verbatim nil :inherit '(shadow fixed-pitch))
  (set-face-attribute 'org-special-keyword nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-meta-line nil :inherit '(font-lock-comment-face fixed-pitch))
  (set-face-attribute 'org-checkbox nil :inherit 'fixed-pitch))

(use-package org
  :hook (org-mode . custom/org-mode-setup)
  :config
  (setq org-ellipsis " ▾")
  (custom/org-font-setup))

(use-package org-bullets
  :after org
  :hook (org-mode . org-bullets-mode)
  :custom
  (org-bullets-bullet-list '("◉" "○" "●" "○" "●" "○" "●")))

(defun custom/org-mode-visual-fill ()
  (setq visual-fill-column-width 125
        visual-fill-column-center-text t)
  (visual-fill-column-mode 1))

(use-package visual-fill-column
  :hook (org-mode . custom/org-mode-visual-fill))

(defun custom/disable-linum-mode ()
  (linum-mode 0))

(add-hook 'org-mode-hook 'custom/disable-linum-mode)


(defun custom/org-confirm-babel-evaluate (lang body)
  (not (or (string= lang "python")
           (string= lang "bash")
           (string= lang "zsh")
           (string= lang "sh"))))

(setq org-confirm-babel-evaluate #'custom/org-confirm-babel-evaluate)


(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

(add-hook 'after-load-theme-hook
    (lambda ()
      ;; (let ((cyberpunk-blue-5 "#4c83ff")
      ;;       (cyberpunk-gray-5 "#333333")
      ;;       (cyberpunk-gray-6 "#1A1A1A")
      ;;       (cyberpunk-gray-7 "#4D4D4D"))

      ;;   (set-face-attribute 'mode-line nil
      ;;                       :background cyberpunk-gray-5
      ;;                       :foreground cyberpunk-blue-5
      ;;                       :box '(:line-width 6 :color cyberpunk-gray-5)
      ;;                       :overline nil
      ;;                       :underline nil)

      ;;   (set-face-attribute 'mode-line-inactive nil
      ;;                       :background cyberpunk-gray-6
      ;;                       :foreground cyberpunk-gray-7
      ;;                       :box '(:line-width 6 :color cyberpunk-gray-6)
      ;;                       :overline nil
      ;;                       :underline nil))

      (set-face-attribute 'mode-line nil
                          :background "#353644"
                          :foreground "#ffffff"
                          :box '(:line-width 8 :color "#353644")
                          :overline nil
                          :underline nil)

      (set-face-attribute 'mode-line-inactive nil
                          :background "#565063"
                          :foreground "#ffffff"
                          :box '(:line-width 8 :color "#565063")
                          :overline nil
                          :underline nil)

))
(defun custom/presentation-setup ()
  ;; Hide the mode line
  (hide-mode-line-mode 1)
  (setq org-hide-emphasis-markers t)
  ;; Hide org-meta-lines
  (hide-lines-matching "^#+")
  (set-face-attribute 'org-meta-line nil :foreground (face-attribute 'default :background))

  ;; Display images inline
  (org-display-inline-images) ;; Can also use org-startup-with-inline-images

  ;; Scale the text.  The next line is for basic scaling:
  (setq text-scale-mode-amount 2)
  (text-scale-mode 1)

  )


  ;; This option is more advanced, allows you to scale other faces too
  ;; (setq-local face-remapping-alist '((default (:height 2.0) variable-pitch)
  ;;                                    (org-verbatim (:height 1.75) org-verbatim)
  ;;                                    (org-block (:height 1.25) org-block))))

(defun custom/presentation-end ()
  (setq text-scale-mode-amount 1)
  (setq org-hide-emphasis-markers nil)
  ;; Show the mode line again
  (hide-mode-line-mode 0)
  (hide-lines-show-all)
  (set-face-attribute 'org-meta-line nil :foreground nil)
  )

(use-package org-tree-slide
  :hook ((org-tree-slide-play . custom/presentation-setup)
         (org-tree-slide-stop . custom/presentation-end))

  :custom
  ;;(org-tree-slide-slide-in-effect t)
  (org-tree-slide-activate-message "☃")
  (org-tree-slide-deactivate-message "fin.")
  (org-tree-slide-header t)
  (org-tree-slide-breadcrumbs " // ")
  (org-image-actual-width nil))
