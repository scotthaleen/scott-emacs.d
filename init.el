;;
;; package business
;;

;; HowTo - Force recompile all packages after major upgrade
;; M-: (byte-recompile-directory package-user-dir nil 'force)

(require 'package)

(dolist (repo '(("marmalade" . "http://marmalade-repo.org/packages/")
                ("melpa" . "https://melpa.org/packages/")
                ;;("melpa" . "http://melpa.milkbox.net/packages/")
                ))
  (add-to-list 'package-archives repo))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages
  '(
    ac-slime
    ag
    align-cljlet
    all-the-icons
    auto-complete
    cider
    ;;clj-refactor
    clojure-mode
    clojurescript-mode
    company
    cyberpunk-theme
    dired-details
    el-get
    ;;emojify
    ensime
    feature-mode
    fill-column-indicator
    find-file-in-project
    flycheck
    helm
    highlight-symbol
    json-mode
    magit
    markdown-mode
    minimap
    monokai-theme
    neotree
    nodejs-repl
    nyan-mode
    operate-on-number
    org
    paredit
    projectile
    rainbow-delimiters
    restclient
    smartrep
    undo-tree
    web-mode
    which-key
    yaml-mode
    yasnippet
    ))

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

;;yasnippet snippets
(setq yas-snippet-dirs  '("~/.emacs.d/yasnippet-snippets/"))
(yas-global-mode 1)
;;git gutter experimental
;;(global-git-gutter+-mode)
;;(add-hook 'after-init-hook #'global-emojify-mode)

(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; broken 26.1
;;(require 'clj-refactor)

;; (defun my-clojure-mode-hook ()
;;     (clj-refactor-mode 1)
;;     (yas-minor-mode 1) ; for adding require/use/import statements
;;     ;; This choice of keybinding leaves cider-macroexpand-1 unbound
;;     (cljr-add-keybindings-with-prefix "C-c C-m"))

;; (add-hook 'clojure-mode-hook #'my-clojure-mode-hook)

;; NOTE: install fonts `M-x all-the-icons-install-fonts`
(require 'all-the-icons)

(require 'neotree)
(setq neo-banner-message "== neotree ==")
(setq neo-smart-open t)
(setq neo-window-width 34)

(require 'doc-view)
(print doc-view-pdfdraw-program)
(setq doc-view-pdf->png-converter-invocation
      'doc-view-pdf->png-converter-invocation-mupdf)

;;use icons for window system and arrow terminal.
(setq neo-theme (if (display-graphic-p) 'icons 'arrow))


(require 'which-key)
(which-key-mode)

;; fill column indicator

(require 'fill-column-indicator)
(setq fci-rule-column 120)

(defvar fci-whitespace-modes
  '(markdown
    ;;emacs-lisp
    python
    org))

(dolist (mode fci-whitespace-modes)
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'fci-mode)
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'whitespace-mode))


(with-eval-after-load
 'org
 (org-babel-do-load-languages
  'org-babel-load-languages
  '((awk . t)
    (calc .t)
    (dot . t)
    (emacs-lisp . t)
    ;;(gnuplot . t)
    ;;(latex . t)
    (js . t)
    ;;(haskell . t)
    ;;(http . t)
    ;;(perl . t)
    (python . t)
    ;;(R . t)
    (shell . t))))

;;
;; visual settings
;;

(setq inhibit-splash-screen t
      initial-scratch-message nil
      truncate-partial-width-windows nil
      initial-major-mode 'clojure-mode
      linum-format "%d  "
      visual-bell t)

(line-number-mode 1)
(column-number-mode 1)
(global-linum-mode 1)
(setq display-line-numbers 'relative)

(winner-mode 1)
(nyan-mode 1)
;;(global-rainbow-delimiters-mode 1)


(setq mode-line
      '((t (:background "magenta" :foreground "black" :box (:line-width -1 :style released-button))))
      show-paren-match
      '((t (:background "gold" :foreground "black")))
      show-paren-mismatch
      '((t (:background "medium violet red" :foreground "white"))))

;;
;; quirk fixes, behaviors
;;

(add-hook 'before-save-hook 'whitespace-cleanup)

(setq ring-bell-function 'ignore)

;;(define-globalized-minor-mode global-whitespace-mode whitespace-mode (lambda () (whitespace-mode 1)))
;;(global-whitespace-mode 1)

;;; compiz fix
(add-to-list 'default-frame-alist '(alpha . 100))

(setq x-select-enable-clipboard t
      make-backup-files nil
      auto-save-default nil
      diff-switches "-u -w"
      whitespace-style '(trailing lines space-before-tab
                                  face indentation space-after-tab lines-tail))


;;(set-face-attribute 'whitespace-line-column nil :background nil :foreground "gray30")

(setq-default tab-width 2
              indent-tabs-mode nil
              c-basic-offset 2
              sh-basic-offset 2
              ;;js-indent-level 2
              whitespace-line-column 120
              fill-column 120)

(fset 'yes-or-no-p 'y-or-n-p)
(delete-selection-mode t)
(show-paren-mode t)
(auto-compression-mode t)
(recentf-mode 1)
(setq diff-switches "-u -w")

(global-company-mode)

;;(menu-bar-mode 0)

;;
;; custom.el
;;

(setq custom-file "~/.emacs.d/custom.el")
(when (file-exists-p custom-file)
  (load custom-file))

;;
;; creature comforts
;;

(require 'ido)
(ido-mode t)
(setq ido-save-directory-list-file "~/.emacs.d/.ido.last"
      ido-enable-flex-matching t
      ido-use-filename-at-point 'guess
      ido-show-dot-for-dired t)

(defun recentf-ido-find-file ()
  "Find a recent file using ido.
   From Phil Hagelberg's emacs-starter-kit."
  (interactive)
  (let ((file (ido-completing-read "Choose recent file: " recentf-list nil t)))
    (when file
      (find-file file))))

(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(global-set-key (kbd "C-x f") 'recentf-ido-find-file)
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x g") 'magit-status)
(global-set-key (kbd "C-x p f") 'find-file-in-project)

;; kill lines backward
(global-set-key (kbd "C-<backspace>") (lambda ()
                                        (interactive)
                                        (kill-line 0)
                                        (indent-according-to-mode)))



(defun snowman () (interactive) (insert "☃" ))
(defun shrug () (interactive) (insert "¯\\_(ツ)_/¯" ))
(defun rage () (interactive (insert "(╯°□°）╯︵ ┻━┻")))
(defun todo () (interactive) (insert "TODO" ) )
(defun λ () (interactive) (insert "λ"))

(global-set-key (kbd "C-c 8 s") 'snowman)
(global-set-key (kbd "C-c 8 i d k") 'shrug)
(global-set-key (kbd "C-c 8 r a g e") 'rage)
(global-set-key (kbd "C-c 8 t") 'todo)
(global-set-key (kbd "C-c 8 l") 'λ)


;;; :set wrapscan emulation
(defadvice isearch-search (after isearch-no-fail activate)
  (unless isearch-success
    (ad-disable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)
    (isearch-repeat (if isearch-forward 'forward))
    (ad-enable-advice 'isearch-search 'after 'isearch-no-fail)
    (ad-activate 'isearch-search)))

;;; vim dt emulation
(defadvice zap-to-char (after my-zap-to-char-advice (arg char) activate)
  "Kill up to the ARG'th occurence of CHAR, and leave CHAR.
  The CHAR is replaced and the point is put before CHAR."
  (insert char)
  (forward-char -1))

;;
;; org settings
;;

(define-key global-map "\C-ca" 'org-agenda)
(setq org-hide-leading-stars t
      org-src-window-setup 'current-window
      org-todo-keywords (quote ((sequence "TODO" "ONGOING" "DONE")))
      org-todo-keyword-faces
      '(("ONGOING" . "orange")))

;;
;; linux fullscreen
;;

(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen ()
  (interactive)
  (progn
    (set-frame-parameter nil 'width 82)
    (set-frame-parameter nil 'fullscreen 'fullheight)))

(defun my-fullscreen ()
  (interactive)
  (set-frame-parameter nil 'fullscreen 'fullboth))

(defun toggle-fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen)
    (my-fullscreen)))

;;
;; window-system specific
;;

(if window-system
    (progn
      (global-set-key (kbd "M-m") 'toggle-fullscreen)
      (if (boundp 'tool-bar-mode) (tool-bar-mode -1))
      (set-fringe-style -1)
      ;;(tooltip-mode -1)
      (scroll-bar-mode -1)
      (modify-frame-parameters (selected-frame)
                               (list (cons 'cursor-type 'hollow)))
      ;;(load-theme 'monokai t)
      (load-theme 'cyberpunk t)
      )


  (set-face-background 'default "nil"))

(add-hook 'after-init-hook
      (lambda () (load-theme 'cyberpunk t)))

;;
;; lisp jockeying
;;

(setq cider-default-cljs-repl 'figwheel)
(add-to-list 'auto-mode-alist '("\\.cljs.hl\\'" . clojurescript-mode))


(setq html5-elements
      '(a abbr acronym address applet area article aside audio b base basefont
        bdi bdo big blockquote body br button canvas caption center cite code
        col colgroup command data datalist dd del details dfn dir div dl
        dt em embed eventsource fieldset figcaption figure font footer form frame frameset
        h1 h2 h3 h4 h5 h6 head header hgroup hr html i
        iframe img input ins isindex kbd keygen label legend li link html-map
        mark menu html-meta meter nav noframes noscript object ol optgroup
        option output p param pre progress q rp rt ruby
        s p samp script section select small source span strike strong style sub
        summary sup table tbody td textarea tfoot th thead html-time
        title tr track tt u ul html-var video wbr))

(add-hook 'clojurescript-mode-hook
          '(lambda ()
             (dolist (el html5-elements)
               (put-clojure-indent el 'defun))))

(setq company-tooltip-align-annotations t)
(require 'web-mode)

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

(global-set-key (kbd "C-+") 'increase-font-size)
(global-set-key (kbd "C--") 'decrease-font-size)
(global-set-key (kbd "M-j") 'join-line)
(global-set-key (kbd "RET") 'newline-and-indent)

(global-set-key (kbd "S-C-<left>") 'shrink-window-horizontally)
(global-set-key (kbd "S-C-<right>") 'enlarge-window-horizontally)
(global-set-key (kbd "S-C-<down>") 'shrink-window)
(global-set-key (kbd "S-C-<up>") 'enlarge-window)

;; Behave like */# in Vim, jumping to symbols under point.
(global-set-key (kbd "C-x *") 'highlight-symbol-next)
(global-set-key (kbd "C-*") 'highlight-symbol-prev)

(put 'narrow-to-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'set-goal-column 'disabled nil)

;;
;; mvnrepl
;;

(defgroup mvnrepl nil
  "run mvn clojure:repl from emacs"
  :prefix "mvnrepl-"
  :group 'applications)

(defcustom mvnrepl-mvn "mvn"
  "Maven 'mvn' command."
  :type 'string
  :group 'mvnrepl)

(defun mvnrepl-project-root ()
  "Look for pom.xml file to find project root."
  (let ((cwd default-directory)
        (found nil)
        (max 10))
    (while (and (not found) (> max 0))
      (if (file-exists-p (concat cwd "pom.xml"))
          (setq found cwd)
        (setq cwd (concat cwd "../") max (- max 1))))
    (and found (expand-file-name found))))

(defun mvnrepl ()
  "From a buffer with a file in the project open, run M-x mvn-repl to get a project inferior-lisp"
  (interactive)
  (let ((project-root (mvnrepl-project-root)))
    (if project-root
        (inferior-lisp (concat mvnrepl-mvn " -f " project-root "/pom.xml clojure:repl"))
      (message (concat "Maven project not found.")))))

(provide 'mvnrepl)

;;
;; package-specific customizations
;;

;;; operate-on-number
(require 'operate-on-number)
(require 'smartrep)

(smartrep-define-key global-map "C-c ."
  '(("+" . apply-operation-to-number-at-point)
    ("-" . apply-operation-to-number-at-point)
    ("*" . apply-operation-to-number-at-point)
    ("/" . apply-operation-to-number-at-point)
    ("\\" . apply-operation-to-number-at-point)
    ("^" . apply-operation-to-number-at-point)
    ("<" . apply-operation-to-number-at-point)
    (">" . apply-operation-to-number-at-point)
    ("#" . apply-operation-to-number-at-point)
    ("%" . apply-operation-to-number-at-point)
    ("'" . operate-on-number-at-point)))

;;; find-file-in-project

(setq ffip-patterns '("*")
      ffip-use-project-cache nil
      ffip-project-file '("project.clj" ".git"))

(defun ffip-toggle-use-project-cache ()
  "Toggles project file caching for find-file-in-project.el."
  (interactive)
  (setq ffip-use-project-cache (not ffip-use-project-cache))
  (message (concat "Project caching "
                   (if ffip-use-project-cache
                       "enabled."
                     "disabled."))))

(global-set-key (kbd "C-x M-f") 'find-file-in-project)
(global-set-key (kbd "C-x M-F") 'ffip-toggle-use-project-cache)

;; paredit

(defvar paredit-modes
  '(clojure
    emacs-lisp
    lisp
    lisp-interaction
    ielm
    js
    typescript
    repl))

(dolist (mode paredit-modes)
  (add-hook (intern (concat (symbol-name mode) "-mode-hook"))
            (lambda ()
              (paredit-mode +1)
              (define-key paredit-mode-map (kbd "M-)")
                'paredit-forward-slurp-sexp))))

;; rainbow-delimiter modes
(defvar rainbow-delimiter-modes
  '(clojure
    emacs-lisp
    lisp
    lisp-interaction
    ielm
    js
    repl))

(dolist (mode rainbow-delimiter-modes)
  (add-hook (intern (concat (symbol-name mode) "-mode-hook")) #'rainbow-delimiters-mode))


;;; ibuffer

(global-set-key (kbd "C-x C-b") 'ibuffer)
(setq ibuffer-expert t)
(setq ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-mode-hook
          '(lambda ()
             (ibuffer-auto-mode 1)
             (ibuffer-switch-to-saved-filter-groups "home")))

(defun paredit-nonlisp ()
  "Turn on paredit mode for non-lisps."
  (interactive)
  (set (make-local-variable 'paredit-space-for-delimiter-predicates)
       '((lambda (endp delimiter) nil)))
  (paredit-mode 1))

;;react-native
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode))
;; (setq web-mode-markup-indent-offset 2
;;       web-mode-css-indent-offset 2
;;       web-mode-code-indent-offset 2)
;; (setq js-indent-level 2)

;; projectile global

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; python
;; flake8
;; flymake-python-pyflakes
;;(add-hook 'python-mode-hook 'flymake-python-pyflakes-load)
;;(setq flymake-python-pyflakes-executable "flake8")
;;(add-hook 'python-mode-hook 'projectile-mode)
;;(add-hook 'python-mode-hook 'flycheck-mode)



(add-hook 'java-mode-hook #'yas-minor-mode)

;;;###autoload
(eval-after-load 'js
  '(progn (define-key js-mode-map "{" 'paredit-open-curly)
          (define-key js-mode-map "}" 'paredit-close-curly-and-newline)
          (add-hook 'js-mode-hook 'paredit-nonlisp)
          ;;(setq js-indent-level 2)
          ;; fixes problem with pretty function font-lock
          (define-key js-mode-map (kbd ",") 'self-insert-command)
          (font-lock-add-keywords
           'js-mode `(("\\(function *\\)("
                       (0 (progn (compose-region (match-beginning 1)
                                                 (match-end 1)
                                                 ,(make-char 'greek-iso8859-7 107)
                                                 ;;"\u0192"
                                                 )
                                 nil)))))))

;;; typescript add {} to paredit and λ for pretty functions
(eval-after-load 'typescript-mode
  '(progn
     (define-key typescript-mode-map "{" 'paredit-open-curly)
     (define-key typescript-mode-map "}" 'paredit-close-curly-and-newline)
     ;;(add-hook 'typescript-mode-hook (lambda () (lll local-unset-key (kbd "{"))))
     (add-hook 'typescript-mode-hook 'paredit-nonlisp)
     ;;(setq typescript-indent-level 2)
     ;; fixes problem with pretty function font-lock
     (define-key typescript-mode-map (kbd ",") 'self-insert-command)
     (font-lock-add-keywords
      'typescript-mode `(("\\(function *\\)("
                          (0 (progn (compose-region (match-beginning 1)
                                                    (match-end 1)
                                                    ,(make-char 'greek-iso8859-7 107)
                                                    ;;"\u0192"
                                                    )
                                    nil)))))
     ;; (font-lock-add-keywords
     ;;  'typescript-mode `(("\\((\\)\\(.*\\)\\()\\) *=>"
     ;;                      (0 (progn (compose-region (match-beginning 1)
     ;;                                                (match-end 1)
     ;;                                                ,(make-char 'greek-iso8859-7 107)
     ;;                                                ;;"\u0192"
     ;;                                                )
     ;;                                nil))
     ;;                      )))
     ))

(put 'erase-buffer 'disabled nil)


(put 'dired-find-alternate-file 'disabled nil)
