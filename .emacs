;; No splash screen please... jeez
(setq inhibit-startup-screen t)

;; Set proxy. If required by server, it asks for auth.
(setq url-proxy-services
   '(("no_proxy" . "^\\(localhost\\|10.*\\)")
     ("http" . "proxy.rwe.com:8080")
     ("https" . "proxy.rwe.com:8080")))

;; Indent by spaces
(setq-default indent-tabs-mode nil)

;; Melpa Packages
(require 'package)
(setq package-user-dir "~/.emacs.d/elpa/")
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(require 'cl)
;; Guarantee all packages are installed on start
(defvar packages-list
  '(rainbow-mode
    fill-column-indicator
    cursor-chg
    highlight-indentation
    highlight-symbol
    browse-kill-ring
    ido-ubiquitous
    magit
    paredit
    undo-tree
    multiple-cursors     ; Multiple cursor for editting at many places at once
    helm                 ; Incremental completion and narrowing framework / based on anything.el
)
  "List of packages needs to be installed at launch")

(defvar suggested-packages
  '(clojure-mode
    php-mode
    rvm             ; Ruby Version Manager
    markdown-mode
    yasnippet       ; Autoreplace sippets, screencast: http://www.youtube.com/watch?v=ZCGmZK4V7Sg
    protobuf-mode)
  "Packages I want to look into"
  )

(defun has-package-not-installed ()
  (loop for p in packages-list
        when (not (package-installed-p p)) do (return t)
        finally (return nil)))

(when (has-package-not-installed)
  ;; Check for new packages (package versions)
  (message "%s" "Get latest versions of all packages...")
  (package-refresh-contents)
  (message "%s" " done.")
  ;; Install the missing packages
  (dolist (p packages-list)
    (when (not (package-installed-p p))
      (package-install p))))

;; == Global modes

(global-linum-mode t)                 ; Show line numbers

(after 'undo-tree-autoloads           ; Undo-tree
  (global-undo-tree-mode t)
  (setq undo-tree-visualizer-relative-timestamps t)
  (setq undo-tree-visualizer-timestamps t))

(require 'helm-config)                ; Helm completions

;; == Bindings
(global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click)

;; == Legacy code - hate commented out code

;; Add packages in ~/.emacs.d/
;;(add-to-list load-path "~/.emacs.d")

;; Required, if you want to use Emacs as editor in other programs (Chrome, etc.)
;;(require 'edit-server)
;;(edit-server-start)

;; Install only the sweetest of packages
;; from: http://milkbox.net/note/single-file-master-emacs-configuration/

;;(defun mp-install-rad-packages ()
;;   "Install only the sweetest of packages."
;;   (interactive)
;;   (package-refresh-contents)
;;   (mapc '(lambda (package)
;;            (unless (package-installed-p package)
;;              (package-install package)))
;;        ))

