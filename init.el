(package-initialize)

;; Various settings

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(add-to-list 'load-path "~/.emacs.d/lisp/")
(menu-bar-mode -1)
(scroll-bar-mode -1)
(tool-bar-mode -1)
(setq inhibit-splash-screen t)
(load-theme 'light-blue t)
(setq visible-bell 1)
(show-paren-mode 1)
(electric-pair-mode 1)
(add-hook 'text-mode-hook 'turn-on-visual-line-mode)
(setq visual-line-fringe-indicators '(left-curly-arrow right-curly-arrow))
(column-number-mode 1)
(setq-default indent-tabs-mode nil)
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
; active Babel langauges
(org-babel-do-load-languages
 'org-babel-load-languages
 '((perl . t)))
(require 'google-translate)
(require 'google-translate-smooth-ui)
(global-set-key (kbd "C-c t") 'google-translate-smooth-translate)
(require 'thingatpt)
; org tables don't behave with visual-line-mode, so turn it off for org buffers
(add-hook 'org-mode-hook (lambda () (visual-line-mode -1)))
(setq org-table-copy-increment nil)
(ivy-mode 1)
(require 's)
(require 'org-table-sort-rules)
(add-hook 'org-mode-hook
          (lambda ()
            (define-key org-mode-map (kbd "C-c M-^") 'org-table-sort-rules/apply)))
(add-hook 'before-save-hook
          (lambda ()
            (if (equal major-mode 'org-mode)
                (org-table-sort-rules/apply-all))))

;; Various defuns

(defun delete-this-buffer-and-file ()
  "Removes file connected to current buffer and kills buffer."
  (interactive)
  (let ((filename (buffer-file-name))
        (buffer (current-buffer))
        (name (buffer-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file!" name)
      (when (yes-or-no-p "Are you sure you want to remove this file? ")
        (delete-file filename)
        (kill-buffer buffer)
        (message "File '%s' successfully removed" filename)))))
(global-set-key (kbd "C-c C-k") 'delete-this-buffer-and-file)

(defun delete-this-buffer-and-window ()
  "Kills buffer and its window."
  (interactive)
  (kill-buffer)
  (delete-window))
(global-set-key (kbd "C-x C-k") 'delete-this-buffer-and-window)

(defun replace-word-at-point (new-word)
  "Replace word currently under point."
  (backward-word)
  (kill-word 1)
  (insert new-word))

(defun latex-text-size-change-command (make-larger) ; bool argument
  "If point is on a text size command, change to one larger or smaller."
  (let* ((size-command-list '("tiny" "scriptsize" "footnotesize"
                              "small" "normalsize" "large" "Large"
                              "LARGE" "huge" "Huge"))
         (current-command (thing-at-point 'word 'no-properties))
         (command-idx (cl-position current-command size-command-list :test 'equal)))
    (cond ((not (numberp command-idx))
           (error "Word at point '%s' is not a size command (%s)" current-command command-idx))
          ((and (= command-idx 0) (not make-larger)) 
           (message "Already at smallest text size"))
          ((and (= command-idx (1- (length size-command-list))) make-larger)
           (message "Already at largest text size"))
          (make-larger
           (replace-word-at-point (nth (1+ command-idx) size-command-list)))
          ((not make-larger)
           (replace-word-at-point (nth (1- command-idx) size-command-list))))))

(defun latex-text-size-change-larger ()
  (interactive)
  (latex-text-size-change-command t))
(defun latex-text-size-change-smaller ()
  (interactive)
  (latex-text-size-change-command nil))

(add-hook 'latex-mode-hook
          (lambda ()
            (define-key latex-mode-map (kbd "M-n") 'latex-text-size-change-smaller)
            (define-key latex-mode-map (kbd "M-p") 'latex-text-size-change-larger)))

(defun add-to-number-at-point (num-to-add)
  "Add argument to number at point, defaulting to +1"
  (interactive "p")
  (let ((num-to-add (if (null num-to-add) 1 num-to-add))
        (cur-number (thing-at-point 'number 'no-properties)))
    (if (numberp cur-number)
        (replace-word-at-point (number-to-string (+ cur-number num-to-add)))
      (error "Point '%s' not at a number" cur-number))))
(global-set-key (kbd "C-c +") 'add-to-number-at-point)

;; eshell helpers and functions

(defun jmz-shortened-path (path max-len)
  "Return a modified version of `path', replacing some components
      with single characters starting from the left to try and get
      the path down to `max-len'"
  (let* ((components (split-string (abbreviate-file-name path) "/"))
         (len (+ (1- (length components))
                 (cl-reduce '+ components :key 'length)))
         (str ""))
    (while (and (> len max-len)
                (cdr components))
      (setq str (concat str (if (= 0 (length (car components)))
                                "/"
                              (string (elt (car components) 0) ?/)))
            len (- len (1- (length (car components))))
            components (cdr components)))
    (concat str (cl-reduce (lambda (a b) (concat a "/" b)) components))))

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3))
         (eshell-buffer-name (concat "*eshell: "
                                     (jmz-shortened-path (eshell/pwd) 40)
                                     "*")))
    (split-window-vertically (- height))
    (other-window 1)
    (if (and (not (null (get-buffer eshell-buffer-name)))
             (yes-or-no-p (format "Do you want existing eshell buffer %s? " eshell-buffer-name)))
        (switch-to-buffer eshell-buffer-name)
      (progn
        (eshell "new")
        (rename-buffer eshell-buffer-name t)))))
(global-set-key (kbd "C-!") 'eshell-here)

(defun jmz-change-eshell-buffer-name (&optional arguments)
  "Change eshell buffer name when we change directory."
  (rename-buffer (concat "*eshell: " (jmz-shortened-path (eshell/pwd) 40) "*") t))
(advice-add #'eshell/cd :after #'jmz-change-eshell-buffer-name)

(defun eshell/ff (files)
  "Run find-file on all specified files."
  (if (listp files)
      (dolist (f files)
        (find-file f))
    (find-file files)))

(defun eshell/ffs (file)
  "Run find-file on one files; split window."
  (if (not (listp file))
      (progn
        (if (> (window-total-width) (* 2 (window-total-height)))
            (split-window-right)
          (split-window-below))
        (other-window 1)
        (find-file file))
    (error "Command doesn't work for multiple files")))

(defun jmz-truncate-path-to-prefix (path prefix)
  "Given a path, go up to first dir matching the prefix and return path, else path."
  (catch 'early-ret
    (let ((path-components (reverse (split-string path "/"))))
      (while (not (null path-components))
        (let ((dir (pop path-components)))
          (if (string-match (format "^%s" prefix) dir)
              (throw
               'early-ret
               (mapconcat 'identity (reverse
                                     (push dir path-components)) "/"))))))
    (error "Prefix `%s' didn't match any component of %s" prefix path)
    path))

(defun eshell/up (&optional arg)
  "Go 'up' the requested number of directories."
  (if (null arg)
      (setq arg 1))
  (eshell/cd
   (if (numberp arg) (mapconcat 'identity (make-list arg "..") "/")
     (jmz-truncate-path-to-prefix (eshell/pwd) arg))))

(setq eshell-prompt-function
      (lambda ()
        (concat (user-login-name)
                (if (= (user-uid) 0) " # " " $ "))))

;; All my custom BS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-banner-message "")
 '(package-selected-packages
   (quote
    (s google-translate 2048-game minesweeper rainbow-delimiters))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
