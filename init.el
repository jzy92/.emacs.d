(package-initialize)

;; Various settings

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
(setq-default indent-tabs-mode nil)

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

;; eshell helpers and functions

(defun eshell-here ()
  "Opens up a new shell in the directory associated with the
current buffer's file. The eshell is renamed to match that
directory to make multiple eshell windows easier."
  (interactive)
  (let* ((parent (if (buffer-file-name)
                     (file-name-directory (buffer-file-name))
                   default-directory))
         (height (/ (window-total-height) 3)))
    (split-window-vertically (- height))
    (other-window 1)
    (eshell "new")
    (rename-buffer (concat "*eshell: " (eshell/pwd) "*"))))
(global-set-key (kbd "C-!") 'eshell-here)

(defun my-change-eshell-buffer-name (old-function &rest arguments)
  "Change eshell buffer name when we change directory."
  (rename-buffer (concat "*eshell: " (eshell/pwd) "*")))
(advice-add #'eshell/cd :after #'my-change-eshell-buffer-name)

(defun eshell/x ()
  (insert "exit")
  (eshell-send-input)
  (delete-window))

(defun eshell/ff (files)
  "Run find-file on all specified files."
  (if (listp files)
      (dolist (f files)
        (find-file f))
    (find-file files)))

(defun eshell/up (&optional level)
  "Go 'up' the requested number of directories."
  (if (null level)
      (setq level 1))
  (eshell/cd (string-join (make-list level "..") "/")))

;; All my custom BS

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eshell-banner-message ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
