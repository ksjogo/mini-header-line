(require 'face-remap)

(defvar mini-header-line:background "#292929")

(defvar mini-header-line:last-buffer nil)
(defvar mini-header-line:cookie nil)
(defvar mini-header-line:app-has-focus t)

(defun mini-header-line:check ()
  "Check if focus has changed, and if so, update remapping."
  (let ((current-buffer (and mini-header-line:app-has-focus (current-buffer))))
    (unless (eq mini-header-line:last-buffer current-buffer)
      (when (and mini-header-line:last-buffer mini-header-line:cookie)
        (with-current-buffer mini-header-line:last-buffer
          (face-remap-remove-relative mini-header-line:cookie)))
      (setq mini-header-line:last-buffer current-buffer)
      (when current-buffer
        (setq mini-header-line:cookie
              (face-remap-add-relative 'header-line :background mini-header-line:background))))))

(defun mini-header-line:app-focus (state)
  (setq mini-header-line:app-has-focus state)
  (mini-header-line:check))

(defun mini-header-line-formatter (errors warnings)
  (concat (if (> errors 0)
              (propertize (number-to-string errors) 'face 'error)
            "0")
          "/"
          (if (> warnings 0)
              (propertize (number-to-string warnings) 'face 'font-lock-warning-face)
            "0")))

(defvar jg-header-line-format
  (list
   " "
   ;; buffer
   (propertize "%[%b%]" 'face 'font-lock-keyword-face)
   ;; change
   (propertize "%*" 'face 'font-lock-warning-face)
   " "
   ;; error counts
   '(:eval (when (and (boundp 'flycheck-mode) flycheck-mode)
             (pcase flycheck-last-status-change
               (`not-checked "")
               (`no-checker "-/-")
               (`running "*/*")
               (`errored "!/!")
               (`interrupted "-/-")
               (`suspicious "?/?")
               (`finished
                (if flycheck-current-errors
                    (let ((error-counts (flycheck-count-errors flycheck-current-errors)))
                      (mini-header-line-formatter (or (cdr (assq 'error error-counts)) 0) (or (cdr (assq 'warning error-counts)) 0)))
                  "0/0")))))
   '(:eval (when (and (eq major-mode 'js2-mode))
             (mini-header-line-formatter (length (js2-errors)) (length (js2-warnings)))))
   ;; line
   (propertize "%4l" 'face 'font-lock-type-face)
   " "
   ;; relative position, size of file
   `((-3 ,(propertize "%P" 'face 'font-lock-constant-face)))
   "/"
   (propertize "%I" 'face 'font-lock-constant-face)
   " "
   ))

(define-minor-mode mini-header-line-minor-mode "")
(defun mini-header-line-mode-on (&optional param)
  (interactive)
  (when (or (derived-mode-p 'prog-mode)
            (member major-mode '(jape-mode groovy-mode markdown-mode latex-mode scss-mode org-mode css-mode feature-mode enh-ruby-mode html-mode web-mode nxml-mode json-mode)))
    (setq header-line-format jg-header-line-format)))

(define-globalized-minor-mode mini-header-line-mode mini-header-line-minor-mode mini-header-line-mode-on)

;;;###autoload
(defun mini-header-line-on ()
  (interactive)

  (setq-default mode-line-format nil)

  (mini-header-line-mode)

  (defadvice other-window (after mini-header-line activate)
    (mini-header-line:check))
  (defadvice select-window (after mini-header-line activate)
    (mini-header-line:check))

  ;; 25.1 error?
  ;;(defadvice select-frame (after mini-header-line activate)
  ;;  (mini-header-line:check))

  (add-hook 'window-configuration-change-hook 'mini-header-line:check)

  (add-hook 'focus-in-hook (lambda () (mini-header-line:app-focus t)))
  (add-hook 'focus-out-hook (lambda () (mini-header-line:app-focus nil))))

(provide 'mini-header-line)
