;;; book-mode.el --- Book mode -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Maintainer: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; URL: https://github.com/rougier/book-mode
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (nano-theme))
;; Keywords: convenience, mode-line, header-line

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.


;;; Code:
(require 'org)
;; (require 'svg-lib)
(require 'nano-theme)

;; ----------------------------------------------------------------------------
(defun book-mode--log (format-string &rest args)
  "Log a message into the *Messages* buffer if message-log-max is
non-nil. Return the message."

  (with-current-buffer (get-buffer-create "*Messages*")
    (let ((inhibit-read-only t)
          (msg (apply 'format-message format-string args)))
      (when (and msg message-log-max)
        (goto-char (point-max))
        (insert (concat "\n" msg)))
      msg)))


;; ----------------------------------------------------------------------------
(defun book-mode--message (&rest args)
  "Message function advice that override the message function and
saves the message in a buffer local variable."

  ;; Log message
  (when (car args)
    (apply #'book-mode--log args))

  ;; Save message in buffer local book-mode--last-message variable
  ;; and register a timer to clear it after a delay.
  (let* ((msg (if (and (car args) (stringp (car args)))
                  (apply 'format-message args)))
         (msg (if (stringp msg)
                  (replace-regexp-in-string "%" "%%" msg))))
    (setq book-mode--last-message (or msg ""))
    (when (and (boundp 'book-mode--message-timer) book-mode--message-timer)
      (cancel-timer book-mode--message-timer))
    (setq book-mode--message-timer
            (run-at-time minibuffer-message-timeout nil
                         #'book-mode--message-clear))
    (force-mode-line-update)))


;; ----------------------------------------------------------------------------
(defun book-mode--message-clear ()
  "Clear last message"
  
  (setq book-mode--last-message nil)
  (force-mode-line-update))


;; ----------------------------------------------------------------------------
(defun book-mode--command-error-function (data context caller)
  "This command-error function intercepts some message from the C API."

  (if (not (memq (car data) '(buffer-read-only
                              text-read-only
                              beginning-of-buffer
                              end-of-buffer
                              quit)))
      (command-error-default-function data context caller)
      (book-mode--message (format "%s" data))))


;; ----------------------------------------------------------------------------
(defun book-mode--header (&rest args)
  ""
  (apply #'book-mode--build
         (nconc `(:overline nil
                  :underline ,(face-foreground 'default)
                  :margin ,(or (plist-get args ':margin) 0.0)
                  :padding ,(or (plist-get args ':padding) 0.0))
                args)))


;; ----------------------------------------------------------------------------
(defun book-mode--footer (&rest args)
  ""
  (apply #'book-mode--build
         (nconc `(:overline ,(face-foreground 'default)
                  :underline nil
                  :margin ,(or (plist-get args ':margin) 0.0)
                  :padding ,(or (plist-get args ':padding) 0.0))
                args)))


;; ----------------------------------------------------------------------------
(defun book-mode--build (&rest args)
  ""

  (let* ((overline (or (plist-get args ':overline) nil))
         (underline (or (plist-get args ':underline) nil))
         (left (or (plist-get args ':left) #'book-mode-element-empty))
         (right (or (plist-get args ':right) #'book-mode-element-empty))
         (center (or (plist-get args ':center) #'book-mode-element-empty))
         (prefix (or (plist-get args ':prefix) #'book-mode-element-empty))
         (suffix (or (plist-get args ':suffix) #'book-mode-element-empty))
         (margin  (or (plist-get args ':margin) 0.0))
         (padding (or (plist-get args ':padding) 0.0)))

    `(:eval
      (let* ((left (if (stringp (quote ,left))
                       ,left
                     (funcall (quote ,left))))
             (right  (if (stringp (quote ,right))
                         ,right
                       (funcall (quote ,right))))
             (prefix (if (stringp (quote ,prefix))
                         ,prefix
                       (funcall (quote ,prefix))))
             (suffix  (if (stringp (quote ,suffix))
                          ,suffix
                        (funcall (quote ,suffix))))
             (overline ,overline)
             (underline ,underline)
             (width (window-width))
             (margin ,margin)
             (padding ,padding)
             (left-margin (or (car (window-margins)) 0))
             (right-margin (or (cdr (window-margins)) 0))
             (left (truncate-string-to-width left (- width (length right) 1) nil nil "…"))
             (prefix-filler (make-string (max 0 (- left-margin (length prefix))) 32))
             (suffix-filler (make-string (max 0 (- right-margin (length suffix))) 32)))

         (add-face-text-property 0 (length left)
                                 `(:underline ,underline :overline ,overline) nil left)
         (add-face-text-property 0 (length right)
                                 `(:underline ,underline :overline ,overline) nil right)
  
         (concat
         (propertize prefix-filler  'display `(raise ,(+ margin padding)))
         (propertize prefix 'display `((raise ,margin)
                                       ,(get-text-property 0 'display prefix)))
         (propertize left 'display `((raise ,margin)
                                     ,(get-text-property 0 'display left)))
         (propertize " " 'display `((raise ,(+ margin))
                                    (space :align-to (- right ,(length right) 0)))
                        'face `(:box nil :underline ,underline :overline ,overline))
         (propertize right 'display `((raise ,margin)
                                      ,(get-text-property 0 'display right)))
         (propertize suffix 'display `((raise ,margin)
                                       ,(get-text-property 0 'display suffix)))
         
         (propertize suffix-filler 'display `(raise ,(+ margin padding))))))))

;; ----------------------------------------------------------------------------
(defun book-mode-element-status ()
  
  (let ((index (length (member (selected-frame) (frame-list)))))
    (concat
     (propertize (format " %d " index)
                 'face '(nano-popout-i nano-strong))
     "   "
     (cond ((derived-mode-p 'elfeed-show-mode)
                 (propertize " " 'face 'nano-default))
                ((derived-mode-p 'elfeed-search-mode)
                 (propertize " " 'face 'nano-default))
                ((derived-mode-p 'mu4e-headers-mode)
                 (propertize " " 'face 'nano-default))
                ((derived-mode-p 'mu4e-view-mode)
                 (propertize " " 'face 'nano-default))
                (view-mode
                 (propertize " " 'face 'nano-faded))
                ((derived-mode-p 'helpful-mode)
                 (propertize " " 'face 'nano-default))
                (buffer-read-only
                 (propertize " " 'face 'nano-faded))
                ((buffer-modified-p)
                 (propertize " " 'face 'nano-popout))
                (t
                 (propertize " " 'face 'nano-subtle-i))))))

;; ----------------------------------------------------------------------------
(defun book-mode-element-name ()

  (let ((name (cond ;; Elfeed show mode
                     ((derived-mode-p 'elfeed-show-mode)
                     (plist-get (elfeed-feed-meta
                        (elfeed-entry-feed elfeed-show-entry)) :title))
                     ;; Elfeed show mode
                     ((derived-mode-p 'elfeed-search-mode)
                      (concat "" elfeed-search-filter))
                     ;; Mu4e headers mode
                     ((derived-mode-p 'mu4e-headers-mode)
                      (concat "" (mu4e-last-query)))
                     ;; Mu4e view mode
                     ((derived-mode-p 'mu4e-view-mode)
                      "Message")
                    ;; Default
                    (t (buffer-name)))))
    (propertize name 'face '(:inherit nano-strong))))

;; ----------------------------------------------------------------------------
(defun book-mode-element-word-count ()

  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (word-count (count-words beg end))
         (char-count (- end beg)))
    (propertize (format "%d words / %d chars" word-count char-count)
                'face 'nano-faded)))

;; ----------------------------------------------------------------------------
(defun book-mode-element-word-target ()

  (let* ((word-count (count-words (point-min) (point-max)))
         (word-total 300)
         (ratio (/ (float word-count) (float word-total))))
    (propertize "  " 'display (svg-lib-progress-pie ratio nil))))


;; ----------------------------------------------------------------------------
(defun book-mode-element-dedicated ()
  (propertize "    " 'face (if (window-dedicated-p)
                             '(:inherit nano-default)
                           '(:inherit nano-subtle-i))
                   'display '(raise 0)
              ))


;; ----------------------------------------------------------------------------
(defun book-mode-element-empty ()
  "")


;; ----------------------------------------------------------------------------
(defun book-mode-element-message ()
  (if (boundp 'book-mode--last-message)
      (or book-mode--last-message "")
    ""))


;; ----------------------------------------------------------------------------
(defun book-mode-element-line/total ()
  (propertize (format "%s/%s"(format-mode-line "%l")
                      (save-excursion
                        (goto-char (point-max))
                        (format-mode-line "%l")))))



;; ---------------------------------------------------------------------
(defun book-mode-element-mu4e-query ()
  (mu4e-last-query))


;; ---------------------------------------------------------------------
(defun book-mode-element-mu4e-context ()
  substring-no-properties (mu4e-context-label))


;; ---------------------------------------------------------------------
(defun book-mode-element-mu4e-total ()
  (format "%d" (plist-get mu4e--server-props :doccount)))




;; ----------------------------------------------------------------------------
(defun overlay-extend-to-margin (overlay &optional face left right)
  (let* ((face (or face 'nano-popout-i))
         (left-width (or (car (window-margins)) 0))
         (right-width (or (cdr (window-margins)) 0))
         (left (or left (make-string left-width ?\ )))
         (right (or right (make-string right-width ?\ )))
         (line-prefix (get-text-property (line-beginning-position) 'display))
         (line-prefix (if (and (seqp line-prefix)
                               (equal (car line-prefix) '(margin left-margin)))
                          (cadr line-prefix)))
         (left (if (stringp line-prefix) line-prefix left)))
    (overlay-put overlay 'line-prefix
       (concat
          (propertize " "
                      'display `((margin right-margin)
                                 ,(propertize right 'face face)))
          (propertize " "
                      'display `((margin left-margin)
                                 ,(propertize left 'face face)))))
    
    (overlay-put overlay 'wrap-prefix
       (concat
        (propertize " "
                    'display `((margin right-margin)
                               ,(propertize right 'face face)))
        (propertize " "
                    'display `((margin left-margin)
                               ,(propertize left 'face face)))))))

;; (defun region-activate ()
;;   (when (boundp 'region-overlay)
;;     (overlay-extend-to-margin region-overlay 'region)
;;     (region-update)
;;     (add-hook #'post-command-hook #'region-update)))

;; (defun region-deactivate ()
;;   (remove-hook #'post-command-hook #'region-update)
;;   (when (boundp 'region-overlay)
;;     (move-overlay region-overlay (point-min) (point-min))))

;; (defun region-update ()
;;   (when (and (boundp 'region-overlay) (use-region-p))
;;     (move-overlay region-overlay (region-beginning) (region-end))))

;; (add-hook 'activate-mark-hook #'region-activate)
;; (add-hook 'deactivate-mark-hook #'region-deactivate)
;; (remove-hook 'activate-mark-hook #'region-activate)
;; (remove-hook 'deactivate-mark-hook #'region-deactivate)


(defun book-mode-hl-line-range-function ()
  (overlay-extend-to-margin
   (or hl-line-overlay global-hl-line-overlay) 'hl-line)
  (cons (line-beginning-position) (line-beginning-position 2)))


;; ----------------------------------------------------------------------------
(defun book-mode (&optional global)

  (interactive)
  (let ((left-margin    8)
        (right-margin   6)
        (top-margin     2.25)
        (top-padding    0.25)
        (bottom-margin  1.50)
        (bottom-padding 0.00))
    (if global
        (setq-default left-margin-width left-margin
                      right-margin-width right-margin))
    (set-window-margins (selected-window) left-margin right-margin)
    (setq left-margin-width left-margin)
    (setq right-margin-width right-margin)
    
    (set-frame-parameter (selected-frame) 'internal-border-width 1)
    (set-frame-parameter (selected-frame) 'width (+ 81
                                                    left-margin
                                                    right-margin))
    (set-frame-parameter (selected-frame) 'height 50)
    (set-face-background 'internal-border "#000000" (selected-frame))
    
    (setq-local book-mode--message-timer nil)
    (setq-local book-mode--message-last nil)

    (setq line-spacing 1)
    (fringe-mode '(0 . 0))
    (set (make-local-variable 'region-overlay)
         (make-overlay (point-min) (point-min)))

    (if global
        (progn
          (set-face-attribute 'header-line nil
                              :background (face-background 'default))
          (set-face-attribute 'mode-line nil
                             :foreground (face-foreground 'default)
                             :background (face-background 'default)
                             :height (face-attribute 'default :height))
          (set-face-attribute 'mode-line-inactive nil
                             :foreground (face-foreground 'default)
                             :background (face-background 'default)
                             :height (face-attribute 'default :height))
          (set-face-attribute 'region nil
                              :background "#e0e0ff")
          (set-face-attribute 'hl-line nil
                              :background "#f5f5ff"))
      (progn
        (face-remap-add-relative 'header-line
                                 :background (face-background 'default))
        (face-remap-add-relative 'mode-line
                                 :foreground (face-foreground 'default)
                                 :background (face-background 'default)
                                 :height (face-attribute 'default :height))
        (face-remap-add-relative 'mode-line-inactive
                                 :foreground (face-foreground 'default)
                                 :background (face-background 'default)
                                 :height (face-attribute 'default :height))
        (face-remap-add-relative 'region :background "#e0e0ff")
        (face-remap-add-relative 'hl-line :background "#f5f5ff")))

    (setq-local header-line-format
                (book-mode--header :prefix #'book-mode-element-status
                                   :left #'book-mode-element-name
                                   :right #'book-mode-element-word-count
                                   ;; :right #'book-mode-element-word-target
                                   ;; ;suffix #'book-mode-element-word-target
                                   :suffix #'book-mode-element-dedicated
                                   :margin  (- top-margin)
                                   :padding (- top-padding))
                mode-line-format
                (book-mode--footer :left #'book-mode-element-message
                                   :right  #'book-mode-element-line/total
                                   :margin bottom-margin
                                   :padding bottom-padding))
    (when global
      (setq-default header-line-format header-line-format)
      (setq-default mode-line-format mode-line-format))

    (add-to-list 'font-lock-extra-managed-props 'display)
    (let ((margin-format (format "%%%ds" left-margin)))
      (font-lock-add-keywords nil
       `(

         ("^\\(\\- \\)\\(.*\\)$"
          1 '(face nano-default display ((margin left-margin)
                                        ,(propertize (format margin-format "• ")
                                                     'face '(:inherit nano-default :weight light)) append)))

          ("^\\(\\*\\{1\\} \\)\\(.*\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "# ")
                                                    'face '(:inherit nano-faded :weight light)) append))
          2 '(face bold append))

         ("^\\(\\*\\{2\\} \\)\\(.*\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "## ")
                                                    'face '(:inherit nano-faded :weight light)) append))
          2 '(face bold append))

         ("^\\(\\*\\{3\\} \\)\\(.*\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "### ")
                                                    'face '(:inherit nano-faded :weight light)) append))
          2 '(face bold append))

         ("^\\*\\{4\\} .*?\\(\n\\)"
          1 '(face nil display " - "))

         ("^\\(\\*\\{4\\} \\)\\(.*?\\)$"
          1 '(face nano-faded display ((margin left-margin)
                                       ,(propertize (format margin-format "§ ")
                                                    'face '(:inherit nano-faded :weight light))  append))
          2 '(face bold append))))))

  (book-mode--message-clear)
  (advice-add 'message :override #'book-mode--message)
  (font-lock-fontify-buffer)
  (visual-line-mode)
  (set (make-local-variable 'region-overlay)
       (make-overlay (point-min) (point-min)))
  (setq hl-line-range-function #'book-mode-hl-line-range-function))

(provide 'book-mode)
;;; book-mode.el ends here


