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
(require 'seq)
(require 'nano-theme)
;; (require 'nano-command)

(defgroup book-mode nil
  "Book mode"
  :group 'convenience)

(defcustom book-mode-left-margin 8
  "Left margin size, measured in characters"
  :type 'int
  :group 'book-mode)

(defcustom book-mode-right-margin 6
  "Right margin size, measured in characters"
  :type 'int
  :group 'book-mode)

(defcustom book-mode-top-margin 2.25
  "Top margin size, measured in characters"
  :type 'float
  :group 'book-mode)

(defcustom book-mode-top-padding 0.25
  "Bottom margin size, measured in characters"
  :type 'float
  :group 'book-mode)

(defcustom book-mode-bottom-margin 1.45
  "Bottom margin size, measured in characters"
  :type 'float
  :group 'book-mode)

(defcustom book-mode-bottom-padding 0.00
  "Bottom margin size, measured in characters"
  :type 'float
  :group 'book-mode)

(defcustom book-mode-frame-size '(81 . 51)
  "Frame size"
  :type '(cons (integer :tag "width")
               (integer :tag "height"))
  :group 'book-mode)

(defcustom book-mode-frame-border-color nano-dark-background
  "Frame border color"
  :type 'color
  :group 'book-mode)

(defcustom book-mode-hl-line-extend t
  "Whether to extend hl-line to margins"
  :type 'boolean
  :group 'book-mode)

(defcustom book-mode-display-frame-index nil
  "Whether to display frame index"
  :type 'boolean
  :group 'book-mode)

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
    (unless isearch-mode
      (setq book-mode--message-timer
            ;; (run-at-time minibuffer-message-timeout nil
            ;;               #'book-mode--message-clear))
            (run-at-time 2.0 nil #'book-mode--message-clear)))
    (force-mode-line-update)))


(defun book-mode--message-cancel-clear ()
  (when (and (boundp 'book-mode--message-timer) book-mode--message-timer)
    (cancel-timer book-mode--message-timer)))


(defun book-mode--message-clear ()
  "Clear last message"
  
  (setq book-mode--last-message nil)
  (force-mode-line-update))


(defun book-mode--command-error-function (data context caller)
  "This command-error function intercepts some message from the C API."

  (if (not (memq (car data) '(buffer-read-only
                              text-read-only
                              beginning-of-buffer
                              end-of-buffer
                              quit)))
      (command-error-default-function data context caller)
      (book-mode--message (format "%s" data))))


(defun book-mode--header (&rest args)
  ""
  (apply #'book-mode--build
         (nconc `(:overline nil
                  :underline ,(face-foreground 'default)
                  :margin ,(or (plist-get args ':margin)
                               (- book-mode-top-margin))
                  :padding ,(or (plist-get args ':padding)
                               (- book-mode-top-padding)))
                args)))


(defun book-mode--footer (&rest args)
  ""
  (apply #'book-mode--build
         (nconc `(:overline ,(face-foreground 'default)
                  :underline nil
                  :margin ,(or (plist-get args ':margin)
                               book-mode-bottom-margin)
                  :padding ,(or (plist-get args ':padding)
                                book-mode-bottom-padding))
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


(defun book-mode-element-frame-count ()
  (let* ((frames (seq-filter  (lambda (frame)
                               (not (frame-parent frame)))
                             (frame-list)))
         (index (length (member (selected-frame) frames))))
    (propertize (format " %d "index)
                'face `(:inherit (nano-subtle nano-strong)
                        :foreground ,(face-foreground 'nano-default)))))

(defun book-mode-element-frame-count-icon (&optional icon)
  "Prefix element displaying frame count and an icon."
  
  (concat
   (if book-mode-display-frame-index (book-mode-element-frame-count) (book-mode-element-empty))
   "   "
   (cond (icon 
          (propertize (format "%s " icon) 'face 'nano-default))
         ((or (derived-mode-p 'elfeed-show-mode)
              (derived-mode-p 'elfeed-search-mode))
          (propertize " " 'face 'nano-default))
         ((derived-mode-p 'org-agenda-mode)
          (propertize " " 'face 'nano-default))
         ((derived-mode-p 'mastodon-mode)
          (propertize " " 'face 'nano-default))
         ((derived-mode-p 'mu4e-headers-mode)
          (propertize " " 'face 'nano-default))
         ((derived-mode-p 'mu4e-view-mode)
          (propertize " " 'face 'nano-default))
         (view-mode
          (propertize " " 'face 'nano-faded))
         ((derived-mode-p 'helpful-mode)
          (propertize " " 'face 'nano-default))
         (buffer-read-only
          (propertize " " 'face 'nano-faded))
         ((buffer-modified-p)
          (propertize " " 'face 'nano-popout))
         (t
          (propertize " " 'face 'nano-subtle-i)))))

(defun book-mode-element-prefix-elfeed ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-agenda ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-mastodon ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-mu4e-headers ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-mu4e-view ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-helpful ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-view ()
  (book-mode-element-frame-count-icon ""))

(defun book-mode-element-prefix-generic ()
  (book-mode-element-frame-count-icon 
   (cond (buffer-read-only
          (propertize "" 'face 'nano-faded))
         ((buffer-modified-p)
          (propertize "" 'face 'nano-popout))
         (t
          (propertize "" 'face 'nano-subtle-i)))))

(defun book-mode-element-elfeed-feed-name ()
  (plist-get (elfeed-feed-meta
              (elfeed-entry-feed elfeed-show-entry)) :title))

(defun book-mode-element-elfeed-search-filter ()
  elfeed-search-filter)

(defun book-mode-element-agenda-name ()
  (save-excursion
    (goto-char (point-min))
    (buffer-substring-no-properties (line-beginning-position))))

(defun book-mode-element-mu4e-context ()
  (if (> (length (mu4e-context-label)) 0)
      (substring-no-properties (mu4e-context-label) 1 -1)
    ""))

(defun book-mode-element-mu4e-search-query ()
  (mu4e-last-query))

(defun book-mode-element-mu4e-message-subject ()
  "")

(defun book-mode-element-mu4e-message-sender ()
  "")

(defun book-mode-element-mu4e-message-date ()
  "")

(defun book-mode-element-buffer-name ()
  (buffer-name))

(defun book-mode-element-name ()

  (let ((name (cond ;; Elfeed show mode
                     ((derived-mode-p 'elfeed-show-mode)
                     (plist-get (elfeed-feed-meta
                        (elfeed-entry-feed elfeed-show-entry)) :title))
                     ;; Elfeed show mode
                     ((derived-mode-p 'elfeed-search-mode)
                      (concat "" elfeed-search-filter))

                     ;; Org agenda mode
                     ((derived-mode-p 'org-agenda-mode)
                      (save-excursion
                        (let ((inhibit-read-only t))
                          (goto-char (point-min))
                          ;; (set-text-properties (line-beginning-position)
                          ;;                      (+ (line-end-position) 1)
                          ;;                      '(invisible t))
                          (buffer-substring-no-properties (line-beginning-position)
                                                          (- (line-end-position) 1)))))

                     ;; Mu4e headers mode
                     ((derived-mode-p 'mu4e-headers-mode)
                      (concat "" (mu4e-last-query)))
                     ;; Mu4e view mode
                     ((derived-mode-p 'mu4e-view-mode)
                      "Message")
                    ;; Default
                    (t (buffer-name)))))
    (propertize name 'face '(:inherit nano-strong))))


(defun book-mode-element-word-count ()

  (let* ((beg (if (use-region-p) (region-beginning) (point-min)))
         (end (if (use-region-p) (region-end) (point-max)))
         (word-count (count-words beg end))
         (char-count (- end beg)))
    (propertize (format "%d words / %d chars" word-count char-count)
                'face 'nano-faded)))

(defun book-mode-element-word-target ()

  (let* ((word-count (count-words (point-min) (point-max)))
         (word-total 300)
         (ratio (/ (float word-count) (float word-total))))
    (propertize "  " 'display (svg-lib-progress-pie ratio nil))))

(defun book-mode-element-dedicated ()
  (propertize "    " 'face (if (window-dedicated-p)
                             '(:inherit nano-default)
                           '(:inherit nano-subtle-i))
                   'display '(raise 0)))

(defun book-mode-element-empty ()
  "")

(defun book-mode-element-message ()
  (if (boundp 'book-mode--last-message)
      (or book-mode--last-message "")
    ""))

(defun book-mode-element-line/total ()
  (propertize (format "%s/%s"(format-mode-line "%l")
                      (save-excursion
                        (goto-char (point-max))
                        (format-mode-line "%l")))))

(defun book-mode-element-mu4e-query ()
  (mu4e-last-query))

(defun book-mode-element-mu4e-context ()
  (substring-no-properties (mu4e-context-label)))

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
   (or hl-line-overlay global-hl-line-overlay)
   (if (derived-mode-p 'mu4e-headers-mode)
       'mu4e-header-highlight-face
     'hl-line))
  (cons (line-beginning-position) (line-beginning-position 2)))


;; ----------------------------------------------------------------------------
;; (defun book-mode-old (&optional global)

;;   (interactive)
;;   (let ((left-margin    8)
;;         (right-margin   6)
;;         (top-margin     2.25)
;;         (top-padding    0.25)
;;         (bottom-margin  1.45)
;;         (bottom-padding 0.00))
;;     (if global
;;         (setq-default left-margin-width left-margin
;;                       right-margin-width right-margin))
;;     (set-window-margins (selected-window) left-margin right-margin)
;;     (setq left-margin-width left-margin)
;;     (setq right-margin-width right-margin)
    
;;     (set-frame-parameter (selected-frame) 'internal-border-width 1)
;;     (set-frame-parameter (selected-frame) 'width (+ 81
;;                                                     left-margin
;;                                                     right-margin))
;;     (set-frame-parameter (selected-frame) 'height 50)
;;     (set-face-background 'internal-border nano-dark-background
;;                          (selected-frame))
    
;;     (setq-local book-mode--message-timer nil)
;;     (setq-local book-mode--message-last nil)

;;     (setq line-spacing 1)
;;     (fringe-mode '(0 . 0))
;;     (set (make-local-variable 'region-overlay)
;;          (make-overlay (point-min) (point-min)))

;;     (if global
;;         (progn
;;           (set-face-attribute 'header-line nil
;;                               :background (face-background 'default))
;;           (set-face-attribute 'mode-line nil
;;                              :foreground (face-foreground 'default)
;;                              :background (face-background 'default)
;;                              :height (face-attribute 'default :height))
;;           (set-face-attribute 'mode-line-inactive nil
;;                              :foreground (face-foreground 'default)
;;                              :background (face-background 'default)
;;                              :height (face-attribute 'default :height))
;;           (set-face-attribute 'region nil
;;                               :background "#e0e0ff")
;;           (set-face-attribute 'hl-line nil
;;                               :background "#f5f5ff"))
;;       (progn
;;         (face-remap-add-relative 'header-line
;;                                  :background (face-background 'default))
;;         (face-remap-add-relative 'mode-line
;;                                  :foreground (face-foreground 'default)
;;                                  :background (face-background 'default)
;;                                  :height (face-attribute 'default :height))
;;         (face-remap-add-relative 'mode-line-inactive
;;                                  :foreground (face-foreground 'default)
;;                                  :background (face-background 'default)
;;                                  :height (face-attribute 'default :height))
;;         (face-remap-add-relative 'region :background "#e0e0ff")
;;         (face-remap-add-relative 'hl-line :background "#f5f5ff")))

;;     (setq-local header-line-format
;;                 (book-mode--header :prefix #'book-mode-element-prefix
;;                                    :left #'book-mode-element-name
;;                                    :suffix #'book-mode-element-dedicated
;;                                    :margin  (- top-margin)
;;                                    :padding (- top-padding))
;;                 mode-line-format
;;                 (book-mode--footer :left #'book-mode-element-message
;;                                    :right #'book-mode-element-line/total
;;                                    :margin bottom-margin
;;                                    :padding bottom-padding))
;;     (when global
;;       (setq-default header-line-format header-line-format)
;;       (setq-default mode-line-format mode-line-format))

;;     (when (derived-mode-p 'org-mode)
;;       (add-to-list 'font-lock-extra-managed-props 'display)
;;       (let ((margin-format (format "%%%ds" left-margin)))
;;         (font-lock-add-keywords nil
;;          `(
;;            ("^\\(\\- \\)\\(.*\\)$"
;;             1 '(face nano-default display ((margin left-margin)
;;                                            ,(propertize (format margin-format "• ")
;;                                                         'face '(:inherit nano-default :weight light)) append)))
           
;;            ("^\\(\\*\\{1\\} \\)\\(.*\\)$"
;;            1 '(face nano-faded display ((margin left-margin)
;;                                         ,(propertize (format margin-format "# ")
;;                                                      'face '(:inherit nano-faded :weight light)) append))
;;            2 '(face bold append))

;;            ("^\\(\\*\\{2\\} \\)\\(.*\\)$"
;;             1 '(face nano-faded display ((margin left-margin)
;;                                          ,(propertize (format margin-format "## ")
;;                                                       'face '(:inherit nano-faded :weight light)) append))
;;             2 '(face bold append))

;;            ("^\\(\\*\\{3\\} \\)\\(.*\\)$"
;;             1 '(face nano-faded display ((margin left-margin)
;;                                          ,(propertize (format margin-format "### ")
;;                                                       'face '(:inherit nano-faded :weight light)) append))
;;             2 '(face bold append))

;;            ("^\\*\\{4\\} .*?\\(\n\\)"
;;             1 '(face nil display " - "))

;;            ("^\\(\\*\\{4\\} \\)\\(.*?\\)$"
;;             1 '(face nano-faded display ((margin left-margin)
;;                                          ,(propertize (format margin-format "§ ")
;;                                                       'face '(:inherit nano-faded :weight light))  append))
;;             2 '(face bold append))))))
;;     )

;;   (book-mode--message-clear)
;;   (advice-add 'message :override #'book-mode--message)
;;   (when (derived-mode-p 'org-mode)
;;     (font-lock-fontify-buffer)
;;     (visual-line-mode))
;;   (set (make-local-variable 'region-overlay)
;;        (make-overlay (point-min) (point-min)))
;;    (setq hl-line-range-function #'book-mode-hl-line-range-function)
;;   )


;;;###autoload
(defun book-mode ()

  (interactive)
  (setq linum-format
        (format (format "%%%ds" (- book-mode-left-margin 2)) "%4d"))
  (set-window-margins (selected-window) book-mode-left-margin
                      book-mode-right-margin)
  (setq left-margin-width book-mode-left-margin)
  (setq right-margin-width book-mode-right-margin)

  (set-frame-parameter (selected-frame) 'internal-border-width 1)
  (set-frame-parameter (selected-frame) 'width (+ (car book-mode-frame-size)
                                                  book-mode-left-margin
                                                  book-mode-right-margin))
  (set-frame-parameter (selected-frame) 'height (cdr book-mode-frame-size))
  (set-face-background 'internal-border book-mode-frame-border-color
                       (selected-frame))
  (setq-local book-mode--message-timer nil)
  (make-local-variable 'book-mode--message-timer)

  (setq-local book-mode--message-last nil)
  (make-local-variable 'book-mode--last-message)

  (setq line-spacing 1)
  (set-frame-parameter (selected-frame) 'right-divider-width 1)
  
  (fringe-mode '(0 . 0))
  (set (make-local-variable 'region-overlay)
       (make-overlay (point-min) (point-min)))
  (face-remap-add-relative 'header-line
                           :background (face-background 'default))
  (face-remap-add-relative 'window-divider
                           :foreground (face-foreground 'default))

  (face-remap-set-base 'mode-line nil)
  (face-remap-set-base 'mode-line-inactive nil)

  (face-remap-add-relative 'mode-line
                           :foreground (face-foreground 'default)
                           :background (face-background 'default)
                           :height (face-attribute 'default :height))
  (face-remap-add-relative 'mode-line-inactive
                           :foreground (face-foreground 'default)
                           :background (face-background 'default)
                           :height (face-attribute 'default :height))
  ;; (face-remap-add-relative 'region :background "#e0e0ff")
  ;; (face-remap-add-relative 'hl-line :background "#f5f5ff")
  (setq-local header-line-format
              (book-mode--header :prefix #'book-mode-element-frame-count-icon
                                 :left #'book-mode-element-name
                                 :suffix #'book-mode-element-dedicated))
  (setq-local mode-line-format
              (book-mode--footer :left #'book-mode-element-message
                                 :right  #'book-mode-element-line/total))

  (add-hook 'isearch-mode-hook
            (lambda ()
              (setq-local mode-line-format
                          (book-mode--footer
                           :prefix " "
                           :left #'book-mode-element-message
                           :right  #'book-mode-element-line/total))
              (book-mode--message-cancel-clear)))
  
  (add-hook 'isearch-mode-end-hook
            (lambda ()
              (setq-local mode-line-format
                          (book-mode--footer
                           :left #'book-mode-element-message
                           :right  #'book-mode-element-line/total))
              (book-mode--message-clear)))
  
  (book-mode--message-clear)
  (advice-add 'message :override #'book-mode--message)
  (set (make-local-variable 'region-overlay)
       (make-overlay (point-min) (point-min)))
  (setq hl-line-range-function #'book-mode-hl-line-range-function))

(defun my/hide-cursor ()
  (setq cursor-type nil))

(defun my/hide-org-agenda-header-line ()
  (save-excursion
    (let ((inhibit-read-only t))
      (goto-char (point-min))
      (set-text-properties (line-beginning-position)
                           (+ (line-end-position) 1)
                           '(invisible t)))))

(add-hook 'elfeed-search-mode-hook 'book-mode)
(add-hook 'elfeed-show-mode-hook 'book-mode)
(add-hook 'org-agenda-mode-hook 'book-mode)
(add-hook 'org-agenda-finalize-hook 'my/hide-org-agenda-header-line)
(add-hook 'mu4e-headers-mode-hook 'book-mode)
(add-hook 'mu4e-headers-mode-hook 'my/hide-cursor)
(add-hook 'mu4e-loading-mode-hook 'book-mode)
(add-hook 'mu4e-view-mode-hook 'book-mode)
(add-hook 'mu4e-compose-mode-hook 'book-mode)
(add-hook 'help-mode-hook 'book-mode)
(add-hook 'helpful-mode-hook 'book-mode)
(add-hook 'mastodon-mode-hook 'book-mode)
(add-hook 'python-mode-hook 'book-mode)
(add-hook 'emacs-lisp-mode-hook 'book-mode)

(provide 'book-mode)
;;; book-mode.el ends here


;; (defun book-command--update (prompt text)
;;   (let* ((prompt (string-trim (substring-no-properties prompt)))
;;          (prompt (propertize prompt 'face 'nano-strong )))
;;      (setq-local book-mode--last-message
;;                  (concat prompt " " text))))

;; (defun book-command (prompt &optional hook icon)
;;   (advice-add 'nano-command--update :override #'book-command--update)
;;   (setq-local mode-line-format
;;               (book-mode--footer :prefix (or icon "")
;;                                  :left #'book-mode-element-message
;;                                  :right  #'book-mode-element-line/total
;;                                  :margin 0.0
;;                                  :padding 0.0))
;;   (let ((command (nano-command prompt hook
;;                                (propertize "\n" 'face '(:height 1.2)))))
;;     (setq-local mode-line-format
;;                 (book-mode--footer :left #'book-mode-element-message
;;                                    :right  #'book-mode-element-line/total))
;;     (advice-remove 'nano-command--update #'book-command--update)
;;     (setq-local book-mode--last-message "")
;;     (force-mode-line-update)
;;     command))

;; (bind-key "H-&" #'(lambda ()
;;                     (interactive)
;;                     (let ((command (book-command "SHELL:" nil " ")))
;;                       (when command
;;                         (async-shell-command command)))))


;; (defun org-quick-meeting-hook ()
;;   (insert (format-time-string (concat " "
;;                                (cdr org-time-stamp-formats))))
;;   (goto-char (point-min))
;;   (org-mode))
  
;; (defun org-quick-meeting-refile (file headline)
;;   (let ((pos (save-excursion
;;                (find-file file)
;;                (org-find-exact-headline-in-buffer headline))))
;;     (org-refile nil nil (list headline file nil pos))))

;; (defun org-quick-meeting ()
;;   "This function allows to register a meeting (org)"

;;   (interactive)
;;   (let ((capture (book-command "MEETING " #'org-quick-meeting-hook " ")))
;;     (when capture
;;       (let ((current-buffer (current-buffer)))
;;         (with-temp-buffer
;;           (insert (format "** %s\n" capture))
;;           (goto-char (point-min))
;;           (org-quick-meeting-refile "~/Documents/org/agenda.org" "Future"))
;;         (switch-to-buffer current-buffer)))))

;; (bind-key "H-m" #'org-quick-meeting)

