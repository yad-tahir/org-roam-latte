;;; org-roam-latte.el --- Highlight org roam node -*- lexical-binding: t; -*-
;;;

;;   ___
;;  / _ \ _ __ __ _
;; | | | | '__/ _` |
;; | |_| | | | (_| |
;;  \___/|_|  \__, |
;;            |___/
;;
;;      ____                         _          _   _
;;     |  _ \ ___   __ _ _ __ ___   | |    __ _| |_| |_ ___
;;     | |_) / _ \ / _` | '_ ` _ \  | |   / _` | __| __/ _ \
;;     |  _ < (_) | (_| | | | | | | | |__| (_| | |_| ||  __/
;;     |_| \_\___/ \__,_|_| |_| |_| |_____\__,_|\__|\__\___|
;;

;; Author: Yad Tahir <yad at ieee.org>
;; URL: https://github.com/yad-tahir/org-roam-latte
;; Package-Requires: ((emacs "27.1"))
;; Version: 0.0.1
;; Description: Automatically highlights words if they exist in org-roam
;; Keywords: faces, outline

;; This file is not part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Collects titles and alias found in the org-roam database. Latte then
;; automatically highlights in a buffer where `org-roam-latte-mode' is active.
;; This implementation is designed to be snappy and minimizes UI redrawing to
;; maintain high performance.

;;; Code:

(require 'org-roam)
(require 'seq)

(defgroup org-roam-latte nil "A simple notebook manager with auto highlighting built on
top of the beloved Org-mode." :group 'org-roam-latte)

(defcustom org-roam-latte-highlight-prog-comments t
  "If enabled (t), highlight keywords in prog-mode comment sections only."
  :group 'org-roam-latte
  :type 'boolean)

(defcustom org-roam-latte-ignore-words '()
  "The words in the list will not be treated as keywords"
  :group 'org-roam-latte
  :type '(repeat string))

(defvar org-roam-latte-keyword-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'org-roam-latte-open-at-point)
    (define-key map (kbd "<RET>") 'org-roam-latte-open-at-point)
    (define-key map (kbd "<M-RET>") 'org-roam-latte-complete-at-point)
    map)

  "Keymap for highlighted keywords.")

(defface org-roam-latte-keyword-face
  '((t (:underline (:color "#665555" :style wave :position nil))))
  "org-roam-latte mode face used to highlight keywords and topic titles"
  :group 'org-roam-latte)


;;; Internal variables

(defvar org-roam-latte--keywords (make-hash-table :test 'equal)
  "Holds list of keywords.

This global data structure is modified primarily by `org-roam-latte--db-modified'.

Both `org-roam-latte--highlight-buffer' and `org-roam-latte--delete-overlays' use this list
to update UI accordingly.")

(defvar org-roam-latte--initialized nil
  "Holds t if notebook's timers are initialized and started. Otherwise, nil.

This variable is used to ensure only one instance of the timers exists
globally.")

(defvar-local org-roam-latte--prev-start-win 0
  "Holds window start position before scrolling.")

(defvar-local org-roam-latte--prev-end-win 0
  "Holds window end position before scrolling.")

(defvar-local org-roam-latte--prev-win nil
  "Holds last window object this buffer used during scrolling.")

;;;
;;; Helpers
;;;

(defun org-roam-latte--change-major-mode ()
  "Called internally when the major mode has changed in the current buffer."

  (org-roam-latte--delete-overlays nil nil t))

(defun org-roam-latte--delete-overlays (&optional start end force)
  "Called internally to delete overlays that are between START and END,
and no longer valid.

Goes through each overlay existing in the current buffer, and performs some
checking such as whether it still has still valid  keyword. If not, this function
deletes the overlay.

When FORCE is non-nil, keyword checking is not performed. The overlay deleted immediately."

  (setq start (or start (point-min))
        end (or end (point-max)))

  (dolist (o (overlays-in start end))
    (let* ((o-start (overlay-start o))
           (o-end (overlay-end o))
           (o-f (overlay-get o 'face))
           (keyword (downcase (buffer-substring-no-properties o-start o-end))))
      (when (or force
                (and (equal o-f 'org-roam-latte-keyword-face)
                     ;; Check if it still points at a keyword
                     (not (gethash keyword org-roam-latte--keywords))))
        (delete-overlay o)))))

(defun org-roam-latte--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD exists between START and END."

  (catch 'org-roam-latte--overlay-found
    (dolist (co (overlays-in start end))
      (when (equal keyword (overlay-get co 'org-roam-latte-keyword))
        (if (and (equal (overlay-start co) start)
                 (equal (overlay-end co) end))
            (throw 'org-roam-latte--overlay-found t)

          ;; Region mismatch; e.g. an old overlay that does not
          ;; accommodate the extra length. Clean it and continue searching.
          (delete-overlay co)
          nil)))))

(defun org-roam-latte--phrase-checker (phrase)
  "Returns t if PHRASE is a keyword."

  (gethash phrase org-roam-latte--keywords))

(defun org-roam-latte--highlight-buffers ()
  (dolist (buffer (buffer-list))
    (org-roam-latte--highlight-buffer nil nil buffer)))

(defun org-roam-latte--highlight-buffer (&optional start end buffer)
  (setq buffer (or buffer (current-buffer)))

  (with-current-buffer buffer
    (when (bound-and-true-p org-roam-latte-mode)
      (when-let ((b-win (get-buffer-window)))
        (let ((b-start (or start (window-start b-win)))
              (b-end (or end (window-end b-win t))))
          (org-roam-latte--make-overlays buffer b-start b-end))))))

(defun org-roam-latte--make-overlays (buffer &optional start end)
  "Highlights all the instances of KEYWORD in the current buffer. For each
  instance, this function creates a clickable overlay.

The optional second argument START indicates starting position. Highlighting
must not occur before START. A value of nil means search from `(point-min)'.

The optional third argument END indicates ending position. Highlight must not
occur after END. A value of nil means search from `(point-max)'."

  (with-current-buffer buffer
    (ignore-errors
      (setq start (or start (point-min))
            end (min (or end (point-max)) (point-max)))
      (save-excursion
        (save-restriction
          (with-silent-modifications
            (narrow-to-region start end)
            ;; Go to starting point
            (org-roam-latte--delete-overlays start end)
            (maphash (lambda (key value)
                       (goto-char start)
                       (while (word-search-forward key nil t)
                         (let ((match-beg (match-beginning 0))
                               (match-end (match-end 0)))

                           (unless (or (org-roam-latte--overlay-exists value match-beg match-end)
                                       (and (derived-mode-p 'org-mode)
                                            (eq (org-element-type (org-element-context)) 'link))
                                       (and org-roam-latte-highlight-prog-comments
                                            (derived-mode-p 'prog-mode)
                                            ;; Comment section?
                                            ;; from https://github.com/blorbx/evil-quickscope
                                            (not (nth 4 (syntax-ppss)))))
                             (let ((o (make-overlay match-beg match-end)))
                               (overlay-put o 'face 'org-roam-latte-keyword-face)

                               ;; Vanish when empty (deleted text):
                               (overlay-put o 'evaporate t)
                               (overlay-put o 'keymap org-roam-latte-keyword-map)
                               (overlay-put o 'mouse-face 'highlight)
                               ;; Use the pure form to improve the quality of the
                               ;; search when requested.
                               (overlay-put o 'org-roam-latte-keyword value)

                               ;; The priority is calculated based on the number of the
                               ;; characters. Thus, overlays with longer phrases are on
                               ;; top.
                               (overlay-put o 'priority (length key)))))))

                     org-roam-latte--keywords)))))))

(defun org-roam-latte--pluralize (phrase)
  "Return the plural form of PHRASE using standard English grammar rules.

Handles simple phrases like 'inverse element' -> 'inverse elements'."
  (let ((case-fold-search t))
    (cond
     ;; Common irregulars
     ((string-equal phrase "child") "children")
     ((string-equal phrase "person") "people")
     ((string-equal phrase "man") "men")
     ((string-equal phrase "woman") "women")
     ((string-equal phrase "tooth") "teeth")
     ((string-equal phrase "foot") "feet")

     ;; Words ending in s, x, z, ch, sh
     ;; FIX: Wrapped in \\(...\\) so $ applies to all alternatives
     ((string-match-p "\\([sxz]\\|ch\\|sh\\)$" phrase)
      (concat phrase "es"))

     ;; Words ending in consonant + y
     ((string-match-p "[^aeiou]y$" phrase)
      (concat (substring phrase 0 -1) "ies"))

     ;; Words ending in f or fe; remove f/fe, add "ves"
     ((string-match-p "\\(li\\|wi\\|lo\\|lea\\|shel\\|thie\\)fe?$" phrase)
      (replace-regexp-in-string "fe?$" "ves" phrase))

     (t (concat phrase "s")))))

(defun org-roam-latte--add-keyword (keyword)
  "Called internally to add KEYWORD to `org-roam-latte--keywords'.

   This functions makes sure that there is no duplicated
   keywords in org-roam-latte--keywords."

  ;; If it is a new keyword and not blacklisted!
  (unless (or (gethash keyword org-roam-latte--keywords)
              (member keyword org-roam-latte-ignore-words))
    ;; Add KEYWORD along with all possible chopped forms
    (puthash keyword keyword org-roam-latte--keywords)
    (puthash (org-roam-latte--pluralize keyword) keyword org-roam-latte--keywords)))

(defun org-roam-latte--keyword-at-point ()
  "Return the highlighted keyword at point."

  (let ((p (overlays-at (point) t))
        (lk nil))
    (catch 'org-roam-latte--found
      (dolist (o p)
        (when-let (k (overlay-get o 'org-roam-latte-keyword))
          (throw 'org-roam-latte--found (setq lk (downcase k))))))
    (or lk
        (when (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))
        (word-at-point)
        "")))

(defun org-roam-latte--db-modified (&rest args)
  "Use org-roam database to updates `org-roam-latte--keywords'. "

  (setq org-roam-latte--keywords (make-hash-table :test 'equal))
  (mapcar #'(lambda (node)
              (org-roam-latte--add-keyword (downcase (org-roam-node-title node))))
          (org-roam-node-list))
  (org-roam-latte--highlight-buffers)
  t)

(defun org-roam-latte--after-change-function (beginning end &optional _old-len)
  "Highlight new keywords text modification events occur."
  (save-excursion
    (goto-char beginning)
    ;; redraw the entire line
    (org-roam-latte--highlight-buffer (line-beginning-position) (line-end-position))))

(defun org-roam-latte--after-revert-function (&rest args)
  "Re-highlight keywords when revert events occur."

  (org-roam-latte--highlight-buffer))

(defun org-roam-latte--scroll-handler (win start)
  "Called when window scroll events occur.

This function is also triggered when a window is just attached to a buffer."

  (let* ((start (window-start win))
         (end (window-end win t))
         (diff (- start org-roam-latte--prev-start-win))
         (full-render (or (not org-roam-latte--prev-win)
                          (not (eq org-roam-latte--prev-win win))
                          (> (abs diff)
                             ;; Less than 1/3 of window size
                             (/ (- org-roam-latte--prev-end-win org-roam-latte--prev-start-win) 3)))))
    (cond
     ((and (>= diff 1) ;; Moving downward
           (not full-render))
      (org-roam-latte--highlight-buffer org-roam-latte--prev-end-win end))
     ((and (< diff 1) ;; Moving upward
           (not full-render))
      (org-roam-latte--highlight-buffer start org-roam-latte--prev-start-win))
     (t
      (org-roam-latte--highlight-buffer start end )))

    (setq org-roam-latte--prev-start-win start
          org-roam-latte--prev-end-win end
          org-roam-latte--prev-win win)))

(defun org-roam-latte--node-link-insert (keyword &optional beg end)
  "Insert an org-roam node link.

If BEG and END are provided, replace the text in that range.
If not, but the region is active, replace the active region.
Otherwise, insert at point.

KEYWORD is used as the initial input for the node search."
  (let* ((use-region (and (not beg) (region-active-p)))
         (start (if use-region (region-beginning) beg))
         (final (if use-region (region-end) end))
         (replace-p (and start final))
         (original-text (when replace-p
                          (org-link-display-format
                           (buffer-substring-no-properties start final))))
         ;; We gather all user input BEFORE starting the atomic change group.
         (node (org-roam-node-read keyword nil))
         (description (or original-text
                          (org-roam-node-formatted node))))

    (when (org-roam-node-id node)
      (atomic-change-group
        (when replace-p
          (delete-region start final))

        (insert (org-link-make-string
                 (concat "id:" (org-roam-node-id node))
                 description))

        ;; Trigger hooks
        (run-hook-with-args 'org-roam-post-node-insert-hook
                            (org-roam-node-id node)
                            description))

      ;; Only deactivate the mark if we implicitly used the user's active region.
      ;; If we used explicit BEG/END args, we leave the user's cursor state alone.
      (when use-region
        (deactivate-mark)))))

(defun org-roam-latte-complete-at-point (&optional point)
  "Places a link for the node for the overlay at POINT.

If POINT is nil, then the current cursor position will be used."
  (interactive)
  (setq point (or point (point)))
  (save-excursion
    (goto-char point)
    ;; Use seq-find to safely locate the specific overlay
    (if-let ((overlay (seq-find (lambda (o) (overlay-get o 'org-roam-latte-keyword))
                                (overlays-at (point)))))
        (org-roam-latte--node-link-insert (overlay-get overlay 'org-roam-latte-keyword)
                                          (overlay-start overlay)
                                          (overlay-end overlay))
      (message "No org-roam-latte overlay found at point."))))

;;; Minor mode
;;;

;;;###autoload
(define-minor-mode org-roam-latte-mode
  "Minor mode highlights notebook's keywords throughout the buffer."

  :init-value nil
  :lighter latte
  :keymap nil
  :require 'org-roam-latte
  :group 'org-roam-latte

  (if org-roam-latte-mode
      (progn ;;on
        ;; Local hooks
        (add-hook 'window-scroll-functions 'org-roam-latte--scroll-handler t t)
        (add-hook 'after-revert-hook 'org-roam-latte--after-revert-function t t)
        (add-hook 'after-change-functions 'org-roam-latte--after-change-function t t)
        (add-hook 'change-major-mode-hook 'org-roam-latte--change-major-mode t t)

        ;; Globally exec once
        (unless org-roam-latte--initialized
          (advice-add 'org-roam-db-update-file :after #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-insert-file :after  #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-clear-file :after  #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-update-file :after  #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-sync :after #'org-roam-latte--db-modified)

          (org-roam-latte--db-modified)
          (setq org-roam-latte--initialized t)))

    (progn ;;off
      ;; Remove our overlays
      (org-roam-latte--delete-overlays nil nil t)

      ;; Remove local hooks
      (remove-hook 'window-scroll-functions 'org-roam-latte--scroll-handler t)
      (remove-hook 'after-revert-hook 'org-roam-latte--after-revert-function t)
      (remove-hook 'after-change-functions 'org-roam-latte--after-change-function t)
      (remove-hook 'change-major-mode-hook 'org-roam-latte--change-major-mode t)))
  t)

;;;###autoload
(defun org-roam-latte-open-at-point ()
  "Search for the keyword at point in the notebook, and then show all the
  headings in which the keyword has been used."

  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context)))
    (if (memq type '(link))
        (org-open-at-point)
      (org-roam-node-find nil (org-roam-latte--keyword-at-point)))))


(provide 'org-roam-latte)

;;; org-roam-latte.el ends here
