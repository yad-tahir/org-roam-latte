;;; org-roam-latte.el --- Auto-highlight unlinked Org-roam references -*- lexical-binding: t; -*-

;;  ___
;; / _ \  _ __ __ _
;; | | | | '__/ _` |
;; | |_| | | | (_| |
;  \___/ |_|  \__, |
;;            |___/
;;      ____                         _          _   _
;;     |  _ \ ___   __ _ _ __ ___   | |    __ _| |_| |_ ___
;;     | |_) / _ \ / _` | '_ ` _ \  | |   / _` | __| __/ _ \
;;     |  _ < (_) | (_| | | | | | | | |__| (_| | |_| ||  __/
;;     |_| \_\___/ \__,_|_| |_| |_| |_____\__,_|\__|\__\___|
;;

;; Author: Yad Tahir <yad at ieee.org>
;; URL: https://github.com/yad-tahir/org-roam-latte
;; Package-Requires: ((emacs "27.1") (org-roam "2.0") (inflections "1.1"))
;; Package-Version: 0.8
;; Description: Auto-highlight unlinked Org-roam references
;; Keywords: hypermedia, outlines, org-roam, convenience

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
;; Org-roam Latte is a minor mode that highlights unlinked references to
;; existing org-roam nodes. It scans your text for words matching existing
;; org-roam node titles or aliases and highlights them, allowing you to quickly
;; navigate to those nodes or convert the text into a formal link.
;;
;; Features:
;; - Fast: Uses an optimized inverted search strategy. It scans only the VISIBLE
;; section of a buffer, and checks against a hash table. It stays snappy even
;; with thousands of nodes.
;; - Smart Linking: Highlighted words are navigatable and con be converted
;; to links easily.
;; - Pluralization: Automatically handles pluralization (e.g., a node titled
;; "Algorithm" will highlight "algorithms" in your text).
;; Org-roam Alias Support: Recognizes and highlights your node aliases as well.
;; - Context Aware: Ignores existing Org links. Intelligent handling of code
;; blocks (only highlights inside comments).
;; - Theme Aware: Adapts colors automatically for Light and Dark themes.

;;; Code:

(require 'org-roam)
(require 'seq)
(require 'inflections)

(defgroup org-roam-latte nil
  "A minor mode that automatically highlights unlinked org-roam references."
  :group 'org-roam)

(defcustom org-roam-latte-highlight-prog-comments t
  "When non-nil, highlight keywords inside comments when in programming modes."
  :group 'org-roam-latte
  :type 'boolean)

(defcustom org-roam-latte-exclude-words '()
  "A list of strings to ignore.
Words in this list will not be highlighted even if they match an Org-roam node."
  :group 'org-roam-latte
  :type '(repeat string))

(defcustom org-roam-latte-exclude-org-elements '(link node-property keyword)
  "List of Org element types where highlight should not be created.

Common types include `link', `node-property', `keyword', `code', and `verbatim'.
See `org-element-all-elements' for a comprehensive list."
  :type '(repeat symbol)
  :group 'org-roam-latte)

(defcustom org-roam-latte-exclude-current-node t
  "When non-nil, disable highlighting for current node title/aliases."
  :group 'org-roam-latte
  :type 'boolean)

(defvar org-roam-latte-keyword-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<mouse-1>") 'org-roam-latte-open-at-point)
    (define-key map (kbd "<RET>") 'org-roam-latte-open-at-point)
    (define-key map (kbd "<M-RET>") 'org-roam-latte-complete-at-point)
    map)
  "Keymap active on highlighted unlinked org-roam references.")

(defface org-roam-latte-keyword-face
  '((((class color) (background light))
     :underline (:color "cyan" :style wave))
    (((class color) (background dark))
     :underline (:color "purple" :style wave))
    (t :inherit highlight))
  "Face used to highlight unlinked org-roam references (node titles and aliases)."
  :group 'org-roam-latte)

(defcustom org-roam-latte-base-priority 0
  "Base priority for overlays created by `org-roam-latte'.

The actual priority of a highlight is calculated dynamically as the sum of
this base value and the length of the matched phrase. This ensures that
longer, more specific matches are prioritized over shorter ones.

Increase this value to ensure Latte highlights appear above overlays
from other minor modes; decrease it to render them below."
  :type 'integer
  :group 'org-roam-latte)

;;; Internal variables

(defvar org-roam-latte--keywords (make-hash-table :test 'equal)
  "Hash table containing all known Org-roam node titles and aliases.

This structure is updated via `org-roam-latte--db-modified` hooks.
Keys include the downcased title, aliases, and their pluralized forms.")

(defvar org-roam-latte--initialized nil
  "Non-nil if Org-roam-latte hooks and advice have been initialized.")

(defvar-local org-roam-latte--prev-win nil
  "The window object associated with the buffer during the last scroll.

This is used by `org-roam-latte--scroll-handler'")

(defvar-local org-roam-latte--win-prev-start 0
  "Window start position before the most recent scroll event.

This is used by `org-roam-latte--scroll-handler'")

(defvar-local org-roam-latte--win-prev-end 0
  "Window end position before the most recent scroll event.

This is used by `org-roam-latte--scroll-handler'")

;;;
;;; Helpers
;;;

(defun org-roam-latte--delete-overlays (&optional start end force)
  "Delete Latte overlays between START and END.

If FORCE is non-nil, delete overlays immediately.
If FORCE is nil, only delete overlays that no longer match a valid keyword
in `org-roam-latte--keywords`."
  (setq start (or start (point-min))
        end (or end (point-max)))

  (dolist (o (overlays-in start end))
    (let* ((o-f (overlay-get o 'face))
           ;; Optimization: Only grab substring if we suspect it's our overlay
           (keyword (when (eq o-f 'org-roam-latte-keyword-face)
                      (downcase (buffer-substring-no-properties
                                 (overlay-start o) (overlay-end o))))))
      (when (or force
                (and (eq o-f 'org-roam-latte-keyword-face)
                     ;; Check if it still points at a valid keyword
                     (not (org-roam-latte--phrase-checker keyword))))
        (delete-overlay o)))))

(defun org-roam-latte--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD already exists between START and END."
  (catch 'org-roam-latte--overlay-found
    (dolist (co (overlays-in start end))
      (when (equal keyword (overlay-get co 'org-roam-latte-keyword))
        (if (and (equal (overlay-start co) start)
                 (equal (overlay-end co) end))
            (throw 'org-roam-latte--overlay-found t)
          ;; Else region mismatch; e.g. an old overlay that does not
          ;; accommodate the extra length. Clean it and continue searching.
          (delete-overlay co)
          nil)))))

(defun org-roam-latte--phrase-checker (phrase)
  "Return t if PHRASE is a known keyword in the database."
  (gethash phrase org-roam-latte--keywords))

(defun org-roam-latte--highlight-buffers ()
  "Trigger highlighting for all buffers where the mode is active."
  (dolist (buffer (buffer-list))
    (org-roam-latte--highlight-buffer nil nil buffer)))

(defun org-roam-latte--highlight-buffer (&optional start end buffer)
  "Highlight keywords in BUFFER between START and END positions.

If BUFFER is nil, use current buffer. If START/END are nil, use the visible
window."
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (when (bound-and-true-p org-roam-latte-mode)
      (when-let ((b-win (get-buffer-window)))
        (let ((b-start (or start (window-start b-win)))
              (b-end (or end (window-end b-win t))))
          (org-roam-latte--make-overlays buffer b-start b-end))))))

(defun org-roam-latte--match-highlightable (keyword)
  (if (not org-roam-latte-exclude-current-node)
      t
    (let ((current-node (org-roam-node-at-point)))
      (if (not current-node)
          t
        (let* ((current-id (org-roam-node-id current-node))
               ;; Use 'like' for easier case-insensitivity
               (ids (org-roam-db-query
                     [:select [id] :from nodes :where (= title $s1)
                      :union
                      :select [node_id] :from aliases :where (= alias $s1)]
                     keyword))
               ;; org-roam-db-query returns a list of lists: (("id1") ("id2"))
               ;; We must wrap current-id in a list to match that structure
               (is-member (member (list current-id) ids)))
          (not is-member))))))

(defun org-roam-latte--find-longest-match (start limit)
  "Look ahead from START to find the longest phrase existing in the DB.
Returns a cons cell (start . end) of the match, or nil.

LIMIT determines where the search should stop."

  (let ((result nil)
        (current-end start))
    ;; Optimistically check up to 6 words ahead for multi-word titles.
    (dotimes (_ 6)
      (save-excursion
        (goto-char current-end)
        (when (re-search-forward "\\b\\w+\\b" limit t)
          (setq current-end (point))
          (let* ((phrase (downcase
                         (buffer-substring-no-properties start current-end)))
                (keyword (org-roam-latte--phrase-checker phrase)))
            (when (and keyword
                       (org-roam-latte--match-highlightable keyword))
              (setq result (cons start current-end)))))))
    result))

(defun org-roam-latte--make-overlays (buffer &optional start end)
  "Create overlays for keywords in BUFFER between START and END.

This function uses an inverted search strategy: it scans the buffer text for
word boundaries (O(M)) and verifies them against the keyword hash table (O(1)).
This avoids the performance penalty of iterating through the entire database."
  (with-current-buffer buffer
    (ignore-errors
      (setq start (or start (point-min))
            end (min (or end (point-max)) (point-max)))
      (save-excursion
        (save-restriction
          (with-silent-modifications
            (narrow-to-region start end)
            ;; Cleanup invalid overlays first
            (org-roam-latte--delete-overlays start end)

            (goto-char start)
            ;; Search for words/phrases in the buffer
            (while (re-search-forward "\\b\\w+\\b" end t)
              (let* ((word-beg (save-excursion (backward-word) (point)))
                     ;; Check if this word starts a multi-word keyword
                     (full-match (org-roam-latte--find-longest-match
                                  word-beg end)))
                (when full-match
                  (let* ((match-beg (car full-match))
                         (match-end (cdr full-match))
                         (keyword-text (downcase (buffer-substring-no-properties
                                                  (car full-match)
                                                  (cdr full-match))))
                         (node-name (downcase
                                     (org-roam-latte--phrase-checker keyword-text))))

                    ;; If we found a multi-word match, move point there to avoid
                    ;; double-counting
                    (goto-char match-end)

                    (unless (or (org-roam-latte--overlay-exists
                                 keyword-text match-beg match-end)
                                (and (derived-mode-p 'org-mode)
                                     ;; Avoid highlighting on excluded elements
                                     (memq (org-element-type (org-element-context))
                                           org-roam-latte-exclude-org-elements))
                                ;; Handle prog-mode comments logic
                                (and org-roam-latte-highlight-prog-comments
                                     (derived-mode-p 'prog-mode)
                                     (not (nth 4 (syntax-ppss)))))

                      (let ((o (make-overlay match-beg match-end)))
                        (overlay-put o 'face 'org-roam-latte-keyword-face)
                        (overlay-put o 'evaporate t)
                        (overlay-put o 'keymap org-roam-latte-keyword-map)
                        (overlay-put o 'mouse-face 'highlight)
                        (overlay-put o 'org-roam-latte-keyword node-name)
                        ;; Longer phrases get higher priority
                        (overlay-put o 'priority
                                     (+ org-roam-latte-base-priority
                                        (length keyword-text)))))))))))))))

(defun org-roam-latte--pluralize (phrase)
  "Return the plural form of PHRASE using standard grammar rules.

Used to match pluralized text against singular node titles."
  (inflection-pluralize-string phrase))

(defun org-roam-latte--add-keyword (keyword)
  "Add KEYWORD and its plural form to the cache safely."
  (when (and keyword (not (string-blank-p keyword)))
    (unless (or (org-roam-latte--phrase-checker keyword)
                (member keyword org-roam-latte-exclude-words))
      (puthash (downcase keyword) keyword org-roam-latte--keywords)
      (puthash (downcase (org-roam-latte--pluralize keyword))
               keyword
               org-roam-latte--keywords))))

(defun org-roam-latte--node-link-insert (keyword &optional beg end)
  "Convert KEYWORD into an Org-Roam node link using atomic change groups.

If BEG and END are provided, replace the text in that range.
If not, but the region is active, replace the active region.
Otherwise, insert at point."
  (let* ((use-region (and (not beg) (region-active-p)))
         (start (if use-region (region-beginning) beg))
         (final (if use-region (region-end) end))
         (replace-p (and start final))
         (original-text (when replace-p
                          (org-link-display-format
                           (buffer-substring-no-properties start final))))
         ;; Remove case sensitivity. Check `org-roam-node-downtitle'
         (org-roam-node-display-template "${lattedowntitle}")
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
      (when use-region
        (deactivate-mark)))))

(defun org-roam-latte--keyword-at-point ()
  "Return the text of the Latte keyword overlay at point."
  (let ((p (overlays-at (point) t))
        (lk nil))
    (catch 'org-roam-latte--found
      (dolist (o p)
        (when-let ((k (overlay-get o 'org-roam-latte-keyword)))
          (throw 'org-roam-latte--found (setq lk k)))))
    (or lk
        (when (use-region-p)
          (buffer-substring-no-properties (region-beginning) (region-end)))
        (word-at-point)
        "")))

(cl-defmethod org-roam-node-lattedowntitle (node)
  "A temporary org-roam display template.

Used with the variable `org-roam-node-display-template' to downcase NODE
title/alias."
  (downcase (org-roam-node-title node)))
;;
;; Hooks and Advisors
;;

(defun org-roam-latte--db-modified (&rest _args)
  "Rebuild the keyword hash table from the Org-roam database.
Populates `org-roam-latte--keywords` with titles and aliases."
  (setq org-roam-latte--keywords (make-hash-table :test 'equal))
  (dolist (node (org-roam-node-list))
    (let ((title (org-roam-node-title node))
          (aliases (org-roam-node-aliases node)))
      (org-roam-latte--add-keyword title)
      (dolist (alias aliases)
        (org-roam-latte--add-keyword alias))))
  (org-roam-latte--highlight-buffers)
  t)

(defun org-roam-latte--after-change-function (beginning _end &optional _old-len)
  "Hook to re-highlight the current line after text modifications.

BEGINNING sets the starting position of line."
  (save-excursion
    (goto-char beginning)
    (org-roam-latte--highlight-buffer (line-beginning-position)
                                      (line-end-position))))

(defun org-roam-latte--scroll-handler (win _start)
  "Handle window scrolling to highlight only the visible region.
This optimization minimizes the work done during rapid scrolling.

WIN The window object in which the scroll event has occurred."
  (let* ((start (window-start win))
         (end (window-end win t))
         (diff (- start org-roam-latte--win-prev-start))
         (full-render (or (not org-roam-latte--prev-win)
                          (not (eq org-roam-latte--prev-win win))
                          (> (abs diff)
                             ;; Less than 1/3 of window size
                             (/ (- org-roam-latte--win-prev-end
                                   org-roam-latte--win-prev-start) 3)))))
    (cond
     ((and (>= diff 1) (not full-render))
      (org-roam-latte--highlight-buffer org-roam-latte--win-prev-end end))
     ((and (< diff 1) (not full-render))
      (org-roam-latte--highlight-buffer start org-roam-latte--win-prev-start))
     (t
      (org-roam-latte--highlight-buffer start end)))

    (setq org-roam-latte--win-prev-start start
          org-roam-latte--win-prev-end end
          org-roam-latte--prev-win win)))

;;
;; Public functions
;;

;;;###autoload
(defun org-roam-latte-complete-at-point (&optional point)
  "Convert the highlighted reference at POINT into a formal Org-roam link."
  (interactive)
  (setq point (or point (point)))
  (save-excursion
    (goto-char point)
    (if-let ((overlay (seq-find (lambda (o)
                                  (overlay-get o 'org-roam-latte-keyword))
                                (overlays-at (point)))))
        (org-roam-latte--node-link-insert
         (overlay-get overlay 'org-roam-latte-keyword)
         (overlay-start overlay)
         (overlay-end overlay))
      (error "No org-roam-latte highlight found at point"))))

;;;###autoload
(defun org-roam-latte-open-at-point ()
  "Visit the Org-roam node corresponding to the highlighted reference at point."
  (interactive)
  (let* ((context (org-element-context))
         (type (org-element-type context))
         ;; Remove case sensitivity. Check `org-roam-node-downtitle'
         (org-roam-node-display-template "${lattedowntitle}"))
    (if (memq type '(link))
        (org-open-at-point)
      (org-roam-node-find nil (org-roam-latte--keyword-at-point)))))

;;;
;;; Minor mode
;;;

;;;###autoload
(define-minor-mode org-roam-latte-mode
  "Minor mode to highlight unlinked Org-roam references in the current buffer.

When enabled:
1. Scans the buffer for text matching titles or aliases in your Org-roam
database.
2. Highlights matching text (ignoring existing links).
3. Provides keybindings set in `org-roam-latte-keyword-map' on highlighted
terms."
  :init-value nil
  :lighter " Latte"
  :keymap nil
  :require 'org-roam-latte
  :group 'org-roam-latte

  (if org-roam-latte-mode
      (progn ;; On
        (unless org-roam-latte--initialized
          ;; Globally called once scope
          ;; Hook into Org Roam DB updates to keep keywords fresh
          (advice-add 'org-roam-db-update-file :after #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-insert-file :after #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-clear-file :after  #'org-roam-latte--db-modified)
          (advice-add 'org-roam-db-sync :after #'org-roam-latte--db-modified)
          (org-roam-latte--db-modified)
          (setq org-roam-latte--initialized t))

        (add-hook 'window-scroll-functions #'org-roam-latte--scroll-handler t t)
        (add-hook 'after-change-functions #'org-roam-latte--after-change-function t t)
        ;; Initial highlighting
        (org-roam-latte--highlight-buffer))

    (progn ;; Off
      (org-roam-latte--delete-overlays nil nil t)
      (remove-hook 'window-scroll-functions #'org-roam-latte--scroll-handler t)
      (remove-hook 'after-change-functions #'org-roam-latte--after-change-function t)))

  org-roam-latte-mode)

(provide 'org-roam-latte)

;;; org-roam-latte.el ends here
