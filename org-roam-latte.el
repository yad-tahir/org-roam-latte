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
;; Package-Version: 0.9
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
;; - Fast: Scans only the visible section of a buffer, and checks against a hash
;; table. It stays snappy even with thousands of nodes.
;; - Instant: Automatically highlights keywords as you type, instantly surfacing
;; potential links.
;; - Non-org Modes: Allows you to have virtual, navigable links in any mode: text,
;; code, shells, man pages, etc.
;; - Smart Linking: Highlighted words are navigatable.
;;     Click / RET: Visit the node.
;;     M-RET: Convert the text into a formal org-roam ID link.
;; - Pluralization: Automatically handles pluralization (e.g., a node titled
;; "Algorithm" will highlight "algorithms" in your text).
;; - Context Aware: Ignores existing Org links and node self-referencing. It
;; intelligent handles code blocks (only highlights inside comments).
;; - Theme Aware: Adapts colors automatically for light and dark themes.

;;; Code:

(require 'cl-lib)
(require 'inflections)
(require 'org-roam)
(require 'seq)

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

(defcustom org-roam-latte-exclude-scope 'node
  "Determine the scope for excluding keywords from highlighting.

This controls where the highlighter should stop if the keyword matches the
current context.

- nil           : Do not exclude anything (Highlight everything).
- 'node         : Do not highlight keywords defined by the current node.
- 'parents      : Do not highlight keywords defined by the current node or
                  any of its parent headings (ancestors).

WARNING: The 'parents option requires traversing the document structure upwards
for every potential match. This has naturally slow performance and is NOT
recommended for use in large Org files."
  :group 'org-roam-latte
  :type '(choice
          (const :tag "Highlight everything (No exclusion)"
                 nil)
          (const :tag "Exclude keywords defined by the current node"
                 node)
          (const :tag "Exclude keywords defined by current node or its parents"
                 parents)))

(defcustom org-roam-latte-respect-node-tags nil
  "When non-nil, strictly require shared tags for highlighting.

This variable refines the behavior of `org-roam-latte-exclude-scope` by adding
a mandatory tag check. If enabled, a keyword will NOT be highlighted unless it
shares at least one tag with the current node (or its ancestors - depending on
the scope).

This effectively upgrades the `org-roam-latte-exclude-scope' as follows:
- nil      : Highlight only if a shared tag exists
- 'node    : Exclude current node AND require tag match
- 'parents : Exclude parents AND require tag match with either current mode or
             one of its parents

If the keyword's definition has no tags, it is treated as a wildcard and
accepted."
  :group 'org-roam-latte
  :type 'boolean)

(defcustom org-roam-latte-exclude-org-elements '(link node-property keyword)
  "List of Org element types where highlight should not be created.

Common types include `link', `node-property', `keyword', `code', and `verbatim'.
See `org-element-all-elements' for a comprehensive list."
  :type '(repeat symbol)
  :group 'org-roam-latte)

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

This structure is updated via `org-roam-latte--db-modified` hooks. A key are
keywords derived from the downcased title, aliases, and their pluralized forms.
A value is a downcase string of title/alias attached to that keyword.")

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

(defun org-roam-latte--overlay-to-keyword (overlay)
  "Convert OVERLAY to keyword.

Return nil if the overlay cannot be converted."
  (when overlay
    (let ((key (overlay-get overlay 'org-roam-latte-key)))
      (org-roam-latte--phrase-to-keyword key))))

(defun org-roam-latte--delete-overlays (&optional start end)
  "Delete Latte overlays in region defined by START and END.

If START is nil, then `(point-min)' will be used.
If END is nil, then `(point-max)' will be used."
  (setq start (or start (point-min))
        end (or end (point-max)))
  (dolist (overlay (overlays-in start end))
    (let* ((key (overlay-get overlay 'org-roam-latte-key)))
      (when key
        (delete-overlay overlay)))))

(defun org-roam-latte--overlay-exists (keyword start end)
  "Return t if an overlay for KEYWORD already exists between START and END."
  (catch 'org-roam-latte--overlay-found
    (dolist (overlay (overlays-in start end))
      (if (and (equal keyword (org-roam-latte--overlay-to-keyword overlay))
               (equal (overlay-start overlay) start)
               (equal (overlay-end overlay) end))
          (throw 'org-roam-latte--overlay-found t)
        ;; Else:
        ;; 1- Region mismatch; e.g. an old overlay that does not
        ;; accommodate the extra length. Clean it and continue searching.
        ;; 2- keyword mismatch.
        (delete-overlay overlay))
      nil)))

(defun org-roam-latte--phrase-to-keyword (phrase)
  "Return keyword if PHRASE is a known keyword in org-roam.

Otherwise, nil."
  (when (and phrase
             (stringp phrase))
    (setq pharse (downcase (substring-no-properties phrase)))
    (gethash phrase org-roam-latte--keywords)))

(defun org-roam-latte--highlight-buffer (start end &optional buffer)
  "Highlight keywords in BUFFER between START and END positions.

If BUFFER is nil, use current buffer. If START/END are nil, buffer window start
and end will be used, respectively."
  (setq buffer (or buffer (current-buffer)))
  (with-current-buffer buffer
    (when (bound-and-true-p org-roam-latte-mode)
      (when-let ((b-win (get-buffer-window nil 'visible)))
        (let ((b-start (or start (window-start b-win)))
              (b-end (or end (window-end b-win t))))
          (org-roam-latte--make-overlays buffer b-start b-end))))))

(defun org-roam-latte--highlight-buffers ()
  "Trigger highlighting for all buffers where the mode is active."
  (dolist (buffer (buffer-list))
    (org-roam-latte--highlight-buffer nil nil buffer)))

(defun org-roam-latte--has-common-tag-p (keyword-nodes node)
  "Return non-nil if NODE shares a tag with KEYWORD-NODES or has no tags."
  (let ((node-tags (org-roam-node-tags node)))
    (or (null node-tags)
        (cl-loop for node in keyword-nodes
           thereis (cl-intersection (org-roam-node-tags node)
                                    node-tags
                                    :test #'string=)))))

(defun org-roam-latte--check-ancestors (keyword-nodes scope)
  "Check ancestor lineage against KEYWORD-NODES using SCOPE rules."
  (save-restriction
    (widen)
    (let ((ancestors (org-element-lineage (org-element-at-point)))
          (check-tags-p (eq scope 'parents-tags)))
      (not (cl-loop for parent in ancestors
                    do (goto-char (org-element-property :begin parent))
                    for parent-node = (org-roam-node-at-point)
                    ;; Exclude if parent is the source of the keyword
                    if (member parent-node keyword-nodes)
                    return t
                    ;; Exclude if strict tags are on and tags don't match
                    if (and check-tags-p
                            (not (org-roam-latte--has-common-tag-p
                                  keyword-nodes parent-node)))
                    return t)))))

(defun org-roam-latte--match-highlightable (keyword)
  "Return non-nil if KEYWORD is suitable for highlighting in current context.

This function determines if a KEYWORD should be highlighted by
- In Prog mode and a code comment.
- Or in Org mode, not in `org-roam-latte-exclude-org-elements', and satisfies
the rules set by `org-roam-latte-exclude-scope' and
`org-roam-latte-respect-node-tags'."
  (let ((in-org (derived-mode-p 'org-mode))
        (in-prog (derived-mode-p 'prog-mode)))
    (cond
     ;; Prog mode but not in comments
     ((and in-prog
           org-roam-latte-highlight-prog-comments
           (not (nth 4 (syntax-ppss))))
      nil)
     ;; Org mode but in exclude org elements
     ((and in-org
           (memq (org-element-type (org-element-context))
                 org-roam-latte-exclude-org-elements))
      nil)
     ;; Org mode
     (in-org
      (let* ((keyword-nodes (get-text-property 0 'nodes keyword))
             ;; Determine the effective scope rule based on strict-tags setting
             (scope (if org-roam-latte-respect-node-tags
                        (pcase org-roam-latte-exclude-scope
                          ('nil     'tags)         ; Strict tags only
                          ('node    'node-tags)    ; Node scope + Strict tags
                          ('parents 'parents-tags) ; Parents scope + Strict tags
                          (_ org-roam-latte-exclude-scope))
                      org-roam-latte-exclude-scope)))
        (when keyword-nodes
          (if (null scope)
              t
            (let ((current-node (org-roam-node-at-point)))
              (pcase scope
                ('node
                 (not (member current-node keyword-nodes)))
                ('tags
                 (org-roam-latte--has-common-tag-p keyword-nodes current-node))
                ('node-tags
                 (and (not (member current-node keyword-nodes))
                      (org-roam-latte--has-common-tag-p
                       keyword-nodes current-node)))
                ((or 'parents 'parents-tags)
                 (org-roam-latte--check-ancestors keyword-nodes scope))))))))
     ;; Everything else allowed
     (t t))))

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
                 (keyword (org-roam-latte--phrase-to-keyword phrase)))
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
                         (phrase-text (downcase (buffer-substring-no-properties
                                                 (car full-match)
                                                 (cdr full-match))))
                         (keyword (org-roam-latte--phrase-to-keyword phrase-text)))

                    ;; If we found a multi-word match, move point to end to
                    ;; avoid double-counting
                    (goto-char match-end)

                    (unless (org-roam-latte--overlay-exists
                             keyword match-beg match-end)

                      (let ((o (make-overlay match-beg match-end)))
                        (overlay-put o 'face 'org-roam-latte-keyword-face)
                        (overlay-put o 'evaporate t)
                        (overlay-put o 'keymap org-roam-latte-keyword-map)
                        (overlay-put o 'mouse-face 'highlight)
                        ;; To avoid cache inconsistency, we attach the key
                        ;; instead of the keyword. This will also reduce the
                        ;; memory load on Emacs. Other functions should use
                        ;; `org-roam-latte--overlay-to-keyword' to get the
                        ;; keyword
                        (overlay-put o 'org-roam-latte-key
                                     (substring-no-properties keyword))
                        ;; Longer phrases get higher priority
                        (overlay-put o 'priority
                                     (+ org-roam-latte-base-priority
                                        (length phrase-text)))))))))))))))

(defun org-roam-latte--pluralize (phrase)
  "Return the plural form of PHRASE using standard grammar rules.

Used to match pluralized text against singular node titles."
  (inflection-pluralize-string phrase))

(defun org-roam-latte--add-keyword (phrase node)
  "Add PHRASE and its plural form as keywords to the `org-roam-latte--keywords'.

Stores NODE in a list as a text property 'nodes on the KEYWORD string."
  (when (and phrase (not (string-blank-p phrase)))
    (unless (member phrase org-roam-latte-exclude-words)
      (let* ((key (downcase phrase))
             (value (or (gethash key org-roam-latte--keywords)
                        key))
             (nodes (get-text-property 0 'nodes value)))
        ;; Compare by ID
        (unless (cl-member (org-roam-node-id node) nodes
                           :key #'org-roam-node-id
                           :test #'string=)
          (push node nodes))
        (put-text-property 0 (length value) 'nodes nodes value)

        ;; Store the string object (carrying the property) in the hash map
        (puthash key value org-roam-latte--keywords)
        (puthash (downcase (org-roam-latte--pluralize key))
                 value
                 org-roam-latte--keywords)))))

(defun org-roam-latte--node-link-insert (keyword &optional beg end)
  "Convert KEYWORD into an Org-Roam node link.

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
         ;; We gather all user input BEFORE starting the atomic change group.
         (keyword-nodes (get-text-property 0 'nodes keyword))
         (node (if (eq (length keyword-nodes) 1)
                   (car keyword-nodes)
                 (org-roam-node-read nil 'org-roam-latte--completation-filter)))
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
  "Return highlighted keyword at POINT. Otherwise, nil."
  (let ((overlays (overlays-at (point) t))) ;; t for needed sorting
    (catch 'org-roam-latte--found
      (dolist (o overlays)
        (when-let ((k (org-roam-latte--overlay-to-keyword o)))
          (throw 'org-roam-latte--found k)))
      (throw 'org-roam-latte--found nil))))

;;
;; Hooks and Advisors
;;

(defun org-roam-latte--node-clear (&rest args)
  "Called when a node has been deleted in org-roam.

ARGS arguments passed from the hook."
  (org-roam-latte--db-modified args))

(defun org-roam-latte--node-modified (fn &rest args)
  "Called when a node has been modified in org-roam.

FN: The 'around' function object.
ARGS: the rest of arguments passed from the hook."

  ;; Org-roam updates a file node by first deleting it then
  ;; re-inserting it. We disable clear hook temporary during update
  ;; so that, buffers are now re-highlighted twice.
  (advice-remove 'org-roam-db-clear-file #'org-roam-latte--node-clear)
  (unwind-protect
      ;; Try
      (progn
        (with-demoted-errors "%S"
          (apply fn args))
        (with-demoted-errors "Org-roam-latte Error: %S"
          (org-roam-latte--db-modified args)))
    ;; Finally
    (advice-add 'org-roam-db-clear-file :after #'org-roam-latte--node-clear)))

(defun org-roam-latte--db-modified (&rest _args)
  "Rebuild the keyword hash table from the Org-roam database.
Populates `org-roam-latte--keywords` with titles and aliases."
  (setq org-roam-latte--keywords (make-hash-table :test 'equal))
  (dolist (node (org-roam-node-list))
    (let ((title (org-roam-node-title node))
          (aliases (org-roam-node-aliases node)))
      (org-roam-latte--add-keyword title node)
      (dolist (alias aliases)
        (org-roam-latte--add-keyword alias node))))
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

(defun org-roam-latte--completation-filter (node)
  (let* ((keyword (org-roam-latte--keyword-at-point))
         (nodes (get-text-property 0 'nodes keyword)))
    (when nodes
      (member node nodes))))
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
    (if-let ((overlay
              (seq-find (lambda (o)
                          (org-roam-latte--overlay-to-keyword o))
                        (overlays-at (point) t))))
        (org-roam-latte--node-link-insert
         (org-roam-latte--overlay-to-keyword overlay)
         (overlay-start overlay)
         (overlay-end overlay))
      (error "No org-roam-latte highlight found at point"))))

;;;###autoload
(defun org-roam-latte-open-at-point ()
  "Visit the Org-roam node corresponding to the highlighted reference at point."
  (interactive)
  (let* ((keyword (org-roam-latte--keyword-at-point))
         (nodes (get-text-property 0 'nodes keyword)))
    (if (eq (length nodes) 1)
        (org-roam-node-visit (car nodes))
      (progn
        (org-roam-node-find nil nil
                            'org-roam-latte--completation-filter)))))

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

          ;; Hook into Org Roam DB updates to refresh hash tables and redraw
          ;; visible buffers.
          (advice-add 'org-roam-db-update-file :around #'org-roam-latte--node-modified)
          ;; clear hook can be temporary removed; check `org-roam-latte--node-modified'
          (advice-add 'org-roam-db-clear-file :after #'org-roam-latte--node-clear)

          ;; Populate the hash table
          (org-roam-latte--db-modified)
          (setq org-roam-latte--initialized t))

        (add-hook 'window-scroll-functions #'org-roam-latte--scroll-handler t t)
        (add-hook 'after-change-functions #'org-roam-latte--after-change-function t t)
        ;; Initial highlighting
        (org-roam-latte--highlight-buffer nil nil))

    (progn ;; Off
      (org-roam-latte--delete-overlays)
      (remove-hook 'window-scroll-functions #'org-roam-latte--scroll-handler t)
      (remove-hook 'after-change-functions #'org-roam-latte--after-change-function t)))

  org-roam-latte-mode)

(provide 'org-roam-latte)

;;; org-roam-latte.el ends here
