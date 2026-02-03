# ☕ Org-roam Latte

[![Emacs](https://img.shields.io/badge/editor-emacs-7F5AB6.svg?logo=gnu-emacs&logoColor=white)](https://www.gnu.org/software/emacs/) [![MELPA](https://melpa.org/packages/org-roam-latte-badge.svg)](https://melpa.org/#/org-roam-latte)

**Org-roam Latte** is a minor mode that automatically highlights **unlinked org-roam references** in your buffer. It scans your current text and identifies words or phrases that match existing org-roam nodes (titles and aliases). It brings the "Unlinked Mentions" feature found in tools like Obsidian or Roam Research directly into Emacs, helping you discover connections you might have missed.

## Features

* **Fast:** Scans only the **visible** section of a buffer, and checks against a hash table. It stays snappy even with thousands of nodes.
* **Instant:** Automatically highlights keywords as you type, instantly surfacing potential links.
* **Smart Linking:** Highlighted words are navigatable.
    * **Click / RET**: Visit the node.
    * **M-RET**: Convert the text into a formal org-roam ID link.
* **Pluralization:** Automatically handles pluralization (e.g., a node titled "Algorithm" will highlight "algorithms" in your text).
* **Context Aware:** Ignores existing Org links and node self-referencing. It intelligent handles code blocks (only highlights inside comments).
* **Theme Aware:** Adapts colors automatically for _light_ and _dark_ themes.

## Great Performance

Many "auto-linker" packages suffer from performance issues because they search the buffer for every single node in your database ($N \times M$ complexity).

**Org-Roam Latte is different.**
1.  On startup (or DB sync), it builds a fast Hash Table of your titles and aliases.
2.  When you scroll, it scans the *visible* text for word boundaries.
3.  It checks those candidate words against the Hash Table (fast lookup).

This ensures that scrolling remains is smooth, regardless of whether you have 100 notes or 10,000. This also ensures that there is no performance hit even when you open _extermely large files_.

## Installation

### Manual
Clone the repository and add it to your load path. Then, add hooks to enable it automatically for Org files and Programming buffers:

```elisp
(add-to-list 'load-path "/path/to/org-roam-latte")
(require 'org-roam-latte)

;; Enable in all Text buffers (including org-mode)
(add-hook 'text-mode-hook #'org-roam-latte-mode)
;; Enable in Programming buffers (highlights comments only)
(add-hook 'prog-mode-hook #'org-roam-latte-mode)
```

### Use-package with [Melpa](https://github.com/melpa/melpa)
```elisp
(use-package org-roam-latte
  :hook ((text-mode . org-roam-latte-mode)
	 (prog-mode . org-roam-latte-mode)))
```

## Interactions
When you navigate to a highlighted word (declared in `org-roam-latte-keyword-map`):
* **<Mouse-1> (Left Click)**: Open the node.
* **RET**: Open the node.
* **M-RET** (Alt+Enter): Convert the highlighted text into a link.

## Customization

### Customization Variables
| Variable | Default | Description |
| :--- | :--- | :--- |
| `org-roam-latte-exclude-words` | `'()` | A list of strings to exclude from highlighting. |
| `org-roam-latte-exclude-org-elements` | `'(link node-property keyword)` | A list of org element types to exclude from highlighting. |
| `org-roam-latte-exclude-scope` | `'node` | Filters out keywords linking to the current context: nil (highlight all), 'node (exclude title/aliases defined by current node), or 'parents (exclude current node and its ancestors) - See 'Self-Referencing Exclusion' section. |
| `org-roam-latte-highlight-prog-comments` | `t` | If `t`, Latte highlights keywords inside comments in programming modes. |
| `org-roam-latte-base-priority` | `0` | The base priority for highlights, allowing you to control their stacking order relative to overlays from other modes. |

### Self-Referencing Exclusion
By default, `org-roam-latte` prevents a node from highlighting its own title or aliases within its own body text. You can control how strict this *self-referencing* filter is using the `org-roam-latte-exclude-scope` variable. To explain this further, consider the following example:

Imagine your Org file looks like this.

```text
* Solar System                 <-- Org-roam Note 1 /(Parent Node)
  :PROPERTIES: ... :END:
   
  ** Mars                      <-- Org-roam Note 2
     :PROPERTIES: ... :END:
      
     "I am studying the Solar System while standing on Mars."
                        ^                              ^
                        |                              |
                   Matches Parent                 Matches Current
```

Adjusting `org-roam-latte-exclude-scope` gives:

| Setting | Behavior | Resulting Text Highlighting |
| :--- | :--- | :--- |
| **`nil`** | **Highlight Everything.** <br>All self references are allowed. | "I am studying the `[[Solar System]]` while standing on `[[Mars]]`." |
| **`'node`** | **Exclude Current** (Default). <br>Ignores the title and aliases defined by Mars. | "I am studying the `[[Solar System]]` while standing on Mars." |
| **`'parents`** | **Exclude Ancestors.** <br>Ignores the title/aliases defined by both Mars and its parents (only one in our case, Solar System). | "I am studying the Solar System while standing on Mars." |

**Note:** The `'parents` option requires *traversing* the document structure upwards for every match. While effective, it may be slow in **very large** Org files.

### Theming
The highlighting face is `org-roam-latte-keyword-face`. It defaults to **Purple** (Light Mode) or **Cyan** (Dark Mode) with a wavy underline. You can customize this in your config, e.g.:

```elisp
(custom-set-faces
 '(org-roam-latte-keyword-face ((t (:inherit warning :underline (:style wave))))))
```
## Fancy Screenshots!

![Package In Action Screenshot 1](resources/images/screenshot1.png)
![Package In Action Screenshot 2](resources/images/screenshot2.png)
