# ☕ Org-Roam Latte

**Org-Roam Latte** is a minor mode that automatically highlights org-roam **unlinked references** in your buffer.

It scans your current text and identifies words or phrases that match existing Org-roam nodes (titles and aliases). It brings the "Unlinked Mentions" feature found in tools like Obsidian or Roam Research directly into Emacs, helping you discover connections you might have missed.

## Features

* **Fast:** Uses an optimized inverted search strategy. It scans the visible buffer text first and checks against a hash map ($O(1)$), rather than iterating through your entire database. It stays snappy even with thousands of nodes.
* **Smart Linking:** Highlighted words are clickable.
    * **Click / RET**: Visit the node.
    * **M-RET**: Instantly convert the text into a formal Org-roam ID link.
* **Pluralization:** Automatically handles pluralization (e.g., a node named "Algorithm" will highlight "algorithms" in your text).
* **Org-roam Alias Support:** Recognizes and highlights node aliases.
* **Context Aware:**
    * Ignores existing Org links.
    * Intelligent handling of code blocks (only highlights inside comments).
    * Adapts colors automatically for Light and Dark themes.

