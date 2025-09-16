# org-roam-vector-search

Semantic similarity search for org-roam notes using vector embeddings.

This package adds vector embedding support to org-roam, enabling you to find conceptually similar notes rather than just keyword matches. It uses [Ollama](https://ollama.ai/) to generate embeddings and stores them as org-mode properties.

## Features

- **Semantic similarity search** - Find notes by concepts, not just keywords
- **Automatic embedding generation** - Embeddings stored as org-mode properties  
- **Batch processing** - Generate embeddings for all notes at once
- **Integration-friendly** - Export functions for external tools
- **Status monitoring** - Check embedding coverage across your knowledge base

## Prerequisites

1. [Ollama](https://ollama.ai/) installed and running
2. An embedding model installed in Ollama (recommended: `nomic-embed-text`)

```bash
# Install Ollama, then:
ollama pull nomic-embed-text
```

## Installation

### Via Straight.el (Recommended)

```elisp
;; Install the package
(straight-use-package 
  '(org-roam-vector-search :host github :repo "dcruver/org-roam-semantic"))

;; Load the vector search module
(require 'org-roam-vector-search)

;; Configure
(setq org-roam-semantic-ollama-url "http://localhost:11434")

;; Set up keybindings (these are set automatically)
(global-set-key (kbd "C-c v s") 'org-roam-semantic-search)
(global-set-key (kbd "C-c v i") 'org-roam-semantic-insert-similar)
(global-set-key (kbd "C-c v r") 'org-roam-semantic-insert-related)
```

### Via use-package (If you have straight.el integration)

```elisp
(use-package org-roam-vector-search
  :straight (:host github :repo "dcruver/org-roam-semantic")
  :after org-roam
  :config
  (setq org-roam-semantic-ollama-url "http://localhost:11434")
  :bind (("C-c v s" . org-roam-semantic-search)
         ("C-c v i" . org-roam-semantic-insert-similar)
         ("C-c v r" . org-roam-semantic-insert-related)))
```

### Manual Installation

1. Clone this repository:
```bash
git clone https://github.com/dcruver/org-roam-semantic.git
```

2. Add to your load path and require:
```elisp
(add-to-list 'load-path "/path/to/org-roam-semantic")
(require 'org-roam-vector-search)
```

## Configuration

```elisp
;; Ollama server URL (default: http://localhost:11434)
(setq org-roam-semantic-ollama-url "http://localhost:11434")

;; Embedding model (default: nomic-embed-text)
(setq org-roam-semantic-embedding-model "nomic-embed-text")

;; Text generation model for AI features (default: llama3.1:8b)
(setq org-roam-semantic-generation-model "llama3.1:8b")

;; Embedding dimensions - must match your model (default: 768)
(setq org-roam-semantic-embedding-dimensions 768)

;; Similarity cutoff for related notes (default: 0.55)
;; Higher values = more similar notes only
;; Lower values = more loosely related notes
(setq org-roam-semantic-similarity-cutoff 0.55)
```

## Usage

### Interactive Functions

#### `org-roam-semantic-generate-embedding`
Generate and store an embedding for the current note. The embedding is saved as an `:EMBEDDING:` property in the note's properties drawer.

#### `org-roam-semantic-generate-all-embeddings`
Generate embeddings for all org-roam notes that don't already have them. Shows progress and skips notes that already have embeddings.

#### `org-roam-semantic-search`
**Keybinding:** `C-c v s`

Search for notes similar to a concept and display results in a clickable buffer. Enter a search term and get a ranked list of similar notes with similarity scores.

#### `org-roam-semantic-insert-similar`
**Keybinding:** `C-c v i`

Find notes similar to the current note and insert org-roam links at point. Uses the configurable similarity cutoff (`org-roam-semantic-similarity-cutoff`) to filter results - only notes above the threshold are inserted. You can also provide a custom cutoff with a prefix argument.

**Changed in v1.2.0:** Now inserts ALL notes above the similarity threshold instead of just 5 notes.

#### `org-roam-semantic-insert-related`
**Keybinding:** `C-c v r`

Search for notes related to a specific concept and insert org-roam links at point. Prompts for a search term, then inserts links to related notes.

#### `org-roam-semantic-status`
Display the current status of vector embeddings in your knowledge base, including coverage percentage and notes without embeddings.

#### `org-roam-semantic-debug-embedding`
Debug the embedding for a specific file. Shows embedding dimensions and first few values for troubleshooting.

## Key Bindings

The following key bindings are set up automatically:

- `C-c v s` - Search for similar notes by concept
- `C-c v i` - Insert similar notes to current note (uses similarity cutoff)
- `C-c v r` - Insert notes related to a concept

## Workflow

1. **Initial setup**: Run `org-roam-semantic-generate-all-embeddings` to create embeddings for existing notes
2. **Daily use**: New notes get embeddings automatically when saved
3. **Discovery**: Use `C-c v s` to explore conceptual connections in your knowledge base
4. **Linking**: Use `C-c v i` to quickly add related note links

## Automatic Embedding Generation

Embeddings are automatically generated and updated when you save org-roam files, thanks to the `before-save-hook` integration. This means:

- New notes get embeddings when first saved
- Modified notes get updated embeddings
- No manual intervention needed for day-to-day use

## Troubleshooting

### "No similar notes found"
- Check that embeddings exist: `M-x org-roam-semantic-status`
- Verify Ollama is running: `curl http://localhost:11434/api/tags`
- Generate missing embeddings: `M-x org-roam-semantic-generate-all-embeddings`

### "Error calling Ollama"
- Confirm Ollama server URL in configuration
- Check that the embedding model is installed: `ollama list`
- Pull the required model: `ollama pull nomic-embed-text`

### Poor similarity results
- Try different search terms (more specific or more general)
- Consider using a different embedding model
- Ensure your notes have sufficient content for meaningful embeddings
- Adjust the similarity cutoff: lower values include more loosely related notes

### Too many/few related notes
- Increase `org-roam-semantic-similarity-cutoff` to get fewer, more similar notes
- Decrease the cutoff to get more loosely related notes
- Use a prefix argument with `C-c v i` to temporarily override the cutoff

## Integration

This package provides functions used by other org-roam extensions and can be integrated with external tools via:

- `org-roam-semantic-get-similar-data` - Programmatic access to similarity search results
- Org-to-markdown export functions for external processing
- JSON-compatible data structures for API integration

## Data Functions for External Use

### `org-roam-semantic-get-similar-data`
Returns similarity data as a list of `(file similarity-score)` pairs, suitable for programmatic use by other functions or external tools.

**Parameters:**
- `query-text` - Text to search for
- `limit` - Maximum number of results (optional)
- `chunk-level` - Search chunks instead of files (optional)
- `cutoff` - Similarity threshold filter (optional)

## License

GPL-3.0-or-later