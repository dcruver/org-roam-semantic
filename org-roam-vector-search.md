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
(use-package org-roam-vector-search
  :straight (:host github :repo "dcruver/org-roam-semantic")
  :after org-roam
  :config
  (setq my/ollama-base-url "http://localhost:11434")
  :bind (("C-c v s" . my/search-notes-by-concept)
         ("C-c v i" . my/insert-similar-notes)))
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
(setq my/ollama-base-url "http://localhost:11434")

;; Embedding model (default: nomic-embed-text)
(setq my/embedding-model "nomic-embed-text")

;; Text generation model for AI features (default: llama3.1:8b)
(setq my/generation-model "llama3.1:8b")

;; Embedding dimensions - must match your model (default: 768)
(setq my/embedding-dimensions 768)
```

## Usage

### Interactive Functions

#### `my/generate-embedding-for-note`
Generate and store an embedding for the current note. The embedding is saved as an `:EMBEDDING:` property in the note's properties drawer.

#### `my/generate-embeddings-for-all-notes`
Generate embeddings for all org-roam notes that don't already have them. Shows progress and skips notes that already have embeddings.

#### `my/search-notes-by-concept`
**Keybinding:** `C-c v s`

Search for notes similar to a concept and display results in a clickable buffer. Enter a search term and get a ranked list of similar notes with similarity scores.

#### `my/insert-similar-notes`  
**Keybinding:** `C-c v i`

Find notes similar to the current note and insert org-roam links at point. Useful for building connections between related notes.

#### `my/insert-related-notes`
**Keybinding:** `C-c v r`

Search for notes related to a specific concept and insert org-roam links at point. Prompts for a search term, then inserts links to related notes.

#### `my/vector-search-status`
Display the current status of vector embeddings in your knowledge base, including coverage percentage and notes without embeddings.

#### `my/debug-embedding`
Debug the embedding for a specific file. Shows embedding dimensions and first few values for troubleshooting.

## Key Bindings

The following key bindings are set up automatically:

- `C-c v s` - Search for similar notes by concept
- `C-c v i` - Insert similar notes to current note  
- `C-c v r` - Insert notes related to a concept

## Workflow

1. **Initial setup**: Run `my/generate-embeddings-for-all-notes` to create embeddings for existing notes
2. **Daily use**: New notes get embeddings automatically when saved
3. **Discovery**: Use `C-c v s` to explore conceptual connections in your knowledge base
4. **Linking**: Use `C-c v i` to quickly add related note links

## Troubleshooting

### "No similar notes found"
- Check that embeddings exist: `M-x my/vector-search-status`
- Verify Ollama is running: `curl http://localhost:11434/api/tags`
- Generate missing embeddings: `M-x my/generate-embeddings-for-all-notes`

### "Error calling Ollama"
- Confirm Ollama server URL in configuration
- Check that the embedding model is installed: `ollama list`

### Poor similarity results
- Try different search terms (more specific or more general)
- Consider using a different embedding model
- Ensure your notes have sufficient content for meaningful embeddings

## Integration

This package provides functions used by `org-roam-ai-assistant` and can be integrated with external tools via the org-to-markdown export functions.

## License

GPL-3.0-or-later