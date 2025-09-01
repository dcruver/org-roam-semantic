# org-roam-semantic

AI-powered semantic search and intelligent assistance for org-roam.

Transform your org-roam knowledge base with semantic similarity search and context-aware AI enhancement. Find conceptually related notes, not just keyword matches, and get AI assistance that understands your existing knowledge.

## Features

### Semantic Vector Search
- **Conceptual similarity** - Find notes by meaning, not just keywords
- **Vector embeddings** stored as org-mode properties  
- **Batch processing** for existing note collections
- **Real-time search** with ranked similarity scores
- **Automatic embedding generation** for new notes

### Context-Aware AI Assistant  
- **Intelligent enhancement** using related notes as context
- **Multiple workflows** - explain concepts, improve writing, suggest connections
- **Knowledge analysis** - find gaps, identify improvements
- **Smart integration** with your existing knowledge graph

## Quick Start

### Prerequisites

1. **Ollama** installed and running:
```bash
# Install Ollama, then pull required models:
ollama pull nomic-embed-text    # For semantic embeddings
ollama pull llama3.1:8b         # For AI text generation
```

2. **org-roam** configured and working

### Installation

#### Via straight.el (Recommended)

```elisp
;; Install the package
(straight-use-package 
  '(org-roam-semantic :host github :repo "dcruver/org-roam-semantic"))

;; Load both modules
(require 'org-roam-vector-search)
(require 'org-roam-ai-assistant)

;; Configure (must use customize-set-variable, not setq)
(customize-set-variable 'org-roam-semantic-ollama-url "http://localhost:11434")

;; Key bindings are automatically configured:
;; C-c v s/i/r - Vector search functions
;; C-c a f/e/p/s/g - AI assistant functions
```

#### Via use-package (If you have straight.el integration configured)

```elisp
(use-package org-roam-vector-search
  :straight (:host github :repo "dcruver/org-roam-semantic")
  :after org-roam
  :config
  (setq org-roam-semantic-ollama-url "http://localhost:11434"))

(use-package org-roam-ai-assistant
  :straight (:host github :repo "dcruver/org-roam-semantic")
  :after (org-roam org-roam-vector-search)
  :config
  (setq org-roam-ai-default-model "llama3.1:8b"
        org-roam-ai-context-limit 3))
```

#### Manual Installation

```elisp
;; 1. Clone the repository:
;; git clone https://github.com/dcruver/org-roam-semantic.git

;; 2. Add to your configuration:
(add-to-list 'load-path "/path/to/org-roam-semantic")
(require 'org-roam-vector-search)
(require 'org-roam-ai-assistant)

;; 3. Configure as shown above - key bindings are automatic
```

### First Steps

1. **Generate embeddings** for your existing notes:
   ```
   M-x org-roam-semantic-generate-all-embeddings
   ```

2. **Try semantic search**:
   - `C-c v s` - Search for notes by concept
   - `C-c v i` - Insert similar notes to current note

3. **Enhance with AI**:
   - `C-c a f` - Flesh out current note with AI assistance
   - `C-c a e` - Explain concept at point

## How It Works

### The Semantic Layer
org-roam-semantic uses vector embeddings to understand the **meaning** of your notes, not just their words. When you search for "docker networking," it finds notes about container communication, bridge networks, and port mapping - even if they don't contain those exact terms.

### The Intelligence Layer  
The AI assistant uses semantic search to find related notes, then includes that context when making suggestions. Instead of generic AI responses, you get enhancements that:
- Reference your existing knowledge
- Build on concepts you've already documented  
- Suggest connections to related notes
- Maintain consistency with your knowledge base

### Example Workflow
```
1. You have notes about "Machine Learning," "Python," and "Data Analysis"
2. You create a new note: "Neural Network Implementation"  
3. Semantic search finds your related notes automatically
4. AI assistant uses that context to suggest:
   - Specific Python libraries you've documented
   - Data preprocessing steps from your analysis notes
   - ML concepts and techniques from your learning notes
```

## Key Bindings

### Semantic Search (`C-c v`)
- `C-c v s` - Search notes by concept
- `C-c v i` - Insert similar notes to current note  
- `C-c v r` - Insert notes related to a specific concept

### AI Assistant (`C-c a`)
- `C-c a f` - **F**lesh out note with AI enhancement
- `C-c a e` - **E**xplain concept at point
- `C-c a p` - Improve current **p**aragraph
- `C-c a s` - **S**uggest note connections
- `C-c a g` - Find knowledge **g**aps
- `C-c a ?` - Check system status

## Documentation

- **[Vector Search Documentation](org-roam-vector-search.md)** - Detailed guide to semantic search features
- **[AI Assistant Documentation](org-roam-ai-assistant.md)** - Complete AI enhancement workflows  

## Configuration

### Basic Setup
```elisp
;; Ollama server (default: http://localhost:11434)
(setq org-roam-semantic-ollama-url "http://your-server:11434")

;; Models (defaults shown)
(setq org-roam-semantic-embedding-model "nomic-embed-text")
(setq org-roam-semantic-generation-model "llama3.1:8b")
(setq org-roam-ai-default-model "llama3.1:8b")

;; AI context (default: 3 similar notes)
(setq org-roam-ai-context-limit 5)  ; Use more context
```

### Advanced Options
```elisp
;; Embedding dimensions (must match your model)
(setq org-roam-semantic-embedding-dimensions 768)  ; nomic-embed-text default

;; AI response length
(setq org-roam-ai-max-response-tokens 3000)  ; Longer responses
```

## Repository Structure

```
org-roam-semantic/
├── README.md                    # This file
├── org-roam-vector-search.el    # Semantic search engine
├── org-roam-vector-search.md    # Vector search documentation  
├── org-roam-ai-assistant.el     # AI enhancement system
└── org-roam-ai-assistant.md     # AI assistant documentation
```

## Troubleshooting

### "No similar notes found"
- Check embedding coverage: `M-x org-roam-semantic-status`  
- Generate missing embeddings: `M-x org-roam-semantic-generate-all-embeddings`
- Verify Ollama is running: `curl http://localhost:11434/api/tags`

### "Failed to get AI response"  
- Check model availability: `ollama list`
- Test connectivity: `curl -X POST http://localhost:11434/api/generate -d '{"model":"llama3.1:8b","prompt":"test"}'`
- Verify configuration: `M-x org-roam-ai-system-status`

### Setup Issues
- Run comprehensive check: `M-x org-roam-ai-setup-check`
- Verify dependencies are loaded: `M-x org-roam-semantic-version`

## Contributing

Issues and pull requests welcome! This project aims to make org-roam more intelligent and connected through semantic understanding.

## License

GPL-3.0-or-later

---

**Tip**: Start with semantic search (`C-c v s`) to explore your knowledge base in new ways, then use AI assistance (`C-c a f`) to enhance notes with context from related content.