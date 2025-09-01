# org-roam-ai-assistant

AI-powered enhancement and analysis for org-roam notes with vector context.

This package provides intelligent AI assistance for your org-roam knowledge base. It uses vector similarity search to find related notes and includes that context when making AI suggestions, resulting in more relevant and connected enhancements.

## Features

- **Context-aware AI enhancement** - Uses similar notes to inform AI responses
- **Multiple AI workflows** - Note expansion, concept explanation, paragraph improvement
- **Knowledge analysis** - Find gaps, suggest connections, identify improvements
- **Smart integration** - Automatically updates embeddings after AI modifications

## Prerequisites

1. **org-roam-vector-search** package installed and configured:

```elisp
(straight-use-package 
  '(org-roam-vector-search :host github :repo "dcruver/org-roam-semantic"))
(require 'org-roam-vector-search)
(setq org-roam-semantic-ollama-url "http://localhost:11434")
```

2. **Ollama** with both embedding and text generation models:

```bash
# Install required models
ollama pull nomic-embed-text    # For embeddings (via vector-search)
ollama pull llama3.1:8b         # For text generation
```

## Installation

### Via Straight.el (Recommended)

```elisp
;; First install org-roam-vector-search (required dependency)
(straight-use-package 
  '(org-roam-vector-search :host github :repo "dcruver/org-roam-semantic"))

;; Load both modules
(require 'org-roam-vector-search)
(require 'org-roam-ai-assistant)

;; Configure
(setq org-roam-semantic-ollama-url "http://localhost:11434")

;; Key bindings are automatically set up under C-c a
```

### Via use-package (If you have straight.el integration)

```elisp
(use-package org-roam-ai-assistant
  :straight (:host github :repo "dcruver/org-roam-semantic")
  :after (org-roam org-roam-vector-search)
  :config
  (setq org-roam-ai-default-model "llama3.1:8b"
        org-roam-ai-context-limit 3))
```

### Manual Installation

1. Clone this repository:
```bash
git clone https://github.com/dcruver/org-roam-semantic.git
```

2. Add to your load path and require:
```elisp
(add-to-list 'load-path "/path/to/org-roam-semantic")
(require 'org-roam-ai-assistant)
```

## Configuration

```elisp
;; Ollama server URL (inherited from vector-search)
(setq org-roam-semantic-ollama-url "http://localhost:11434")

;; AI-specific configuration
(setq org-roam-ai-default-model "llama3.1:8b")           ; Text generation model
(setq org-roam-ai-context-limit 3)                       ; Number of similar notes for context
(setq org-roam-ai-max-response-tokens 2000)              ; Maximum response length
```

## Usage

All functions work within org-roam notes and use vector similarity to find relevant context from your knowledge base. The AI assistant uses a prefix key system with all commands under `C-c a`.

### Core Enhancement Functions

#### `org-roam-ai-enhance-with-context`
**Keybinding:** `C-c a f`

The primary AI enhancement function. Expands and enriches the current note using context from similar notes in your knowledge base. With a prefix argument (`C-u C-c a f`), prompts for a specific enhancement request.

**Example uses:**
- Add technical details and examples
- Expand on concepts with practical applications  
- Include related information from connected notes

#### `org-roam-ai-explain-concept`  
**Keybinding:** `C-c a e`

Get a quick explanation of the concept at point or selected region. Provides concise, contextual explanations based on your domain and existing knowledge.

#### `org-roam-ai-improve-paragraph`
**Keybinding:** `C-c a p`

Improve the current paragraph for clarity, technical accuracy, and practical value. Maintains the original meaning while enhancing readability and usefulness.

### Analysis and Discovery Functions

#### `org-roam-ai-suggest-connections`
**Keybinding:** `C-c a s`  

Analyze the current note and suggest which related notes would be most valuable to link to. Explains why each connection would be beneficial for understanding or building upon the topic.

#### `org-roam-ai-find-knowledge-gaps`
**Keybinding:** `C-c a g`

Identify missing topics, techniques, or concepts that would complement your current knowledge. Suggests specific areas to explore based on your existing notes and the current topic.

### System Functions

#### `org-roam-ai-system-status`
**Keybinding:** `C-c a ?`

Check the status of the AI system including vector search coverage and AI connectivity. Useful for troubleshooting and monitoring system health.

#### `org-roam-ai-setup-check`

Interactive setup verification that checks dependencies, Ollama connectivity, and embedding coverage.

## Key Bindings

The AI assistant uses a prefix key system. All commands are under `C-c a`:

- `C-c a f` - **F**lesh out note with AI enhancement
- `C-c a e` - **E**xplain concept at point
- `C-c a p` - Improve current **p**aragraph
- `C-c a s` - **S**uggest note connections
- `C-c a g` - Find knowledge **g**aps  
- `C-c a ?` - Check system status

The prefix key design makes it easy to remember commands and discover functionality.

## Workflow

### Initial Enhancement Workflow
1. Open an org-roam note with basic content
2. Use `C-c a f` to flesh out the note with AI assistance
3. The AI will use similar notes from your knowledge base as context
4. Review and edit the AI-generated content as needed

### Concept Exploration Workflow  
1. Highlight an unfamiliar term or concept
2. Use `C-c a e` for a quick explanation in context
3. Use `C-c a g` to discover related topics to explore
4. Use `C-c a s` to find existing notes worth linking

### Content Improvement Workflow
1. Select a paragraph that needs improvement
2. Use `C-c a p` to enhance clarity and technical accuracy
3. The AI considers your domain and existing knowledge base
4. Embeddings are automatically updated after changes

## How Context Works

The AI assistant leverages your existing knowledge base by:

1. **Finding similar notes** using vector similarity search
2. **Including relevant context** from 3-5 most similar notes  
3. **Building comprehensive prompts** that reference your existing knowledge
4. **Providing connected insights** rather than generic responses

This means AI responses become more valuable and relevant as your knowledge base grows.

## Advanced Configuration

### Custom Models

```elisp
;; Use different models for different purposes
(setq org-roam-ai-default-model "qwen2.5:7b")       ; Alternative text model
(setq org-roam-semantic-embedding-model "all-minilm") ; Faster embeddings
```

### Context Tuning

```elisp
;; Adjust context for different use cases
(setq org-roam-ai-context-limit 5)          ; More context for complex topics
(setq org-roam-ai-max-response-tokens 3000) ; Longer responses
```

## Troubleshooting

### "Failed to get AI response"
- Check Ollama connectivity: `curl http://localhost:11434/api/tags`
- Verify text generation model is installed: `ollama list`
- Test with: `M-x org-roam-ai-system-status`

### Poor AI suggestions
- Ensure you have embeddings: `M-x org-roam-ai-setup-check`
- Try more specific requests with prefix argument: `C-u C-c a f`
- Add more content to your notes for better context

### "This function only works in org-roam files"
- Ensure you're in a file within your `org-roam-directory`
- Check that the file is recognized: `M-x org-roam-file-p`

### Setup Issues
Run `M-x org-roam-ai-setup-check` for an interactive diagnostic that checks:
- Dependencies and configuration
- Ollama connectivity and models
- Embedding coverage
- Common setup problems

## Dependencies

- **org-roam** - Core knowledge management system
- **org-roam-vector-search** - Required for context-aware AI responses
- **Ollama** - Local AI server with text generation model

## Integration

The AI assistant integrates seamlessly with:

- **Vector search** for contextual awareness
- **org-roam** for note management and linking
- **Automatic embedding updates** after AI modifications
- **Multiple AI models** via Ollama

## License

GPL-3.0-or-later