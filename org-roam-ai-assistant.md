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
  '(org-roam-semantic :host github :repo "dcruver/org-roam-semantic"))
(require 'org-roam-vector-search)
(setq my/ollama-base-url "http://localhost:11434")
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
  '(org-roam-semantic :host github :repo "dcruver/org-roam-semantic"))

;; Load both modules
(require 'org-roam-vector-search)
(require 'org-roam-ai-assistant)

;; Configure
(setq my/ollama-base-url "http://localhost:11434")

;; Set up AI assistant keybindings
(global-set-key (kbd "C-c a f") 'my/ai-flesh-out-with-vector-context)
(global-set-key (kbd "C-c a e") 'my/ai-quick-explain)
(global-set-key (kbd "C-c a p") 'my/ai-improve-paragraph)
(global-set-key (kbd "C-c a s") 'my/ai-suggest-connections)
(global-set-key (kbd "C-c a g") 'my/ai-find-knowledge-gaps)
```

### Via use-package (If you have straight.el integration)

```elisp
(use-package org-roam-ai-assistant
  :straight (:host github :repo "dcruver/org-roam-semantic")
  :after (org-roam org-roam-vector-search)
  :bind (("C-c a f" . my/ai-flesh-out-with-vector-context)
         ("C-c a e" . my/ai-quick-explain)
         ("C-c a p" . my/ai-improve-paragraph)
         ("C-c a s" . my/ai-suggest-connections)
         ("C-c a g" . my/ai-find-knowledge-gaps)))
```

### Manual Installation

1. Clone this repository:
```bash
git clone https://github.com/dcruver/org-roam-ai-assistant.git
```

2. Add to your load path and require:
```elisp
(add-to-list 'load-path "/path/to/org-roam-ai-assistant")
(require 'org-roam-ai-assistant)
```

## Configuration

The AI assistant uses the same Ollama configuration as org-roam-vector-search:

```elisp
;; Ollama server URL
(setq my/ollama-base-url "http://localhost:11434")

;; Text generation model  
(setq my/ai-default-model "llama3.1:8b")

;; Number of similar notes to include as context (default: 3)
(setq my/ai-context-limit 3)

;; Maximum tokens for AI responses (default: 2000)
(setq my/ai-max-response-tokens 2000)
```

## Usage

All functions work within org-roam notes and use vector similarity to find relevant context from your knowledge base.

### Core Enhancement Functions

#### `my/ai-flesh-out-with-vector-context`
**Keybinding:** `C-c a f`

The primary AI enhancement function. Expands and enriches the current note using context from similar notes in your knowledge base. With a prefix argument (`C-u C-c a f`), prompts for a specific enhancement request.

**Example uses:**
- Add technical details and examples
- Expand on concepts with practical applications  
- Include related information from connected notes

#### `my/ai-quick-explain`  
**Keybinding:** `C-c a e`

Get a quick explanation of the concept at point or selected region. Provides concise, contextual explanations based on your domain and existing knowledge.

#### `my/ai-improve-paragraph`
**Keybinding:** `C-c a p`

Improve the current paragraph for clarity, technical accuracy, and practical value. Maintains the original meaning while enhancing readability and usefulness.

### Analysis and Discovery Functions

#### `my/ai-suggest-connections`
**Keybinding:** `C-c a s`  

Analyze the current note and suggest which related notes would be most valuable to link to. Explains why each connection would be beneficial for understanding or building upon the topic.

#### `my/ai-find-knowledge-gaps`
**Keybinding:** `C-c a g`

Identify missing topics, techniques, or concepts that would complement your current knowledge. Suggests specific areas to explore based on your existing notes and the current topic.

### System Functions

#### `my/ai-system-status`
**Keybinding:** `C-c a ?`

Check the status of the AI system including vector search coverage and AI connectivity. Useful for troubleshooting and monitoring system health.

#### `my/ai-assistant-setup-check`

Interactive setup verification that checks dependencies, Ollama connectivity, and embedding coverage.

## Key Bindings

The following key bindings are configured automatically:

- `C-c a f` - Flesh out note with AI enhancement
- `C-c a e` - Explain concept at point
- `C-c a p` - Improve current paragraph
- `C-c a s` - Suggest note connections
- `C-c a g` - Find knowledge gaps  
- `C-c a ?` - Check system status

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

## Troubleshooting

### "Failed to get AI response"
- Check Ollama connectivity: `curl http://localhost:11434/api/tags`
- Verify text generation model is installed: `ollama list`
- Check server URL in configuration

### Poor AI suggestions
- Ensure you have embeddings: `M-x my/ai-system-status`
- Try more specific requests with prefix argument: `C-u C-c a f`
- Add more content to your notes for better context

### "This function only works in org-roam files"
- Ensure you're in a file within your `org-roam-directory`
- Check that the file is recognized: `M-x org-roam-file-p`

## Dependencies

- org-roam
- org-roam-vector-search
- Ollama server with text generation model

## License

GPL-3.0-or-later