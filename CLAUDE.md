# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

org-roam-semantic is an Emacs Lisp package that adds AI-powered semantic search and intelligent assistance to org-roam. The project consists of two main modules that work together to enhance org-roam with vector embeddings and AI capabilities.

## Architecture

### Core Components

1. **org-roam-vector-search.el** - The semantic search engine
   - Vector embedding generation and storage using Ollama API
   - Cosine similarity search for finding related notes
   - Embedding storage as org-mode properties in note files
   - Automatic embedding generation hooks

2. **org-roam-ai-assistant.el** - The AI enhancement system
   - Context-aware AI assistance using related notes
   - Multiple AI workflows (explain, enhance, suggest connections)
   - Integration with vector search for intelligent context retrieval

### Key Dependencies

- **org-roam**: Core knowledge management system
- **Ollama**: Local AI server for embeddings and text generation
- **json, url, org, ox-md**: Standard Emacs libraries

### Data Flow

1. Notes are processed to generate vector embeddings via Ollama API
2. Embeddings stored as `:EMBEDDING:` properties in org files
3. Similarity search uses cosine distance between vectors
4. AI assistant retrieves similar notes as context for enhanced responses

## Development Commands

### Testing and Development Workflow

```bash
# No build system - this is pure Emacs Lisp
# Testing is done interactively in Emacs

# Load and test the package:
emacs -Q -l org-roam-vector-search.el -l org-roam-ai-assistant.el

# Or use your existing Emacs configuration and test interactively
```

### Essential Testing Commands

```elisp
;; Load modules for testing
(require 'org-roam-vector-search)
(require 'org-roam-ai-assistant)

;; System diagnostics (run these first)
M-x org-roam-semantic-status        ; Check embedding coverage
M-x org-roam-ai-system-status       ; Check AI connectivity
M-x org-roam-ai-setup-check         ; Comprehensive system check

;; Version verification
M-x org-roam-semantic-version       ; Current version info
```

### Core Functions to Test

- `org-roam-semantic-generate-all-embeddings` - Process all notes for embeddings
- `org-roam-semantic-search` - Interactive concept-based search
- `org-roam-ai-flesh-out-note` - AI-enhanced note expansion

### Chunking Functions to Test

- `org-roam-semantic-generate-chunks-for-file` - Generate embeddings for sections in current file
- `org-roam-semantic-generate-all-chunks` - Generate embeddings for all sections in all files
- `org-roam-semantic-search-chunks` - Search sections by concept

### Manual Testing Workflow

1. **Prerequisites Check**: Ensure Ollama is running with required models
2. **Load Package**: Use `require` statements above
3. **Generate Embeddings**: Run `org-roam-semantic-generate-all-embeddings`
4. **Test Search**: Use `C-c v s` to search by concept
5. **Test AI Enhancement**: Use `C-c a f` to enhance a note

### Chunking Testing Workflow

1. **Enable Chunking**: `(setq org-roam-semantic-enable-chunking t)`
2. **Generate Chunks**: Use `C-c v g` in a file or `C-c v G` for all files
3. **Test Chunk Search**: Use `C-c v c` to search within sections
4. **Check Status**: Use `M-x org-roam-semantic-status` to see chunk coverage

## Configuration Variables

### Core Settings
- `org-roam-semantic-ollama-url` - Ollama server URL (default: http://localhost:11434)
- `org-roam-semantic-embedding-model` - Model for embeddings (default: nomic-embed-text)
- `org-roam-semantic-generation-model` - Model for text generation (default: llama3.1:8b)
- `org-roam-semantic-embedding-dimensions` - Vector dimensions (default: 768)

### Chunking Settings
- `org-roam-semantic-enable-chunking` - Enable section-level chunking (default: nil)
- `org-roam-semantic-min-chunk-size` - Minimum words for chunk embedding (default: 100)
- `org-roam-semantic-max-chunk-size` - Maximum words per chunk (default: 1000)

### Similarity Settings
- `org-roam-semantic-similarity-cutoff` - Threshold for related notes (default: 0.55)
  - Higher values (closer to 1.0) return only very similar notes
  - Lower values return more loosely related notes
  - Used by `org-roam-semantic-insert-similar` to filter results

### AI Assistant Settings
- `org-roam-ai-default-model` - Default AI model
- `org-roam-ai-context-limit` - Number of similar notes to use as context (default: 3)
- `org-roam-ai-max-response-tokens` - Maximum response length (default: 2000)

## Key Binding Structure

The package uses a hierarchical key binding system:

- `C-c v` prefix - Vector search functions
  - `C-c v s` - Search notes by concept
  - `C-c v i` - Insert similar notes to current note (uses similarity cutoff)
  - `C-c v r` - Insert notes related to a concept
  - `C-c v c` - Search chunks by concept (requires chunking enabled)
  - `C-c v g` - Generate chunks for current file
  - `C-c v G` - Generate chunks for all files

- `C-c a` prefix - AI assistant functions
  - `C-c a f` - Flesh out note with AI
  - `C-c a e` - Explain concept at point
  - `C-c a p` - Improve current paragraph
  - `C-c a s` - Suggest note connections
  - `C-c a g` - Find knowledge gaps
  - `C-c a ?` - Check system status

## Important Implementation Details

### Embedding Storage Architecture
- **Storage Format**: Embeddings stored as JSON arrays in `:EMBEDDING:` org property
- **File Integration**: No external database - embeddings live within org files
- **Multi-level Support**: File-level and section-level embeddings using same property
- **Chunking Strategy**: Sections get unique IDs and individual embeddings when enabled
- **Content Processing**: Safe org-to-markdown conversion for API calls
- **Normalization**: Automatic whitespace and formatting cleanup

### API Integration Patterns
- **HTTP Requests**: Uses built-in `url` library for Ollama API calls
- **JSON Handling**: Native Emacs `json` library for request/response processing
- **Model Management**: Configurable embedding and generation models
- **Timeout Handling**: Graceful degradation on network failures

### Error Handling Strategy
- **Network Resilience**: API failures handled without breaking workflows
- **Diagnostics**: Comprehensive status functions for troubleshooting
- **Fallback Behavior**: Operations continue even when AI features are unavailable
- **User Feedback**: Clear error messages and system status reporting

### Performance Architecture
- **Batch Processing**: Embeddings generated in bulk for efficiency
- **Memory-Based Search**: Cosine similarity computed in-memory for speed
- **Context Management**: Configurable limits prevent AI context overflow
- **Lazy Loading**: Embeddings generated on-demand when missing

## Development Prerequisites

### Required External Dependencies

1. **Ollama Server**: Must be running and accessible
   ```bash
   # Install and start Ollama
   curl -fsSL https://ollama.ai/install.sh | sh
   ollama serve  # Start the server

   # Pull required models
   ollama pull nomic-embed-text    # For embeddings (768 dimensions)
   ollama pull llama3.1:8b         # For text generation
   ```

2. **org-roam**: Package must be installed and configured
   - Database initialized with `org-roam-db-sync`
   - Basic org-roam functionality working

### Development Environment Setup

```elisp
;; Add to your Emacs configuration for development
(add-to-list 'load-path "/path/to/org-roam-semantic")

;; Load dependencies
(require 'org-roam)    ; Core dependency
(require 'json)        ; JSON handling for API calls
(require 'url)         ; HTTP requests to Ollama
(require 'ox-md)       ; Org to markdown conversion
(require 'cl-lib)      ; Common Lisp extensions (for chunking compatibility)

;; Load the packages
(require 'org-roam-vector-search)
(require 'org-roam-ai-assistant)
```

## Troubleshooting

### Common Issues

1. **"Symbols function definition is void: fourth" error**
   - **Cause**: Missing cl-lib dependency
   - **Fix**: Ensure `(require 'cl-lib)` is loaded before the package

2. **"position X is not at a heading" error**
   - **Cause**: Position tracking issues during chunk parsing
   - **Fix**: The code includes fallback logic to find nearest headings
   - **Debug**: Check file for malformed org headings or unusual formatting

3. **Multi-heading processing issues**
   - **Symptoms**: Only first heading gets embeddings
   - **Debug**: Enable debug messages in `org-roam-semantic-generate-chunks-for-file`
   - **Common causes**: Headings below word count threshold, malformed org structure

### Debugging Chunking Issues

The chunking system includes detailed debug logging. When experiencing issues:

1. **Enable debugging**: Debug messages are built into chunk generation functions
2. **Check output**: Look for "Debug: Chunk 'X' at position Y with Z words" messages
3. **Verify structure**: Ensure org headings are properly formatted with asterisks and spaces
4. **Word count**: Check if sections meet minimum word count requirements

## Version Information

Current version: 1.3.0
- Check version: `M-x org-roam-semantic-version`
- Both modules share version numbering