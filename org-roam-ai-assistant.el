;;; org-roam-ai-assistant.el --- AI-powered enhancement and analysis for org-roam -*- lexical-binding: t; -*-

;;; Commentary:
;; AI-powered enhancement and analysis for org-roam notes with vector context.
;;
;; This package provides intelligent AI assistance for your org-roam knowledge base.
;; It uses vector similarity search to find related notes and includes that context
;; when making AI suggestions, resulting in more relevant and connected enhancements.

;;; Code:

(require 'org-roam)
(require 'org-roam-vector-search)
(require 'json)

;;; Configuration

(defgroup org-roam-ai-assistant nil
  "AI-powered assistance for org-roam notes."
  :group 'org-roam
  :prefix "org-roam-ai-")

(defcustom org-roam-ai-default-model "llama3.1:8b"
  "Default model to use for AI text generation.
This should be a model available in your Ollama installation.
Popular options:
- llama3.1:8b (recommended default)
- qwen2.5:7b (good alternative)
- gemma2:9b (Google's model)"
  :type 'string
  :group 'org-roam-ai-assistant)

(defcustom org-roam-ai-context-limit 3
  "Number of similar notes to include as context for AI requests.
Higher values provide more context but may overwhelm the AI with information.
Recommended range: 2-5."
  :type 'integer
  :group 'org-roam-ai-assistant)

(defcustom org-roam-ai-max-response-tokens 2000
  "Maximum number of tokens for AI responses.
Longer responses provide more detail but take more time and resources.
Adjust based on your model's capabilities and needs."
  :type 'integer
  :group 'org-roam-ai-assistant)

;;; Internal Helper Functions

(defun org-roam-ai--get-context-for-note (file &optional limit)
  "Get contextual information from similar notes for FILE.
Returns a formatted string with content from the most similar notes."
  (let* ((limit (or limit org-roam-ai-context-limit))
         (content (org-roam-semantic--get-content file))
         (similarities (when content
                        (org-roam-semantic-get-similar-data content (1+ limit)))))

    ;; Filter out the current file and take requested number
    (setq similarities (seq-remove (lambda (result)
                                    (string= (car result) file))
                                  similarities))
    (setq similarities (seq-take similarities limit))

    (if similarities
        (let ((context-parts '()))
          (dolist (result similarities)
            (let* ((similar-file (car result))
                   (similarity (cadr result))
                   (title (org-roam-semantic--get-title similar-file))
                   (content (org-roam-semantic--get-content similar-file)))
              (when content
                (push (format "=== Related Note: %s (similarity: %.3f) ===\n%s\n"
                             title similarity content)
                      context-parts))))
          (if context-parts
              (concat "\n--- CONTEXT FROM YOUR KNOWLEDGE BASE ---\n"
                     (string-join (reverse context-parts) "\n")
                     "\n--- END CONTEXT ---\n")
            ""))
      "")))

(defun org-roam-ai--build-enhanced-prompt (base-prompt current-content context)
  "Build an enhanced prompt that includes context from the knowledge base."
  (format "%s

Current note content:
%s

%s

Please provide a response that builds upon and connects with the existing knowledge shown in the context above. Be specific and practical, referencing relevant concepts from the related notes when helpful."
          base-prompt
          (or current-content "")
          (or context "")))

(defun org-roam-ai--call-ai-with-context (prompt &optional system-prompt)
  "Call the AI with the given prompt and optional system prompt.
Uses org-roam-ai-generate-text from the vector search package."
  (let ((response (org-roam-ai-generate-text prompt system-prompt)))
    (when response
      ;; Clean up the response
      (string-trim response))))

;;; Core AI Assistant Functions

;;;###autoload
(defun org-roam-ai-enhance-with-context (&optional custom-request)
  "Enhance the current org-roam note using AI with context from similar notes.
With prefix argument, prompts for a custom enhancement request."
  (interactive "P")
  (unless (and (derived-mode-p 'org-mode) (org-roam-file-p))
    (user-error "This function only works in org-roam files"))

  (let* ((file (buffer-file-name))
         (current-content (org-roam-semantic--get-content file))
         (context (org-roam-ai--get-context-for-note file))
         (enhancement-request
          (if custom-request
              (read-string "Enhancement request: "
                          "Expand and enrich this note with technical details and practical examples")
            "Flesh out and expand this note with detailed information, technical details, examples, and practical applications. Make it comprehensive and valuable."))
         (system-prompt
          "You are an AI assistant helping to enhance technical knowledge notes. Provide detailed, accurate, and practical information. Build upon existing knowledge and make meaningful connections between concepts.")
         (prompt (org-roam-ai--build-enhanced-prompt enhancement-request current-content context)))

    (message "Generating AI enhancement with context from %d related notes..."
             (min org-roam-ai-context-limit (length (org-roam-list-files))))

    (let ((response (org-roam-ai--call-ai-with-context prompt system-prompt)))
      (if response
          (progn
            (goto-char (point-max))
            (unless (bolp) (insert "\n"))
            (insert "\n** AI Enhancement\n\n")
            (insert response)
            (insert "\n")
            (message "AI enhancement complete. Review and edit as needed."))
        (message "Failed to get AI response. Check your Ollama configuration.")))))

;;;###autoload
(defun org-roam-ai-explain-concept ()
  "Get a quick AI explanation of the concept at point or selected region."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (org-roam-file-p))
    (user-error "This function only works in org-roam files"))

  (let* ((concept (if (use-region-p)
                     (buffer-substring-no-properties (region-beginning) (region-end))
                   (thing-at-point 'word t)))
         (file (buffer-file-name))
         (context (org-roam-ai--get-context-for-note file 2)) ; Use less context for explanations
         (system-prompt
          "You are an AI assistant providing concise, accurate explanations of technical concepts. Focus on clarity and practical understanding.")
         (prompt (format "Explain the concept '%s' in the context of the domain shown in the related notes. Be concise but informative.%s"
                        concept context)))

    (if concept
        (let ((response (org-roam-ai--call-ai-with-context prompt system-prompt)))
          (if response
              (message "Concept '%s': %s" concept response)
            (message "Failed to get explanation. Check your Ollama configuration.")))
      (message "No concept found at point. Select text or place cursor on a word."))))

;;;###autoload
(defun org-roam-ai-improve-paragraph ()
  "Improve the current paragraph using AI for clarity and technical accuracy."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (org-roam-file-p))
    (user-error "This function only works in org-roam files"))

  (save-excursion
    (let* ((paragraph-bounds (bounds-of-thing-at-point 'paragraph))
           (start (car paragraph-bounds))
           (end (cdr paragraph-bounds))
           (paragraph-text (when paragraph-bounds
                            (buffer-substring-no-properties start end)))
           (file (buffer-file-name))
           (context (org-roam-ai--get-context-for-note file 2))
           (system-prompt
            "You are an AI assistant helping improve technical writing. Enhance clarity, accuracy, and practical value while maintaining the original meaning and structure.")
           (prompt (format "Improve this paragraph for clarity, technical accuracy, and practical value:\n\n%s\n\n%s\n\nReturn only the improved paragraph, maintaining the same general structure and meaning."
                          paragraph-text context)))

      (if paragraph-text
          (let ((response (org-roam-ai--call-ai-with-context prompt system-prompt)))
            (if response
                (progn
                  (delete-region start end)
                  (insert response)
                  (message "Paragraph improved with AI assistance."))
              (message "Failed to get AI response. Check your Ollama configuration.")))
        (message "No paragraph found at point.")))))

;;;###autoload
(defun org-roam-ai-suggest-connections ()
  "Analyze current note and suggest valuable connections to other notes."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (org-roam-file-p))
    (user-error "This function only works in org-roam files"))

  (let* ((file (buffer-file-name))
         (current-content (org-roam-semantic--get-content file))
         (context (org-roam-ai--get-context-for-note file 5)) ; More context for connections
         (system-prompt
          "You are an AI assistant helping to identify valuable connections between knowledge notes. Focus on meaningful relationships that would enhance understanding.")
         (prompt (format "Based on the current note and the related notes shown in the context, suggest 3-5 specific connections that would be most valuable. For each suggestion, explain why the connection would be beneficial for understanding or building upon this topic.\n\nCurrent note:\n%s\n\n%s"
                        current-content context)))

    (let ((response (org-roam-ai--call-ai-with-context prompt system-prompt)))
      (if response
          (with-current-buffer (get-buffer-create "*AI Connection Suggestions*")
            (erase-buffer)
            (org-mode)
            (insert (format "* Connection Suggestions for: %s\n\n"
                           (org-roam-semantic--get-title file)))
            (insert response)
            (insert "\n\n** How to Use\n")
            (insert "- Review each suggested connection\n")
            (insert "- Add relevant org-roam links: [[id:node-id][Title]]\n")
            (insert "- Use C-c v s to search for notes by concept\n")
            (goto-char (point-min))
            (display-buffer (current-buffer))
            (message "Connection suggestions generated. Check the *AI Connection Suggestions* buffer."))
        (message "Failed to get AI response. Check your Ollama configuration.")))))

;;;###autoload
(defun org-roam-ai-find-knowledge-gaps ()
  "Identify missing topics or concepts that would complement current knowledge."
  (interactive)
  (unless (and (derived-mode-p 'org-mode) (org-roam-file-p))
    (user-error "This function only works in org-roam files"))

  (let* ((file (buffer-file-name))
         (current-content (org-roam-semantic--get-content file))
         (context (org-roam-ai--get-context-for-note file 4))
         (system-prompt
          "You are an AI assistant helping to identify knowledge gaps and learning opportunities. Focus on practical, actionable suggestions.")
         (prompt (format "Based on the current note and related knowledge shown in the context, identify 3-5 missing topics, techniques, or concepts that would significantly complement and enhance this knowledge area. For each gap, explain why it would be valuable to learn and how it connects to existing knowledge.\n\nCurrent note:\n%s\n\n%s"
                        current-content context)))

    (let ((response (org-roam-ai--call-ai-with-context prompt system-prompt)))
      (if response
          (with-current-buffer (get-buffer-create "*AI Knowledge Gaps*")
            (erase-buffer)
            (org-mode)
            (insert (format "* Knowledge Gaps for: %s\n\n"
                           (org-roam-semantic--get-title file)))
            (insert response)
            (insert "\n\n** Next Steps\n")
            (insert "- Choose 1-2 high-priority gaps to explore\n")
            (insert "- Create new notes for topics you want to learn\n")
            (insert "- Use these as research and learning objectives\n")
            (goto-char (point-min))
            (display-buffer (current-buffer))
            (message "Knowledge gap analysis complete. Check the *AI Knowledge Gaps* buffer."))
        (message "Failed to get AI response. Check your Ollama configuration.")))))

;;; System Status and Maintenance Functions

;;;###autoload
(defun org-roam-ai-system-status ()
  "Check the status of the AI assistant system including dependencies."
  (interactive)
  (let* ((vector-status (org-roam-semantic-status))
         (ollama-url org-roam-semantic-ollama-url)
         (ai-model org-roam-ai-default-model)
         (embedding-model org-roam-semantic-embedding-model))

    (with-current-buffer (get-buffer-create "*AI System Status*")
      (erase-buffer)
      (insert "* AI Assistant System Status\n\n")

      ;; Configuration
      (insert "** Configuration\n")
      (insert (format "- Ollama URL: %s\n" ollama-url))
      (insert (format "- AI Model: %s\n" ai-model))
      (insert (format "- Embedding Model: %s\n" embedding-model))
      (insert (format "- Context Limit: %d notes\n" org-roam-ai-context-limit))
      (insert (format "- Max Response Tokens: %d\n\n" org-roam-ai-max-response-tokens))

      ;; Dependencies
      (insert "** Dependencies\n")
      (insert (format "- org-roam-vector-search: %s\n"
                     (if (featurep 'org-roam-vector-search) "✓ Loaded" "✗ Not loaded")))
      (insert (format "- Vector search status: Check output below\n\n"))

      ;; Test AI connectivity
      (insert "** AI Connectivity Test\n")
      (let ((test-response (org-roam-ai--call-ai-with-context
                           "Please respond with 'AI system operational' to confirm connectivity.")))
        (if test-response
            (insert (format "✓ AI Response: %s\n" test-response))
          (insert "✗ AI connection failed - check Ollama server and model\n")))

      (insert "\n** Vector Search Status\n")
      (insert "See message area for embedding coverage details.\n")

      (goto-char (point-min))
      (display-buffer (current-buffer)))

    ;; Also show vector search status
    (org-roam-semantic-status)))

;;;###autoload
(defun org-roam-ai-setup-check ()
  "Interactive setup verification and troubleshooting guide."
  (interactive)
  (let ((issues '())
        (warnings '()))

    ;; Check org-roam
    (unless (featurep 'org-roam)
      (push "org-roam not loaded" issues))

    ;; Check vector search dependency
    (unless (featurep 'org-roam-vector-search)
      (push "org-roam-vector-search not loaded - required dependency" issues))

    ;; Check if we're in org-roam directory
    (unless org-roam-directory
      (push "org-roam-directory not configured" issues))

    ;; Check Ollama configuration
    (unless org-roam-semantic-ollama-url
      (push "Ollama URL not configured" issues))

    ;; Test AI connectivity
    (let ((ai-test (org-roam-ai--call-ai-with-context "test" "Respond with 'OK'")))
      (unless ai-test
        (push "Cannot connect to AI model - check Ollama server" issues)))

    ;; Check embedding coverage
    (let* ((all-files (org-roam-list-files))
           (files-with-embeddings (seq-count #'org-roam-semantic--has-embedding-p all-files))
           (coverage (if (> (length all-files) 0)
                        (/ (* 100.0 files-with-embeddings) (length all-files))
                      0)))
      (when (< coverage 50)
        (push (format "Low embedding coverage (%.1f%%) - run org-roam-semantic-generate-all-embeddings" coverage)
              warnings)))

    ;; Display results
    (with-current-buffer (get-buffer-create "*AI Setup Check*")
      (erase-buffer)
      (insert "* AI Assistant Setup Check\n\n")

      (if issues
          (progn
            (insert "** Issues Found\n")
            (dolist (issue issues)
              (insert (format "✗ %s\n" issue)))
            (insert "\n"))
        (insert "** Status: ✓ No critical issues found\n\n"))

      (when warnings
        (insert "** Warnings\n")
        (dolist (warning warnings)
          (insert (format "⚠ %s\n" warning)))
        (insert "\n"))

      (insert "** Setup Instructions\n")
      (insert "1. Install Ollama: https://ollama.ai\n")
      (insert "2. Pull required models:\n")
      (insert (format "   ollama pull %s\n" org-roam-semantic-embedding-model))
      (insert (format "   ollama pull %s\n" org-roam-ai-default-model))
      (insert "3. Configure org-roam-semantic-ollama-url\n")
      (insert "4. Generate embeddings: M-x org-roam-semantic-generate-all-embeddings\n")

      (goto-char (point-min))
      (display-buffer (current-buffer)))

    (if issues
        (message "Setup check complete - %d issues found. See *AI Setup Check* buffer." (length issues))
      (message "Setup check complete - system ready!"))))

;;; Key Bindings

(defvar org-roam-ai-assistant-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "f") 'org-roam-ai-enhance-with-context)
    (define-key map (kbd "e") 'org-roam-ai-explain-concept)
    (define-key map (kbd "p") 'org-roam-ai-improve-paragraph)
    (define-key map (kbd "s") 'org-roam-ai-suggest-connections)
    (define-key map (kbd "g") 'org-roam-ai-find-knowledge-gaps)
    (define-key map (kbd "?") 'org-roam-ai-system-status)
    map)
  "Keymap for org-roam AI assistant commands.")

;; Set up key bindings
(global-set-key (kbd "C-c a") org-roam-ai-assistant-map)

(provide 'org-roam-ai-assistant)

;;; org-roam-ai-assistant.el ends here
