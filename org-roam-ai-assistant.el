;;; org-roam-ai-assistant.el --- AI assistance for org-roam using vector context -*- lexical-binding: t; -*-

;;; Commentary:
;; This package provides AI-powered assistance for org-roam notes, leveraging
;; the vector embedding system from org-roam-vector-search.el to provide
;; context-aware AI enhancements.
;;
;; Requires:
;; - org-roam-vector-search.el (for vector similarity search)
;; - Ollama server with text generation model

;;; Code:

(require 'org-roam-vector-search)
(require 'json)
(require 'url)

;;; Configuration

(defvar my/org-roam-ai-assistant-version "2.0.5"
  "Version of org-roam-ai-assistant package.")

(defvar my/ai-context-limit 3
  "Number of similar notes to include in AI context.")

(defvar my/ai-max-response-tokens 2000
  "Maximum tokens for AI responses.")

(defvar my/ai-default-model "gpt-oss:20b"
  "Default model for AI text generation.")

(defvar my/ai-enhancement-section-title "** AI Enhancement"
  "Title for AI enhancement sections in notes.")

;;; Utility Functions

(defun my/ai-clean-content (content)
  "Clean content for AI processing - remove problematic characters and normalize text."
  (when content
    (let ((cleaned content))
      ;; Remove or replace problematic characters
      (setq cleaned (replace-regexp-in-string "[\r]" "" cleaned)) ; Remove carriage returns
      (setq cleaned (replace-regexp-in-string "[\t]+" " " cleaned)) ; Replace tabs with spaces
      (setq cleaned (replace-regexp-in-string "[ ]+" " " cleaned)) ; Normalize multiple spaces
      (setq cleaned (replace-regexp-in-string "\n\n\n+" "\n\n" cleaned)) ; Normalize multiple newlines
      ;; Remove non-printable characters except newlines and basic punctuation
      (setq cleaned (replace-regexp-in-string "[^\x20-\x7E\n]" "" cleaned))
      (string-trim cleaned))))

(defun my/ai-get-note-context ()
  "Get context from current note including title, content, and metadata."
  (let* ((title (org-roam-get-keyword "TITLE"))
         (domain (org-roam-get-keyword "DOMAIN"))
         (tags (org-get-tags))
         (content (buffer-substring-no-properties (point-min) (point-max)))
         ;; Remove properties drawer and other metadata for cleaner context
         (clean-content (replace-regexp-in-string
                        "^:PROPERTIES:.*?:END:\n" "" content))
         ;; Clean the content for AI processing
         (safe-content (my/ai-clean-content clean-content)))
    `((title . ,(or title "Untitled"))
      (domain . ,(or domain "general"))
      (tags . ,(or tags '()))
      (content . ,safe-content))))

(defun my/ai-get-similar-notes-context (title limit)
  "Get context from similar notes for AI prompting."
  (let* ((similarities (my/get-similar-notes-data title limit))
         (context-notes '()))
    (dolist (result similarities)
      (let* ((file (car result))
             (similarity (cadr result))
             (note-title (file-name-sans-extension
                         (file-name-nondirectory file)))
             (note-content (my/get-note-content file)))
        (when note-content
          (push `(,note-title . ,(substring note-content 0
                                           (min 300 (length note-content))))
                context-notes))))
    context-notes))

(defun my/ai-build-enhancement-prompt (context similar-notes user-request)
  "Build a comprehensive prompt for AI enhancement with vector context."
  (let* ((current-note (alist-get 'content context))
         (title (alist-get 'title context))
         (domain (alist-get 'domain context))
         (similar-context (if similar-notes
                             (mapconcat
                              (lambda (note)
                                (format "- %s: %s"
                                       (car note)
                                       (cdr note)))
                              similar-notes "\n")
                           "No directly related notes found.")))
    (format "You are helping enhance a technical note in an org-roam knowledge base.

CURRENT NOTE:
Title: %s
Domain: %s

Content:
%s

RELATED NOTES FROM KNOWLEDGE BASE:
%s

ENHANCEMENT REQUEST: %s

Please provide a thoughtful enhancement that:
1. Builds on the existing content naturally
2. References specific concepts from the related notes when relevant
3. Maintains the technical depth and style
4. Adds practical value for homelab/infrastructure work
5. Uses clear, concise language
6. Provides actionable insights or examples

IMPORTANT: Use only standard ASCII characters (no fancy dashes, smart quotes, Unicode bullets, etc.). Use regular hyphens (-), straight quotes (\"), and asterisks (*) for bullets.

Focus on practical insights, specific examples, and actionable information. Do not repeat existing content verbatim. If the related notes mention specific tools, configurations, or approaches, incorporate that knowledge appropriately."
            title domain current-note similar-context user-request)))

;;; Core AI API Functions

(defun my/ai-clean-response (response)
  "Clean AI response by converting Unicode characters to ASCII equivalents."
  (when response
    (let ((cleaned response))
      ;; Use character codes to avoid Unicode parsing issues
      ;; En-dash (U+2013), em-dash (U+2014), hyphen-minus variants
      (setq cleaned (replace-regexp-in-string (string 8211) "-" cleaned))  ; en-dash
      (setq cleaned (replace-regexp-in-string (string 8212) "-" cleaned))  ; em-dash
      (setq cleaned (replace-regexp-in-string (string 8208) "-" cleaned))  ; hyphen
      ;; Smart quotes
      (setq cleaned (replace-regexp-in-string (string 8216) "'" cleaned))  ; left single quote
      (setq cleaned (replace-regexp-in-string (string 8217) "'" cleaned))  ; right single quote
      (setq cleaned (replace-regexp-in-string (string 8220) "\"" cleaned)) ; left double quote
      (setq cleaned (replace-regexp-in-string (string 8221) "\"" cleaned)) ; right double quote
      ;; Bullets and symbols
      (setq cleaned (replace-regexp-in-string (string 8226) "*" cleaned))  ; bullet
      (setq cleaned (replace-regexp-in-string (string 9702) "*" cleaned))  ; white bullet
      (setq cleaned (replace-regexp-in-string (string 8230) "..." cleaned)) ; ellipsis
      (setq cleaned (replace-regexp-in-string (string 8594) "->" cleaned)) ; right arrow
      (setq cleaned (replace-regexp-in-string (string 8592) "<-" cleaned)) ; left arrow
      (setq cleaned (replace-regexp-in-string (string 8596) "<->" cleaned)) ; left-right arrow
      (setq cleaned (replace-regexp-in-string (string 215) "x" cleaned))   ; multiplication
      (setq cleaned (replace-regexp-in-string (string 177) "+/-" cleaned)) ; plus-minus
      (setq cleaned (replace-regexp-in-string (string 176) " degrees" cleaned)) ; degree
      ;; Remove any remaining non-ASCII characters except newlines and tabs
      (setq cleaned (replace-regexp-in-string "[^\x20-\x7E\n\t]" "" cleaned))
      ;; Clean up formatting
      (setq cleaned (replace-regexp-in-string "[ \t]+" " " cleaned))
      (setq cleaned (replace-regexp-in-string "\n\n\n+" "\n\n" cleaned))
      (string-trim cleaned))))

(defun my/ai-call-ollama-generate (prompt &optional model)
  "Send prompt to Ollama and return response.
MODEL defaults to my/ai-default-model."
  (let* ((model (or model my/ai-default-model))
         ;; Clean and encode the prompt properly
         (clean-prompt (encode-coding-string
                       (replace-regexp-in-string "[\r\n]+" "\n" prompt)
                       'utf-8))
         (url-request-method "POST")
         (url-request-extra-headers '(("Content-Type" . "application/json; charset=utf-8")))
         (payload (encode-coding-string
                  (json-encode `((model . ,model)
                                (prompt . ,clean-prompt)
                                (stream . :json-false)
                                (options . ((num_predict . ,my/ai-max-response-tokens)))))
                  'utf-8))
         (url-request-data payload)
         (url (format "%s/api/generate" my/ollama-base-url)))
    (condition-case err
        (with-current-buffer (url-retrieve-synchronously url)
          (goto-char (point-min))
          (re-search-forward "^$" nil 'move)
          (let* ((json-response (decode-coding-string
                                (buffer-substring (point) (point-max)) 'utf-8))
                 (data (json-read-from-string json-response))
                 (raw-response (cdr (assoc 'response data)))
                 ;; Clean the response to remove problematic Unicode characters
                 (cleaned-response (my/ai-clean-response raw-response)))
            (kill-buffer (current-buffer))
            cleaned-response))
      (error
       (message "Error calling Ollama generate: %s" err)
       nil))))

;;; Main AI Assistance Functions

(defun my/ai-flesh-out-with-vector-context (&optional user-request)
  "Enhance current note using AI with context from similar notes.
With prefix arg, prompts for specific enhancement request."
  (interactive "P")
  (if (not (org-roam-file-p))
      (message "This function only works in org-roam files")
    (let* ((user-input (if user-request
                          (read-string "Enhancement request: ")
                        "Expand and enhance this note with relevant technical details"))
           (context (my/ai-get-note-context))
           (title (alist-get 'title context))
           ;; Get vector context from similar notes
           (similar-notes (my/ai-get-similar-notes-context title my/ai-context-limit))
           (prompt (my/ai-build-enhancement-prompt context similar-notes user-input)))

      (message "Getting AI enhancement with context from %d similar notes..."
               (length similar-notes))

      (let ((enhancement (my/ai-call-ollama-generate prompt)))
        (if enhancement
            (progn
              (goto-char (point-max))
              (unless (bolp) (insert "\n"))
              (insert (format "\n%s\n" my/ai-enhancement-section-title))
              (insert enhancement)
              (insert "\n")
              (message "Note enhanced with AI assistance"))
          (message "Failed to get AI response"))))))

(defun my/ai-quick-explain ()
  "Get a quick explanation of the concept at point or region."
  (interactive)
  (let* ((text (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (thing-at-point 'symbol t)))
         (context (my/ai-get-note-context))
         (domain (alist-get 'domain context))
         (prompt (format "Briefly explain '%s' in the context of %s and homelab infrastructure. Keep it concise but technically accurate (2-3 sentences max)."
                        text (or domain "technical work"))))

    (if text
        (let ((explanation (my/ai-call-ollama-generate prompt)))
          (if explanation
              (message "Explanation: %s" explanation)
            (message "Failed to get explanation")))
      (message "No text selected or at point"))))

(defun my/ai-improve-paragraph ()
  "Improve the current paragraph with AI assistance."
  (interactive)
  (save-excursion
    (let* ((paragraph-start (progn (backward-paragraph) (point)))
           (paragraph-end (progn (forward-paragraph) (point)))
           (original-text (buffer-substring-no-properties paragraph-start paragraph-end))
           (context (my/ai-get-note-context))
           (domain (alist-get 'domain context))
           (prompt (format "Improve this paragraph for clarity, technical accuracy, and practical value in %s context:

Original:
%s

Please rewrite it to be more clear, concise, and technically useful while maintaining the original meaning. Return only the improved paragraph."
                          (or domain "technical documentation") original-text)))

      (if (string-empty-p (string-trim original-text))
          (message "No paragraph content to improve")
        (let ((improved (my/ai-call-ollama-generate prompt)))
          (if improved
              (progn
                (delete-region paragraph-start paragraph-end)
                (insert improved)
                (message "Paragraph improved with AI assistance"))
            (message "Failed to get AI improvement")))))))

(defun my/ai-suggest-connections ()
  "Suggest connections to other notes based on current content."
  (interactive)
  (if (not (org-roam-file-p))
      (message "This function only works in org-roam files")
    (let* ((context (my/ai-get-note-context))
           (title (alist-get 'title context))
           (content-sample (substring (alist-get 'content context) 0
                                    (min 500 (length (alist-get 'content context)))))
           (similar-notes-data (my/get-similar-notes-data title 10))
           (note-titles (mapcar (lambda (result)
                                 (file-name-sans-extension
                                  (file-name-nondirectory (car result))))
                               similar-notes-data))
           (prompt (format "Based on this note content, suggest which of these related notes would be most valuable to link to and why:

Current note: %s
Content sample: %s

Available related notes:
%s

Suggest the top 3 most relevant connections and briefly explain why each connection would be valuable for understanding or building upon this topic."
                          title content-sample
                          (mapconcat #'identity note-titles "\n- "))))

      (let ((suggestions (my/ai-call-ollama-generate prompt)))
        (if suggestions
            (with-output-to-temp-buffer "*AI Connection Suggestions*"
              (princ (format "Connection suggestions for: %s\n\n%s" title suggestions)))
          (message "Failed to get connection suggestions"))))))

(defun my/ai-find-knowledge-gaps ()
  "Identify potential knowledge gaps related to current note."
  (interactive)
  (if (not (org-roam-file-p))
      (message "This function only works in org-roam files")
    (let* ((context (my/ai-get-note-context))
           (title (alist-get 'title context))
           (domain (alist-get 'domain context))
           (content-sample (substring (alist-get 'content context) 0
                                    (min 800 (length (alist-get 'content context)))))
           (similar-notes-data (my/get-similar-notes-data title 5))
           (related-topics (mapcar (lambda (result)
                                   (file-name-sans-extension
                                    (file-name-nondirectory (car result))))
                                 similar-notes-data))
           (prompt (format "Analyze this note and related topics to identify knowledge gaps that would be valuable to fill:

Current note: %s (Domain: %s)
Content: %s

Related existing notes: %s

What important topics, techniques, or concepts are missing that would complement this knowledge? Focus on practical gaps that would improve homelab/infrastructure understanding. Suggest 3-5 specific areas to explore."
                          title domain content-sample
                          (string-join related-topics ", "))))

      (let ((gaps (my/ai-call-ollama-generate prompt)))
        (if gaps
            (with-output-to-temp-buffer "*Knowledge Gaps Analysis*"
              (princ (format "Knowledge gaps analysis for: %s\n\n%s" title gaps)))
          (message "Failed to analyze knowledge gaps"))))))

(defun my/ai-homelab-advisor ()
  "Get specific homelab advice based on current note context."
  (interactive)
  (if (not (org-roam-file-p))
      (message "This function only works in org-roam files")
    (let* ((context (my/ai-get-note-context))
           (title (alist-get 'title context))
           (domain (alist-get 'domain context))
           (content (alist-get 'content context))
           (similar-notes (my/ai-get-similar-notes-context title 3))
           (infrastructure-context (mapconcat
                                   (lambda (note)
                                     (format "%s" (car note)))
                                   similar-notes ", "))
           (prompt (format "As a homelab expert, provide specific advice for this situation:

Current focus: %s
Domain: %s
Context: %s

Related infrastructure: %s

Provide practical, actionable advice including:
1. Best practices for this specific scenario
2. Common pitfalls to avoid
3. Recommended tools or approaches
4. Security considerations if applicable
5. Performance optimization tips

Focus on real-world homelab experience and practical implementation."
                          title domain content infrastructure-context)))

      (let ((advice (my/ai-call-ollama-generate prompt)))
        (if advice
            (with-output-to-temp-buffer "*Homelab Advisor*"
              (princ (format "Homelab advice for: %s\n\n%s" title advice)))
          (message "Failed to get homelab advice"))))))

;;; System Status and Debugging

(defun my/ai-system-status ()
  "Check AI system status and capabilities."
  (interactive)
  (message "Checking AI system status...")
  (let* ((vector-files (org-roam-list-files))
         (embedding-count (length (seq-filter 'my/note-has-embedding-p vector-files)))
         (total-files (length vector-files))
         (coverage (if (> total-files 0)
                      (/ (* 100.0 embedding-count) total-files)
                    0))
         (test-response (my/ai-call-ollama-generate "Test: respond with exactly 'AI system operational'")))

    (message "AI System Status | Vector Coverage: %d/%d (%.1f%%) | AI Response: %s"
             embedding-count total-files coverage
             (if (and test-response (string-match "operational" test-response)) "✅" "❌"))))

;;; Auto-embedding updates after AI modifications

(defun my/ai-maybe-update-embedding-after-enhancement ()
  "Update embedding if the buffer was significantly modified by AI."
  (when (and (org-roam-file-p)
             (buffer-modified-p)
             (> (buffer-size) 500)) ; Only for substantial notes
    ;; Add a small delay to avoid updating during active editing
    (run-with-timer 3 nil
                    (lambda (file)
                      (when (file-exists-p file)
                        (my/generate-embedding-for-note file)))
                    (buffer-file-name))))

;; Hook to auto-update embeddings after AI enhancements
(add-hook 'after-save-hook 'my/ai-maybe-update-embedding-after-enhancement)

;;; Key Bindings

(defvar my/ai-assistant-map (make-sparse-keymap)
  "Keymap for AI assistant functions.")

;; Global keybindings for AI functions
(global-set-key (kbd "C-c a f") 'my/ai-flesh-out-with-vector-context)
(global-set-key (kbd "C-c a e") 'my/ai-quick-explain)
(global-set-key (kbd "C-c a p") 'my/ai-improve-paragraph)
(global-set-key (kbd "C-c a s") 'my/ai-suggest-connections)
(global-set-key (kbd "C-c a g") 'my/ai-find-knowledge-gaps)
(global-set-key (kbd "C-c a h") 'my/ai-homelab-advisor)
(global-set-key (kbd "C-c a ?") 'my/ai-system-status)

;;; Interactive Setup

(defun my/ai-assistant-setup-check ()
  "Interactive setup check for AI assistant."
  (interactive)
  (message "Checking AI Assistant setup...")

  ;; Check dependencies
  (if (featurep 'org-roam-vector-search)
      (message "✅ org-roam-vector-search loaded")
    (message "❌ org-roam-vector-search not found - load it first"))

  ;; Check Ollama connectivity
  (let ((response (my/ai-call-ollama-generate "test")))
    (if response
        (message "✅ Ollama connection working")
      (message "❌ Ollama connection failed - check server")))

  ;; Check embedding coverage
  (my/ai-system-status))

(provide 'org-roam-ai-assistant)

;;; org-roam-ai-assistant.el ends here
